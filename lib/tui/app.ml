open Lwt.Syntax

(* ---------- Lwt_mvar-based state cell ---------- *)
(* Synchronous peek: safe because Lwt is cooperative and the mvar
   always has a value (take resolves immediately, put resolves immediately
   on the now-empty mvar). No yield point between take and put. *)
let peek mvar =
  let p = Lwt_mvar.take mvar in
  match Lwt.poll p with
  | Some v -> Lwt.ignore_result (Lwt_mvar.put mvar v); Some v
  | None -> Lwt.cancel p; None

let update mvar f =
  let* v = Lwt_mvar.take mvar in
  Lwt_mvar.put mvar (f v)

let ncpu =
  try int_of_string (String.trim (Sys.getenv "URME_WORKERS"))
  with _ ->
    try
      let ic = Unix.open_process_in "sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null" in
      let n = try int_of_string (String.trim (input_line ic)) with _ -> 4 in
      ignore (Unix.close_process_in ic); max 2 n
    with _ -> 4

(* Worker pool: N workers pull from a shared queue — no barrier stalls.
   Lwt is cooperative so Queue access between yield points is safe. *)
let iter_workers ~n f items =
  let q = Queue.create () in
  List.iter (fun x -> Queue.push x q) items;
  let worker () =
    let rec loop () =
      match Queue.take_opt q with
      | None -> Lwt.return_unit
      | Some item -> let* () = f item in loop ()
    in loop ()
  in
  Lwt.join (List.init n (fun _ -> worker ()))

let iteri_workers ~n f items =
  let q = Queue.create () in
  List.iteri (fun i x -> Queue.push (i, x) q) items;
  let worker () =
    let rec loop () =
      match Queue.take_opt q with
      | None -> Lwt.return_unit
      | Some (i, item) -> let* () = f i item in loop ()
    in loop ()
  in
  Lwt.join (List.init n (fun _ -> worker ()))

(* Sort file paths by size descending — biggest first for better parallelism *)
let sort_by_size_desc paths =
  let with_size = List.filter_map (fun p ->
    try Some (p, (Unix.stat p).Unix.st_size)
    with _ -> Some (p, 0)) paths in
  List.sort (fun (_, a) (_, b) -> Int.compare b a) with_size
  |> List.map fst

(* ---------- Types ---------- *)

type ui_mode = Git | History


type conv_entry =
  | User_msg of string
  | Assistant_text of string
  | Thinking_block of string
  | Tool_use_block of {
      tool_name : string;
      tool_use_id : string;
      input : Yojson.Safe.t;
      inline_diff : string option;
      timestamp : float;
    }
  | Tool_result_block of {
      tool_name : string;
      content : string;
      is_error : bool;
    }
  | System_info of string
  | Turn_separator

type git_focus = Branches | Commits | Files | Links

type git_conv_link = Urme_engine.Git_link_types.link

type git_state = {
  branches : string list;
  current_branch : string;
  commits : (string * float * string) list;
  files : string list;
  focus : git_focus;
  branch_idx : int;
  commit_idx : int;
  file_idx : int;
  link_idx : int;
  diff_preview : string;
  diff_scroll_git : int;
  file_diff_filter : string option;
  git_links : (string * string, git_conv_link list) Hashtbl.t;
    (* (commit_sha, file_basename) → links, multiple edits may map to same commit+file *)
  human_edits : (string * string, bool) Hashtbl.t;
    (* (commit_sha, file_basename) → true if human also edited *)
  human_diffs : (string * string, string) Hashtbl.t;
    (* (commit_sha, file_basename) → human-only diff text *)
  link_candidates : git_conv_link list;
}

type history_state = {
  sessions : string list;           (* JSONL file paths, newest first *)
  session_idx : int;                (* which session file *)
  turns : conv_entry list list;     (* turns for current session *)
  turn_idx : int;                   (* which turn within session *)
  hist_scroll : int;
  return_mode : ui_mode;
  search_active : bool;
  search_query : string;
  search_results : (string * string * string * int * string * float) list;
    (* session_id, user_text, document, interaction_index, timestamp, distance *)
  result_idx : int;
  showing_results : bool;
}

let empty_git_state = {
  branches = []; current_branch = ""; commits = []; files = [];
  focus = Branches; branch_idx = 0; commit_idx = 0; file_idx = 0;
  link_idx = 0; diff_preview = ""; diff_scroll_git = 0;
  file_diff_filter = None; git_links = Hashtbl.create 0;
  human_edits = Hashtbl.create 0; human_diffs = Hashtbl.create 0; link_candidates = [];
}

let empty_history_state = {
  sessions = []; session_idx = 0; turns = []; turn_idx = 0;
  hist_scroll = 0; return_mode = History;
  search_active = false; search_query = "";
  search_results = []; result_idx = 0; showing_results = false;
}

(* Split a flat conv_entry list into turns, each starting with a User_msg *)
let split_into_turns entries =
  let turns = ref [] in
  let current = ref [] in
  List.iter (fun e ->
    match e with
    | User_msg _ ->
      if !current <> [] then turns := List.rev !current :: !turns;
      current := [e]
    | Turn_separator -> ()  (* skip separators *)
    | _ -> current := e :: !current
  ) entries;
  if !current <> [] then turns := List.rev !current :: !turns;
  List.rev !turns

(* Filter out internal Claude CLI noise from text *)
let is_internal_noise s =
  let s = String.trim s in
  s = "" ||
  (let has_prefix p = String.length s >= String.length p &&
    String.sub s 0 (String.length p) = p in
   has_prefix "<local-command-caveat>" ||
   has_prefix "<system-reminder>" ||
   has_prefix "Unknown skill:" ||
   has_prefix "<" ||
   has_prefix "<command-" ||
   has_prefix "</")

(* Strip XML-like tags from text for display *)
let strip_xml_tags s =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  let len = String.length s in
  while !i < len do
    if s.[!i] = '<' then begin
      (* Skip until closing > *)
      let j = ref (!i + 1) in
      while !j < len && s.[!j] <> '>' do incr j done;
      if !j < len then i := !j + 1
      else (Buffer.add_char buf s.[!i]; incr i)
    end else begin
      Buffer.add_char buf s.[!i]; incr i
    end
  done;
  let result = Buffer.contents buf in
  let result = String.trim result in
  result


(* ---------- Command palette ---------- *)

type command_info = { cmd : string; description : string }

let builtin_commands = [
  { cmd = "/exit";     description = "Exit urme" };
  { cmd = "/git";      description = "Open git browser" };
  { cmd = "/history";  description = "Browse past conversations" };
  { cmd = "/verbose";  description = "Toggle verbose mode" };
]

(* Load custom commands from ~/.config/urme/commands.json
   Format: [{"cmd": "/foo", "description": "Do foo"}, ...] *)
let load_custom_commands () =
  let path = Filename.concat (Urme_core.Config.config_dir ()) "commands.json" in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content = In_channel.input_all ic in
      close_in ic;
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      json |> to_list |> List.filter_map (fun entry ->
        try
          let cmd = entry |> member "cmd" |> to_string in
          let description = entry |> member "description" |> to_string in
          let cmd = if String.length cmd > 0 && cmd.[0] <> '/' then "/" ^ cmd else cmd in
          Some { cmd; description }
        with _ -> None)
    with _ -> []
  else []

let commands = lazy (
  let custom = load_custom_commands () in
  let builtin_names = List.map (fun c -> c.cmd) builtin_commands in
  let custom_new = List.filter (fun c ->
    not (List.mem c.cmd builtin_names)) custom in
  let all = builtin_commands @ custom_new in
  List.sort (fun a b -> String.compare a.cmd b.cmd) all
)

let filter_commands query =
  let q = String.lowercase_ascii query in
  List.filter (fun c ->
    let lc = String.lowercase_ascii c.cmd in
    let len = String.length q in
    if len = 0 then true
    else
      let rec has_sub i =
        if i + len > String.length lc then false
        else String.sub lc i len = q || has_sub (i + 1)
      in
      has_sub 0
  ) (Lazy.force commands)

(* Fully immutable state record *)
type state = {
  mode : ui_mode;
  input_text : string;
  config : Urme_core.Config.config;
  project_dir : string;
  status_extra : string;
  verbose : bool;
  palette_open : bool;
  palette_selected : int;
  git : git_state;
  history : history_state;
  usage : Urme_core.Usage.usage_data option;
  started_chroma : bool;
  started_ollama : bool;
  tui_bridge : Tui_bridge.t option;
}

(* ---------- Helpers ---------- *)


let initial_state ~config ~project_dir = {
  mode = History;
  input_text = "";
  config; project_dir;
  status_extra = "Ready";
  verbose = false;
  palette_open = false; palette_selected = 0;
  git = empty_git_state; history = empty_history_state;
  usage = None;
  started_chroma = false; started_ollama = false;
  tui_bridge = None;
}


(* ---------- Terminal ref + redraw (forward-declared, filled in after render_screen) ---------- *)

let term_ref : Notty_lwt.Term.t option ref = ref None

(* redraw is defined after render_screen; use a ref for forward reference *)
let redraw_ref : (state Lwt_mvar.t -> unit) ref = ref (fun _ -> ())
let redraw mvar = !redraw_ref mvar

(* ---------- Colors (from config theme) ---------- *)

let _theme = (Urme_core.Config.load ()).theme
let lc (c : Urme_core.Config.rgb) = Notty.A.rgb_888 ~r:c.r ~g:c.g ~b:c.b
let c_bg = lc _theme.bg
let c_fg = lc _theme.fg
let c_border = lc _theme.border
let c_user = lc _theme.user
let c_assistant = lc _theme.assistant
let c_status_bg = lc _theme.status_bg
let c_input_bg = lc _theme.input_bg
let c_highlight = lc _theme.highlight
let c_thinking = lc _theme.thinking
let c_tool_name = lc _theme.tool_name
let c_selection_bg = lc _theme.selection_bg
let c_tool_result = lc _theme.tool_result
let c_error = lc _theme.error
let c_diff_add_fg = lc _theme.diff_add_fg
let c_diff_add_bg = lc _theme.diff_add_bg
let c_diff_del_fg = lc _theme.diff_del_fg
let c_diff_del_bg = lc _theme.diff_del_bg
let c_separator = lc _theme.separator

(* ---------- UTF-8 / drawing ---------- *)

let sanitize_utf8 s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      let byte = Char.code s.[i] in
      if byte < 0x80 then begin
        (* Replace control chars (below 0x20) with space, except allow nothing below *)
        if byte >= 0x20 || byte = 0x09 then  (* allow tab *)
          Buffer.add_char buf s.[i]
        else
          Buffer.add_char buf ' ';
        loop (i + 1)
      end else
        let expected =
          if byte land 0xE0 = 0xC0 then 2
          else if byte land 0xF0 = 0xE0 then 3
          else if byte land 0xF8 = 0xF0 then 4
          else 0 in
        if expected = 0 || i + expected > len then begin
          Buffer.add_char buf '?'; loop (i + 1)
        end else
          let rec valid j =
            j >= expected || (Char.code s.[i + j] land 0xC0 = 0x80 && valid (j + 1))
          in
          if valid 1 then begin
            Buffer.add_string buf (String.sub s i expected);
            loop (i + expected)
          end else begin
            Buffer.add_char buf '?'; loop (i + 1)
          end
  in
  loop 0

let wrap_text text width =
  if width <= 0 then [""]
  else
    String.split_on_char '\n' text
    |> List.concat_map (fun line ->
      if String.length line <= width then [line]
      else
        let rec split acc pos =
          if pos >= String.length line then List.rev acc
          else
            let len = min width (String.length line - pos) in
            split (String.sub line pos len :: acc) (pos + len)
        in
        split [] 0)


(* Parse a session JSONL file into conv_entry list *)
let load_session_entries filepath =
  let entries = ref [] in
  let ic = open_in filepath in
  (try while true do
    let line = input_line ic in
    (try
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      let typ = json |> member "type" |> to_string_option
                |> Option.value ~default:"" in
      (match typ with
       | "user" ->
         let content = json |> member "message" |> member "content" in
         (match content with
          | `String s ->
            if not (is_internal_noise s) then
              entries := User_msg (strip_xml_tags s) :: !entries
          | `List items ->
            List.iter (fun item ->
              match item |> member "type" |> to_string_option with
              | Some "tool_result" ->
                let tid = item |> member "tool_use_id" |> to_string_option
                          |> Option.value ~default:"" in
                let c = (try item |> member "content" |> to_string
                         with _ -> "(result)") in
                let is_err = (try item |> member "is_error" |> to_bool
                              with _ -> false) in
                if not (is_internal_noise c) then
                  entries := Tool_result_block {
                    tool_name = tid; content = c; is_error = is_err } :: !entries
              | Some "text" ->
                let t = item |> member "text" |> to_string_option
                        |> Option.value ~default:"" in
                if not (is_internal_noise t) then
                  entries := User_msg (strip_xml_tags t) :: !entries
              | _ -> ()
            ) items
          | _ -> ())
       | "assistant" ->
         let ts_str = json |> member "timestamp" |> to_string_option
                      |> Option.value ~default:"" in
         let ts = Urme_core.Types.iso8601_to_epoch ts_str in
         let blocks = json |> member "message" |> member "content"
           |> (fun j -> try to_list j with _ -> []) in
         List.iter (fun block ->
           match block |> member "type" |> to_string_option with
           | Some "text" ->
             let t = block |> member "text" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then entries := Assistant_text t :: !entries
           | Some "thinking" ->
             let t = block |> member "thinking" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then entries := Thinking_block t :: !entries
           | Some "tool_use" ->
             let name = block |> member "name" |> to_string_option
                        |> Option.value ~default:"tool" in
             let tuid = block |> member "id" |> to_string_option
                        |> Option.value ~default:"" in
             let input = (try block |> member "input" with _ -> `Null) in
             let inline_diff = None in
             entries := Tool_use_block { tool_name = name; tool_use_id = tuid;
                                         input; inline_diff; timestamp = ts } :: !entries
           | _ -> ()
         ) blocks
       | "result" ->
         entries := Turn_separator :: !entries
       | _ -> ())
     with _ -> ())
  done with End_of_file -> ());
  close_in ic;
  List.rev !entries

(* Split session into interaction-aligned turns by parsing JSONL directly.
   Each turn = one real user interaction (type=user, content=String -> assistant response).
   Esc/cancel and tool result messages don't start new turns. *)
let split_into_interaction_turns ~filepath =
  let turns = ref [] in
  let current = ref [] in
  let flush_turn () =
    if !current <> [] then turns := List.rev !current :: !turns;
    current := [] in
  let ic = open_in filepath in
  (try while true do
    let line = input_line ic in
    (try
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      let typ = json |> member "type" |> to_string_option
                |> Option.value ~default:"" in
      (match typ with
       | "user" ->
         let content = json |> member "message" |> member "content" in
         (match content with
          | `String s ->
            (* Real user message — start new turn *)
            if not (is_internal_noise s) then begin
              flush_turn ();
              current := [User_msg (strip_xml_tags s)]
            end
          | `List items ->
            (* Tool result / continuation — accumulate into current turn *)
            List.iter (fun item ->
              match item |> member "type" |> to_string_option with
              | Some "tool_result" ->
                let tid = item |> member "tool_use_id" |> to_string_option
                          |> Option.value ~default:"" in
                let c = (try item |> member "content" |> to_string
                         with _ -> "(result)") in
                let is_err = (try item |> member "is_error" |> to_bool
                              with _ -> false) in
                if not (is_internal_noise c) then
                  current := Tool_result_block {
                    tool_name = tid; content = c; is_error = is_err } :: !current
              | Some "text" ->
                let t = item |> member "text" |> to_string_option
                        |> Option.value ~default:"" in
                if not (is_internal_noise t) then
                  current := System_info (strip_xml_tags t) :: !current
              | _ -> ()
            ) items
          | _ -> ())
       | "assistant" ->
         let ts_str = json |> member "timestamp" |> to_string_option
                      |> Option.value ~default:"" in
         let ts = Urme_core.Types.iso8601_to_epoch ts_str in
         let blocks = json |> member "message" |> member "content"
           |> (fun j -> try to_list j with _ -> []) in
         List.iter (fun block ->
           match block |> member "type" |> to_string_option with
           | Some "text" ->
             let t = block |> member "text" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then current := Assistant_text t :: !current
           | Some "thinking" ->
             let t = block |> member "thinking" |> to_string_option
                     |> Option.value ~default:"" in
             if t <> "" then current := Thinking_block t :: !current
           | Some "tool_use" ->
             let name = block |> member "name" |> to_string_option
                        |> Option.value ~default:"tool" in
             let tuid = block |> member "id" |> to_string_option
                        |> Option.value ~default:"" in
             let input = (try block |> member "input" with _ -> `Null) in
             let inline_diff = None in
             current := Tool_use_block { tool_name = name; tool_use_id = tuid;
                                         input; inline_diff; timestamp = ts } :: !current
           | _ -> ()
         ) blocks
       | _ -> ())
    with _ -> ())
  done with End_of_file -> ());
  flush_turn ();
  close_in ic;
  List.rev !turns

let truncate_result content =
  let lines = String.split_on_char '\n' content in
  let total = List.length lines in
  if total <= 20 && String.length content <= 1000 then content
  else
    let kept = List.filteri (fun i _ -> i < 20) lines in
    let text = String.concat "\n" kept in
    let text = if String.length text > 1000
      then String.sub text 0 1000 ^ "..." else text in
    if total > 20 then
      text ^ Printf.sprintf "\n... (%d lines omitted)" (total - 20)
    else text

(* ---------- Rendering ---------- *)

type styled_line = {
  fg : Notty.A.color;
  bg : Notty.A.color option;
  text : string;
}

let render_entry ~width ~verbose entry =
  let mk ?(bg=None) fg text = { fg; bg; text } in
  let wrap_prefix prefix fg lines =
    List.mapi (fun i line ->
      mk fg (if i = 0 then prefix ^ line
             else String.make (String.length prefix) ' ' ^ line)
    ) lines
  in
  match entry with
  | User_msg text ->
    wrap_prefix "> " c_user (wrap_text text (width - 2)) @ [mk c_fg ""]
  | Assistant_text text ->
    wrap_prefix "  " c_assistant (wrap_text text (width - 2))
  | Thinking_block text ->
    if verbose then wrap_prefix "  [thinking] " c_thinking (wrap_text text (width - 14))
    else []
  | Tool_use_block { tool_name; inline_diff; _ } ->
    let header = Printf.sprintf "  [%s]" tool_name in
    let hdr = List.map (mk c_tool_name) (wrap_text header width) in
    let diff = match inline_diff with
      | None -> []
      | Some diff ->
        let lines = String.split_on_char '\n' diff in
        let lines = if List.length lines > 30 then
          List.filteri (fun i _ -> i < 30) lines @
          [Printf.sprintf "... (%d more lines)" (List.length lines - 30)]
        else lines in
        let parse_hunk line =
          (* Parse @@ -old_start,old_len +new_start,new_len @@ *)
          try Scanf.sscanf line "@@ -%d,%d +%d,%d @@%_s"
                (fun o _ n _ -> Some (o, n))
          with _ ->
            try Scanf.sscanf line "@@ -%d +%d,%d @@%_s"
                  (fun o n _ -> Some (o, n))
            with _ ->
              try Scanf.sscanf line "@@ -%d,%d +%d @@%_s"
                    (fun o _ n -> Some (o, n))
              with _ -> None
        in
        let old_ln = ref 0 in
        let new_ln = ref 0 in
        List.map (fun line ->
          if String.length line > 0 then match line.[0] with
            | '@' ->
              (match parse_hunk line with
               | Some (o, n) -> old_ln := o; new_ln := n
               | None -> ());
              mk c_tool_name ("    " ^ line)
            | '+' ->
              let prefix = Printf.sprintf "    %4d " !new_ln in
              new_ln := !new_ln + 1;
              mk ~bg:(Some c_diff_add_bg) c_diff_add_fg (prefix ^ line)
            | '-' ->
              let prefix = Printf.sprintf " %4d    " !old_ln in
              old_ln := !old_ln + 1;
              mk ~bg:(Some c_diff_del_bg) c_diff_del_fg (prefix ^ line)
            | _ ->
              let prefix = Printf.sprintf " %4d %4d " !old_ln !new_ln in
              old_ln := !old_ln + 1; new_ln := !new_ln + 1;
              mk c_fg (prefix ^ line)
          else mk c_fg ""
        ) lines
    in
    hdr @ diff
  | Tool_result_block { content; is_error; _ } ->
    let color = if is_error then c_error else c_tool_result in
    List.map (fun l -> mk color ("    " ^ l))
      (wrap_text (truncate_result content) (width - 4)) @ [mk c_fg ""]
  | System_info text ->
    wrap_prefix "  " c_error (wrap_text text (width - 2))
  | Turn_separator ->
    [mk c_separator ("  " ^ String.make (min 40 (width - 2)) '-'); mk c_fg ""]

(* ---------- Notty image builders ---------- *)

(* Local infix operators to avoid opening Notty.I (which shadows width, height, pad, empty, string) *)
let ( <|> ) = Notty.I.( <|> )
let ( <-> ) = Notty.I.( <-> )
let ( </> ) = Notty.I.( </> )

(* Build a single row image of exactly width w, with given attr *)
let mk_row ~attr w text =
  let text = sanitize_utf8 text in
  let len = String.length text in
  if len = 0 then
    Notty.I.char attr ' ' w 1
  else
    let display_len = min len w in
    let text = if display_len < len then String.sub text 0 display_len else text in
    let pad_n = max 0 (w - display_len) in
    let txt_img = (try Notty.I.string attr text
      with _ -> Notty.I.string attr (String.make display_len '?')) in
    if pad_n > 0 then txt_img <|> Notty.I.char attr ' ' pad_n 1
    else txt_img

(* Build an empty background block *)
let bg_block w h =
  Notty.I.char Notty.A.(bg c_bg) ' ' w h

(* Render the status bar (1 row) *)
let render_status_bar state w =
  let sbg = Notty.A.(fg c_fg ++ bg c_status_bg) in
  let mode_tag = match state.mode with
    | Git -> "GIT" | History -> "HIST" in
  let left = Printf.sprintf " [%s] urme" mode_tag in
  let right_str = match state.config.plan, state.usage with
    | (Urme_core.Config.Pro | Urme_core.Config.Max), Some u ->
      Urme_core.Usage.format_status_bar u
    | _ -> ""
  in
  (* Build the bar: left portion (bold highlight), center (status_extra), right *)
  let left_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let left_img = mk_row ~attr:left_attr (String.length left) left in
  let right_len = String.length right_str in
  let center_start = max (String.length left + 2)
      ((w - String.length state.status_extra) / 2) in
  let center_end = center_start + String.length state.status_extra in
  let right_start = max 0 (w - right_len - 1) in
  (* Build as a single row: pad left, center text, right text *)
  let gap1 = max 0 (center_start - String.length left) in
  let gap2 = max 0 (right_start - center_end) in
  let remaining = max 0 (w - right_start - right_len) in
  let open Notty.I in
  left_img
  <|> char sbg ' ' gap1 1
  <|> (try string sbg (sanitize_utf8 state.status_extra) with _ -> empty)
  <|> char sbg ' ' gap2 1
  <|> (if right_len > 0 then
         (try string sbg (sanitize_utf8 right_str) with _ -> empty)
       else empty)
  <|> char sbg ' ' remaining 1

(* Render the conversation pane *)
(* ---------- Git mode rendering ---------- *)

let render_git_panel ~height ~title ~focused ~items ~sel_idx ~width =
  if height <= 0 then Notty.I.empty
  else begin
    let hdr_bg = if focused then c_highlight else c_status_bg in
    let hdr_fg = if focused then c_bg else c_highlight in
    let hdr_attr = Notty.A.(fg hdr_fg ++ bg hdr_bg ++ st bold) in
    let hdr_text = Printf.sprintf " %s %s" title
        (String.make (max 0 (width - String.length title - 2)) ' ') in
    let hdr_row = mk_row ~attr:hdr_attr width hdr_text in
    let content_h = max 0 (height - 1) in
    let scroll = max 0 (sel_idx - content_h + 1) in
    let rows = List.mapi (fun i (label, is_current, is_dim) ->
      let vi = i - scroll in
      if vi >= 0 && vi < content_h then begin
        let selected = focused && i = sel_idx in
        let fg_c = if selected then c_highlight
                 else if is_dim then c_separator
                 else if is_current then c_user
                 else c_fg in
        let bg_c = if selected then c_status_bg else c_bg in
        let label_trunc = if String.length label > width - 1
          then String.sub label 0 (width - 1) else label in
        let attr = Notty.A.(fg fg_c ++ bg bg_c ++ (if selected then st bold else empty)) in
        Some (mk_row ~attr width label_trunc)
      end else None
    ) items in
    let visible_rows = List.filter_map Fun.id rows in
    let content_img = match visible_rows with
      | [] -> bg_block width content_h
      | _ ->
        let img = Notty.I.vcat visible_rows in
        let used = Notty.I.height img in
        if used < content_h then
          img <-> bg_block width (content_h - used)
        else img in
    hdr_row <-> content_img
  end

let commit_has_links g sha =
  Hashtbl.fold (fun (s, _) _ found -> found || s = sha) g.git_links false

let next_linked_commit g from =
  let n = List.length g.commits in
  let rec find i =
    if i >= n then from
    else match List.nth_opt g.commits i with
      | Some (sha, _, _) when commit_has_links g sha -> i
      | _ -> find (i + 1)
  in find (from + 1)

let prev_linked_commit g from =
  let rec find i =
    if i < 0 then from
    else match List.nth_opt g.commits i with
      | Some (sha, _, _) when commit_has_links g sha -> i
      | _ -> find (i - 1)
  in find (from - 1)

let file_has_links g f =
  let sha = match List.nth_opt g.commits g.commit_idx with
    | Some (s, _, _) -> s | None -> "" in
  Hashtbl.mem g.git_links (sha, Filename.basename f)

let next_linked_file g from =
  let n = List.length g.files in
  let rec find i =
    if i >= n then from
    else match List.nth_opt g.files i with
      | Some f when file_has_links g f -> i
      | _ -> find (i + 1)
  in find (from + 1)

let prev_linked_file g from =
  let rec find i =
    if i < 0 then from
    else match List.nth_opt g.files i with
      | Some f when file_has_links g f -> i
      | _ -> find (i - 1)
  in find (from - 1)

let render_git_left_panels state w h =
  let g = state.git in
  let total_h = h in
  let width = w in
  let branch_items = List.map (fun b ->
    let prefix = if b = g.current_branch then "* " else "  " in
    (prefix ^ b, b = g.current_branch, false)
  ) g.branches in
  let commit_items = List.map (fun (sha, _ts, msg) ->
    let short_sha = if String.length sha >= 7 then String.sub sha 0 7 else sha in
    let w' = max 1 (width - 10) in
    let msg_trunc = if String.length msg > w' then String.sub msg 0 w' else msg in
    let has_links = Hashtbl.fold (fun (s, _) _ found ->
      found || s = sha) g.git_links false in
    (Printf.sprintf " %s %s" short_sha msg_trunc, false, not has_links)
  ) g.commits in
  let cur_sha = match List.nth_opt g.commits g.commit_idx with
    | Some (sha, _, _) -> sha | None -> "" in
  let file_items = List.map (fun f ->
    let fb = Filename.basename f in
    let has_links = Hashtbl.mem g.git_links (cur_sha, fb) in
    let has_human = Hashtbl.mem g.human_edits (cur_sha, fb) in
    let is_human = not has_links || has_human in
    ("  " ^ f, is_human, false)
  ) g.files in
  let sorted_links = List.sort (fun (a : git_conv_link) (b : git_conv_link) ->
    let c = Int.compare a.turn_idx b.turn_idx in
    if c <> 0 then c else Int.compare a.entry_idx b.entry_idx
  ) g.link_candidates in
  let link_items = List.map (fun (link : git_conv_link) ->
    let is_human_only = let ek = link.edit_key in
      String.length ek > 11 &&
      String.sub ek (String.length ek - 11) 11 = ":human-only" in
    let is_human_mod = let ek = link.edit_key in
      String.length ek > 6 &&
      String.sub ek (String.length ek - 6) 6 = ":human" in
    let is_human = is_human_only || is_human_mod in
    if is_human_only then
      (" H human edit", true, false)
    else
      let sid = if String.length link.session_id > 4
        then String.sub link.session_id 0 4 else link.session_id in
      let marker = if is_human_mod then "H" else " " in
      (Printf.sprintf "%s%s t%d e%d" marker sid link.turn_idx link.entry_idx,
       is_human, false)
  ) sorted_links in
  let panels = [
    ("Branches", g.focus = Branches, branch_items, g.branch_idx);
    ("Commits", g.focus = Commits, commit_items, g.commit_idx);
    ("Files", g.focus = Files, file_items, g.file_idx);
    (Printf.sprintf "Links (%d)" (List.length g.link_candidates),
     g.focus = Links, link_items, g.link_idx);
  ] in
  let n_panels = List.length panels in
  let focused_idx = match g.focus with
    | Branches -> 0 | Commits -> 1 | Files -> 2 | Links -> 3 in
  let focused_h = max 3 (total_h / 2) in
  let rest_h = max 0 (total_h - focused_h) in
  let other_h = if n_panels > 1 then max 2 (rest_h / (n_panels - 1)) else 0 in
  let heights = List.mapi (fun i _ ->
    if i = focused_idx then focused_h else other_h
  ) panels in
  let sum_h = List.fold_left (+) 0 heights in
  let heights = if sum_h <> total_h then
    let diff = total_h - sum_h in
    List.mapi (fun i h -> if i = focused_idx then h + diff else h) heights
  else heights in
  let panel_images = List.mapi (fun i (title, focused, items, sel_idx) ->
    let panel_h = List.nth heights i in
    render_git_panel ~height:panel_h ~title ~focused
      ~items ~sel_idx ~width
  ) panels in
  Notty.I.vcat panel_images

let render_git_diff state w h =
  let g = state.git in
  let header = match g.file_diff_filter with
    | Some f -> Printf.sprintf " Diff: %s " f
    | None -> " Diff " in
  let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let title_row = mk_row ~attr:title_attr w
    (Printf.sprintf "%s%s" header
       (String.make (max 0 (w - String.length header)) ' ')) in
  let content_h = max 0 (h - 1) in
  let diff_text = match g.file_diff_filter with
    | None -> g.diff_preview
    | Some file ->
      let lines = String.split_on_char '\n' g.diff_preview in
      let file_base = Filename.basename file in
      let in_section = ref false in
      let result = ref [] in
      List.iter (fun line ->
        if String.length line > 11 && String.sub line 0 10 = "diff --git" then begin
          let parts = String.split_on_char ' ' line in
          match List.rev parts with
          | last :: _ ->
            let f = if String.length last > 2 && last.[0] = 'b' && last.[1] = '/'
              then String.sub last 2 (String.length last - 2) else last in
            in_section := Filename.basename f = file_base
          | [] -> in_section := false
        end;
        if !in_section then result := line :: !result
      ) lines;
      String.concat "\n" (List.rev !result) in
  let lines = String.split_on_char '\n' diff_text in
  let parse_hunk line =
    try Scanf.sscanf line "@@ -%d,%d +%d,%d @@%_s"
          (fun o _ n _ -> Some (o, n))
    with _ ->
      try Scanf.sscanf line "@@ -%d +%d,%d @@%_s"
            (fun o n _ -> Some (o, n))
      with _ ->
        try Scanf.sscanf line "@@ -%d,%d +%d @@%_s"
              (fun o _ n -> Some (o, n))
        with _ -> None
  in
  let old_ln = ref 0 in
  let new_ln = ref 0 in
  let numbered = List.map (fun line ->
    if String.length line > 0 then match line.[0] with
      | '@' ->
        (match parse_hunk line with
         | Some (o, n) -> old_ln := o; new_ln := n
         | None -> ());
        (line, Notty.A.cyan, c_bg)
      | '+' ->
        let prefix = Printf.sprintf "%4d " !new_ln in
        new_ln := !new_ln + 1;
        (prefix ^ line, c_diff_add_fg, c_diff_add_bg)
      | '-' ->
        let prefix = Printf.sprintf "%4d " !old_ln in
        old_ln := !old_ln + 1;
        (prefix ^ line, c_diff_del_fg, c_diff_del_bg)
      | _ ->
        let prefix = Printf.sprintf "%4d " !new_ln in
        old_ln := !old_ln + 1; new_ln := !new_ln + 1;
        (prefix ^ line, c_fg, c_bg)
    else ("", c_fg, c_bg)
  ) lines in
  let scroll = g.diff_scroll_git in
  let visible = List.filteri (fun i _ -> i >= scroll && i < scroll + content_h) numbered in
  let rows = List.map (fun (text, fg_c, bg_c) ->
    let attr = Notty.A.(fg fg_c ++ bg bg_c) in
    mk_row ~attr w (" " ^ text)
  ) visible in
  let content_img = match rows with
    | [] -> bg_block w content_h
    | _ ->
      let img = Notty.I.vcat rows in
      let used = Notty.I.height img in
      if used < content_h then
        img <-> bg_block w (content_h - used)
      else img in
  title_row <-> content_img

(* ---------- History mode rendering ---------- *)

let render_history_results state w h =
  let hs = state.history in
  let title = Printf.sprintf " Search: \"%s\" (%d results)"
      hs.search_query (List.length hs.search_results) in
  let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
  let title_row = mk_row ~attr:title_attr w
    (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
  let content_h = max 0 (h - 1) in
  if hs.search_results = [] then
    let empty_msg = mk_row ~attr:Notty.A.(fg c_border ++ bg c_bg) w "  (no results)" in
    let fill = bg_block w (max 0 (content_h - 1)) in
    (title_row <-> empty_msg <-> fill)
  else begin
    let rows = ref [] in
    List.iteri (fun i (session_id, user_text, _doc, _idx, _ts, distance) ->
      if List.length !rows < content_h then begin
        let selected = i = hs.result_idx in
        let marker = if selected then ">" else " " in
        let fg_c = if selected then c_highlight else c_fg in
        let bg_c = if selected then c_selection_bg else c_bg in
        let label = if user_text = "" then "(no text)" else
          let max_len = w - 14 in
          if String.length user_text > max_len
          then String.sub user_text 0 max_len ^ "..."
          else user_text in
        let dist = Printf.sprintf "(%.2f)" distance in
        let line1 = Printf.sprintf " %s %s  %s" marker label dist in
        let line1 = if String.length line1 > w then String.sub line1 0 w else line1 in
        let attr1 = Notty.A.(fg fg_c ++ bg bg_c ++ (if selected then st bold else empty)) in
        rows := mk_row ~attr:attr1 w line1 :: !rows;
        if List.length !rows < content_h then begin
          let sid = if String.length session_id > 8
            then String.sub session_id 0 8 else session_id in
          let line2 = Printf.sprintf "   session: %s" sid in
          let line2 = if String.length line2 > w then String.sub line2 0 w else line2 in
          let attr2 = Notty.A.(fg c_border ++ bg bg_c) in
          rows := mk_row ~attr:attr2 w line2 :: !rows
        end
      end
    ) hs.search_results;
    let content_img = match List.rev !rows with
      | [] -> bg_block w content_h
      | rs ->
        let img = Notty.I.vcat rs in
        let used = Notty.I.height img in
        if used < content_h then
          img <-> bg_block w (content_h - used)
        else img in
    title_row <-> content_img
  end

let render_history_content state w h =
  let hs = state.history in
  if hs.showing_results then
    render_history_results state w h
  else if hs.search_active then begin
    (* Show search input *)
    let title = " Search history" in
    let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
    let title_row = mk_row ~attr:title_attr w
      (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
    let content_h = max 0 (h - 1) in
    let prompt = Printf.sprintf " > %s_" hs.search_query in
    let prompt_attr = Notty.A.(fg c_highlight ++ bg c_bg) in
    let prompt_row = mk_row ~attr:prompt_attr w prompt in
    let gap = bg_block w 1 in  (* blank row before prompt *)
    let fill = bg_block w (max 0 (content_h - 2)) in
    title_row <-> gap <-> prompt_row <-> fill
  end else begin
    let n_turns = List.length hs.turns in
    let n_sessions = List.length hs.sessions in
    let title = if n_sessions = 0 then " History (no sessions)"
      else
        let sid = match List.nth_opt hs.sessions hs.session_idx with
          | Some path -> Filename.basename path |> Filename.chop_extension
          | None -> "?" in
        let short_id = if String.length sid > 8 then String.sub sid 0 8 else sid in
        let result_info = if hs.search_results <> [] && not hs.showing_results then
          Printf.sprintf "  [result %d/%d, b=list, S-arrows=jump]"
            (hs.result_idx + 1) (List.length hs.search_results)
        else "" in
        Printf.sprintf " Session %s  Turn %d/%d  (session %d/%d)%s"
          short_id (hs.turn_idx + 1) n_turns (hs.session_idx + 1) n_sessions result_info in
    let title_attr = Notty.A.(fg c_highlight ++ bg c_status_bg ++ st bold) in
    let title_row = mk_row ~attr:title_attr w
      (Printf.sprintf "%s%s" title (String.make (max 0 (w - String.length title)) ' ')) in
    let content_h = max 0 (h - 1) in
    let current_turn = List.nth_opt hs.turns hs.turn_idx in
    match current_turn with
    | None | Some [] ->
      let empty_msg = mk_row ~attr:Notty.A.(fg c_border ++ bg c_bg) w "  (no conversation loaded)" in
      let fill = bg_block w (max 0 (content_h - 1)) in
      (title_row <-> empty_msg <-> fill)
    | Some entries ->
      let entry_width = max 1 (w - 2) in
      let visible_entries = List.filteri (fun i _ -> i >= hs.hist_scroll) entries in
      let all_lines =
        List.concat_map (render_entry ~width:entry_width ~verbose:true) visible_entries in
      let visible = List.filteri (fun i _ -> i < content_h) all_lines in
      let rows = List.map (fun line ->
        let attr = Notty.A.(fg line.fg ++ bg (match line.bg with Some b -> b | None -> c_bg)) in
        mk_row ~attr w (" " ^ line.text)
      ) visible in
      let content_img = match rows with
        | [] -> bg_block w content_h
        | _ ->
          let img = Notty.I.vcat rows in
          let used = Notty.I.height img in
          if used < content_h then
            img <-> bg_block w (content_h - used)
          else img in
      title_row <-> content_img
  end

(* Render the input line (1 row) with colored key hints *)
(* Each hint is (key, description); rendered as key=highlight desc=dim sep=border *)
let render_hint_bar hints w =
  let key_attr = Notty.A.(fg c_highlight ++ bg c_input_bg ++ st bold) in
  let desc_attr = Notty.A.(fg c_fg ++ bg c_input_bg) in
  let sep_attr = Notty.A.(fg c_border ++ bg c_input_bg) in
  let pad_attr = Notty.A.(fg c_fg ++ bg c_input_bg) in
  let imgs = List.mapi (fun i (key, desc) ->
    let sep = if i > 0 then Notty.I.string sep_attr " | " else Notty.I.string pad_attr " " in
    let k = Notty.I.string key_attr key in
    let d = Notty.I.string desc_attr (" " ^ desc) in
    sep <|> k <|> d
  ) hints in
  let content = List.fold_left ( <|> ) Notty.I.empty imgs in
  let used = Notty.I.width content in
  let pad = max 0 (w - used) in
  content <|> Notty.I.char pad_attr ' ' pad 1

let render_input_line state w =
  let hints = match state.mode with
    | Git ->
      [("Tab", "panel"); ("j/k", "nav"); ("Enter", "select");
       ("[/]", "scroll"); ("h", "history"); ("Esc", "back"); ("q", "quit")]
    | History ->
      let h = state.history in
      if h.showing_results && h.return_mode = Git then
        [("\xe2\x86\x91\xe2\x86\x93", "select"); ("Enter", "view"); ("q", "quit")]
      else if h.showing_results then
        [("\xe2\x86\x91\xe2\x86\x93", "select"); ("Enter", "view"); ("q", "quit")]
      else if h.search_active then
        [("type query...", ""); ("Enter", "search"); ("Esc", "cancel")]
      else if h.return_mode = Git && h.search_results <> [] then
        [("\xe2\x86\x90/\xe2\x86\x92", "step"); ("S-arrows", "jump"); ("b", "list"); ("Esc", "git"); ("\xe2\x86\x91\xe2\x86\x93", "scroll")]
      else if h.search_results <> [] then
        [("\xe2\x86\x90/\xe2\x86\x92", "step"); ("S-arrows", "jump"); ("b", "list"); ("/", "search"); ("q", "quit")]
      else
        [("\xe2\x86\x90/\xe2\x86\x92", "step"); ("/", "search"); ("\xe2\x86\x91\xe2\x86\x93", "scroll"); ("g", "git"); ("i", "init"); ("u", "update"); ("q", "quit")]
  in
  render_hint_bar hints w

(* Render the command palette overlay *)
let render_palette state w h input_row =
  if not state.palette_open then Notty.I.void w h
  else begin
    let candidates = filter_commands state.input_text in
    let n = List.length candidates in
    if n = 0 then Notty.I.void w h
    else begin
      let max_show = min n 6 in
      let box_width = min (w - 2) 50 in
      let box_top = max 1 (input_row - max_show) in
      let sel_bg_c = Notty.A.rgb_888 ~r:50 ~g:50 ~b:90 in
      let norm_bg_c = Notty.A.rgb_888 ~r:35 ~g:35 ~b:60 in
      let sel_attr = Notty.A.(fg c_highlight ++ bg sel_bg_c ++ st bold) in
      let norm_attr = Notty.A.(fg c_fg ++ bg norm_bg_c) in
      let sel = state.palette_selected mod n in
      let scroll_off =
        if sel < max_show then 0
        else sel - max_show + 1 in
      let rows = List.mapi (fun i c ->
        let vi = i - scroll_off in
        if vi >= 0 && vi < max_show then begin
          let selected = i = sel in
          let attr = if selected then sel_attr else norm_attr in
          let label = Printf.sprintf " %-12s %s" c.cmd c.description in
          let label = if String.length label >= box_width
            then String.sub label 0 (box_width - 1) else label in
          let padded = label ^ String.make (max 0 (box_width - String.length label)) ' ' in
          Some (mk_row ~attr box_width padded)
        end else None
      ) candidates in
      let visible_rows = List.filter_map Fun.id rows in
      match visible_rows with
      | [] -> Notty.I.void w h
      | _ ->
        let palette_img = Notty.I.vcat visible_rows in
        (* Position: pad top by box_top rows, left by 1 col *)
        Notty.I.pad ~l:1 ~t:box_top palette_img
    end
  end

(* Main compose function: build the full screen image *)
let render_screen state w h =
  let status = render_status_bar state w in
  let input_row = h - 1 in
  let pane_height = max 1 (h - 2) in
  let content = match state.mode with
    | Git ->
      let left_w = max 1 (w / 4) in
      let right_w = max 1 (w - left_w - 1) in
      let left = render_git_left_panels state left_w pane_height in
      let sep_attr = Notty.A.(fg c_border ++ bg c_bg) in
      let sep = Notty.I.vcat (List.init pane_height (fun _ ->
        Notty.I.string sep_attr "\xe2\x94\x82")) in
      let right = render_git_diff state right_w pane_height in
      left <|> sep <|> right
    | History -> render_history_content state w pane_height in
  let input = render_input_line state w in
  let base = status <-> content <-> input in
  let palette = render_palette state w h input_row in
  base </> palette

(* Now wire up the real redraw *)
let () =
  redraw_ref := fun mvar ->
    match !term_ref, peek mvar with
    | Some term, Some state ->
      let (w, h) = Notty_lwt.Term.size term in
      let img = render_screen state w h in
      Lwt.async (fun () ->
        Lwt.catch (fun () ->
          let open Lwt.Syntax in
          let* () = Notty_lwt.Term.image term img in
          Notty_lwt.Term.cursor term None
        ) (fun _ -> Lwt.return_unit))
    | _ -> ()

(* ---------- Commit-centric git <-> conversation link index ---------- *)

let log_debug msg =
  let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
  Printf.fprintf oc "[%s] %s\n" (string_of_float (Unix.gettimeofday ())) msg;
  close_out oc

let parse_git_info = Urme_engine.Git_link_types.parse_git_info_json

(* update_git_links: delegate to Git_link engine module *)
let update_git_links ~project_dir ~port ~collection_id ?(sessions_filter : string list option) mvar =
  let on_status msg =
    let* () = update mvar (fun s -> { s with status_extra = msg }) in
    redraw mvar; Lwt.return_unit in
  Urme_engine.Git_link.update_index ~project_dir ~port ~collection_id
    ?sessions_filter ~on_status ()

(* ---------- Mode switching helpers ---------- *)

let load_git_data ~cwd =
  let errs = ref [] in
  let p_branches = Lwt.catch
    (fun () -> Urme_git.Ops.run_git ~cwd ["branch"; "--list"; "--no-color"])
    (fun exn -> errs := ("branches: " ^ Printexc.to_string exn) :: !errs; Lwt.return "") in
  let p_cur = Lwt.catch
    (fun () -> Urme_git.Ops.current_branch ~cwd)
    (fun exn -> errs := ("cur_branch: " ^ Printexc.to_string exn) :: !errs; Lwt.return "") in
  let p_commits = Lwt.catch
    (fun () -> Urme_git.Ops.walk_log ~cwd ~max_count:200 ())
    (fun exn -> errs := ("commits: " ^ Printexc.to_string exn) :: !errs; Lwt.return []) in
  let* branches_raw = p_branches
  and* cur = p_cur
  and* commits = p_commits in
  let branches = String.split_on_char '\n' branches_raw
    |> List.filter_map (fun s ->
      let s = String.trim s in
      if s = "" then None
      else Some (if String.length s > 2 && s.[0] = '*' then
        String.trim (String.sub s 2 (String.length s - 2)) else s)) in
  (* diff + files depend on commits, but are independent of each other *)
  let p_diff = match commits with
    | (sha, _, _) :: _ ->
      Lwt.catch (fun () -> Urme_git.Ops.commit_diff ~cwd ~sha) (fun _ -> Lwt.return "")
    | [] -> Lwt.return "" in
  let p_files = match commits with
    | (sha, _, _) :: _ ->
      Lwt.catch (fun () -> Urme_git.Ops.commit_changed_files ~cwd ~sha) (fun _ -> Lwt.return [])
    | [] -> Lwt.return [] in
  let* diff_preview = p_diff
  and* files = p_files in
  let debug = Printf.sprintf "cwd=%s branches=%d commits=%d files=%d errs=[%s]"
    cwd (List.length branches) (List.length commits) (List.length files)
    (String.concat "; " !errs) in
  Lwt.return ({ branches; current_branch = cur; commits; files;
               focus = Branches; branch_idx = 0; commit_idx = 0; file_idx = 0;
               link_idx = 0; diff_preview; diff_scroll_git = 0;
               file_diff_filter = None; git_links = Hashtbl.create 0;
               human_edits = Hashtbl.create 0; human_diffs = Hashtbl.create 0;
               link_candidates = [] }, debug)

(* Rebuild in-memory git_links from ChromaDB's git_info metadata *)
let load_git_links_from_chroma ~port ~project =
  Lwt.catch (fun () ->
    let* collection_id =
      Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
    let* existing_gis =
      Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id in
    let links : (string * string, git_conv_link list) Hashtbl.t =
      Hashtbl.create 256 in
    List.iter (fun (_id, gi_str, session_id, _idx, _ts) ->
      let tbl = parse_git_info gi_str in
      Hashtbl.iter (fun ek value ->
        match value with
        | Some (gi : Urme_engine.Git_link_types.git_info) ->
          let file_base = match String.split_on_char ':' ek with
            | fb :: _ -> fb | [] -> "" in
          if file_base <> "" then begin
            let lk = (gi.commit_sha, file_base) in
            let existing = match Hashtbl.find_opt links lk with
              | Some l -> l | None -> [] in
            if not (List.exists (fun (l : git_conv_link) -> l.edit_key = ek) existing) then begin
              let link : git_conv_link = { commit_sha = gi.commit_sha; file = file_base;
                           session_id; turn_idx = gi.turn_idx; entry_idx = gi.entry_idx;
                           edit_key = ek } in
              Hashtbl.replace links lk (existing @ [link])
            end
          end
        | None -> ()
      ) tbl
    ) existing_gis;
    Lwt.return links
  ) (fun _ -> Lwt.return (Hashtbl.create 0))

let switch_to_git mvar =
  let* s = Lwt_mvar.take mvar in
  let* () = Lwt_mvar.put mvar { s with status_extra = "Loading git data..." } in
  redraw mvar;
  let* s = Lwt_mvar.take mvar in
  let* (git, debug) = load_git_data ~cwd:s.project_dir in
  (* If in-memory git_links are empty, start ChromaDB if needed and load *)
  let* git_links =
    if Hashtbl.length s.git.git_links > 0 then
      Lwt.return s.git.git_links
    else begin
      let port = s.config.chromadb_port in
      let* () = Lwt_mvar.put mvar { s with status_extra = "Starting ChromaDB..." } in
      redraw mvar;
      let start_chroma () =
        let chroma_dir = Filename.concat s.project_dir "chroma" in
        (try Unix.mkdir chroma_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
        ignore (Sys.command (Printf.sprintf
          "chroma run --port %d --path %s > /tmp/chromadb.log 2>&1 &" port chroma_dir));
        let rec wait n =
          if n <= 0 then Lwt.return_unit
          else Lwt.catch (fun () ->
            let uri = Uri.of_string (Printf.sprintf "http://[::1]:%d/api/v2/heartbeat" port) in
            let* _resp, body = Cohttp_lwt_unix.Client.get uri in
            let* _ = Cohttp_lwt.Body.to_string body in
            Lwt.return_unit
          ) (fun _ -> let* () = Lwt_unix.sleep 1.0 in wait (n - 1))
        in wait 10
      in
      let* () = Lwt.catch (fun () ->
        let uri = Uri.of_string (Printf.sprintf "http://[::1]:%d/api/v2/heartbeat" port) in
        let* _resp, body = Cohttp_lwt_unix.Client.get uri in
        let* _ = Cohttp_lwt.Body.to_string body in
        Lwt.return_unit
      ) (fun _ -> start_chroma ()) in
      let* s = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar { s with status_extra = "Loading git links..." } in
      redraw mvar;
      let* links = load_git_links_from_chroma ~port
          ~project:(Filename.basename s.project_dir) in
      let* s = Lwt_mvar.take mvar in
      ignore s; Lwt.return links
    end in
  let git = { git with git_links;
              human_edits = s.git.human_edits;
              human_diffs = s.git.human_diffs } in
  let status = if git.branches = [] then
    Printf.sprintf "Git: no branches! %s" debug
  else Printf.sprintf "Git browser (%d link keys)" (Hashtbl.length git_links) in
  let* () = Lwt_mvar.put mvar { s with mode = Git; git; status_extra = status } in
  redraw mvar; Lwt.return_unit

(* Check if a service is reachable *)
let check_port port =
  Lwt.catch (fun () ->
    let uri = Uri.of_string (Printf.sprintf "http://[::1]:%d/api/v2/heartbeat" port) in
    let* resp, body = Cohttp_lwt_unix.Client.get uri in
    let* _ = Cohttp_lwt.Body.to_string body in
    Lwt.return (Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200)
  ) (fun _ -> Lwt.return false)

let check_ollama url =
  Lwt.catch (fun () ->
    let uri = Uri.of_string (url ^ "/api/tags") in
    let* resp, body = Cohttp_lwt_unix.Client.get uri in
    let* _ = Cohttp_lwt.Body.to_string body in
    Lwt.return (Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200)
  ) (fun _ -> Lwt.return false)

(* Start services if not running, wait for them to be ready.
   Starts services if not already running. *)
let ensure_services mvar ~chromadb_port ~ollama_url ~project_dir =
  let* chroma_ok = check_port chromadb_port in
  let* did_start_chroma = if not chroma_ok then begin
    let* () = update mvar (fun s -> { s with status_extra = "Starting ChromaDB..." }) in
    redraw mvar;
    let chroma_dir = Filename.concat project_dir "chroma" in
    (try Unix.mkdir chroma_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let cmd = Printf.sprintf
      "chroma run --port %d --path %s > /tmp/chromadb.log 2>&1 &"
      chromadb_port chroma_dir in
    ignore (Sys.command cmd);
    let rec wait n =
      if n <= 0 then Lwt.return false
      else
        let* ok = check_port chromadb_port in
        if ok then Lwt.return true
        else let* () = Lwt_unix.sleep 1.0 in wait (n - 1)
    in
    wait 10
  end else Lwt.return false in
  let* ollama_ok = check_ollama ollama_url in
  let* did_start_ollama = if not ollama_ok then begin
    let* () = update mvar (fun s -> { s with status_extra = "Starting Ollama..." }) in
    redraw mvar;
    ignore (Sys.command "ollama serve > /tmp/ollama.log 2>&1 &");
    let rec wait n =
      if n <= 0 then Lwt.return false
      else
        let* ok = check_ollama ollama_url in
        if ok then Lwt.return true
        else let* () = Lwt_unix.sleep 1.0 in wait (n - 1)
    in
    wait 10
  end else Lwt.return false in
  (* Track what we started *)
  let* () = update mvar (fun s ->
    { s with started_chroma = s.started_chroma || did_start_chroma;
             started_ollama = s.started_ollama || did_start_ollama }) in
  Lwt.return_unit

(* Extract individual turns (user_text, assistant_text) from a session JSONL *)
(* Index all session JSONL files into ChromaDB — incremental with git correlation *)
let index_sessions mvar =
  let* () = update mvar (fun s ->
    { s with status_extra = "Starting services..." }) in
  redraw mvar;
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* s_val = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar s_val in
      let port = s_val.config.chromadb_port in
      let ollama_url = s_val.config.ollama_url in
      let project = Filename.basename s_val.project_dir in
      let cwd = s_val.project_dir in
      let* () = ensure_services mvar ~chromadb_port:port ~ollama_url
          ~project_dir:cwd in
      let* () = update mvar (fun s -> { s with status_extra = "Updating index..." }) in
      redraw mvar;
      let* collection_id =
        Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
      (* Batch-fetch all existing IDs in one call *)
      let* existing_ids =
        Urme_search.Chromadb.get_all_interaction_ids ~port ~collection_id in
      let existing_set = Hashtbl.create (List.length existing_ids) in
      List.iter (fun id -> Hashtbl.replace existing_set id ()) existing_ids;
      let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
          ~project_dir:cwd in
      let sessions = sort_by_size_desc
        (Urme_search.Jsonl_reader.list_sessions ~jsonl_dir) in
      let total = List.length sessions in
      let n_new = ref 0 in
      let n_skip = ref 0 in
      let indexed_count = ref 0 in
      (* Collect all new interactions, then batch-save *)
      let pending = ref [] in
      List.iter (fun filepath ->
        let session_id = Filename.basename filepath |> Filename.chop_extension in
        let interactions = Urme_search.Jsonl_reader.parse_interactions ~filepath in
        if interactions <> [] then begin
          incr indexed_count;
          List.iteri (fun i (interaction : Urme_core.Types.interaction) ->
            let id = Printf.sprintf "%s_%d" session_id i in
            if Hashtbl.mem existing_set id then
              incr n_skip
            else begin
              incr n_new;
              pending := (id, session_id, i,
                interaction.user_text, interaction.assistant_summary,
                interaction.user_uuid, interaction.timestamp,
                interaction.files_changed) :: !pending
            end
          ) interactions
        end
      ) sessions;
      let all_pending = List.rev !pending in
      let batch_size = 64 in
      let batches = ref [] in
      let cur = ref [] in
      let cur_n = ref 0 in
      List.iter (fun item ->
        cur := item :: !cur;
        incr cur_n;
        if !cur_n >= batch_size then begin
          batches := List.rev !cur :: !batches;
          cur := []; cur_n := 0
        end
      ) all_pending;
      if !cur <> [] then batches := List.rev !cur :: !batches;
      let all_batches = List.rev !batches in
      let n_batches = List.length all_batches in
      let batch_i = ref 0 in
      let* () = Lwt_list.iter_s (fun batch ->
        incr batch_i;
        let* () = update mvar (fun s ->
          { s with status_extra =
              Printf.sprintf "Indexing batch %d/%d (%d new, %d skip, %d sessions)"
                !batch_i n_batches !n_new !n_skip total }) in
        redraw mvar;
        Lwt.catch (fun () ->
          Urme_search.Chromadb.save_interactions_batch ~port ~collection_id batch
        ) (fun exn ->
          let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
          Printf.fprintf oc "[%.2f] Batch %d FAILED: %s\n%!" (Unix.gettimeofday ()) !batch_i
            (Printexc.to_string exn);
          close_out oc;
          Lwt.return_unit)
      ) all_batches in
      (* Find sessions with interactions missing git_info *)
      let* all_ids =
        Urme_search.Chromadb.get_all_interaction_ids ~port ~collection_id in
      let* gis =
        Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id in
      let has_gi = Hashtbl.create (List.length gis) in
      List.iter (fun (id, _, _, _, _) -> Hashtbl.replace has_gi id ()) gis;
      let sessions_needing_scan = Hashtbl.create 16 in
      List.iter (fun id ->
        if not (Hashtbl.mem has_gi id) then
          (* id = "session_id_index" — extract session_id *)
          match String.rindex_opt id '_' with
          | Some pos -> Hashtbl.replace sessions_needing_scan
              (String.sub id 0 pos) ()
          | None -> ()
      ) all_ids;
      let sids = Hashtbl.fold (fun k () acc -> k :: acc) sessions_needing_scan [] in
      let n_scan = List.length sids in
      if n_scan = 0 && !n_new = 0 then begin
        (* Rebuild in-memory links from ChromaDB *)
        let* git_links = load_git_links_from_chroma ~port ~project in
        let n_links = Hashtbl.length git_links in
        let* () = update mvar (fun s ->
          { s with git = { s.git with git_links };
                   status_extra =
              Printf.sprintf "Up to date: %d existing, %d links (%d sessions)"
                !n_skip n_links total }) in
        redraw mvar; Lwt.return_unit
      end else begin
        let* () = update mvar (fun s ->
          { s with status_extra =
              Printf.sprintf "Git links: scanning %d sessions (%d new)..."
                n_scan !n_new }) in
        redraw mvar;
        let* (git_links, human_edits, human_diffs, gl_edits, gl_matched, _gl_commits, _gl_relinked) =
          update_git_links ~project_dir:cwd ~port ~collection_id
            ~sessions_filter:sids mvar in
        let n_links = Hashtbl.length git_links in
        let* () = update mvar (fun s ->
          { s with git = { s.git with git_links; human_edits; human_diffs };
                   status_extra =
              Printf.sprintf "Done: %d new, %d scanned | %d edits, %d matched, %d links"
                !n_new n_scan gl_edits gl_matched n_links }) in
        redraw mvar; Lwt.return_unit
      end
    ) (fun exn ->
      let* () = update mvar (fun s ->
        { s with status_extra = Printf.sprintf "Index error: %s"
            (Printexc.to_string exn) }) in
      redraw mvar; Lwt.return_unit));
  Lwt.return_unit

let execute_search mvar query =
  let* () = update mvar (fun s ->
    { s with status_extra = "Starting services..." ;
             history = { s.history with search_active = false } }) in
  redraw mvar;
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* s_val = Lwt_mvar.take mvar in
      let* () = Lwt_mvar.put mvar s_val in
      let port = s_val.config.chromadb_port in
      let ollama_url = s_val.config.ollama_url in
      let project = Filename.basename s_val.project_dir in
      let* () = ensure_services mvar ~chromadb_port:port ~ollama_url
          ~project_dir:s_val.project_dir in
      let* () = update mvar (fun s -> { s with status_extra = "Searching..." }) in
      redraw mvar;
      let* collection_id =
        Urme_search.Chromadb.ensure_interactions_collection ~port ~project in
      let* results = Urme_search.Chromadb.search_all_interactions ~port
          ~collection_id ~query ~n:20 in
      let* () = update mvar (fun s ->
        { s with history = { s.history with
            search_results = results;
            showing_results = true;
            search_query = query;
            result_idx = 0 };
          status_extra = Printf.sprintf "Found %d results" (List.length results) }) in
      redraw mvar; Lwt.return_unit
    ) (fun exn ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with showing_results = false };
                 status_extra = Printf.sprintf "Search error: %s"
                   (Printexc.to_string exn) }) in
      redraw mvar; Lwt.return_unit));
  Lwt.return_unit

let switch_to_history mvar =
  let* () = update mvar (fun s ->
    let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
        ~project_dir:s.project_dir in
    let sessions = Urme_search.Jsonl_reader.list_sessions ~jsonl_dir in
    let turns = match sessions with
      | path :: _ -> split_into_turns (load_session_entries path)
      | [] -> [] in
    let n = List.length sessions in
    { s with mode = History;
             history = { sessions; session_idx = 0; turns; turn_idx = 0;
                         hist_scroll = 0; return_mode = s.mode;
                         search_active = false; search_query = "";
                         search_results = []; result_idx = 0;
                         showing_results = false };
             status_extra = Printf.sprintf "History (%d sessions)" n }) in
  redraw mvar; Lwt.return_unit

(* ---------- Usage refresh ---------- *)

let refresh_usage mvar =
  Lwt.async (fun () ->
    Lwt.catch (fun () ->
      let* result = Urme_core.Usage.fetch () in
      match result with
      | Some u ->
        let* () = update mvar (fun s -> { s with usage = Some u }) in
        redraw mvar; Lwt.return_unit
      | None -> Lwt.return_unit
    ) (fun _exn -> Lwt.return_unit))

(* ---------- Main ---------- *)

let run ~config ~project_dir () =
  let project_dir = if project_dir = "." then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir in
  let mvar = Lwt_mvar.create (initial_state ~config ~project_dir) in
  let term = Notty_lwt.Term.create ~mouse:false ~nosig:true () in
  term_ref := Some term;

  (* Load sessions for History mode on startup *)
  let* () = switch_to_history mvar in

  (* Initial render *)
  redraw mvar;

  (* Initial usage fetch for Pro/Max plans *)
  (match config.plan with
   | Urme_core.Config.Pro | Urme_core.Config.Max -> refresh_usage mvar
   | Urme_core.Config.Api -> ());

  let do_quit () =
    let s = match peek mvar with Some s -> s | None -> initial_state ~config ~project_dir in
    (match s.tui_bridge with
     | Some tb ->
       Lwt.catch (fun () -> Tui_bridge.stop tb) (fun _ -> Lwt.return_unit)
       |> Lwt.ignore_result
     | None -> ());
    (* Gracefully stop services we started *)
    if s.started_chroma then
      ignore (Sys.command "pkill -TERM -f 'chroma run' 2>/dev/null");
    if s.started_ollama then
      ignore (Sys.command "pkill -TERM -x ollama 2>/dev/null");
    (* Quit terminal, then hard-exit to skip at_exit handlers that hit stale FDs *)
    (try Lwt.ignore_result (Notty_lwt.Term.release term) with _ -> ());
    Unix._exit 0
  in

  (* Navigate to result ri in the search_results list (works for both search and git links) *)
  let jump_to_result s ri =
    let h = s.history in
    match List.nth_opt h.search_results ri with
    | Some (session_id, user_text, _doc, _turn_idx, _ts, _dist) ->
      (* Check if this is a human edit link *)
      let is_human_link = match List.nth_opt s.git.link_candidates ri with
        | Some link ->
          let ek = link.edit_key in
          (String.length ek > 6 && String.sub ek (String.length ek - 6) 6 = ":human") ||
          (String.length ek > 11 && String.sub ek (String.length ek - 11) 11 = ":human-only")
        | None -> false in
      if is_human_link then
        let link_opt = List.nth_opt s.git.link_candidates ri in
        let file = match link_opt with Some l -> l.file | None -> "" in
        let commit_sha = match link_opt with Some l -> l.commit_sha | None -> "" in
        let diff_text = match Hashtbl.find_opt s.git.human_diffs (commit_sha, file) with
          | Some d -> d
          | None -> "No human-specific diff computed" in
        let short = if String.length commit_sha > 7
          then String.sub commit_sha 0 7 else commit_sha in
        if session_id = "" then
          (* Purely human — show diff only *)
          { s with history = { h with showing_results = false; result_idx = ri;
                                       turns = [[System_info (Printf.sprintf
                                         "Human edit to %s in %s\n\n%s" file short diff_text)]];
                                       turn_idx = 0; hist_scroll = 0 };
                   status_extra = Printf.sprintf "Human edit: %s" file }
        else
          (* Human modification of Claude edit — show Claude's turn + diff *)
          let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
              ~project_dir:s.project_dir in
          let path = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
          if Sys.file_exists path then
            let turns = split_into_interaction_turns ~filepath:path in
            let ti = min (max 0 (_turn_idx - 1)) (max 0 (List.length turns - 1)) in
            let note = System_info (Printf.sprintf
              "--- Human modified this Claude edit ---\n%s" diff_text) in
            let turns = List.mapi (fun i t ->
              if i = ti then t @ [note] else t) turns in
            let new_si = let target = session_id ^ ".jsonl" in
              let rec find i = function
                | [] -> h.session_idx | p :: rest ->
                  if Filename.basename p = target then i else find (i+1) rest
              in find 0 h.sessions in
            { s with history = { h with result_idx = ri; session_idx = new_si;
                                         showing_results = false;
                                         turns; turn_idx = ti; hist_scroll = 0 };
                     git = { s.git with link_idx = ri };
                     status_extra = Printf.sprintf "Human edit: %s t%d" file _turn_idx }
          else s
      else
      let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
          ~project_dir:s.project_dir in
      let path = Filename.concat jsonl_dir (session_id ^ ".jsonl") in
      let new_si = let target = session_id ^ ".jsonl" in
        let rec find i = function
          | [] -> h.session_idx | p :: rest ->
            if Filename.basename p = target then i else find (i+1) rest
        in find 0 h.sessions in
      if not (Sys.file_exists path) then s
      else if h.return_mode = Git then
        (* Git link: interaction-aligned turns + turn_idx *)
        let turns = split_into_interaction_turns ~filepath:path in
        let ti = min (max 0 (_turn_idx - 1)) (max 0 (List.length turns - 1)) in
        let scroll_pos = match List.nth_opt turns ti,
          List.nth_opt s.git.link_candidates ri with
          | Some entries, Some link ->
            let ec = ref 0 in let pos = ref 0 in let found = ref false in
            List.iteri (fun i e -> if not !found then match e with
              | Tool_use_block { tool_name; _ }
                when tool_name = "Edit" || tool_name = "Write" ->
                if !ec = link.entry_idx then (pos := i; found := true); incr ec
              | _ -> ()) entries; !pos
          | _ -> 0 in
        { s with history = { h with result_idx = ri; session_idx = new_si;
                                     showing_results = false;
                                     turns; turn_idx = ti; hist_scroll = scroll_pos };
                 git = { s.git with link_idx = ri };
                 status_extra = Printf.sprintf "Link %d/%d"
                   (ri + 1) (List.length h.search_results) }
      else
        (* Search result: match by user text *)
        let turns = split_into_turns (load_session_entries path) in
        let needle = String.trim user_text in
        let ti = let rec f i = function
          | [] -> min _turn_idx (max 0 (List.length turns - 1))
          | turn :: rest ->
            if List.exists (fun e -> match e with
              | User_msg t -> let t = String.trim t in
                (needle <> "" && String.length t >= String.length needle &&
                 String.sub t 0 (String.length needle) = needle) || t = needle
              | _ -> false) turn then i else f (i+1) rest
          in f 0 turns in
        { s with history = { h with result_idx = ri; session_idx = new_si;
                                     showing_results = false;
                                     turns; turn_idx = ti; hist_scroll = 0 };
                 status_extra = Printf.sprintf "Result %d/%d"
                   (ri + 1) (List.length h.search_results) }
    | None -> s
  in

  let events = Notty_lwt.Term.events term in

  let rec loop () =
    let* event = Lwt_stream.next events in
    match event with
    | `Key (`ASCII c, mods) when (c = 'C' || c = 'D') && List.mem `Ctrl mods ->
      do_quit ()

    | `Key (`ASCII c, mods) when c = 'V' && List.mem `Ctrl mods ->
      let* () = update mvar (fun s -> { s with verbose = not s.verbose }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'g', mods) when List.mem `Meta mods ->
      let* () = switch_to_git mvar in loop ()

    | `Key (`ASCII 'h', mods) when List.mem `Meta mods ->
      let* () = switch_to_history mvar in loop ()

    (* --- Palette open: intercept keys --- *)
    | `Key (`Enter, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let cmd = match peek mvar with
        | Some s ->
          let candidates = filter_commands s.input_text in
          let n = List.length candidates in
          if n > 0 then (List.nth candidates (s.palette_selected mod n)).cmd
          else s.input_text
        | None -> "" in
      let* () = update mvar (fun s ->
        { s with input_text = ""; palette_open = false; palette_selected = 0 }) in
      if cmd = "/exit" || cmd = "/quit" then do_quit ()
      else begin
        let* () = (match cmd with
          | "/git" -> switch_to_git mvar
          | "/history" -> switch_to_history mvar
          | "/verbose" | "/v" ->
            update mvar (fun s ->
              { s with verbose = not s.verbose;
                       status_extra = if s.verbose then "Verbose off" else "Verbose on" })
          | _ ->
            update mvar (fun s ->
              { s with status_extra = Printf.sprintf "Unknown: %s" cmd })
        ) in
        redraw mvar; loop ()
      end

    | `Key (`Escape, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        { s with input_text = ""; palette_open = false; palette_selected = 0 }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Up, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let n = List.length (filter_commands s.input_text) in
        let sel = if n = 0 then 0
          else (s.palette_selected - 1 + n) mod n in
        { s with palette_selected = sel }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let n = List.length (filter_commands s.input_text) in
        let sel = if n = 0 then 0
          else (s.palette_selected + 1) mod n in
        { s with palette_selected = sel }) in
      redraw mvar; loop ()

    | `Key (`Backspace, _)
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let* () = update mvar (fun s ->
        let len = String.length s.input_text in
        if len > 0 then
          let text = String.sub s.input_text 0 (len - 1) in
          { s with input_text = text; palette_selected = 0;
                   palette_open = String.length text > 0 && text.[0] = '/' }
        else
          { s with palette_open = false; palette_selected = 0 }) in
      redraw mvar; loop ()

    | `Key (`ASCII c, [])
      when (match peek mvar with Some s -> s.palette_open | None -> false) ->
      let ch = Char.code c in
      let* () =
        if ch >= 32 && ch < 127 then
          let* () = update mvar (fun s ->
            { s with input_text = s.input_text ^ String.make 1 c;
                     palette_selected = 0 }) in
          (redraw mvar; Lwt.return_unit)
        else Lwt.return_unit
      in
      loop ()

    (* --- Git mode keys --- *)
    | `Key (`Escape, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        match s.git.file_diff_filter with
        | Some _ ->
          { s with git = { s.git with file_diff_filter = None; diff_scroll_git = 0 };
                   status_extra = "Git browser" }
        | None ->
          { s with mode = History; status_extra = "Ready" }) in
      redraw mvar; loop ()

    (* Tab: next panel *)
    | `Key (`Tab, mods)
      when (match peek mvar with Some s -> s.mode = Git | None -> false)
        && not (List.mem `Shift mods) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let next = match g.focus with
          | Branches -> Commits | Commits -> Files
          | Files -> Links | Links -> Branches in
        let link_candidates = match next with
          | Links ->
            (match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
             | Some (sha, _, _), Some file ->
               (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
                | Some l -> l | None -> [])
             | _ -> [])
          | _ -> g.link_candidates in
        { s with git = { g with focus = next; link_candidates; link_idx = 0 } }) in
      redraw mvar; loop ()

    (* Shift+Tab: previous panel *)
    | `Key (`Tab, mods)
      when (match peek mvar with Some s -> s.mode = Git | None -> false)
        && List.mem `Shift mods ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let prev = match g.focus with
          | Branches -> Links | Commits -> Branches
          | Files -> Commits | Links -> Files in
        let link_candidates = match prev with
          | Links ->
            (match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
             | Some (sha, _, _), Some file ->
               (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
                | Some l -> l | None -> [])
             | _ -> [])
          | _ -> g.link_candidates in
        { s with git = { g with focus = prev; link_candidates; link_idx = 0 } }) in
      redraw mvar; loop ()

    (* Up / k: navigate up *)
    | `Key (`Arrow `Up, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = max 0 (g.branch_idx - 1) }
          | Commits -> { g with commit_idx = prev_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = prev_linked_file g g.file_idx }
          | Links -> { g with link_idx = max 0 (g.link_idx - 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'k', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = max 0 (g.branch_idx - 1) }
          | Commits -> { g with commit_idx = prev_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = prev_linked_file g g.file_idx }
          | Links -> { g with link_idx = max 0 (g.link_idx - 1) } in
        { s with git }) in
      redraw mvar; loop ()

    (* Down / j: navigate down *)
    | `Key (`Arrow `Down, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = min (List.length g.branches - 1) (g.branch_idx + 1) }
          | Commits -> { g with commit_idx = next_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = next_linked_file g g.file_idx }
          | Links -> { g with link_idx = min (max 0 (List.length g.link_candidates - 1)) (g.link_idx + 1) } in
        { s with git }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'j', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let git = match g.focus with
          | Branches -> { g with branch_idx = min (List.length g.branches - 1) (g.branch_idx + 1) }
          | Commits -> { g with commit_idx = next_linked_commit g g.commit_idx }
          | Files -> { g with file_idx = next_linked_file g g.file_idx }
          | Links -> { g with link_idx = min (max 0 (List.length g.link_candidates - 1)) (g.link_idx + 1) } in
        { s with git }) in
      redraw mvar; loop ()

    (* [ / ]: scroll diff up/down *)
    | `Key (`ASCII '[', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        { s with git = { s.git with diff_scroll_git = max 0 (s.git.diff_scroll_git - 1) } }) in
      redraw mvar; loop ()

    | `Key (`ASCII ']', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* () = update mvar (fun s ->
        let g = s.git in
        let n = List.length (String.split_on_char '\n' g.diff_preview) in
        { s with git = { g with diff_scroll_git = min (max 0 (n - 1)) (g.diff_scroll_git + 1) } }) in
      redraw mvar; loop ()

    (* h: switch to history — if file has links, show them as a navigable list *)
    | `Key (`ASCII 'h', [])
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let has_file_links = match peek mvar with
        | Some s ->
          let g = s.git in
          (match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
           | Some (sha, _, _), Some file ->
             (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
              | Some (_ :: _) -> true | _ -> false)
           | _ -> false)
        | None -> false in
      if has_file_links then begin
        let* () = update mvar (fun s ->
          let g = s.git in
          let links_unsorted = match List.nth_opt g.commits g.commit_idx, List.nth_opt g.files g.file_idx with
            | Some (sha, _, _), Some file ->
              (match Hashtbl.find_opt g.git_links (sha, Filename.basename file) with
               | Some l -> l | None -> [])
            | _ -> [] in
          let links = List.sort (fun (a : git_conv_link) (b : git_conv_link) ->
            let c = Int.compare a.turn_idx b.turn_idx in
            if c <> 0 then c else Int.compare a.entry_idx b.entry_idx
          ) links_unsorted in
          (* Convert links to search_results format *)
          let results = List.mapi (fun i (link : git_conv_link) ->
            let label = Printf.sprintf "t%d e%d %s"
              link.turn_idx link.entry_idx link.edit_key in
            (link.session_id, label, "", link.turn_idx, "", Float.of_int i)
          ) links in
          let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
              ~project_dir:s.project_dir in
          let sessions = Urme_search.Jsonl_reader.list_sessions ~jsonl_dir in
          { s with mode = History;
                   history = { sessions; session_idx = 0;
                               turns = []; turn_idx = 0; hist_scroll = 0;
                               return_mode = Git;
                               search_active = false; search_query = "";
                               search_results = results; result_idx = 0;
                               showing_results = true };
                   git = { g with link_candidates = links; link_idx = 0 };
                   status_extra = Printf.sprintf "%d links" (List.length links) }) in
        redraw mvar; loop ()
      end else begin
        let* () = switch_to_history mvar in loop ()
      end

    (* Enter: context-dependent activate *)
    | `Key (`Enter, _)
      when (match peek mvar with Some s -> s.mode = Git | None -> false) ->
      let* s = Lwt_mvar.take mvar in
      let g = s.git in
      let* () = match g.focus with
        | Branches ->
          let branch = match List.nth_opt g.branches g.branch_idx with
            | Some b -> b | None -> g.current_branch in
          let* commits = Lwt.catch
            (fun () ->
              let* _out = Urme_git.Ops.run_git ~cwd:s.project_dir
                ["log"; branch; "--format=%H%n%at%n%s%n---"; "--max-count=200"] in
              let lines = String.split_on_char '\n' _out in
              let rec parse = function
                | sha :: ts :: msg :: "---" :: rest ->
                  let timestamp = try float_of_string ts with _ -> 0.0 in
                  (sha, timestamp, msg) :: parse rest
                | _ -> [] in
              Lwt.return (parse lines))
            (fun _ -> Lwt.return []) in
          let* diff_preview = match commits with
            | (sha, _, _) :: _ ->
              Lwt.catch (fun () -> Urme_git.Ops.commit_diff ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return "")
            | [] -> Lwt.return "" in
          let* files = match commits with
            | (sha, _, _) :: _ ->
              Lwt.catch (fun () -> Urme_git.Ops.commit_changed_files ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return [])
            | [] -> Lwt.return [] in
          let first_linked = next_linked_commit { g with commits; git_links = g.git_links } (-1) in
          let ci = if first_linked >= 0 then first_linked else 0 in
          Lwt_mvar.put mvar { s with git = { g with commits; commit_idx = ci;
                                                     files; file_idx = 0; diff_preview;
                                                     diff_scroll_git = 0; focus = Commits;
                                                     file_diff_filter = None } }
        | Commits ->
          (match List.nth_opt g.commits g.commit_idx with
           | Some (sha, _, _) ->
             let* diff = Lwt.catch
               (fun () -> Urme_git.Ops.commit_diff ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return "") in
             let* files = Lwt.catch
               (fun () -> Urme_git.Ops.commit_changed_files ~cwd:s.project_dir ~sha) (fun _ -> Lwt.return []) in
             Lwt_mvar.put mvar { s with git = { g with diff_preview = diff; files;
                                                        file_idx = 0; diff_scroll_git = 0;
                                                        file_diff_filter = None } }
           | None -> Lwt_mvar.put mvar s)
        | Files ->
          (match List.nth_opt g.files g.file_idx with
           | Some file ->
             let file_base = Filename.basename file in
             (match g.file_diff_filter with
              | Some f when Filename.basename f = file_base ->
                let all_links = match List.nth_opt g.commits g.commit_idx with
                  | Some (sha, _, _) ->
                    (match Hashtbl.find_opt g.git_links (sha, file_base) with
                     | Some l -> l | None -> [])
                  | None -> [] in
                if all_links <> [] then
                  Lwt_mvar.put mvar { s with
                    git = { g with focus = Links; link_candidates = all_links;
                                   link_idx = 0 };
                    status_extra = Printf.sprintf "%d links for %s"
                      (List.length all_links) file_base }
                else
                  Lwt_mvar.put mvar { s with
                    git = { g with file_diff_filter = None; diff_scroll_git = 0 };
                    status_extra = Printf.sprintf "No links for %s" file_base }
              | _ ->
                Lwt_mvar.put mvar { s with
                  git = { g with file_diff_filter = Some file; diff_scroll_git = 0 };
                  status_extra = Printf.sprintf "Diff: %s" file })
           | None -> Lwt_mvar.put mvar s)
        | Links ->
          (match List.nth_opt g.link_candidates g.link_idx with
           | Some link ->
             let jsonl_dir = Urme_search.Jsonl_reader.find_jsonl_dir
                 ~project_dir:s.project_dir in
             let path = Filename.concat jsonl_dir (link.session_id ^ ".jsonl") in
             if Sys.file_exists path then begin
               let turns = split_into_interaction_turns ~filepath:path in
               let ti = min (max 0 (link.turn_idx - 1)) (max 0 (List.length turns - 1)) in
               let new_session_idx =
                 let target = link.session_id ^ ".jsonl" in
                 let rec find i = function
                   | [] -> s.history.session_idx
                   | p :: rest ->
                     if Filename.basename p = target then i else find (i + 1) rest
                 in find 0 s.history.sessions in
               let scroll_pos = match List.nth_opt turns ti with
                 | Some entries ->
                   let edit_count = ref 0 in
                   let pos = ref 0 in
                   let found = ref false in
                   List.iteri (fun i entry ->
                     if not !found then
                       match entry with
                       | Tool_use_block { tool_name; _ }
                         when tool_name = "Edit" || tool_name = "Write" ->
                         if !edit_count = link.entry_idx then begin
                           pos := i; found := true
                         end;
                         incr edit_count
                       | _ -> ()
                   ) entries;
                   !pos
                 | None -> 0 in
               Lwt_mvar.put mvar { s with
                 mode = History;
                 history = { s.history with
                   showing_results = false;
                   session_idx = new_session_idx;
                   turns; turn_idx = ti; hist_scroll = scroll_pos;
                   return_mode = Git };
                 status_extra = Printf.sprintf "Session: %s turn %d entry %d"
                   link.session_id (ti + 1) scroll_pos }
             end else
               Lwt_mvar.put mvar { s with
                 status_extra = Printf.sprintf "Session %s not found" link.session_id }
           | None -> Lwt_mvar.put mvar s)
      in
      redraw mvar; loop ()

    (* --- History mode: search input active --- *)
    | `Key (`Escape, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = false; search_query = "" };
                 status_extra = "History" }) in
      redraw mvar; loop ()

    | `Key (`Enter, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let query = (match peek mvar with
        | Some s -> s.history.search_query | None -> "") in
      if query <> "" then begin
        let* () = execute_search mvar query in
        loop ()
      end else begin
        let* () = update mvar (fun s ->
          { s with history = { s.history with search_active = false } }) in
        redraw mvar; loop ()
      end

    | `Key (`Backspace, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let* () = update mvar (fun s ->
        let q = s.history.search_query in
        let q' = if String.length q > 0
          then String.sub q 0 (String.length q - 1) else "" in
        { s with history = { s.history with search_query = q' } }) in
      redraw mvar; loop ()

    | `Key (`ASCII c, [])
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_active | _ -> false) ->
      let ch = Char.code c in
      if ch >= 32 && ch < 127 then begin
        let* () = update mvar (fun s ->
          let q = s.history.search_query ^ String.make 1 c in
          { s with history = { s.history with search_query = q } }) in
        redraw mvar; loop ()
      end else loop ()

    (* --- History mode: showing search results --- *)
    | `Key (`Escape, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        if s.history.return_mode = Git then
          (* Back to Git mode *)
          { s with mode = Git; status_extra = "Git";
                   history = { s.history with showing_results = false; result_idx = 0;
                               search_results = [] } }
        else
          { s with history = { s.history with showing_results = false; result_idx = 0 };
                   status_extra = "History" }) in
      redraw mvar; loop ()

    | `Key (`Enter, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        jump_to_result s s.history.result_idx) in
      redraw mvar; loop ()

    | `Key (`Arrow `Up, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
            result_idx = max 0 (s.history.result_idx - 1) } }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let max_idx = max 0 (List.length s.history.search_results - 1) in
        { s with history = { s.history with
            result_idx = min max_idx (s.history.result_idx + 1) } }) in
      redraw mvar; loop ()

    | `Key (`ASCII '/', [])
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = true; search_query = "";
                                             showing_results = false } }) in
      redraw mvar; loop ()

    (* --- History mode: normal browsing --- *)
    | `Key (`Escape, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with mode = s.history.return_mode; status_extra = "Ready" }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'g', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = switch_to_git mvar in loop ()

    | `Key (`ASCII 'q', [])
      when (match peek mvar with Some s -> s.mode = History || s.mode = Git | None -> false) ->
      do_quit ()

    | `Key (`ASCII '/', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with search_active = true; search_query = "" } }) in
      redraw mvar; loop ()

    | `Key (`ASCII 'u', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = index_sessions mvar in
      loop ()

    | `Key (`ASCII 'i', [])
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      (* Wipe ChromaDB collections and re-index *)
      let* () = update mvar (fun s ->
        { s with status_extra = "Initialising (wiping DB)..." }) in
      redraw mvar;
      Lwt.async (fun () ->
        Lwt.catch (fun () ->
          let* s_val = Lwt_mvar.take mvar in
          let* () = Lwt_mvar.put mvar s_val in
          let port = s_val.config.chromadb_port in
          let project = Filename.basename s_val.project_dir in
          let* () = ensure_services mvar ~chromadb_port:port
              ~ollama_url:s_val.config.ollama_url ~project_dir:s_val.project_dir in
          let* () = Lwt.catch (fun () ->
            Urme_search.Chromadb.delete_collection ~port
              ~name:(project ^ "_interactions")) (fun _ -> Lwt.return_unit) in
          let* () = Lwt.catch (fun () ->
            Urme_search.Chromadb.delete_collection ~port
              ~name:(project ^ "_experiences")) (fun _ -> Lwt.return_unit) in
          let* () = update mvar (fun s ->
            { s with status_extra = "Initialising: indexing..." }) in
          redraw mvar;
          (* Now run index_sessions *)
          index_sessions mvar
        ) (fun exn ->
          let* () = update mvar (fun s ->
            { s with status_extra = Printf.sprintf "Wipe error: %s"
                (Printexc.to_string exn) }) in
          redraw mvar; Lwt.return_unit));
      loop ()

    (* Shift+Left/Right: jump to prev/next result (search or git link) *)
    | `Key (`Arrow `Left, mods)
      when List.mem `Shift mods
        && (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let ri = max 0 (s.history.result_idx - 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    | `Key (`Arrow `Right, mods)
      when List.mem `Shift mods
        && (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let max_ri = max 0 (List.length s.history.search_results - 1) in
        let ri = min max_ri (s.history.result_idx + 1) in
        if ri = s.history.result_idx then s
        else jump_to_result s ri) in
      redraw mvar; loop ()

    (* b: back to results list *)
    | `Key (`ASCII 'b', [])
      when (match peek mvar with
            | Some s -> s.mode = History && s.history.search_results <> []
                        && not s.history.showing_results | _ -> false) ->
      let* () = update mvar (fun s ->
        let label = if s.history.return_mode = Git then
          Printf.sprintf "%d links" (List.length s.history.search_results)
        else Printf.sprintf "Search: \"%s\"" s.history.search_query in
        { s with history = { s.history with showing_results = true };
                 status_extra = label }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Left, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        let h = s.history in
        if h.turn_idx > 0 then
          (* Previous turn in same session *)
          { s with history = { h with turn_idx = h.turn_idx - 1; hist_scroll = 0 } }
        else
          (* Go to previous session, last turn *)
          let si = max 0 (h.session_idx - 1) in
          if si <> h.session_idx then
            let turns = match List.nth_opt h.sessions si with
              | Some path -> split_into_turns (load_session_entries path)
              | None -> [] in
            let ti = max 0 (List.length turns - 1) in
            { s with history = { h with session_idx = si; turns; turn_idx = ti;
                                         hist_scroll = 0 } }
          else s) in
      redraw mvar; loop ()

    | `Key (`Arrow `Right, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        let h = s.history in
        if h.turn_idx < List.length h.turns - 1 then
          (* Next turn in same session *)
          { s with history = { h with turn_idx = h.turn_idx + 1; hist_scroll = 0 } }
        else
          (* Go to next session, first turn *)
          let max_si = max 0 (List.length h.sessions - 1) in
          let si = min max_si (h.session_idx + 1) in
          if si <> h.session_idx then
            let turns = match List.nth_opt h.sessions si with
              | Some path -> split_into_turns (load_session_entries path)
              | None -> [] in
            { s with history = { h with session_idx = si; turns; turn_idx = 0;
                                         hist_scroll = 0 } }
          else s) in
      redraw mvar; loop ()

    | `Key (`Arrow `Up, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
          hist_scroll = max 0 (s.history.hist_scroll - 1) } }) in
      redraw mvar; loop ()

    | `Key (`Arrow `Down, _)
      when (match peek mvar with Some s -> s.mode = History | None -> false) ->
      let* () = update mvar (fun s ->
        { s with history = { s.history with
          hist_scroll = s.history.hist_scroll + 1 } }) in
      redraw mvar; loop ()

    | _ when (match peek mvar with Some s -> s.mode = Git || s.mode = History | None -> false) ->
      loop ()

    | `Resize _ -> redraw mvar; loop ()
    | _ -> loop ()
  in
  loop ()
