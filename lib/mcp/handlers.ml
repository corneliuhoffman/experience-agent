(* MCP tool handlers — V2 SQLite + pure analysis engine. No ChromaDB.

   The six tools run against .urme/db.sqlite (populated by `urme init`)
   and use Git_link's pure analysis functions for per-edit provenance. *)

open Lwt.Syntax
open Urme_engine.Git_link_types

module D = Urme_store.Db
module Schema = Urme_store.Schema
module Search = Urme_search.Search
module Cg = Urme_store.Callgraph_store
module S = Sqlite3

type state = {
  project_dir : string;
  mutable db : S.db option;
  mutable repo : Urme_store.Project_store.Store.Repo.t option;
  mutable edits : edit list option;
  mutable branch_label : (string, string) Hashtbl.t option;
}

let create_state ~project_dir =
  let project_dir =
    if project_dir = "." || project_dir = "./" then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir
  in
  { project_dir; db = None; repo = None; edits = None; branch_label = None }

let ensure_db st =
  match st.db with
  | Some db -> db
  | None ->
    let db = Schema.open_or_create ~project_dir:st.project_dir in
    st.db <- Some db;
    db

let ensure_repo st =
  match st.repo with
  | Some r -> Lwt.return r
  | None ->
    let* r = Urme_store.Project_store.open_repo ~project_dir:st.project_dir in
    st.repo <- Some r;
    Lwt.return r

let ensure_edits st =
  match st.edits with
  | Some e -> Lwt.return e
  | None ->
    let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
    let e = Urme_engine.Edit_extract.edits_of_sessions ~pool
        ~project_dir:st.project_dir in
    Domainslib.Task.teardown_pool pool;
    st.edits <- Some e;
    Lwt.return e

let ensure_branch_label st =
  match st.branch_label with
  | Some bl -> Lwt.return bl
  | None ->
    let* bl = Urme_engine.Branch_topo.label_commits ~cwd:st.project_dir in
    st.branch_label <- Some bl;
    Lwt.return bl

(* ---------- Result formatters ---------- *)

let text_result text =
  `Assoc [
    "content", `List [
      `Assoc ["type", `String "text"; "text", `String text];
    ];
  ]

(* Compact, not pretty-printed: MCP results are read by a model, and
   pretty-printing spends tokens on indentation in every response. *)
let json_result j = text_result (Yojson.Safe.to_string j)

let rec provenance_to_json = function
  | DirectEdit e ->
    `Assoc [
      "type", `String "claude_edit";
      "edit_key", `String e.edit_key;
      "file", `String e.file_base;
      "session_id", `String e.session_id;
      "turn_idx", `Int e.turn_idx;
      "entry_idx", `Int e.entry_idx;
      "timestamp", `Float e.timestamp;
      "old_string", `String (if String.length e.old_string > 200
        then String.sub e.old_string 0 200 ^ "..." else e.old_string);
      "new_string", `String (if String.length e.new_string > 200
        then String.sub e.new_string 0 200 ^ "..." else e.new_string);
    ]
  | Incoming (provs, branch) ->
    let items = List.filter_map (fun p -> match p with
      | DirectEdit _ -> Some (provenance_to_json p) | _ -> None) provs in
    `Assoc ["type", `String "incoming_merge";
            "branch", `String branch; "items", `List items]
  | HumanEdit (e, human_text) ->
    `Assoc [
      "type", `String "human_edit";
      "edit_key", `String e.edit_key;
      "file", `String e.file_base;
      "claude_new_string", `String (if String.length e.new_string > 200
        then String.sub e.new_string 0 200 ^ "..." else e.new_string);
      "human_version", `String (if String.length human_text > 200
        then String.sub human_text 0 200 ^ "..." else human_text);
    ]
  | ConflictChoice _ -> `Assoc ["type", `String "conflict_choice"]
  | ConflictResolution _ -> `Assoc ["type", `String "conflict_resolution"]
  | Unexplained msg -> `Assoc ["type", `String "unexplained"; "message", `String msg]

let decomposition_to_json (d : decomposition) =
  let claude_edits = List.filter_map (fun item -> match item with
    | DirectEdit _ -> Some (provenance_to_json item)
    | Incoming _ -> Some (provenance_to_json item)
    | _ -> None) d.items in
  let warnings = List.filter_map (fun item -> match item with
    | Unexplained msg -> Some (`String msg) | _ -> None) d.items in
  `Assoc [
    "commit_sha", `String d.commit_sha;
    "file", `String d.file;
    "claude_edits", `List claude_edits;
    "warnings", `List warnings;
    "n_edits", `Int (List.length claude_edits);
  ]

let hit_to_json (h : Search.hit) =
  `Assoc [
    "step_id", `Int h.step_id;
    "session_id",
      (match h.session_id with Some s -> `String s | None -> `Null);
    "turn_index", `Int h.turn_index;
    "timestamp", `Float h.timestamp;
    "summary", `String h.summary;
    "tags", `String h.tags;
    "prompt_text", `String h.prompt_text;
    "files_touched", (try Yojson.Safe.from_string h.files_touched
                      with _ -> `List []);
    "commit_before",
      (match h.commit_before with Some s -> `String s | None -> `Null);
    "commit_after",
      (match h.commit_after with Some s -> `String s | None -> `Null);
    "score", `Float h.score;
  ]

(* Generic "row from steps" returned as JSON, shared between file_history,
   commit_links, etc. Columns must match the SELECT we use below. *)
let step_row_to_json cols =
  `Assoc [
    "step_id", `Int (D.data_to_int cols.(0));
    "session_id",
      (match D.data_to_string_opt cols.(1) with
       | Some s -> `String s | None -> `Null);
    "turn_index", `Int (D.data_to_int cols.(2));
    "timestamp", `Float (D.data_to_float cols.(3));
    "summary", `String (D.data_to_string cols.(4));
    "tags", `String (D.data_to_string cols.(5));
    "prompt_text", `String (D.data_to_string cols.(6));
    "files_touched", (try Yojson.Safe.from_string (D.data_to_string cols.(7))
                      with _ -> `List []);
    "commit_before",
      (match D.data_to_string_opt cols.(8) with
       | Some s -> `String s | None -> `Null);
    "commit_after",
      (match D.data_to_string_opt cols.(9) with
       | Some s -> `String s | None -> `Null);
  ]

let step_select_cols =
  "s.id, s.session_id, s.turn_index, s.timestamp, \
   COALESCE(s.summary,''), COALESCE(s.tags,''), \
   COALESCE(s.prompt_text,''), COALESCE(s.files_touched,'[]'), \
   s.commit_before, s.commit_after"

(* ---------- Push to the running URME TUI (Unix socket) ---------- *)

let send_to_tui ~project_dir ~msg =
  let socket_path = Urme_core.Paths.tui_socket_path ~project_dir in
  if not (Sys.file_exists socket_path) then Lwt.return_unit
  else
    Lwt.catch (fun () ->
      let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let addr = Unix.ADDR_UNIX socket_path in
      let* () = Lwt_unix.connect socket addr in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
      let* () = Lwt_io.write_line oc (Yojson.Safe.to_string msg) in
      let* () = Lwt_io.flush oc in
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
      let* _ack = Lwt_io.read_line ic in
      Lwt_unix.close socket
    ) (fun _exn -> Lwt.return_unit)

(* ---------- Tool implementations ---------- *)

(* [handle_search_history]

   Two-sink delivery:

   - Push the full enriched payload (hits + assistant_text for top 5)
     to the running URME TUI over a Unix socket. URME shows results
     live; zero context cost on Claude.
   - Return a SLIM summary to the calling Claude: step_id, session
     prefix, turn, date, one-line summary per hit. Enough for Claude
     to rank and cite, nothing more.

   If the user needs the full text of a single turn for synthesis,
   Claude calls [get_turn] on it — pay-per-turn instead of dumping
   everything. *)
let handle_search_history st args =
  let open Yojson.Safe.Util in
  let parse_iso_date s =
    try
      Scanf.sscanf s "%4d-%2d-%2d" (fun y m d ->
        let dim = [|31;28;31;30;31;30;31;31;30;31;30;31|] in
        let leap yr =
          (yr mod 4 = 0 && yr mod 100 <> 0) || yr mod 400 = 0 in
        let rec yd acc yr =
          if yr <= 1970 then acc
          else yd (acc + if leap (yr - 1) then 366 else 365) (yr - 1) in
        let md = ref 0 in
        for mi = 0 to m - 2 do
          md := !md + dim.(mi);
          if mi = 1 && leap y then md := !md + 1
        done;
        Some (Float.of_int (((yd 0 y + !md + d - 1) * 86400))))
    with _ -> None in
  let fts_terms =
    try args |> member "fts_terms" |> to_string with _ -> "" in
  let query_fallback =
    try args |> member "query" |> to_string with _ -> "" in
  let limit = try args |> member "limit" |> to_int with _ -> 20 in
  let order_by =
    match (try args |> member "order_by" |> to_string with _ -> "relevance")
          |> String.lowercase_ascii with
    | "earliest" -> Urme_claude.Prompts.Earliest
    | "latest"   -> Urme_claude.Prompts.Latest
    | _          -> Urme_claude.Prompts.Relevance in
  let require_summary =
    try args |> member "require_summary" |> to_bool with _ -> true in
  let date_opt key =
    match args |> member key with
    | `String d -> parse_iso_date d
    | _ -> None in
  let after  = date_opt "after" in
  let before = date_opt "before" in
  let db = ensure_db st in
  let hits =
    if fts_terms <> "" then
      let spec : Urme_claude.Prompts.query_spec = {
        fts_terms; order_by;
        limit = max 10 (min 200 limit);
        require_summary; after; before } in
      let hs = Search.run_spec ~db spec in
      if hs = [] && query_fallback <> "" then
        Search.run_with_fallback ~db ~limit query_fallback
      else hs
    else if query_fallback <> "" then
      Search.run_with_fallback ~db ~limit query_fallback
    else [] in
  let full_text_count = 5 in
  let iso_of_ts ts =
    let tm = Unix.gmtime ts in
    Printf.sprintf "%04d-%02d-%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday in
  let short_sid s =
    if String.length s >= 8 then String.sub s 0 8 else s in
  (* Enriched response for Claude: slim fields for every hit, plus full
     prompt_text + assistant_text for the top N. Calling-Claude has a
     wide context window, no clamping. *)
  let results =
    List.mapi (fun i (h : Search.hit) ->
      let sid_opt = match h.session_id with
        | Some s -> `String (short_sid s)
        | None -> `Null in
      let base = [
        "step_id", `Int h.step_id;
        "session", sid_opt;
        "session_id",
          (match h.session_id with Some s -> `String s | None -> `Null);
        "turn", `Int h.turn_index;
        "date", `String (iso_of_ts h.timestamp);
        "summary", `String h.summary;
      ] in
      if i < full_text_count then
        let assistant_text = match h.session_id with
          | Some sid ->
            Search.fetch_assistant_text
              ~project_dir:st.project_dir
              ~session_id:sid ~turn_index:h.turn_index
          | None -> "" in
        `Assoc (base @ [
          "prompt_text", `String h.prompt_text;
          "assistant_text", `String assistant_text;
        ])
      else `Assoc base
    ) hits in
  Lwt.return (json_result (`Assoc [
    "fts_terms", `String fts_terms;
    "query", `String query_fallback;
    "n_results", `Int (List.length results);
    "results", `List results;
    "note",
      `String
        "Top 5 results include full prompt_text + assistant_text as \
         evidence. Rank them yourself: drop tangential, lexical-only, \
         or opposite-direction hits (if the user asks when a feature \
         was ADDED, drop turns about REMOVING it, and vice versa). \
         Prefer 0 or 1 clear citation over several weak ones. After \
         producing your one-or-two-sentence answer, call \
         `push_synthesis` with the answer text and the cited \
         {session_id, turn} pairs so the running URME TUI can show \
         the user your conclusion and the evidence behind it.";
  ]))

let handle_file_history st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let basename = Filename.basename file_path in
  let db = ensure_db st in
  (* Step-level view: turns that touched the file, ordered by timestamp. *)
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE s.files_touched LIKE ? OR s.files_touched LIKE ? \
       ORDER BY s.timestamp ASC"
      step_select_cols
  in
  let rows = D.query_list db sql
    [S.Data.TEXT ("%\"" ^ basename ^ "\"%");
     S.Data.TEXT ("%" ^ file_path ^ "%")]
    ~f:step_row_to_json in
  (* Per-edit provenance via diff_match for each commit that touched the
     file — this is the fine-grained view the old Chroma-backed handler
     used to return. *)
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* decompositions = Urme_engine.Git_link.file_history
      ~project_dir:st.project_dir ~file_path
      ~edits ~branch_label ~repo in
  Lwt.return (json_result (`Assoc [
    "file_path", `String file_path;
    "n_steps", `Int (List.length rows);
    "steps", `List rows;
    "n_commits", `Int (List.length decompositions);
    "commits", `List (List.map decomposition_to_json decompositions);
  ]))

let handle_region_blame st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let start_line = args |> member "start_line" |> to_int in
  let end_line = args |> member "end_line" |> to_int in
  let* blame_lines = Lwt.catch (fun () ->
    Urme_git.Ops.blame ~cwd:st.project_dir
      ~line_range:(start_line, end_line) ~filepath:file_path ()
  ) (fun _ -> Lwt.return []) in
  let blame_json = List.map (fun (sha, line_num, content) ->
    `Assoc [
      "sha", `String (if String.length sha >= 8 then String.sub sha 0 8 else sha);
      "line", `Int line_num;
      "content", `String content;
    ]
  ) blame_lines in
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* decompositions = Urme_engine.Git_link.region_history
      ~project_dir:st.project_dir ~path:file_path
      ~start_line ~end_line ~edits ~branch_label ~repo in
  Lwt.return (json_result (`Assoc [
    "file_path", `String file_path;
    "lines", `String (Printf.sprintf "%d-%d" start_line end_line);
    "blame", `List blame_json;
    "claude_history",
      `List (List.map decomposition_to_json decompositions);
  ]))

(* A commit's "explanation" = raw diff + every step whose commit_after
   matches, ideally filtered to the file at hand. *)
let handle_explain_change st args =
  let open Yojson.Safe.Util in
  let sha = args |> member "commit_sha" |> to_string in
  let file_path = args |> member "file_path" |> to_string in
  let basename = Filename.basename file_path in
  let* diff = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd:st.project_dir
      ["diff"; sha ^ "^"; sha; "--"; file_path]
  ) (fun _ -> Lwt.return "") in
  let db = ensure_db st in
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE (s.commit_after LIKE ? OR s.commit_before LIKE ?) \
         AND s.files_touched LIKE ? \
       ORDER BY s.timestamp ASC"
      step_select_cols
  in
  let sha_like = sha ^ "%" in
  let rows = D.query_list db sql
    [S.Data.TEXT sha_like;
     S.Data.TEXT sha_like;
     S.Data.TEXT ("%\"" ^ basename ^ "\"%")]
    ~f:step_row_to_json in
  Lwt.return (json_result (`Assoc [
    "commit_sha", `String sha;
    "file", `String file_path;
    "diff", `String (if String.length diff > 5000
      then String.sub diff 0 5000 ^ "\n... (truncated)" else diff);
    "explanatory_steps", `List rows;
    "n_steps", `Int (List.length rows);
  ]))

let handle_commit_links st args =
  let open Yojson.Safe.Util in
  let sha = args |> member "commit_sha" |> to_string in
  let db = ensure_db st in
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE s.commit_after LIKE ? OR s.commit_before LIKE ? \
       ORDER BY s.timestamp ASC"
      step_select_cols
  in
  let sha_like = sha ^ "%" in
  let rows = D.query_list db sql
    [S.Data.TEXT sha_like; S.Data.TEXT sha_like]
    ~f:step_row_to_json in
  Lwt.return (json_result (`Assoc [
    "commit_sha", `String sha;
    "n_steps", `Int (List.length rows);
    "steps", `List rows;
  ]))

let handle_search_by_file st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let n = try args |> member "n" |> to_int with _ -> 10 in
  let basename = Filename.basename file_path in
  let db = ensure_db st in
  (* First pass: exact files_touched containment; then fall back to FTS5
     which indexes the basename if it appears in summary/tags/prompt. *)
  let sql =
    Printf.sprintf
      "SELECT %s FROM steps s \
       WHERE s.files_touched LIKE ? \
       ORDER BY s.timestamp DESC LIMIT ?"
      step_select_cols
  in
  let rows = D.query_list db sql
    [S.Data.TEXT ("%\"" ^ basename ^ "\"%");
     S.Data.INT (Int64.of_int n)]
    ~f:step_row_to_json in
  let hits_json =
    if List.length rows >= n then rows
    else
      let fts_hits = Search.run ~db ~limit:n basename in
      rows @ List.map hit_to_json fts_hits
  in
  Lwt.return (json_result (`Assoc [
    "file", `String basename;
    "n_results", `Int (List.length hits_json);
    "results", `List hits_json;
  ]))

(* ---------- Push to TUI (unchanged) ---------- *)

(* Fetch one turn's full prompt + assistant text. Cheap, pay-per-turn
   alternative to dumping everything in [search_history]. *)
let handle_get_turn st args =
  let open Yojson.Safe.Util in
  let session_id = args |> member "session_id" |> to_string in
  let turn_index = args |> member "turn_index" |> to_int in
  let assistant_text =
    Search.fetch_assistant_text
      ~project_dir:st.project_dir ~session_id ~turn_index in
  (* Also pull the user prompt from the steps row. *)
  let db = ensure_db st in
  let sql =
    "SELECT COALESCE(prompt_text,''), COALESCE(summary,''), timestamp \
     FROM steps WHERE session_id = ? AND turn_index = ? LIMIT 1" in
  let row =
    D.query_list db sql
      [S.Data.TEXT session_id; S.Data.INT (Int64.of_int turn_index)]
      ~f:(fun cols ->
        `Assoc [
          "prompt_text", `String (D.data_to_string cols.(0));
          "summary",     `String (D.data_to_string cols.(1));
          "timestamp",   `Float  (D.data_to_float  cols.(2));
        ]) in
  let base = match row with
    | x :: _ -> x
    | [] -> `Assoc [
      "prompt_text", `String "";
      "summary", `String "";
      "timestamp", `Float 0.;
    ] in
  let merged = match base with
    | `Assoc fs ->
      `Assoc (fs @ [
        "session_id", `String session_id;
        "turn_index", `Int turn_index;
        "assistant_text", `String assistant_text;
      ])
    | other -> other in
  Lwt.return (json_result merged)

(* Push the calling Claude's final synthesis to the running URME TUI,
   along with the full text of the cited turns (so the user can verify
   the conclusion against the evidence). This is the only socket push
   the search pipeline makes — `search_history` itself is silent. *)
(* Find substring [needle] in [s], starting at [from]. Returns the
   index of the first match or None. *)
let find_substring_from s ~from needle =
  let nlen = String.length needle in
  let slen = String.length s in
  if nlen = 0 || from < 0 || from + nlen > slen then None
  else
    let rec scan i =
      if i + nlen > slen then None
      else if String.sub s i nlen = needle then Some i
      else scan (i + 1)
    in scan from

(* Walk forward from [start] (which must point at '['), tracking bracket
   depth and string state, return the index just past the matching ']'.
   Used to extract a complete JSON array from inside a free-text blob. *)
let json_array_end s start =
  let len = String.length s in
  if start >= len || s.[start] <> '[' then None
  else
    let depth = ref 0 in
    let in_str = ref false in
    let escape = ref false in
    let i = ref start in
    let result = ref None in
    while !result = None && !i < len do
      let c = s.[!i] in
      (if !in_str then
         if !escape then escape := false
         else if c = '\\' then escape := true
         else if c = '"' then in_str := false
         else ()
       else
         match c with
         | '"' -> in_str := true
         | '[' -> incr depth
         | ']' ->
           decr depth;
           if !depth = 0 then result := Some (!i + 1)
         | _ -> ());
      incr i
    done;
    !result

(* Salvage a [cited] array that the model embedded in the synthesis
   string as <parameter name="cited">[...]</parameter> instead of
   passing it as a sibling JSON argument. Returns (cleaned_synthesis,
   cited_json) when found, else (synthesis, `Null). *)
let salvage_embedded_cited synthesis =
  let tag = "<parameter name=\"cited\">" in
  match find_substring_from synthesis ~from:0 tag with
  | None -> (synthesis, `Null)
  | Some tag_pos ->
    let arr_start = tag_pos + String.length tag in
    (* Skip whitespace before the '['. *)
    let len = String.length synthesis in
    let rec skip_ws i =
      if i < len && (synthesis.[i] = ' ' || synthesis.[i] = '\n'
                     || synthesis.[i] = '\t' || synthesis.[i] = '\r')
      then skip_ws (i + 1) else i
    in
    let arr_start = skip_ws arr_start in
    match json_array_end synthesis arr_start with
    | None -> (synthesis, `Null)
    | Some arr_end ->
      let arr_text = String.sub synthesis arr_start (arr_end - arr_start) in
      (match try Some (Yojson.Safe.from_string arr_text) with _ -> None with
       | Some json ->
         (* Strip from tag_pos to either end of </parameter> or arr_end. *)
         let close_tag = "</parameter>" in
         let strip_end =
           match find_substring_from synthesis ~from:arr_end close_tag with
           | Some p -> p + String.length close_tag
           | None -> arr_end in
         let before = String.sub synthesis 0 tag_pos in
         let after = String.sub synthesis strip_end (len - strip_end) in
         (String.trim (before ^ after), json)
       | None -> (synthesis, `Null))

let handle_push_synthesis st args =
  let open Yojson.Safe.Util in
  let synthesis_raw =
    match args |> member "synthesis" with
    | `String s when String.trim s <> "" -> s
    | `String _ ->
      failwith "push_synthesis: 'synthesis' must be a non-empty string"
    | `Null ->
      failwith "push_synthesis: missing required field 'synthesis'"
    | _ ->
      failwith "push_synthesis: 'synthesis' must be a string" in
  let cited_arg = args |> member "cited" in
  (* If the model leaked the cited array into the synthesis text as
     <parameter name="cited">[...]</parameter>, recover it and strip
     the XML from the displayed synthesis. *)
  let synthesis, cited_arg =
    match cited_arg with
    | `List (_ :: _) -> (synthesis_raw, cited_arg)
    | _ ->
      let cleaned, salvaged = salvage_embedded_cited synthesis_raw in
      (cleaned, (match salvaged with `Null -> cited_arg | j -> j)) in
  let cited =
    match cited_arg with
    | `Null -> []
    | `List xs ->
      List.mapi (fun i j ->
        let sid =
          match j |> member "session_id" with
          | `String s when s <> "" -> s
          | _ ->
            failwith (Printf.sprintf
              "push_synthesis: cited[%d].session_id must be a non-empty string"
              i) in
        let turn =
          match j |> member "turn_index" with
          | `Int n -> n
          | _ ->
            failwith (Printf.sprintf
              "push_synthesis: cited[%d].turn_index must be an integer" i) in
        (sid, turn)) xs
    | _ ->
      failwith "push_synthesis: 'cited' must be a JSON array" in
  let cited_results =
    List.map (fun (session_id, turn_index) ->
      let assistant_text =
        Search.fetch_assistant_text
          ~project_dir:st.project_dir ~session_id ~turn_index in
      let db = ensure_db st in
      let sql =
        "SELECT COALESCE(prompt_text,''), COALESCE(summary,''), timestamp \
         FROM steps WHERE session_id = ? AND turn_index = ? LIMIT 1" in
      let row =
        D.query_list db sql
          [S.Data.TEXT session_id; S.Data.INT (Int64.of_int turn_index)]
          ~f:(fun cols ->
            (D.data_to_string cols.(0),
             D.data_to_string cols.(1),
             D.data_to_float  cols.(2))) in
      let prompt_text, summary, timestamp = match row with
        | x :: _ -> x
        | [] -> ("", "", 0.) in
      `Assoc [
        "session_id", `String session_id;
        "turn_index", `Int turn_index;
        "timestamp", `Float timestamp;
        "summary", `String summary;
        "prompt_text", `String prompt_text;
        "assistant_text", `String assistant_text;
      ]) cited in
  let tui_msg = `Assoc [
    "type", `String "synthesis";
    "synthesis", `String synthesis;
    "cited", `List cited_results;
  ] in
  let* () = send_to_tui ~project_dir:st.project_dir ~msg:tui_msg in
  Lwt.return (text_result
    (Printf.sprintf "Pushed synthesis (%d cited) to TUI."
       (List.length cited_results)))

(* ---------- Annotated call graph ---------- *)

(* Build the call graph by running the opengrep interfile exporter (the
   static analyzer — descriptions still come from the calling session),
   then load the JSON into the store. *)
let handle_graph_init st args =
  let open Yojson.Safe.Util in
  let lang = args |> member "lang" |> to_string in
  let root =
    match args |> member "root" with
    | `String r when r <> "" ->
      if Filename.is_relative r then Filename.concat st.project_dir r else r
    | _ -> st.project_dir in
  let ncores = try args |> member "ncores" |> to_int with _ -> 4 in
  let exporter =
    match args |> member "exporter" with
    | `String e when e <> "" -> e
    | _ ->
      (match Sys.getenv_opt "URME_OPENGREP_EXPORTER" with
       | Some p when p <> "" -> p
       | _ -> "opengrep-interfile-graph") in
  let urme_dir = Filename.concat st.project_dir ".urme" in
  if not (Sys.file_exists urme_dir) then Unix.mkdir urme_dir 0o755;
  let out_json =
    Filename.concat urme_dir (Printf.sprintf "callgraph-%s.json" lang) in
  let cmd =
    (exporter,
     [| exporter; "export"; "--lang"; lang; "-r"; root;
        "-o"; out_json; "-j"; string_of_int ncores |]) in
  let tail s =
    let n = String.length s in
    if n > 800 then String.sub s (n - 800) 800 else s in
  Lwt.catch
    (fun () ->
      let proc = Lwt_process.open_process_full cmd in
      let* out = Lwt_io.read proc#stdout in
      let* err = Lwt_io.read proc#stderr in
      let* status = proc#close in
      match status with
      | Unix.WEXITED 0 ->
        let db = ensure_db st in
        let (nn, ne) =
          Urme_engine.Callgraph_load.build ~db ~json_path:out_json in
        let (total, described, ready) = Cg.status db in
        let note =
          if nn = 0 then
            "0 nodes: the analyzer found no files — check `lang`, and \
             point `root` at a standalone repo root (target discovery \
             skips paths nested inside another git repo)."
          else
            "Graph loaded. Now annotate leaves-first: call \
             graph_next_batch, write a short but comprehensive \
             description for every returned function (lead with the \
             binding name for _tmp_lambda nodes), post them with \
             graph_set_descriptions, \
             and repeat until remaining = 0. For large graphs, split \
             batches across parallel subagents — ready units are \
             independent." in
        Lwt.return (json_result (`Assoc [
          "lang", `String lang;
          "root", `String root;
          "json", `String out_json;
          "nodes", `Int nn;
          "edges", `Int ne;
          "total", `Int total;
          "described", `Int described;
          "remaining", `Int (total - described);
          "ready_units", `Int ready;
          "note", `String note;
        ]))
      | _ ->
        let code = match status with
          | Unix.WEXITED c -> Printf.sprintf "exit %d" c
          | Unix.WSIGNALED s -> Printf.sprintf "signal %d" s
          | Unix.WSTOPPED s -> Printf.sprintf "stopped %d" s in
        Lwt.return (text_result (Printf.sprintf
          "graph_init: %s failed (%s)%s.\nstdout: %s\nstderr: %s"
          exporter code
          (if code = "exit 127" then
             " — binary not found; set URME_OPENGREP_EXPORTER or put \
              opengrep-interfile-graph on PATH"
           else "")
          (tail out) (tail err))))
    (fun exn ->
      Lwt.return (text_result (Printf.sprintf
        "graph_init: could not run %s (%s). Set URME_OPENGREP_EXPORTER \
         to the opengrep-interfile-graph binary or put it on PATH."
        exporter (Printexc.to_string exn))))

let handle_graph_status st _args =
  let db = ensure_db st in
  let (total, described, ready) = Cg.status db in
  Lwt.return (json_result (`Assoc [
    "total", `Int total;
    "described", `Int described;
    "remaining", `Int (total - described);
    "ready_units", `Int ready;
  ]))

let desc_pair (nm, d) =
  `Assoc [ "name", `String nm;
           "description", (match d with Some s -> `String s | None -> `Null) ]

(* Neighbour lists carry names only — an agent that needs a neighbour's
   summary describes it directly, instead of every hit re-sending full
   paragraphs for its whole fan-out. Rendered as compact strings: bare
   "name" when the neighbour lives in the same file as the hit,
   "name (file)" otherwise. *)
let neighbor_str ~hit_file (nm, file) =
  if file = hit_file then `String nm
  else `String (Printf.sprintf "%s (%s)" nm file)

(* First sentence of a description — the "gist" for wide bulk pulls where
   full paragraphs would blow the tool-result size budget. Cuts at the
   first ". " (or the whole string if none), capped so a runaway summary
   can't dominate. *)
let first_sentence s =
  let n = String.length s in
  let stop = ref n in
  let i = ref 0 in
  while !i < n - 1 && !stop = n do
    (if s.[!i] = '.' && s.[!i + 1] = ' ' then stop := !i + 1);
    incr i
  done;
  let stop = if !stop > 220 then 220 else !stop in
  if stop >= n then s else String.sub s 0 stop

(* Node paths are stored relative to the analyzed root (meta cg_root);
   resolve before reading source off disk. *)
let cg_abs_file db file =
  if file <> "" && Filename.is_relative file then
    match Schema.get_meta db "cg_root" with
    | Some root when root <> "" -> Filename.concat root file
    | _ -> file
  else file

let handle_graph_next_batch st args =
  let open Yojson.Safe.Util in
  let limit = try args |> member "limit" |> to_int with _ -> 5 in
  let db = ensure_db st in
  let sccs = Cg.ready_sccs db ~limit:(max 1 limit) in
  let unit_json scc =
    let members = Cg.scc_members db ~scc in
    let callees = Cg.scc_callee_descriptions db ~scc in
    let fns = List.map (fun (m : Cg.node) ->
      let code = Urme_engine.Callgraph_load.extract_code
          ~file:(cg_abs_file db m.file)
          ~start_line:m.start_line ~end_line:m.end_line in
      `Assoc [
        "id", `String m.id;
        "name", `String m.name;
        "file", `String m.file;
        "start_line", `Int m.start_line;
        "end_line", `Int m.end_line;
        "kind", `String m.kind;
        "code", `String code;
      ]) members in
    `Assoc [
      "scc", `Int scc;
      "recursive", `Bool (List.length members > 1);
      "functions", `List fns;
      "callees", `List (List.map desc_pair callees);
    ] in
  let batch = List.map unit_json sccs in
  let (total, described, ready) = Cg.status db in
  Lwt.return (json_result (`Assoc [
    "total", `Int total;
    "described", `Int described;
    "remaining", `Int (total - described);
    "ready_units", `Int ready;
    "batch", `List batch;
    "note", `String
      "Describe every function in `functions` — a short but comprehensive \
       description (usually 1-2 sentences: what it does, its \
       type/signature, and anything non-obvious a caller must know), \
       using `callees` descriptions for context. \
       Recursive units: describe the group together. Then call \
       graph_set_descriptions with {id, description} for each, and call \
       graph_next_batch again until remaining = 0.";
  ]))

let handle_graph_set_descriptions st args =
  let open Yojson.Safe.Util in
  let db = ensure_db st in
  let items = try args |> member "descriptions" |> to_list with _ -> [] in
  let written =
    List.fold_left (fun acc j ->
      match (try Some (j |> member "id" |> to_string) with _ -> None),
            (try Some (j |> member "description" |> to_string) with _ -> None)
      with
      | Some id, Some d when String.trim d <> "" ->
        Cg.set_description db ~id ~description:d ~code_hash:None;
        acc + 1
      | _ -> acc) 0 items in
  let (total, described, ready) = Cg.status db in
  (* Dispatch edges are derived from the descriptions; once every function
     is annotated, (re)materialise them so graph_neighborhood can traverse
     dynamic-dispatch paths. *)
  if total > 0 && described >= total then
    (try Cg.populate_dispatch_edges db with _ -> ());
  Lwt.return (json_result (`Assoc [
    "written", `Int written;
    "total", `Int total;
    "described", `Int described;
    "remaining", `Int (total - described);
    "ready_units", `Int ready;
  ]))

let handle_graph_describe st args =
  let open Yojson.Safe.Util in
  let db = ensure_db st in
  let query = args |> member "query" |> to_string in
  let include_code =
    try args |> member "include_code" |> to_bool with _ -> false in
  (* A dispatched-to function (task queue, plugin registry, signal) has
     no static caller edge, but its dispatchers' summaries name it: when
     the callers list is empty, attach the functions whose descriptions
     mention this one. Synthetic names (<top_level>, _tmp_lambda) would
     only phrase-match noise, so skip them. *)
  let mentions (m : Cg.found) =
    let synthetic =
      m.fname = "" || m.fname.[0] = '<'
      || (String.length m.fname >= 5 && String.sub m.fname 0 5 = "_tmp_") in
    if synthetic then []
    else
      try Cg.mentioned_by db ~name:m.fname ~exclude_id:m.fid ~limit:5
      with _ -> [] in
  let mention_json (h : Cg.found) =
    `Assoc [
      "name", `String h.fname;
      "file", `String h.ffile;
      "line", `Int h.fstart;
      "description",
        (match h.fdesc with Some s -> `String s | None -> `Null);
    ] in
  let include_callees =
    try args |> member "include_callees" |> to_bool with _ -> false in
  (* No `id`: it only restates name|file|line, and consumers query by
     name. No end_line/normal-kind either — dead weight per match.
     Callees are omitted by default: descriptions are written leaves-first
     with callee summaries in hand, so the ones that matter are already
     named in the prose. Callers can't be embedded that way (they don't
     exist yet at annotation time), so they are always returned. *)
  let match_json (m : Cg.found) =
    let callers = Cg.callers db ~id:m.fid in
    let nb = neighbor_str ~hit_file:m.ffile in
    let base = [
      "name", `String m.fname;
      "file", `String m.ffile;
      "line", `Int m.fstart;
    ] in
    let base =
      if m.fkind = "normal" then base
      else base @ [ "kind", `String m.fkind ] in
    let base = base @ [
      "description",
        (match m.fdesc with Some s -> `String s | None -> `Null);
    ] in
    let base =
      if include_callees then
        base @ [ "callees", `List (List.map nb (Cg.callees db ~id:m.fid)) ]
      else base in
    let base = base @ [ "callers", `List (List.map nb callers) ] in
    let base =
      match (if callers = [] then mentions m else []) with
      | [] -> base
      | ms ->
        base @ [ "mentioned_by", `List (List.map mention_json ms) ] in
    let base =
      if include_code then
        base @ [ "code", `String (Urme_engine.Callgraph_load.extract_code
                                    ~file:(cg_abs_file db m.ffile)
                                    ~start_line:m.fstart
                                    ~end_line:m.fend) ]
      else base in
    `Assoc base in
  (* Lambdas carry synthetic node names (e.g. _tmp_lambda), so an exact
     name miss falls back to FTS over name+description — the described
     graph usually knows the binding name from the summary text. *)
  let matches =
    match Cg.lookup db ~query with
    | [] -> (try Cg.search db ~fts:query ~limit:10 with _ -> [])
    | ms -> ms in
  Lwt.return (json_result (`Assoc [
    "query", `String query;
    "n_matches", `Int (List.length matches);
    "matches", `List (List.map match_json matches);
  ]))

(* Reachable-subgraph query: seed from named functions, walk call edges
   `depth` hops in a direction, return only the closure. Turns a question
   ("how does issuance work") into exactly the relevant nodes — the
   entry points' callee-closure — instead of a whole-package dump. *)
let handle_graph_neighborhood st args =
  let open Yojson.Safe.Util in
  let db = ensure_db st in
  let roots =
    try args |> member "roots" |> to_list |> List.filter_map (fun j ->
      try Some (to_string j) with _ -> None)
    with _ -> [] in
  let direction =
    match (try args |> member "direction" |> to_string with _ -> "callees") with
    | "callers" -> Cg.Callers | "both" -> Cg.Both | _ -> Cg.Callees in
  let depth =
    let d = try args |> member "depth" |> to_int with _ -> 3 in
    max 1 (min 8 d) in
  let limit = try args |> member "limit" |> to_int with _ -> 250 in
  let include_code =
    try args |> member "include_code" |> to_bool with _ -> false in
  let detail = try args |> member "detail" |> to_string with _ -> "" in
  let follow_dispatch =
    try args |> member "follow_dispatch" |> to_bool with _ -> true in
  (* Lazy backfill: a graph annotated before dispatch edges existed has an
     empty cg_dispatch; populate it once so this (and later) traversals
     can follow dynamic dispatch. *)
  (if follow_dispatch && Cg.dispatch_edge_count db = 0
      && (let (t, d, _) = Cg.status db in t > 0 && d >= t)
   then try Cg.populate_dispatch_edges db with _ -> ());
  let capped = max 1 (min 2000 limit) in
  let nodes =
    if roots = [] then []
    else try Cg.neighborhood db ~roots ~direction ~depth ~follow_dispatch
               ~limit:capped
         with _ -> [] in
  let brief =
    detail = "brief"
    || (detail <> "full" && not include_code && List.length nodes > 60) in
  let node_json (m : Cg.found) =
    let base = [
      "name", `String m.fname;
      "file", `String m.ffile;
      "line", `Int m.fstart;
    ] in
    let base =
      if m.fkind = "normal" then base else base @ [ "kind", `String m.fkind ] in
    let base = base @ [
      "description",
        (match m.fdesc with
         | Some s -> `String (if brief then first_sentence s else s)
         | None -> `Null);
    ] in
    if include_code then
      base @ [ "code", `String (Urme_engine.Callgraph_load.extract_code
                                  ~file:(cg_abs_file db m.ffile)
                                  ~start_line:m.fstart ~end_line:m.fend) ]
    else base in
  let dir_s = match direction with
    | Cg.Callees -> "callees" | Cg.Callers -> "callers" | Cg.Both -> "both" in
  Lwt.return (json_result (`Assoc [
    "roots", `List (List.map (fun r -> `String r) roots);
    "direction", `String dir_s;
    "depth", `Int depth;
    "n_nodes", `Int (List.length nodes);
    "detail", `String (if brief then "brief" else "full");
    "nodes", `List (List.map (fun m -> `Assoc (node_json m)) nodes);
    "note", `String
      "The reachable subgraph from the seed functions (deduped, \
       leaves-first). This is the task-relevant slice — not a whole-file \
       dump. Widen with a larger `depth`; use graph_describe for a \
       specific function's callers/mentioned_by.";
  ]))

let handle_graph_search st args =
  let open Yojson.Safe.Util in
  let db = ensure_db st in
  let fts = try args |> member "fts_terms" |> to_string with _ -> "" in
  let limit = try args |> member "limit" |> to_int with _ -> 15 in
  let neighbors = try args |> member "neighbors" |> to_bool with _ -> true in
  (* FTS MATCH can raise on odd syntax; degrade to no hits rather than fail. *)
  let hits =
    if String.trim fts = "" then []
    else try Cg.search db ~fts ~limit:(max 1 (min 100 limit)) with _ -> [] in
  (* Callers only: callee knowledge is already embedded in the
     description (written leaves-first with callee summaries in hand). *)
  let hit_json (m : Cg.found) =
    let nb = neighbor_str ~hit_file:m.ffile in
    let base = [
      "name", `String m.fname;
      "file", `String m.ffile;
      "line", `Int m.fstart;
      "description",
        (match m.fdesc with Some s -> `String s | None -> `Null);
    ] in
    let base =
      if neighbors then
        base @ [ "callers", `List (List.map nb (Cg.callers db ~id:m.fid)) ]
      else base in
    `Assoc base in
  Lwt.return (json_result (`Assoc [
    "fts_terms", `String fts;
    "n_results", `Int (List.length hits);
    "results", `List (List.map hit_json hits);
  ]))

(* Run a caller-supplied read-only SELECT over the graph schema and return
   columns + rows. The model writes the query for its question — one tool
   subsumes search / describe / overview / neighborhood / trace. Safety:
   PRAGMA query_only makes any write fail at runtime (real enforcement,
   no keyword heuristics), rows are capped, and only SELECT/WITH start. *)
let handle_graph_query st args =
  let open Yojson.Safe.Util in
  let db = ensure_db st in
  let sql = try args |> member "sql" |> to_string with _ -> "" in
  let limit =
    let l = try args |> member "max_rows" |> to_int with _ -> 300 in
    max 1 (min 1000 l) in
  let low = String.lowercase_ascii (String.trim sql) in
  let starts p =
    String.length low >= String.length p && String.sub low 0 (String.length p) = p in
  if String.trim sql = "" then
    Lwt.return (json_result (`Assoc [ "error", `String "graph_query: empty sql" ]))
  else if not (starts "select" || starts "with") then
    Lwt.return (json_result (`Assoc [
      "error", `String
        "graph_query: only a single read-only SELECT/WITH query is allowed." ]))
  else begin
    (try D.exec db "PRAGMA query_only=ON" with _ -> ());
    let result =
      try Ok (D.query_rows ~max_rows:limit db sql)
      with e -> Error (Printexc.to_string e) in
    (try D.exec db "PRAGMA query_only=OFF" with _ -> ());
    match result with
    | Error msg ->
      Lwt.return (json_result (`Assoc [
        "error", `String ("SQL error: " ^ msg);
        "hint", `String
          "Schema: cg_nodes(id,name,file,start_line,end_line,kind,scc,topo,\
           description); cg_edges(src,dst) caller->callee; cg_dispatch(src,dst) \
           dynamic dispatcher->target; cg_fts(node_id,name,description) FTS5 \
           (use: cg_fts MATCH 'description:\"term\"'). Paths are repo-relative.";
      ]))
    | Ok (headers, rows, truncated) ->
      let data_json = function
        | S.Data.NULL | S.Data.NONE -> `Null
        | S.Data.INT i -> `Int (Int64.to_int i)
        | S.Data.FLOAT f -> `Float f
        | S.Data.TEXT s | S.Data.BLOB s -> `String s in
      let row_json r = `List (Array.to_list (Array.map data_json r)) in
      Lwt.return (json_result (`Assoc [
        "columns", `List (List.map (fun h -> `String h) headers);
        "n_rows", `Int (List.length rows);
        "truncated", `Bool truncated;
        "rows", `List (List.map row_json rows);
      ]))
  end

(* ---------- Dispatch ---------- *)

let dispatch st name args =
  match name with
  | "search_history" -> handle_search_history st args
  | "push_synthesis" -> handle_push_synthesis st args
  | "get_turn"       -> handle_get_turn st args
  | "file_history"   -> handle_file_history st args
  | "region_blame"   -> handle_region_blame st args
  | "explain_change" -> handle_explain_change st args
  | "commit_links"   -> handle_commit_links st args
  | "search_by_file" -> handle_search_by_file st args
  | "graph_init"             -> handle_graph_init st args
  | "graph_status"           -> handle_graph_status st args
  | "graph_next_batch"       -> handle_graph_next_batch st args
  | "graph_set_descriptions" -> handle_graph_set_descriptions st args
  | "graph_describe"         -> handle_graph_describe st args
  | "graph_search"           -> handle_graph_search st args
  | "graph_neighborhood"     -> handle_graph_neighborhood st args
  | "graph_query"            -> handle_graph_query st args
  | _ -> Lwt.return (text_result (Printf.sprintf "Unknown tool: %s" name))
