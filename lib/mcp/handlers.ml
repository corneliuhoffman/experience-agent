(* MCP tool handlers — dispatch to existing urme libraries *)

open Lwt.Syntax
open Urme_engine.Git_link_types

type state = {
  project_dir : string;
  port : int;
  mutable collection_id : string option;
  mutable repo : Urme_store.Project_store.Store.Repo.t option;
  mutable edits : edit list option;
  mutable branch_label : (string, string) Hashtbl.t option;
}

let create_state ~port ~project_dir =
  let project_dir = if project_dir = "." || project_dir = "./"
    then Sys.getcwd ()
    else if Filename.is_relative project_dir
    then Filename.concat (Sys.getcwd ()) project_dir
    else project_dir in
  { project_dir; port; collection_id = None;
    repo = None; edits = None; branch_label = None }

(* Lazy initialization helpers *)

let ensure_collection st =
  match st.collection_id with
  | Some id -> Lwt.return id
  | None ->
    let project = Filename.basename st.project_dir in
    let* id = Urme_search.Chromadb.ensure_interactions_collection
        ~port:st.port ~project in
    st.collection_id <- Some id;
    Lwt.return id

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

(* Result formatters *)

let text_result text =
  `Assoc [
    "content", `List [
      `Assoc ["type", `String "text"; "text", `String text];
    ];
  ]

let json_result j = text_result (Yojson.Safe.pretty_to_string j)

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

(* --- Tool implementations --- *)

let handle_search_history st args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let n = try args |> member "n" |> to_int with _ -> 5 in
  let* collection_id = ensure_collection st in
  let* results = Urme_search.Chromadb.search_all_interactions
      ~port:st.port ~collection_id ~query ~n in
  let items = List.map (fun (session_id, user_text, doc, idx, ts, dist) ->
    `Assoc [
      "session_id", `String session_id;
      "user_text", `String user_text;
      "interaction_index", `Int idx;
      "timestamp", `String ts;
      "distance", `Float dist;
      "document", `String (if String.length doc > 500
        then String.sub doc 0 500 ^ "..." else doc);
    ]
  ) results in
  Lwt.return (json_result (`Assoc [
    "query", `String query;
    "n_results", `Int (List.length items);
    "results", `List items;
  ]))

let handle_file_history st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* decompositions = Urme_engine.Git_link.file_history
      ~project_dir:st.project_dir ~file_path ~edits ~branch_label ~repo in
  let items = List.map decomposition_to_json decompositions in
  Lwt.return (json_result (`Assoc [
    "file_path", `String file_path;
    "n_commits", `Int (List.length items);
    "commits", `List items;
  ]))

let handle_region_blame st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let start_line = args |> member "start_line" |> to_int in
  let end_line = args |> member "end_line" |> to_int in
  (* Git blame *)
  let* blame_lines = Lwt.catch (fun () ->
    Urme_git.Ops.blame ~cwd:st.project_dir
      ~line_range:(start_line, end_line) ~filepath:file_path ()
  ) (fun _ -> Lwt.return []) in
  let blame_json = List.map (fun (sha, line_num, content) ->
    `Assoc [
      "sha", `String (if String.length sha > 8 then String.sub sha 0 8 else sha);
      "line", `Int line_num;
      "content", `String content;
    ]
  ) blame_lines in
  (* Claude edit attribution *)
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* decompositions = Urme_engine.Git_link.region_history
      ~project_dir:st.project_dir ~path:file_path
      ~start_line ~end_line ~edits ~branch_label ~repo in
  let decomp_json = List.map decomposition_to_json decompositions in
  Lwt.return (json_result (`Assoc [
    "file_path", `String file_path;
    "lines", `String (Printf.sprintf "%d-%d" start_line end_line);
    "blame", `List blame_json;
    "claude_history", `List decomp_json;
  ]))

let handle_explain_change st args =
  let open Yojson.Safe.Util in
  let sha = args |> member "commit_sha" |> to_string in
  let file_path = args |> member "file_path" |> to_string in
  let* edits = ensure_edits st in
  let* branch_label = ensure_branch_label st in
  let* repo = ensure_repo st in
  let* d = Urme_engine.Diff_match.decompose_diff ~sha ~file:file_path
      ~branch_label ~edits ~cwd:st.project_dir ~repo in
  let* diff = Lwt.catch (fun () ->
    Urme_git.Ops.commit_diff ~cwd:st.project_dir ~sha
  ) (fun _ -> Lwt.return "") in
  let n_claude = List.length (List.filter (fun item -> match item with
    | DirectEdit _ -> true | _ -> false) d.items) in
  let n_unexplained = List.length (List.filter (fun item -> match item with
    | Unexplained _ -> true | _ -> false) d.items) in
  Lwt.return (json_result (`Assoc [
    "commit_sha", `String sha;
    "file", `String file_path;
    "decomposition", decomposition_to_json d;
    "diff", `String (if String.length diff > 5000
      then String.sub diff 0 5000 ^ "\n... (truncated)" else diff);
    "summary", `String (Printf.sprintf "%d Claude edits, %d unexplained"
      n_claude n_unexplained);
  ]))

let handle_commit_links st args =
  let open Yojson.Safe.Util in
  let sha = args |> member "commit_sha" |> to_string in
  let* collection_id = ensure_collection st in
  let* all_gis = Urme_search.Chromadb.get_all_with_git_info
      ~port:st.port ~collection_id in
  let links = List.concat_map (fun (_id, gi_str, session_id, _idx, _ts) ->
    let tbl = parse_git_info_json gi_str in
    Hashtbl.fold (fun ek value acc ->
      match value with
      | Some gi when String.length gi.commit_sha >= String.length sha &&
                     String.sub gi.commit_sha 0 (String.length sha) = sha ->
        let file_base = match String.split_on_char ':' ek with
          | fb :: _ -> fb | [] -> "" in
        `Assoc [
          "file", `String file_base;
          "session_id", `String session_id;
          "turn_idx", `Int gi.turn_idx;
          "entry_idx", `Int gi.entry_idx;
          "edit_key", `String ek;
        ] :: acc
      | _ -> acc
    ) tbl []
  ) all_gis in
  Lwt.return (json_result (`Assoc [
    "commit_sha", `String sha;
    "n_links", `Int (List.length links);
    "links", `List links;
  ]))

let handle_search_by_file st args =
  let open Yojson.Safe.Util in
  let file_path = args |> member "file_path" |> to_string in
  let n = try args |> member "n" |> to_int with _ -> 10 in
  let basename = Filename.basename file_path in
  let* collection_id = ensure_collection st in
  let* results = Urme_search.Chromadb.search_all_interactions
      ~port:st.port ~collection_id ~query:basename ~n in
  let items = List.map (fun (session_id, user_text, doc, idx, ts, dist) ->
    `Assoc [
      "session_id", `String session_id;
      "user_text", `String user_text;
      "interaction_index", `Int idx;
      "timestamp", `String ts;
      "distance", `Float dist;
      "document", `String (if String.length doc > 500
        then String.sub doc 0 500 ^ "..." else doc);
    ]
  ) results in
  Lwt.return (json_result (`Assoc [
    "file", `String basename;
    "n_results", `Int (List.length items);
    "results", `List items;
  ]))

(* Dispatch *)

let dispatch st name args =
  match name with
  | "search_history" -> handle_search_history st args
  | "file_history" -> handle_file_history st args
  | "region_blame" -> handle_region_blame st args
  | "explain_change" -> handle_explain_change st args
  | "commit_links" -> handle_commit_links st args
  | "search_by_file" -> handle_search_by_file st args
  | _ -> Lwt.return (text_result (Printf.sprintf "Unknown tool: %s" name))
