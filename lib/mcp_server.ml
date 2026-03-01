open Lwt.Syntax
open Types

(* Tool definitions for tools/list *)
let tool_definitions = `List [
  `Assoc [
    "name", `String "save_experience";
    "description", `String "Label and index the current session's interactions. Creates a micro-commit with experience metadata, computes embeddings, and indexes in the vector DB for future search.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "label", `Assoc [
          "type", `String "string";
          "description", `String "Short description of what was accomplished";
        ];
        "intent", `Assoc [
          "type", `String "string";
          "description", `String "The original user request/intent";
        ];
        "from_interaction", `Assoc [
          "type", `String "integer";
          "description", `String "Index of the first interaction to include (0-based, default: 0 = all)";
        ];
      ];
      "required", `List [`String "label"; `String "intent"];
    ];
  ];
  `Assoc [
    "name", `String "search_experience";
    "description", `String "Search past saved experiences for relevant prior work. Use at the start of a task to find similar past solutions. Returns matching experiences with metadata.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of what you're looking for";
        ];
        "n_results", `Assoc [
          "type", `String "integer";
          "description", `String "Number of results to return (default 3)";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "replay_experience";
    "description", `String "Search for a past experience and return the full conversation data for replay. Returns the matching experience with the complete interaction transcript.\n\nFor depth 'full' or 'with_subagents', returns a web URL to view the formatted conversation in the browser instead of dumping raw data.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of the experience to replay";
        ];
        "id", `Assoc [
          "type", `String "string";
          "description", `String "Direct experience ID (commit SHA, alternative to query)";
        ];
        "depth", `Assoc [
          "type", `String "string";
          "description", `String "Level of detail: 'summary' (metadata only), 'full' (main conversation), 'with_subagents' (includes subagent work). Default: full";
        ];
      ];
    ];
  ];
  `Assoc [
    "name", `String "go_back";
    "description", `String "Revert code to a previous snapshot. Searches saved experiences, shows the match, then reverts code on confirmation. The conversation history stays intact as an audit trail. Reverted experiences are marked so they're deprioritized in future searches.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of the state to revert to";
        ];
        "confirm", `Assoc [
          "type", `String "boolean";
          "description", `String "Set to true to confirm and apply the rollback after reviewing the match.";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "link_commit";
    "description", `String "Retroactively tag an existing commit as an experience. Extracts diff and metadata from the commit, summarizes, and indexes for search.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "sha", `Assoc [
          "type", `String "string";
          "description", `String "Git commit SHA to link (default: current HEAD)";
        ];
        "label", `Assoc [
          "type", `String "string";
          "description", `String "Filter: only link experiences matching this label";
        ];
      ];
    ];
  ];
  `Assoc [
    "name", `String "list_experiences";
    "description", `String "List all saved experiences for this project.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "limit", `Assoc [
          "type", `String "integer";
          "description", `String "Maximum number of experiences to return (default 10)";
        ];
      ];
    ];
  ];
  `Assoc [
    "name", `String "delete_experience";
    "description", `String "Delete a saved experience by ID.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "id", `Assoc [
          "type", `String "string";
          "description", `String "The experience ID to delete";
        ];
      ];
      "required", `List [`String "id"];
    ];
  ];
  `Assoc [
    "name", `String "find_by_sha";
    "description", `String "Find experiences associated with a git commit SHA. Matches against commit_sha, head_sha, and id fields.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "sha", `Assoc [
          "type", `String "string";
          "description", `String "Git SHA to search for (full or prefix).";
        ];
      ];
      "required", `List [`String "sha"];
    ];
  ];
  `Assoc [
    "name", `String "search_by_file";
    "description", `String "Find experiences that changed a given file. Uses git history to walk commits that touched the file.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "filepath", `Assoc [
          "type", `String "string";
          "description", `String "File path to search for (relative to project root)";
        ];
      ];
      "required", `List [`String "filepath"];
    ];
  ];
  `Assoc [
    "name", `String "search_by_sha";
    "description", `String "Find experiences near a commit by walking the git DAG.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "sha", `Assoc [
          "type", `String "string";
          "description", `String "Git commit SHA to search around";
        ];
        "radius", `Assoc [
          "type", `String "integer";
          "description", `String "Number of commits to search in each direction (default 5)";
        ];
      ];
      "required", `List [`String "sha"];
    ];
  ];
  `Assoc [
    "name", `String "blame";
    "description", `String "Find which experience last changed each line of a file. Runs git blame and correlates commit SHAs with indexed experiences.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "filepath", `Assoc [
          "type", `String "string";
          "description", `String "File path relative to project root";
        ];
        "line_start", `Assoc [
          "type", `String "integer";
          "description", `String "Starting line number (optional, use with line_end)";
        ];
        "line_end", `Assoc [
          "type", `String "integer";
          "description", `String "Ending line number (optional, use with line_start)";
        ];
        "pattern", `Assoc [
          "type", `String "string";
          "description", `String "Only show lines matching this substring (optional)";
        ];
      ];
      "required", `List [`String "filepath"];
    ];
  ];
  `Assoc [
    "name", `String "explain_change";
    "description", `String "Explain WHY code was changed. Given a file and function/pattern, finds the experiences that last touched those lines and returns the full conversation context — including the user's request, assistant's thinking, and what was done. Use when asked 'why did we change X?' or 'what was the reasoning for this code?'";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "filepath", `Assoc [
          "type", `String "string";
          "description", `String "File path relative to project root";
        ];
        "pattern", `Assoc [
          "type", `String "string";
          "description", `String "Function name or code pattern to search for in the file";
        ];
        "line_start", `Assoc [
          "type", `String "integer";
          "description", `String "Starting line number (alternative to pattern)";
        ];
        "line_end", `Assoc [
          "type", `String "integer";
          "description", `String "Ending line number (alternative to pattern)";
        ];
      ];
      "required", `List [`String "filepath"];
    ];
  ];
]

let text_result text =
  `Assoc [
    "content", `List [
      `Assoc ["type", `String "text"; "text", `String text];
    ];
  ]

let json_result j =
  text_result (Yojson.Safe.pretty_to_string j)

(* State *)
type state = {
  port : int;
  web_port : int;
  project_dir : string;
  jsonl_dir : string;
  mutable collection_id : string option;
  mutable interactions_collection_id : string option;
  mutable last_undo_match : search_result option;
}

let project_name state =
  let dir = if state.project_dir = "." || state.project_dir = "./"
    then Sys.getcwd ()
    else if Filename.is_relative state.project_dir
    then Filename.concat (Sys.getcwd ()) state.project_dir
    else state.project_dir in
  Filename.basename dir

let get_collection state =
  match state.collection_id with
  | Some id -> Lwt.return id
  | None ->
    let project = project_name state in
    let+ id = Chromadb.ensure_collection ~port:state.port ~project in
    state.collection_id <- Some id;
    id

let get_interactions_collection state =
  match state.interactions_collection_id with
  | Some id -> Lwt.return id
  | None ->
    let project = project_name state in
    let+ id = Chromadb.ensure_interactions_collection ~port:state.port ~project in
    state.interactions_collection_id <- Some id;
    id

(* Tool handlers *)

(* save_experience: creates a micro-commit with experience metadata *)
let handle_save_experience state args =
  let open Yojson.Safe.Util in
  let label = args |> member "label" |> to_string in
  let intent = args |> member "intent" |> to_string in
  let from_idx = args |> member "from_interaction" |> to_int_option
    |> Option.value ~default:0 in
  (* Find current session JSONL *)
  match Jsonl_reader.current_session ~jsonl_dir:state.jsonl_dir with
  | None ->
    Lwt.return (text_result "No session JSONL found. Is Claude Code running?")
  | Some filepath ->
    let session_id = Jsonl_reader.session_id_of_path filepath in
    let interactions = Jsonl_reader.parse_interactions ~filepath in
    let selected = List.filter (fun (i : interaction) ->
      i.index >= from_idx
    ) interactions in
    if selected = [] then
      Lwt.return (text_result "No interactions found in current session.")
    else begin
      let* collection_id = get_collection state in
      (* Gather data *)
      let interaction_uuids = List.map (fun (i : interaction) ->
        i.user_uuid) selected in
      let all_files = List.concat_map (fun (i : interaction) ->
        i.files_changed) selected
        |> List.sort_uniq String.compare in
      (* Extract conversational content *)
      let full_conversation = String.concat "\n---\n"
        (List.map (Jsonl_reader.summarizable_text ~filepath) selected) in
      (* Get current state *)
      let* head_sha = Git_ops.current_head ~cwd:state.project_dir in
      let* branch = Git_ops.current_branch ~cwd:state.project_dir in
      (* Build commit message with experience metadata *)
      let uuids_str = String.concat "," interaction_uuids in
      let commit_message = Printf.sprintf
        "[experience] %s\n\nintent: %s\nsession: %s\nuuids: %s"
        label intent session_id uuids_str in
      (* Create real commit *)
      let* commit_sha = Git_ops.create_commit ~cwd:state.project_dir
        ~message:commit_message () in
      (* Get diff from commit *)
      let* diff = Git_ops.commit_diff ~cwd:state.project_dir ~sha:commit_sha in
      let* changed = Git_ops.commit_changed_files ~cwd:state.project_dir
        ~sha:commit_sha in
      let files_changed = if changed <> [] then changed else all_files in
      let exp : experience = {
        id = commit_sha;
        label;
        intent;
        session_id;
        interaction_uuids;
        head_sha;
        commit_sha;
        commit_message;
        parent_sha = head_sha;
        diff;
        branch;
        files_changed;
        timestamp = Unix.gettimeofday ();
        reverted = false;
      } in
      let* () = Chromadb.save_experience ~port:state.port ~collection_id
        exp ~full_conversation in
      (* Save per-interaction embeddings *)
      let* int_collection_id = get_interactions_collection state in
      let* () = Lwt_list.iter_s (fun (i : interaction) ->
        Chromadb.save_interaction ~port:state.port
          ~collection_id:int_collection_id
          ~experience_id:commit_sha
          ~interaction_index:i.index
          ~user_text:i.user_text
          ~assistant_summary:i.assistant_summary
          ~user_uuid:i.user_uuid
          ~timestamp:i.timestamp
      ) selected in
      Lwt.return (json_result (`Assoc [
        "id", `String commit_sha;
        "label", `String label;
        "session_id", `String session_id;
        "interactions_count", `Int (List.length selected);
        "commit_sha", `String commit_sha;
        "head_sha", `String head_sha;
        "branch", `String branch;
        "files_changed", `List (List.map (fun f -> `String f) files_changed);
      ]))
    end

let handle_search_experience state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let n = args |> member "n_results" |> to_int_option |> Option.value ~default:3 in
  let* collection_id = get_collection state in
  let+ results = Chromadb.search ~port:state.port ~collection_id ~query ~n in
  json_result (`Assoc [
    "results", `List (List.map search_result_to_yojson results);
  ])

let handle_replay_experience state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string_option in
  let id = args |> member "id" |> to_string_option in
  let depth = args |> member "depth" |> to_string_option
    |> Option.value ~default:"full" in
  let* collection_id = get_collection state in
  (* Find the experience *)
  let* exp_opt = match id with
    | Some exp_id ->
      let+ exps = Chromadb.list_all ~port:state.port ~collection_id ~limit:100 in
      List.find_opt (fun (e : experience) -> e.id = exp_id) exps
    | None ->
      match query with
      | Some q ->
        let+ results = Chromadb.search ~port:state.port ~collection_id ~query:q ~n:1 in
        (match results with
         | r :: _ -> Some r.experience
         | [] -> None)
      | None -> Lwt.return None
  in
  match exp_opt with
  | None ->
    Lwt.return (text_result "No matching experience found.")
  | Some exp ->
    if depth = "summary" then
      Lwt.return (json_result (experience_to_yojson exp))
    else begin
      (* Return a web URL for viewing the full conversation *)
      let url =
        if state.web_port > 0 then
          Printf.sprintf "http://localhost:%d/?id=%s" state.web_port exp.id
        else
          ""
      in
      let summary = Printf.sprintf "**%s**\n%s\n\nCommit: %s\nBranch: %s\nTimestamp: %.0f%s"
        exp.label exp.intent
        (String.sub exp.id 0 (min 8 (String.length exp.id)))
        exp.branch
        exp.timestamp
        (if exp.reverted then " (reverted)" else "")
      in
      let fields = [
        "experience", experience_to_yojson exp;
        "summary", `String summary;
      ] @ (if url <> "" then [
        "url", `String url;
        "view", `String (Printf.sprintf "View full conversation: %s" url);
      ] else []) in
      Lwt.return (json_result (`Assoc fields))
    end

let handle_go_back state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let confirm = args |> member "confirm" |> to_bool_option
    |> Option.value ~default:false in
  if confirm then begin
    match state.last_undo_match with
    | None ->
      Lwt.return (text_result "No pending undo match. Call go_back with a query first.")
    | Some result ->
      (* Use the commit SHA (experience ID) to restore *)
      let sha = result.experience.id in
      if sha = "" then
        Lwt.return (text_result "Cannot roll back: no commit SHA for this experience.")
      else begin
        let* () = Git_ops.restore_snapshot ~cwd:state.project_dir ~sha in
        (* Mark this and all later experiences as reverted *)
        let* collection_id = get_collection state in
        let* all_exps = Chromadb.list_all ~port:state.port ~collection_id ~limit:1000 in
        let later_exps = List.filter (fun (e : experience) ->
          e.timestamp >= result.experience.timestamp && not e.reverted
        ) all_exps in
        let* () = Lwt_list.iter_s (fun (e : experience) ->
          Chromadb.update_metadata ~port:state.port ~collection_id ~id:e.id
            ~updates:[("reverted", `Bool true)]
        ) later_exps in
        state.last_undo_match <- None;
        Lwt.return (text_result (Printf.sprintf
          "Rolled back to: %s (commit: %s). Marked %d experience(s) as reverted."
          result.experience.label sha (List.length later_exps)))
      end
  end else begin
    let* collection_id = get_collection state in
    let* results = Chromadb.search ~port:state.port ~collection_id ~query ~n:1 in
    match results with
    | [] ->
      Lwt.return (text_result "No matching state found for that description.")
    | best :: _ ->
      state.last_undo_match <- Some best;
      Lwt.return (json_result (`Assoc [
        "match_found", `Bool true;
        "label", `String best.experience.label;
        "intent", `String best.experience.intent;
        "distance", `Float best.distance;
        "commit_sha", `String best.experience.id;
        "branch", `String best.experience.branch;
        "files_changed", `List (List.map (fun f -> `String f) best.experience.files_changed);
        "reverted", `Bool best.experience.reverted;
        "instruction", `String "Call go_back again with confirm=true to apply this rollback.";
      ]))
  end

(* link_commit: retroactively tag an existing commit as an experience *)
let handle_link_commit state args =
  let open Yojson.Safe.Util in
  let sha_opt = args |> member "sha" |> to_string_option in
  let label_filter = args |> member "label" |> to_string_option in
  let* sha = match sha_opt with
    | Some s -> Lwt.return s
    | None -> Git_ops.current_head ~cwd:state.project_dir
  in
  let* collection_id = get_collection state in
  (* Check if this commit is already indexed *)
  let* existing = Chromadb.list_all ~port:state.port ~collection_id ~limit:1000 in
  let already_indexed = List.exists (fun (e : experience) -> e.id = sha) existing in
  if already_indexed then begin
    (* If label filter given, update existing experiences *)
    match label_filter with
    | Some _lbl ->
      let to_link = List.filter (fun (e : experience) ->
        match label_filter with
        | Some lbl -> e.label = lbl
        | None -> e.commit_sha = ""
      ) existing in
      let* () = Lwt_list.iter_s (fun (e : experience) ->
        Chromadb.update_metadata ~port:state.port ~collection_id ~id:e.id
          ~updates:[("commit_sha", `String sha)]
      ) to_link in
      Lwt.return (json_result (`Assoc [
        "linked_count", `Int (List.length to_link);
        "commit_sha", `String sha;
        "already_indexed", `Bool true;
      ]))
    | None ->
      Lwt.return (json_result (`Assoc [
        "commit_sha", `String sha;
        "already_indexed", `Bool true;
        "message", `String "This commit is already indexed as an experience.";
      ]))
  end else begin
    (* Index this commit as a new experience *)
    let* diff = Git_ops.commit_diff ~cwd:state.project_dir ~sha in
    let* changed = Git_ops.commit_changed_files ~cwd:state.project_dir ~sha in
    let* branch = Git_ops.current_branch ~cwd:state.project_dir in
    (* Get commit message *)
    let* message = Lwt.catch
      (fun () -> Git_ops.run_git ~cwd:state.project_dir ["log"; "-1"; "--format=%B"; sha])
      (fun _exn -> Lwt.return "") in
    let* parent = Lwt.catch
      (fun () ->
        let+ out = Git_ops.run_git ~cwd:state.project_dir
          ["rev-parse"; sha ^ "^"] in
        String.trim out)
      (fun _exn -> Lwt.return "") in
    let* timestamp = Lwt.catch
      (fun () ->
        let+ out = Git_ops.run_git ~cwd:state.project_dir
          ["log"; "-1"; "--format=%at"; sha] in
        float_of_string (String.trim out))
      (fun _exn -> Lwt.return (Unix.gettimeofday ())) in
    let label = match label_filter with
      | Some l -> l
      | None ->
        let first_line = match String.split_on_char '\n' message with
          | l :: _ -> l | [] -> "" in
        if String.length first_line > 80
        then String.sub first_line 0 80 ^ "..."
        else first_line
    in
    let exp : experience = {
      id = sha;
      label;
      intent = message;
      session_id = "";
      interaction_uuids = [];
      head_sha = sha;
      commit_sha = sha;
      commit_message = message;
      parent_sha = parent;
      diff;
      branch;
      files_changed = changed;
      timestamp;
      reverted = false;
    } in
    let conversation = Printf.sprintf "Commit %s: %s\n\nChanged files: %s\n\nDiff:\n%s"
      sha message (String.concat ", " changed) diff in
    let* () = Chromadb.save_experience ~port:state.port ~collection_id
      exp ~full_conversation:conversation in
    Lwt.return (json_result (`Assoc [
      "commit_sha", `String sha;
      "label", `String label;
      "files_changed", `List (List.map (fun f -> `String f) changed);
      "indexed", `Bool true;
    ]))
  end

let handle_list_experiences state args =
  let open Yojson.Safe.Util in
  let limit = args |> member "limit" |> to_int_option
    |> Option.value ~default:10 in
  let* collection_id = get_collection state in
  let+ exps = Chromadb.list_all ~port:state.port ~collection_id ~limit in
  json_result (`Assoc [
    "experiences", `List (List.map experience_to_yojson exps);
  ])

let handle_delete_experience state args =
  let open Yojson.Safe.Util in
  let id = args |> member "id" |> to_string in
  let* collection_id = get_collection state in
  let* () = Chromadb.delete ~port:state.port ~collection_id ~id in
  (* Also delete interaction-level documents *)
  let* int_collection_id = get_interactions_collection state in
  let+ () = Chromadb.delete_interactions ~port:state.port
    ~collection_id:int_collection_id ~experience_id:id in
  text_result (Printf.sprintf "Deleted experience %s" id)

let sha_matches ~sha (exp : experience) =
  let prefix_match s =
    String.length s >= String.length sha &&
    String.sub s 0 (String.length sha) = sha
  in
  prefix_match exp.commit_sha || prefix_match exp.head_sha
  || prefix_match exp.id

let handle_find_by_sha state args =
  let open Yojson.Safe.Util in
  let sha = args |> member "sha" |> to_string in
  let* collection_id = get_collection state in
  let+ all_exps = Chromadb.list_all ~port:state.port ~collection_id ~limit:1000 in
  let matches = List.filter (sha_matches ~sha) all_exps in
  json_result (`Assoc [
    "sha", `String sha;
    "matches", `Int (List.length matches);
    "experiences", `List (List.map (fun (e : experience) ->
      `Assoc [
        "id", `String e.id;
        "label", `String e.label;
        "intent", `String e.intent;
        "commit_sha", `String e.commit_sha;
        "head_sha", `String e.head_sha;
        "parent_sha", `String e.parent_sha;
        "branch", `String e.branch;
        "timestamp", `Float e.timestamp;
        "files_changed", `List (List.map (fun f -> `String f) e.files_changed);
        "reverted", `Bool e.reverted;
      ]
    ) matches);
  ])

(* search_by_file: find experiences that changed a given file.
   Uses Irmin tree walk + ChromaDB metadata. *)
let handle_search_by_file state args =
  let open Yojson.Safe.Util in
  let filepath = args |> member "filepath" |> to_string in
  (* First: search Irmin for commits that touched this file *)
  let* irmin_shas = Lwt.catch (fun () ->
    let* repo = Irmin_store.open_repo ~project_dir:state.project_dir in
    Irmin_store.find_commits_changing_file ~repo ~filepath
  ) (fun _exn -> Lwt.return []) in
  (* Second: search ChromaDB metadata for files_changed *)
  let* collection_id = get_collection state in
  let* all_exps = Chromadb.list_all ~port:state.port ~collection_id ~limit:1000 in
  let by_metadata = List.filter (fun (e : experience) ->
    List.exists (fun f ->
      f = filepath ||
      (String.length f >= String.length filepath &&
       String.sub f (String.length f - String.length filepath) (String.length filepath) = filepath)
    ) e.files_changed
  ) all_exps in
  (* Combine: experiences from ChromaDB + any Irmin-found SHAs not yet in results *)
  let metadata_ids = List.map (fun (e : experience) -> e.id) by_metadata in
  let irmin_only = List.filter (fun sha -> not (List.mem sha metadata_ids)) irmin_shas in
  let irmin_exps = List.filter_map (fun sha ->
    List.find_opt (fun (e : experience) -> e.id = sha) all_exps
  ) irmin_only in
  let all_matches = by_metadata @ irmin_exps in
  let sorted = List.sort (fun (a : experience) (b : experience) ->
    Float.compare b.timestamp a.timestamp
  ) all_matches in
  Lwt.return (json_result (`Assoc [
    "filepath", `String filepath;
    "matches", `Int (List.length sorted);
    "irmin_commits", `Int (List.length irmin_shas);
    "experiences", `List (List.map (fun (e : experience) ->
      `Assoc [
        "id", `String e.id;
        "label", `String e.label;
        "commit_sha", `String e.commit_sha;
        "branch", `String e.branch;
        "timestamp", `Float e.timestamp;
        "files_changed", `List (List.map (fun f -> `String f) e.files_changed);
      ]
    ) sorted);
  ]))

(* search_by_sha: find experiences near a commit in the DAG *)
let handle_search_by_sha state args =
  let open Yojson.Safe.Util in
  let sha = args |> member "sha" |> to_string in
  let _radius = args |> member "radius" |> to_int_option
    |> Option.value ~default:5 in
  let* collection_id = get_collection state in
  let* all_exps = Chromadb.list_all ~port:state.port ~collection_id ~limit:1000 in
  (* Direct match *)
  let direct = List.filter (sha_matches ~sha) all_exps in
  (* Also find by parent_sha *)
  let by_parent = List.filter (fun (e : experience) ->
    let prefix_match s =
      String.length s >= String.length sha &&
      String.sub s 0 (String.length sha) = sha
    in
    prefix_match e.parent_sha
  ) all_exps in
  let all_matches = direct @ by_parent |> List.sort_uniq (fun (a : experience) (b : experience) ->
    String.compare a.id b.id
  ) in
  Lwt.return (json_result (`Assoc [
    "sha", `String sha;
    "matches", `Int (List.length all_matches);
    "experiences", `List (List.map (fun (e : experience) ->
      `Assoc [
        "id", `String e.id;
        "label", `String e.label;
        "commit_sha", `String e.commit_sha;
        "parent_sha", `String e.parent_sha;
        "branch", `String e.branch;
        "timestamp", `Float e.timestamp;
        "files_changed", `List (List.map (fun f -> `String f) e.files_changed);
      ]
    ) all_matches);
  ]))

(* blame: per-line commit attribution with experience correlation *)
let handle_blame state args =
  let open Yojson.Safe.Util in
  let filepath = args |> member "filepath" |> to_string in
  let line_start = args |> member "line_start" |> to_int_option in
  let line_end = args |> member "line_end" |> to_int_option in
  let pattern = args |> member "pattern" |> to_string_option in
  let line_range = match line_start, line_end with
    | Some s, Some e -> Some (s, e)
    | Some s, None -> Some (s, s)
    | None, Some e -> Some (1, e)
    | None, None -> None
  in
  let* blame_lines = Git_ops.blame ~cwd:state.project_dir ?line_range
    ~filepath () in
  (* Filter by pattern if given *)
  let filtered = match pattern with
    | None -> blame_lines
    | Some pat ->
      let pat_lower = String.lowercase_ascii pat in
      List.filter (fun (_sha, _ln, content) ->
        let content_lower = String.lowercase_ascii content in
        let rec find_substring haystack needle pos =
          if pos + String.length needle > String.length haystack then false
          else if String.sub haystack pos (String.length needle) = needle then true
          else find_substring haystack needle (pos + 1)
        in
        find_substring content_lower pat_lower 0
      ) blame_lines
  in
  (* Deduplicate SHAs *)
  let unique_shas = List.fold_left (fun acc (sha, _, _) ->
    if List.mem sha acc then acc else sha :: acc
  ) [] filtered |> List.rev in
  (* Batch lookup: find experiences + summaries matching each SHA *)
  let* collection_id = get_collection state in
  let* all_with_summaries = Chromadb.list_all_with_summaries ~port:state.port
    ~collection_id ~limit:1000 in
  let sha_to_exp = List.filter_map (fun sha ->
    match List.find_opt (fun ((e : experience), _summary) ->
      sha_matches ~sha e
    ) all_with_summaries with
    | Some (e, summary) -> Some (sha, (e, summary))
    | None -> None
  ) unique_shas in
  (* Build per-line results *)
  let lines_json = List.map (fun (sha, ln, content) ->
    let short_sha = String.sub sha 0 (min 8 (String.length sha)) in
    let exp_fields = match List.assoc_opt sha sha_to_exp with
      | Some ((e : experience), summary) ->
        ["experience_id", `String e.id;
         "experience_label", `String e.label;
         "experience_intent", `String e.intent;
         "experience_summary", `String summary]
      | None -> []
    in
    `Assoc ([
      "line", `Int ln;
      "content", `String content;
      "sha", `String short_sha;
      "full_sha", `String sha;
    ] @ exp_fields)
  ) filtered in
  (* Build URL to blame web UI *)
  let blame_url =
    if state.web_port > 0 then
      let base = Printf.sprintf "http://localhost:%d/blame?file=%s"
        state.web_port (Uri.pct_encode filepath) in
      let with_pattern = match pattern with
        | Some p -> base ^ "&pattern=" ^ Uri.pct_encode p
        | None -> base
      in
      match line_start, line_end with
      | Some s, Some e -> with_pattern ^ Printf.sprintf "&line_start=%d&line_end=%d" s e
      | Some s, None -> with_pattern ^ Printf.sprintf "&line_start=%d" s
      | None, Some e -> with_pattern ^ Printf.sprintf "&line_end=%d" e
      | None, None -> with_pattern
    else ""
  in
  let exp_count = List.length sha_to_exp in
  if blame_url <> "" then
    Lwt.return (text_result (Printf.sprintf
      "%d lines, %d commits, %d experiences.\n\n%s"
      (List.length filtered)
      (List.length unique_shas)
      exp_count
      blame_url))
  else
    Lwt.return (json_result (`Assoc [
      "filepath", `String filepath;
      "total_lines", `Int (List.length filtered);
      "unique_commits", `Int (List.length unique_shas);
      "experiences_found", `Int exp_count;
      "lines", `List lines_json;
    ]))

(* explain_change: blame + conversation replay in one shot *)
let handle_explain_change state args =
  let open Yojson.Safe.Util in
  let filepath = args |> member "filepath" |> to_string in
  let pattern = args |> member "pattern" |> to_string_option in
  let line_start = args |> member "line_start" |> to_int_option in
  let line_end = args |> member "line_end" |> to_int_option in
  let line_range = match line_start, line_end with
    | Some s, Some e -> Some (s, e)
    | Some s, None -> Some (s, s)
    | None, Some e -> Some (1, e)
    | None, None -> None
  in
  (* Step 1: run blame *)
  let* blame_lines = Git_ops.blame ~cwd:state.project_dir ?line_range
    ~filepath () in
  (* Filter by pattern if given *)
  let filtered = match pattern with
    | None -> blame_lines
    | Some pat ->
      let pat_lower = String.lowercase_ascii pat in
      List.filter (fun (_sha, _ln, content) ->
        let content_lower = String.lowercase_ascii content in
        let rec find pos =
          if pos + String.length pat_lower > String.length content_lower then false
          else if String.sub content_lower pos (String.length pat_lower) = pat_lower then true
          else find (pos + 1)
        in
        find 0
      ) blame_lines
  in
  if filtered = [] then
    Lwt.return (text_result (Printf.sprintf
      "No lines matching '%s' found in %s"
      (Option.value pattern ~default:"(all)") filepath))
  else begin
    (* Step 2: deduplicate SHAs and find experiences *)
    let unique_shas = List.fold_left (fun acc (sha, _, _) ->
      if List.mem sha acc then acc else sha :: acc
    ) [] filtered |> List.rev in
    let* collection_id = get_collection state in
    let* all_with_summaries = Chromadb.list_all_with_summaries ~port:state.port
      ~collection_id ~limit:1000 in
    let sha_to_exp = List.filter_map (fun sha ->
      match List.find_opt (fun ((e : experience), _) ->
        sha_matches ~sha e
      ) all_with_summaries with
      | Some (e, summary) -> Some (sha, (e, summary))
      | None -> None
    ) unique_shas in
    (* Step 3: build URL to the blame web UI *)
    let blame_url =
      if state.web_port > 0 then
        let base = Printf.sprintf "http://localhost:%d/blame?file=%s"
          state.web_port (Uri.pct_encode filepath) in
        let with_pattern = match pattern with
          | Some p -> base ^ "&pattern=" ^ Uri.pct_encode p
          | None -> base
        in
        let with_lines = match line_start, line_end with
          | Some s, Some e -> with_pattern ^ Printf.sprintf "&line_start=%d&line_end=%d" s e
          | Some s, None -> with_pattern ^ Printf.sprintf "&line_start=%d" s
          | None, Some e -> with_pattern ^ Printf.sprintf "&line_end=%d" e
          | None, None -> with_pattern
        in
        with_lines
      else ""
    in
    (* Step 4: brief summaries per experience *)
    let seen_exp_ids = Hashtbl.create 8 in
    let summaries = List.filter_map (fun (_sha, (exp, summary)) ->
      if Hashtbl.mem seen_exp_ids exp.id then None
      else begin
        Hashtbl.add seen_exp_ids exp.id true;
        Some (Printf.sprintf "- %s (commit %s): %s"
          exp.label
          (String.sub exp.id 0 (min 8 (String.length exp.id)))
          (if summary <> "" then summary
           else exp.intent))
      end
    ) sha_to_exp in
    (* Build a concise text response with a clickable URL at the top *)
    let response = Buffer.create 256 in
    if blame_url <> "" then begin
      Buffer.add_string response blame_url;
      Buffer.add_char response '\n';
      Buffer.add_char response '\n'
    end;
    Buffer.add_string response (Printf.sprintf
      "Found %d lines matching '%s' in %s, from %d commit(s) with %d experience(s).\n\n"
      (List.length filtered)
      (Option.value pattern ~default:"(all)")
      filepath
      (List.length unique_shas)
      (List.length summaries));
    List.iter (fun s ->
      Buffer.add_string response s;
      Buffer.add_char response '\n'
    ) summaries;
    Lwt.return (text_result (Buffer.contents response))
  end

let dispatch_tool state name args =
  match name with
  | "save_experience" -> handle_save_experience state args
  | "search_experience" -> handle_search_experience state args
  | "replay_experience" -> handle_replay_experience state args
  | "go_back" -> handle_go_back state args
  | "link_commit" -> handle_link_commit state args
  | "list_experiences" -> handle_list_experiences state args
  | "delete_experience" -> handle_delete_experience state args
  | "find_by_sha" -> handle_find_by_sha state args
  | "search_by_file" -> handle_search_by_file state args
  | "search_by_sha" -> handle_search_by_sha state args
  | "blame" -> handle_blame state args
  | "explain_change" -> handle_explain_change state args
  | _ -> Lwt.return (text_result (Printf.sprintf "Unknown tool: %s" name))

(* JSON-RPC message handling *)
let handle_message state (msg : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let id = msg |> member "id" in
  let method_ = msg |> member "method" |> to_string_option
    |> Option.value ~default:"" in
  match method_ with
  | "initialize" ->
    let result = `Assoc [
      "protocolVersion", `String "2024-11-05";
      "capabilities", `Assoc [
        "tools", `Assoc [];
      ];
      "serverInfo", `Assoc [
        "name", `String "experience-agent";
        "version", `String "3.0.0";
      ];
    ] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "notifications/initialized" ->
    Lwt.return `Null
  | "tools/list" ->
    let result = `Assoc ["tools", tool_definitions] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "tools/call" ->
    let params = msg |> member "params" in
    let name = params |> member "name" |> to_string in
    let args = params |> member "arguments" in
    let* result =
      Lwt.catch
        (fun () -> dispatch_tool state name args)
        (fun exn ->
          let msg = Printexc.to_string exn in
          Lwt.return (text_result (Printf.sprintf "Error: %s" msg)))
    in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "" ->
    Lwt.return `Null
  | m when String.length m > 0 && m.[0] = '$' ->
    Lwt.return `Null
  | m when String.length m >= 14 && String.sub m 0 14 = "notifications/" ->
    Lwt.return `Null
  | _ ->
    let error = `Assoc [
      "code", `Int (-32601);
      "message", `String (Printf.sprintf "Method not found: %s" method_);
    ] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "error", error;
    ])

let create_state ~port ?(web_port=0) ~project_dir () =
  let jsonl_dir = Jsonl_reader.find_jsonl_dir ~project_dir in
  {
    port;
    web_port;
    project_dir;
    jsonl_dir;
    collection_id = None;
    interactions_collection_id = None;
    last_undo_match = None;
  }

(* Main stdio loop *)
let run_with_state state =
  let stdin = Lwt_io.stdin in
  let stdout = Lwt_io.stdout in
  let rec loop () =
    let* line = Lwt_io.read_line_opt stdin in
    match line with
    | None -> Lwt.return_unit
    | Some line ->
      let line = String.trim line in
      if String.length line = 0 then loop ()
      else begin
        let* () =
          Lwt.catch
            (fun () ->
              let* () = Lwt_io.eprintf "experience-agent: received: %s\n"
                (String.sub line 0 (min 200 (String.length line))) in
              let msg = Yojson.Safe.from_string line in
              let* response = handle_message state msg in
              match response with
              | `Null -> Lwt.return_unit
              | resp ->
                let resp_str = Yojson.Safe.to_string resp in
                let* () = Lwt_io.write_line stdout resp_str in
                Lwt_io.flush stdout)
            (fun exn ->
              let err = Printexc.to_string exn in
              let bt = Printexc.get_backtrace () in
              Lwt_io.eprintf "experience-agent error: %s\n%s\nInput was: %s\n" err bt
                (String.sub line 0 (min 200 (String.length line))))
        in
        loop ()
      end
  in
  loop ()

(* One-shot: wipe both collections *)
let wipe_db state =
  let project = project_name state in
  let exp_name = project ^ "_experiences" in
  let int_name = project ^ "_interactions" in
  let* () = Lwt_io.eprintf "Deleting collection: %s\n" exp_name in
  let* () =
    Lwt.catch
      (fun () -> Chromadb.delete_collection ~port:state.port ~name:exp_name)
      (fun _exn -> Lwt_io.eprintf "  (collection did not exist)\n") in
  let* () = Lwt_io.eprintf "Deleting collection: %s\n" int_name in
  let* () =
    Lwt.catch
      (fun () -> Chromadb.delete_collection ~port:state.port ~name:int_name)
      (fun _exn -> Lwt_io.eprintf "  (collection did not exist)\n") in
  state.collection_id <- None;
  state.interactions_collection_id <- None;
  Lwt_io.eprintf "DB wiped for project: %s\n" project

(* Parse ISO 8601 timestamp string to Unix float, fallback to 0.0 *)
let parse_timestamp_float s =
  try
    Scanf.sscanf s "%4d-%2d-%2dT%2d:%2d:%2d"
      (fun y mo d h mi se ->
        let tm = { Unix.tm_sec = se; tm_min = mi; tm_hour = h;
                   tm_mday = d; tm_mon = mo - 1; tm_year = y - 1900;
                   tm_wday = 0; tm_yday = 0; tm_isdst = false } in
        let (t, _) = Unix.mktime tm in
        t)
  with _ -> 0.0

(* One-shot: import a session's interactions as individual experiences *)
let import_session state ~session_id =
  let filepath = Filename.concat state.jsonl_dir (session_id ^ ".jsonl") in
  if not (Sys.file_exists filepath) then begin
    let* () = Lwt_io.eprintf "JSONL file not found: %s\n" filepath in
    Lwt_io.eprintf "Available in: %s\n" state.jsonl_dir
  end else begin
    let interactions = Jsonl_reader.parse_interactions ~filepath in
    let total = List.length interactions in
    let* () = Lwt_io.eprintf "Found %d interactions in session %s\n" total session_id in
    let* collection_id = get_collection state in
    let* int_collection_id = get_interactions_collection state in
    let imported = ref 0 in
    let failed = ref 0 in
    let* () = Lwt_list.iteri_s (fun idx (interaction : interaction) ->
      Lwt.catch (fun () ->
        let id = Uuidm.v4_gen (Random.State.make_self_init ()) ()
          |> Uuidm.to_string in
        let label =
          let text = interaction.user_text in
          if String.length text <= 80 then text
          else String.sub text 0 80 ^ "..." in
        let exp : experience = {
          id;
          label;
          intent = interaction.user_text;
          session_id;
          interaction_uuids = [interaction.user_uuid];
          head_sha = "";
          commit_sha = "";
          commit_message = "";
          parent_sha = "";
          diff = "";
          branch = interaction.branch;
          files_changed = interaction.files_changed;
          timestamp = parse_timestamp_float interaction.timestamp;
          reverted = false;
        } in
        let full_conversation = Jsonl_reader.summarizable_text ~filepath interaction in
        let* () = Chromadb.save_experience ~port:state.port ~collection_id
          exp ~full_conversation in
        let* () = Chromadb.save_interaction ~port:state.port
          ~collection_id:int_collection_id
          ~experience_id:id
          ~interaction_index:interaction.index
          ~user_text:interaction.user_text
          ~assistant_summary:interaction.assistant_summary
          ~user_uuid:interaction.user_uuid
          ~timestamp:interaction.timestamp in
        incr imported;
        Lwt_io.eprintf "Imported interaction %d/%d: %s\n"
          (idx + 1) total (Jsonl_reader.truncate 80 interaction.user_text)
      ) (fun exn ->
        incr failed;
        Lwt_io.eprintf "Failed interaction %d/%d: %s\n  Error: %s\n"
          (idx + 1) total
          (Jsonl_reader.truncate 80 interaction.user_text)
          (Printexc.to_string exn)
      )
    ) interactions in
    Lwt_io.eprintf "Import complete: %d imported, %d failed out of %d\n"
      !imported !failed total
  end

(* --init: bootstrap experience DB from git history + JSONL logs via Irmin *)
let init_from_history state ~starting_from =
  Printf.eprintf "Initializing experience database (Irmin)...\n%!";
  (* Wipe existing DB *)
  let* () = wipe_db state in
  let* collection_id = get_collection state in
  let* int_collection_id = get_interactions_collection state in
  (* Open repo via Irmin *)
  let* repo = Irmin_store.open_repo ~project_dir:state.project_dir in
  Printf.eprintf "Opened git repo via Irmin\n%!";
  (* Walk all commits via Irmin *)
  let since = if starting_from <> "" then
    parse_timestamp_float (starting_from ^ "T00:00:00")
  else 0.0 in
  let* all_commits = Irmin_store.walk_all_commits ~repo ~since in
  Printf.eprintf "Found %d commits via Irmin%s\n%!"
    (List.length all_commits)
    (if starting_from <> "" then Printf.sprintf " since %s" starting_from else "");
  (* Load JSONL sessions for correlation *)
  let sessions = Jsonl_reader.list_sessions ~jsonl_dir:state.jsonl_dir in
  let session_interactions = List.map (fun path ->
    let session_id = Jsonl_reader.session_id_of_path path in
    let interactions = Jsonl_reader.parse_interactions ~filepath:path in
    (session_id, path, interactions)
  ) sessions in
  Printf.eprintf "Found %d JSONL sessions for correlation\n%!" (List.length sessions);
  (* For each commit, build experience via Irmin and correlate with JSONL *)
  let indexed = ref 0 in
  let failed = ref 0 in
  let total = List.length all_commits in
  let* () = Lwt_list.iter_s (fun (sha, _commit) ->
    Lwt.catch (fun () ->
      (* Build experience from commit via Irmin *)
      let* exp_opt = Irmin_store.experience_of_commit ~repo ~sha in
      match exp_opt with
      | None ->
        Printf.eprintf "  SKIP %s: could not parse\n%!" sha;
        Lwt.return_unit
      | Some exp ->
        (* Get branch via git CLI (Irmin doesn't expose HEAD symref easily) *)
        let* branch = Git_ops.current_branch ~cwd:state.project_dir in
        let exp = { exp with branch } in
        (* Correlate with JSONL: find interactions near this commit's timestamp *)
        let commit_time = exp.timestamp in
        let window = 300.0 in (* 5 minute window *)
        let matching_interactions = List.concat_map (fun (session_id, filepath, interactions) ->
          let matches = List.filter (fun (i : interaction) ->
            let i_time = parse_timestamp_float i.timestamp in
            i_time > 0.0 && Float.abs (i_time -. commit_time) < window
          ) interactions in
          List.map (fun i -> (session_id, filepath, i)) matches
        ) session_interactions in
        (* If we found correlated JSONL interactions, enrich the experience *)
        let exp, conversation = match matching_interactions with
          | [] ->
            (* No JSONL match — use commit data only *)
            let conversation = Printf.sprintf "Commit %s: %s\n\nChanged files: %s\n\nDiff:\n%s"
              sha exp.commit_message
              (String.concat ", " exp.files_changed)
              (if String.length exp.diff > 5000
               then String.sub exp.diff 0 5000 ^ "\n... (truncated)"
               else exp.diff) in
            (exp, conversation)
          | correlated ->
            let session_id, filepath, _ = List.hd correlated in
            let interactions = List.map (fun (_, _, i) -> i) correlated in
            let uuids = List.map (fun (i : interaction) -> i.user_uuid) interactions in
            let full_conv = String.concat "\n---\n"
              (List.map (Jsonl_reader.summarizable_text ~filepath) interactions) in
            (* Keep Irmin-derived files (relative paths) as primary source *)
            let exp = { exp with
              session_id;
              interaction_uuids = uuids;
            } in
            (exp, full_conv)
        in
        let* () = Chromadb.save_experience ~port:state.port ~collection_id
          exp ~full_conversation:conversation in
        (* Save per-interaction embeddings if we have JSONL matches *)
        let* () = match matching_interactions with
          | [] -> Lwt.return_unit
          | correlated ->
            Lwt_list.iter_s (fun (_sid, _fp, i) ->
              Chromadb.save_interaction ~port:state.port
                ~collection_id:int_collection_id
                ~experience_id:sha
                ~interaction_index:i.index
                ~user_text:i.user_text
                ~assistant_summary:i.assistant_summary
                ~user_uuid:i.user_uuid
                ~timestamp:i.timestamp
            ) (List.map (fun (_, _, i) -> ("", "", i)) correlated)
        in
        incr indexed;
        let corr_tag = if matching_interactions <> [] then
          Printf.sprintf " [+%d interactions]" (List.length matching_interactions)
        else "" in
        Printf.eprintf "  [%d/%d] %s: %s%s\n%!"
          !indexed total
          (String.sub sha 0 (min 8 (String.length sha)))
          exp.label corr_tag;
        Lwt.return_unit
    ) (fun exn ->
      incr failed;
      Printf.eprintf "  FAILED %s: %s\n%!" sha (Printexc.to_string exn);
      Lwt.return_unit
    )
  ) all_commits in
  Printf.eprintf "Init complete: %d indexed, %d failed out of %d commits\n%!"
    !indexed !failed total;
  Lwt.return_unit

(* --prune: remove experiences older than a cutoff date *)
let prune_before state ~cutoff_str =
  let cutoff = parse_timestamp_float cutoff_str in
  if cutoff = 0.0 then begin
    Lwt_io.eprintf "Invalid date format: %s (use ISO 8601: YYYY-MM-DD)\n" cutoff_str
  end else begin
    let* () = Lwt_io.eprintf "Pruning experiences before %s (%.0f)...\n" cutoff_str cutoff in
    let* collection_id = get_collection state in
    let* int_collection_id = get_interactions_collection state in
    let* all = Chromadb.list_all ~port:state.port ~collection_id ~limit:10000 in
    let to_prune = List.filter (fun (e : experience) ->
      e.timestamp > 0.0 && e.timestamp < cutoff
    ) all in
    let* () = Lwt_io.eprintf "Found %d experiences to prune\n" (List.length to_prune) in
    let* () = Lwt_list.iter_s (fun (e : experience) ->
      let* () = Chromadb.delete ~port:state.port ~collection_id ~id:e.id in
      Chromadb.delete_interactions ~port:state.port
        ~collection_id:int_collection_id ~experience_id:e.id
    ) to_prune in
    Lwt_io.eprintf "Pruned %d experiences older than %s\n"
      (List.length to_prune) cutoff_str
  end

let run ~port ~project_dir =
  let state = create_state ~port ~project_dir () in
  run_with_state state
