open Lwt.Syntax
open Types

(* Tool definitions for tools/list *)
let tool_definitions = `List [
  `Assoc [
    "name", `String "search_experience";
    "description", `String "Search past saved experiences for relevant prior work. Use at the start of a task to find similar past solutions. Returns matching experiences with their full conversation transcripts.";
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
        "broad_k", `Assoc [
          "type", `String "integer";
          "description", `String "Number of broad candidates to consider before re-ranking (default 10)";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "save_step";
    "description", `String "Save the current state as a checkpoint. Creates a git micro-commit and stores the experience (including the full conversation transcript) in the vector DB for future retrieval.";
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
        "conversation", `Assoc [
          "type", `String "string";
          "description", `String "Full conversation transcript since last save point. Include: user messages, your reasoning, all tool calls with inputs/outputs, errors, and the final outcome.";
        ];
      ];
      "required", `List [`String "label"; `String "intent"; `String "conversation"];
    ];
  ];
  `Assoc [
    "name", `String "go_back";
    "description", `String "Semantic undo. Describe the state you want to revert to in natural language. Searches both current session steps and past saved experiences. Returns the best match for confirmation before applying.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of the state to revert to, e.g. 'when you last changed the API' or 'before the auth changes'";
        ];
        "confirm", `Assoc [
          "type", `String "boolean";
          "description", `String "Set to true to confirm and apply the rollback after reviewing the match. First call without confirm to see the match.";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "show_steps";
    "description", `String "Show all steps in the current session with their labels and save status.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [];
    ];
  ];
  `Assoc [
    "name", `String "commit";
    "description", `String "Squash all micro-commits into a single clean commit on the original branch.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "message", `Assoc [
          "type", `String "string";
          "description", `String "Commit message for the squashed commit";
        ];
      ];
      "required", `List [`String "message"];
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
    "name", `String "start_experiences";
    "description", `String "Enter auto-recording mode. After calling this, every conversation turn (user question + Claude's answer/action) becomes a step. Call save_step after each turn. If the user approves (says 'yes'), keep it. If the user rejects, call reject_step. Call save_experience when done to finalize the group.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [];
    ];
  ];
  `Assoc [
    "name", `String "reject_step";
    "description", `String "Reject and roll back the last step. Undoes the git micro-commit and removes the step from tracking and ChromaDB.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [];
    ];
  ];
  `Assoc [
    "name", `String "save_experience";
    "description", `String "Finalize the current group of steps into a single experience group. Squashes git micro-commits into one commit, saves the group as a searchable entity, and resets for the next group. Auto-recording stays on.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "label", `Assoc [
          "type", `String "string";
          "description", `String "Short description of what the experience group accomplished";
        ];
        "intent", `Assoc [
          "type", `String "string";
          "description", `String "The overall user intent/goal for this group of steps";
        ];
      ];
      "required", `List [`String "label"; `String "intent"];
    ];
  ];
  `Assoc [
    "name", `String "replay_experience";
    "description", `String "Search for a past experience or experience group and return the full step-by-step conversation data for replay. Returns the matching experience with all steps and their conversations in order.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language description of the experience to replay";
        ];
      ];
      "required", `List [`String "query"];
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
  project_dir : string;
  mutable collections : collections option;
  mutable tracker : Step_tracker.t option;
  mutable last_undo_match : search_result option;
  mutable auto_recording : bool;
  mutable current_group_steps : string list;
}

let get_collections state =
  match state.collections with
  | Some c -> Lwt.return c
  | None ->
    let dir = if state.project_dir = "." || state.project_dir = "./"
      then Sys.getcwd ()
      else if Filename.is_relative state.project_dir
      then Filename.concat (Sys.getcwd ()) state.project_dir
      else state.project_dir in
    let project = Filename.basename dir in
    let* c = Chromadb.ensure_collections ~port:state.port ~project in
    state.collections <- Some c;
    Lwt.return c

let get_tracker state =
  match state.tracker with
  | Some t -> Lwt.return t
  | None ->
    let* sha = Git_ops.get_current_sha ~cwd:state.project_dir in
    let* collections = get_collections state in
    let t = Step_tracker.create ~base_sha:sha ~port:state.port ~collections in
    state.tracker <- Some t;
    Lwt.return t

(* Tool handlers *)
let handle_search_experience state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let n = args |> member "n_results" |> to_int_option |> Option.value ~default:3 in
  let broad_k = args |> member "broad_k" |> to_int_option |> Option.value ~default:10 in
  let* collections = get_collections state in
  let+ (group_results, step_results) = Chromadb.search_three_stage ~port:state.port
    ~collections ~query ~broad_k ~n in
  let groups_json = `List (List.map (fun (r : group_search_result) ->
    group_search_result_to_yojson r) group_results) in
  let steps_json = `List (List.map experience_to_yojson step_results) in
  json_result (`Assoc [
    "groups", groups_json;
    "experiences", steps_json;
  ])

let handle_save_step state args =
  let open Yojson.Safe.Util in
  let label = args |> member "label" |> to_string in
  let intent = args |> member "intent" |> to_string in
  let conversation = args |> member "conversation" |> to_string in
  let* tracker = get_tracker state in
  let* collections = get_collections state in
  (* Compute diff from last step or base *)
  let from_sha = match Step_tracker.all tracker |> List.rev with
    | last :: _ -> last.commit_sha
    | [] -> Step_tracker.base_sha tracker
  in
  (* Try to create micro-commit, but don't fail if no changes *)
  let step_num = List.length (Step_tracker.all tracker) + 1 in
  let* commit_sha =
    Lwt.catch
      (fun () -> Git_ops.micro_commit ~cwd:state.project_dir ~label ~step_num)
      (fun _exn -> Git_ops.get_current_sha ~cwd:state.project_dir)
  in
  let* diff =
    Lwt.catch
      (fun () -> Git_ops.diff_between ~cwd:state.project_dir
        ~from_sha ~to_sha:commit_sha)
      (fun _exn -> Lwt.return "")
  in
  let* files =
    Lwt.catch
      (fun () -> Git_ops.changed_files ~cwd:state.project_dir ~from_sha)
      (fun _exn -> Lwt.return [])
  in
  (* Add step to tracker (also indexes in session collection) *)
  let* step = Step_tracker.add tracker ~label ~intent ~conversation
    ~commit_sha ~diff ~files in
  (* Save to persistent experience collections *)
  let exp = step_to_experience step in
  let* () = Chromadb.save_experience ~port:state.port ~collections exp in
  (* Track step in current group if auto-recording *)
  if state.auto_recording then
    state.current_group_steps <- state.current_group_steps @ [step.experience_id];
  let result = `Assoc [
    "step_number", `Int step.number;
    "experience_id", `String step.experience_id;
    "commit_sha", `String commit_sha;
    "files_changed", `List (List.map (fun f -> `String f) files);
    "auto_recording", `Bool state.auto_recording;
  ] in
  Lwt.return (json_result result)

let handle_go_back state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let confirm = args |> member "confirm" |> to_bool_option
    |> Option.value ~default:false in
  let* tracker = get_tracker state in
  if confirm then begin
    (* Apply the previously found match *)
    match state.last_undo_match with
    | None ->
      Lwt.return (text_result "No pending undo match. Call go_back with a query first.")
    | Some result ->
      let sha = result.experience.commit_sha in
      if sha = "" then
        Lwt.return (text_result "Cannot roll back: no commit SHA associated with this experience.")
      else begin
      let* () = Git_ops.rollback_to ~cwd:state.project_dir ~sha in
      let* () = match result.source with
        | `Session ->
          (match List.find_opt (fun s ->
            s.experience_id = result.experience.id
          ) (Step_tracker.all tracker) with
          | Some step -> Step_tracker.truncate_after tracker step.number
          | None -> Lwt.return_unit)
        | `Experience | `Group -> Lwt.return_unit
      in
      state.last_undo_match <- None;
      let+ () = Lwt.return_unit in
      text_result (Printf.sprintf "Rolled back to: %s (commit: %s, source: %s)"
        result.experience.label sha
        (match result.source with `Session -> "session" | `Experience -> "experience" | `Group -> "group"))
      end
  end else begin
    (* Search for matching state *)
    (* First try direct step number *)
    let direct_step =
      try
        let n = int_of_string (String.trim query) in
        Step_tracker.get tracker n
      with _ -> None
    in
    match direct_step with
    | Some step ->
      let result = {
        experience = step_to_experience step;
        source = `Session;
        distance = 0.0;
      } in
      state.last_undo_match <- Some result;
      Lwt.return (json_result (`Assoc [
        "match_found", `Bool true;
        "label", `String step.label;
        "source", `String "session";
        "step_number", `Int step.number;
        "files", `List (List.map (fun f -> `String f) step.files);
        "instruction", `String "Call go_back again with confirm=true to apply this rollback.";
      ]))
    | None ->
      let* collections = get_collections state in
      let* results = Chromadb.search_for_undo ~port:state.port
        ~collections ~query ~n:1 in
      match results with
      | [] ->
        Lwt.return (text_result "No matching state found for that description.")
      | best :: _ ->
        state.last_undo_match <- Some best;
        Lwt.return (json_result (`Assoc [
          "match_found", `Bool true;
          "label", `String best.experience.label;
          "source", `String (match best.source with
            `Session -> "session" | `Experience -> "experience" | `Group -> "group");
          "distance", `Float best.distance;
          "files", `List (List.map (fun f -> `String f) best.experience.files);
          "instruction", `String "Call go_back again with confirm=true to apply this rollback.";
        ]))
  end

let handle_show_steps state _args =
  let* tracker = get_tracker state in
  Lwt.return (text_result (Step_tracker.format tracker))

let handle_commit state args =
  let open Yojson.Safe.Util in
  let message = args |> member "message" |> to_string in
  let* tracker = get_tracker state in
  let base = Step_tracker.base_sha tracker in
  let+ sha = Git_ops.squash_commit ~cwd:state.project_dir
    ~message ~base_sha:base in
  json_result (`Assoc [
    "sha", `String sha;
    "message", `String message;
  ])

let handle_list_experiences state args =
  let open Yojson.Safe.Util in
  let limit = args |> member "limit" |> to_int_option
    |> Option.value ~default:10 in
  let* collections = get_collections state in
  let* exps = Chromadb.list_all ~port:state.port ~collections ~limit in
  let+ groups = Chromadb.list_all_groups ~port:state.port ~collections ~limit in
  json_result (`Assoc [
    "experiences", `List (List.map experience_to_yojson exps);
    "groups", `List (List.map experience_group_to_yojson groups);
  ])

let handle_delete_experience state args =
  let open Yojson.Safe.Util in
  let id = args |> member "id" |> to_string in
  let* collections = get_collections state in
  let+ () = Chromadb.delete ~port:state.port ~collections ~id in
  text_result (Printf.sprintf "Deleted experience %s" id)

let handle_start_experiences state _args =
  state.auto_recording <- true;
  state.current_group_steps <- [];
  Lwt.return (text_result (String.concat "\n" [
    "Auto-recording mode enabled.";
    "";
    "Instructions:";
    "- After every conversation turn (user question + your answer/action), call `save_step` with the full turn transcript.";
    "- If the user's next message is 'yes' or starts with 'yes', keep the step.";
    "- If the user rejects (says 'no', expresses disapproval), call `reject_step` to undo it.";
    "- `go_back` still works as usual.";
    "- When done, call `save_experience` with a label and intent to finalize the group.";
  ]))

let handle_reject_step state _args =
  let* tracker = get_tracker state in
  match Step_tracker.last_step tracker with
  | None ->
    Lwt.return (text_result "No steps to reject.")
  | Some last_step ->
    let prev_sha = match Step_tracker.get tracker (last_step.number - 1) with
      | Some prev -> prev.commit_sha
      | None -> Step_tracker.base_sha tracker
    in
    (* Roll back git *)
    let* () = Git_ops.rollback_to ~cwd:state.project_dir ~sha:prev_sha in
    (* Remove from tracker *)
    Step_tracker.remove_last tracker;
    (* Remove from ChromaDB *)
    let* collections = get_collections state in
    let* () = Chromadb.delete ~port:state.port ~collections
      ~id:last_step.experience_id in
    (* Remove from current group steps *)
    state.current_group_steps <- List.filter
      (fun id -> id <> last_step.experience_id) state.current_group_steps;
    Lwt.return (text_result (Printf.sprintf
      "Rejected step %d (%s). Rolled back to %s."
      last_step.number last_step.label prev_sha))

let handle_save_experience state args =
  let open Yojson.Safe.Util in
  let label = args |> member "label" |> to_string in
  let intent = args |> member "intent" |> to_string in
  let* tracker = get_tracker state in
  let* collections = get_collections state in
  let base = Step_tracker.base_sha tracker in
  (* Compute full diff from base to current HEAD *)
  let* current_sha = Git_ops.get_current_sha ~cwd:state.project_dir in
  let* diff =
    Lwt.catch
      (fun () -> Git_ops.diff_between ~cwd:state.project_dir
        ~from_sha:base ~to_sha:current_sha)
      (fun _exn -> Lwt.return "")
  in
  let* files =
    Lwt.catch
      (fun () -> Git_ops.changed_files ~cwd:state.project_dir ~from_sha:base)
      (fun _exn -> Lwt.return [])
  in
  (* Squash micro-commits into one *)
  let* squashed_sha = Git_ops.squash_commit ~cwd:state.project_dir
    ~message:(Printf.sprintf "%s: %s" label intent) ~base_sha:base in
  (* Create experience group *)
  let group_id = Uuidm.v4_gen (Random.State.make_self_init ()) ()
    |> Uuidm.to_string in
  let group : experience_group = {
    group_id;
    label;
    intent;
    commit_sha = squashed_sha;
    step_ids = state.current_group_steps;
    diff;
    files;
    timestamp = Unix.gettimeofday ();
  } in
  (* Save group to ChromaDB *)
  let* () = Chromadb.save_experience_group ~port:state.port
    ~collections group in
  (* Reset tracker for next group *)
  Step_tracker.reset tracker ~new_base_sha:squashed_sha;
  let* () = Chromadb.clear_session ~port:state.port ~collections in
  (* Reset group steps *)
  state.current_group_steps <- [];
  (* Auto-recording stays on *)
  Lwt.return (json_result (`Assoc [
    "group_id", `String group_id;
    "commit_sha", `String squashed_sha;
    "steps_count", `Int (List.length group.step_ids);
    "files", `List (List.map (fun f -> `String f) files);
    "auto_recording", `Bool state.auto_recording;
  ]))

let handle_replay_experience state args =
  let open Yojson.Safe.Util in
  let query = args |> member "query" |> to_string in
  let* collections = get_collections state in
  (* Search groups first *)
  let* group_resp = Chromadb.query_collection ~port:state.port
    ~collection_id:collections.groups_id ~query_text:query ~n:3 in
  let group_hits = Chromadb.parse_query_results group_resp in
  match group_hits with
  | (id, dist, meta) :: _ ->
    (* Found a group match — return it with step details *)
    let group = (Chromadb.group_from_metadata ~id ~distance:dist meta).group in
    (* Fetch individual step experiences for the group *)
    let* step_experiences = Lwt_list.filter_map_s (fun step_id ->
      (* Fetch step by direct ID *)
      let body = `Assoc [
        "ids", `List [`String step_id];
        "include", `List [`String "documents"; `String "metadatas"];
      ] in
      let+ resp = Chromadb.http_post ~port:state.port
        ~path:(Printf.sprintf "/collections/%s/get" collections.intents_id)
        ~body in
      let ids = resp |> member "ids" |> Chromadb.safe_to_list |> List.map to_string in
      let metadatas = resp |> member "metadatas" |> Chromadb.safe_to_list in
      match ids, metadatas with
      | [found_id], [meta] ->
        Some (Chromadb.experience_from_metadata ~id:found_id ~distance:0.0 meta).experience
      | _ -> None
    ) group.step_ids in
    let replay = `Assoc [
      "type", `String "group";
      "group", experience_group_to_yojson group;
      "steps", `List (List.map experience_to_yojson step_experiences);
    ] in
    Lwt.return (json_result replay)
  | [] ->
    (* No group found, try individual experiences *)
    let+ (_, step_results) = Chromadb.search_three_stage ~port:state.port
      ~collections ~query ~broad_k:5 ~n:1 in
    match step_results with
    | exp :: _ ->
      json_result (`Assoc [
        "type", `String "experience";
        "experience", experience_to_yojson exp;
      ])
    | [] ->
      text_result "No matching experience found for replay."

let dispatch_tool state name args =
  match name with
  | "search_experience" -> handle_search_experience state args
  | "save_step" -> handle_save_step state args
  | "go_back" -> handle_go_back state args
  | "show_steps" -> handle_show_steps state args
  | "commit" -> handle_commit state args
  | "list_experiences" -> handle_list_experiences state args
  | "delete_experience" -> handle_delete_experience state args
  | "start_experiences" -> handle_start_experiences state args
  | "reject_step" -> handle_reject_step state args
  | "save_experience" -> handle_save_experience state args
  | "replay_experience" -> handle_replay_experience state args
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
        "version", `String "0.1.0";
      ];
    ] in
    Lwt.return (`Assoc [
      "jsonrpc", `String "2.0";
      "id", id;
      "result", result;
    ])
  | "notifications/initialized" ->
    (* Notification, no response needed *)
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
    (* No method field — probably a response or ping, ignore *)
    Lwt.return `Null
  | m when String.length m > 0 && m.[0] = '$' ->
    (* Internal notifications like $/progress, ignore *)
    Lwt.return `Null
  | m when String.sub m 0 (min 14 (String.length m)) = "notifications/" ->
    (* Any notification, ignore *)
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

(* Main stdio loop *)
let run ~port ~project_dir =
  let state = {
    port;
    project_dir;
    collections = None;
    tracker = None;
    last_undo_match = None;
    auto_recording = false;
    current_group_steps = [];
  } in
  let stdin = Lwt_io.stdin in
  let stdout = Lwt_io.stdout in
  let rec loop () =
    let* line = Lwt_io.read_line_opt stdin in
    match line with
    | None -> Lwt.return_unit  (* EOF, shutdown *)
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
              (* Log error to stderr but don't crash *)
              let err = Printexc.to_string exn in
              let bt = Printexc.get_backtrace () in
              Lwt_io.eprintf "experience-agent error: %s\n%s\nInput was: %s\n" err bt
                (String.sub line 0 (min 200 (String.length line))))
        in
        loop ()
      end
  in
  loop ()
