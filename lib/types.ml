(* An interaction: user message + all responses until next user message *)
(* Main JSONL is linear — simple line-range segmentation *)
type interaction = {
  index : int;
  line_start : int;
  line_end : int;
  user_uuid : string;
  timestamp : string;
  user_text : string;
  assistant_summary : string;
  files_changed : string list;
  branch : string;
}

(* An experience is a commit.
   The commit SHA is the canonical experience ID.
   Experience metadata is encoded in the commit message. *)
type experience = {
  id : string;                    (* = commit SHA *)
  label : string;
  intent : string;
  session_id : string;
  interaction_uuids : string list;
  head_sha : string;             (* HEAD at save time *)
  commit_sha : string;           (* linked commit SHA (may differ from id for retroactive links) *)
  commit_message : string;       (* full commit message *)
  parent_sha : string;           (* parent commit SHA *)
  diff : string;                 (* unified diff *)
  branch : string;
  files_changed : string list;
  timestamp : float;
  reverted : bool;
}

type search_result = {
  experience : experience;
  distance : float;
}

(* Serialization *)

let interaction_to_yojson (i : interaction) : Yojson.Safe.t =
  `Assoc [
    "index", `Int i.index;
    "line_start", `Int i.line_start;
    "line_end", `Int i.line_end;
    "user_uuid", `String i.user_uuid;
    "timestamp", `String i.timestamp;
    "user_text", `String i.user_text;
    "assistant_summary", `String i.assistant_summary;
    "files_changed", `List (List.map (fun f -> `String f) i.files_changed);
    "branch", `String i.branch;
  ]

let experience_to_yojson (e : experience) : Yojson.Safe.t =
  `Assoc [
    "id", `String e.id;
    "label", `String e.label;
    "intent", `String e.intent;
    "session_id", `String e.session_id;
    "interaction_uuids", `List (List.map (fun u -> `String u) e.interaction_uuids);
    "head_sha", `String e.head_sha;
    "commit_sha", `String e.commit_sha;
    "commit_message", `String e.commit_message;
    "parent_sha", `String e.parent_sha;
    "diff", `String e.diff;
    "branch", `String e.branch;
    "files_changed", `List (List.map (fun f -> `String f) e.files_changed);
    "timestamp", `Float e.timestamp;
    "reverted", `Bool e.reverted;
  ]

let experience_of_metadata (id : string) (meta : Yojson.Safe.t) : experience =
  let open Yojson.Safe.Util in
  let str key = meta |> member key |> to_string_option |> Option.value ~default:"" in
  let split_csv s =
    String.split_on_char ',' s |> List.filter (fun s -> String.length s > 0) in
  {
    id;
    label = str "label";
    intent = str "intent";
    session_id = str "session_id";
    interaction_uuids = split_csv (str "interaction_uuids");
    head_sha = str "head_sha";
    commit_sha = str "commit_sha";
    commit_message = str "commit_message";
    parent_sha = str "parent_sha";
    diff = str "diff";
    branch = str "branch";
    files_changed = split_csv (str "files_changed");
    timestamp = (try meta |> member "timestamp" |> to_float with _ -> 0.0);
    reverted = (try meta |> member "reverted" |> to_bool with _ -> false);
  }

let search_result_to_yojson (r : search_result) : Yojson.Safe.t =
  `Assoc [
    "experience", experience_to_yojson r.experience;
    "distance", `Float r.distance;
  ]
