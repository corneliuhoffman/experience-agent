(* Branch-aware index orchestrator.

   Loads the saved git state, diffs against current git, and walks each
   changed branch using Git_walk + Walk_handlers.  Updates saved state
   after each run. *)

open Lwt.Syntax
open Git_link_types

module SMap = Map.Make(String)

(* ---------- Commit info builder ---------- *)

(* Build a Git_walk.commit_info from a commit SHA using Irmin for diffs. *)
let commit_info_of ~repo ~sha ~timestamp : Git_walk.commit_info Lwt.t =
  let* parents =
    Lwt.catch (fun () ->
      let* (_, _, _, p) = Urme_store.Project_store.commit_info ~repo ~sha in
      Lwt.return p
    ) (fun _ -> Lwt.return []) in
  let parent = match parents with p :: _ -> p | [] -> "" in
  let* changes =
    if parent = "" then begin
      (* Initial commit — everything is added *)
      let* files = Urme_store.Project_store.changed_files ~repo ~sha1:sha ~sha2:sha in
      Lwt_list.filter_map_s (fun f ->
        let* content = Urme_store.Project_store.read_blob ~repo ~sha ~path:f in
        match content with
        | Some c -> Lwt.return_some (f, Git_walk.FileCreated c)
        | None -> Lwt.return_none
      ) files
    end else
      let* p_files = Urme_store.Project_store.changed_files
          ~repo ~sha1:parent ~sha2:sha in
      Lwt_list.filter_map_s (fun f ->
        let* before =
          Urme_store.Project_store.read_blob ~repo ~sha:parent ~path:f in
        let* after =
          Urme_store.Project_store.read_blob ~repo ~sha ~path:f in
        match before, after with
        | None, Some a -> Lwt.return_some (f, Git_walk.FileCreated a)
        | Some b, None -> Lwt.return_some (f, Git_walk.FileDeleted b)
        | Some b, Some a -> Lwt.return_some (f, Git_walk.FileEdited (b, a))
        | None, None -> Lwt.return_none
      ) p_files
  in
  Lwt.return Git_walk.{ sha; timestamp; changes }

(* ---------- Edit grouping ---------- *)

(* Group a flat list of edits into EditGroups by (session_id, turn_idx).
   Preserves ordering within a turn. *)
let group_edits (edits : edit list) =
  let by_turn : (string * int, edit list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (e : edit) ->
    let key = (e.session_id, e.turn_idx) in
    let existing = try Hashtbl.find by_turn key with Not_found -> [] in
    Hashtbl.replace by_turn key (existing @ [e])
  ) edits;
  Hashtbl.fold (fun _ es acc -> es :: acc) by_turn []

(* ---------- Event stream ---------- *)

(* Build an ordered event stream for a branch: all edits and commits
   in the given sha range, merged by timestamp. *)
let build_events ~cwd ~repo ~branch_edits ~commits =
  let edit_groups = group_edits branch_edits in
  let edit_events = List.map (fun group ->
    let ts = match group with e :: _ -> e.timestamp | [] -> 0.0 in
    (ts, Git_walk.EditGroup group)
  ) edit_groups in
  let* commit_events = Lwt_list.map_p (fun (sha, ts, _msg) ->
    let+ ci = commit_info_of ~repo ~sha ~timestamp:ts in
    (ts, Git_walk.CommitEvent ci)
  ) commits in
  ignore cwd;
  let all = edit_events @ commit_events in
  let sorted = List.sort (fun (a, _) (b, _) -> Float.compare a b) all in
  Lwt.return (List.map snd sorted)

(* ---------- Per-branch walk ---------- *)

(* Walk one branch: gather events, run Git_walk under handlers.
   Returns the new last_processed SHA (latest commit SHA walked). *)
let walk_branch ~cwd ~repo ~port ~collection_id
    ~branch ~edits ~since_sha ~until_sha =
  let since = if since_sha = "" then "" else since_sha ^ ".." in
  let range = since ^ until_sha in
  let* out = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd
      ["log"; "--format=%H%n%at%n%s%n---"; range]
  ) (fun _ -> Lwt.return "") in
  let commits =
    let lines = String.split_on_char '\n' out in
    let rec parse = function
      | sha :: ts :: msg :: "---" :: rest ->
        (sha, (try float_of_string ts with _ -> 0.0), msg) :: parse rest
      | _ -> []
    in parse lines |> List.rev in
  let branch_edits = List.filter (fun (e : edit) ->
    e.git_branch = branch) edits in
  let* events = build_events ~cwd ~repo ~branch_edits ~commits in
  let last_commit = since_sha in
  let h = Walk_handlers.make ~repo ~port ~collection_id in
  let* _final = Git_walk.walk ~h ~branch ~last_commit events in
  let new_last_processed = match List.rev commits with
    | (sha, _, _) :: _ -> sha
    | [] -> since_sha in
  Lwt.return new_last_processed

(* ---------- Main entry point ---------- *)

(* Collect the set of all commit SHAs reachable from any current branch. *)
let all_reachable_shas ~cwd =
  let* out = Lwt.catch (fun () ->
    Urme_git.Ops.run_git ~cwd ["log"; "--all"; "--format=%H"]
  ) (fun _ -> Lwt.return "") in
  let shas = String.split_on_char '\n' out
             |> List.map String.trim
             |> List.filter (fun s -> s <> "") in
  let set = Hashtbl.create (List.length shas) in
  List.iter (fun s -> Hashtbl.replace set s ()) shas;
  Lwt.return set

(* Scan ChromaDB for git_info entries pointing at orphaned SHAs.
   For each such entry, set its value to Null (Pending).  Writes back
   only the ids whose git_info actually changed. *)
let cleanup_orphans ~cwd ~port ~collection_id =
  let* reachable = all_reachable_shas ~cwd in
  let* gis = Urme_search.Chromadb.get_all_with_git_info ~port ~collection_id in
  Lwt_list.iter_s (fun (id, gi_str, _sid, _idx, _ts) ->
    let tbl = parse_git_info_json gi_str in
    let changed = ref false in
    Hashtbl.iter (fun k v ->
      match v with
      | Some gi when not (Hashtbl.mem reachable gi.commit_sha) ->
        Hashtbl.replace tbl k None;
        changed := true
      | _ -> ()
    ) tbl;
    if !changed then
      let+ _ok = Urme_search.Chromadb.update_interaction_git_info
          ~port ~collection_id ~id
          ~git_info:(serialize_git_info_json tbl) in ()
    else Lwt.return_unit
  ) gis

let update ~project_dir ~port ~collection_id ~edits =
  let* repo = Urme_store.Project_store.open_repo ~project_dir in
  let saved = Git_state.load ~project_dir in
  let* changes = Branch_diff.diff ~cwd:project_dir ~saved in
  let new_state = ref saved in
  let needs_cleanup = ref false in
  let* () = Lwt_list.iter_s (fun change ->
    match change with
    | Branch_diff.Unchanged _ -> Lwt.return_unit
    | Branch_diff.NewBranch { name; tip } ->
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~port
          ~collection_id ~branch:name ~edits ~since_sha:"" ~until_sha:tip in
      new_state := Git_state.update_branch !new_state name
          { tip; last_processed };
      Lwt.return_unit
    | Branch_diff.FastForward { name; old_tip; new_tip } ->
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~port
          ~collection_id ~branch:name ~edits
          ~since_sha:old_tip ~until_sha:new_tip in
      new_state := Git_state.update_branch !new_state name
          { tip = new_tip; last_processed };
      Lwt.return_unit
    | Branch_diff.Rebased { name; old_tip = _; new_tip } ->
      needs_cleanup := true;
      let* last_processed = walk_branch ~cwd:project_dir ~repo ~port
          ~collection_id ~branch:name ~edits ~since_sha:"" ~until_sha:new_tip in
      new_state := Git_state.update_branch !new_state name
          { tip = new_tip; last_processed };
      Lwt.return_unit
    | Branch_diff.Merged { name; _ } ->
      new_state := Git_state.remove_branch !new_state name;
      Lwt.return_unit
    | Branch_diff.Deleted { name; _ } ->
      needs_cleanup := true;
      new_state := Git_state.remove_branch !new_state name;
      Lwt.return_unit
  ) changes in
  let* () = if !needs_cleanup
    then cleanup_orphans ~cwd:project_dir ~port ~collection_id
    else Lwt.return_unit in
  Git_state.save ~project_dir !new_state;
  Lwt.return_unit
