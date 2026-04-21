(* Diff saved git state against current git state to determine what
   changed per branch.  Drives incremental index updates. *)

open Lwt.Syntax
module SMap = Map.Make(String)

type branch_change =
  | NewBranch of { name : string; tip : string }
  | Unchanged of { name : string; tip : string }
  | FastForward of { name : string; old_tip : string; new_tip : string }
  | Rebased of { name : string; old_tip : string; new_tip : string }
  | Merged of { name : string; old_tip : string; target : string }
  | Deleted of { name : string; old_tip : string }

(* ---------- Git helpers ---------- *)

(* List current branches with their tip SHAs *)
let current_branches ~cwd =
  let* out = Urme_git.Ops.run_git ~cwd
      ["for-each-ref"; "--format=%(refname:short)\t%(objectname)"; "refs/heads/"] in
  let lines = String.split_on_char '\n' out in
  let branches = List.filter_map (fun line ->
    match String.split_on_char '\t' (String.trim line) with
    | [name; sha] when name <> "" && sha <> "" -> Some (name, sha)
    | _ -> None
  ) lines in
  Lwt.return (List.fold_left (fun acc (n, s) -> SMap.add n s acc) SMap.empty branches)

(* Is [a] an ancestor of [b]?  Returns true iff git exits 0. *)
let is_ancestor ~cwd a b =
  Lwt.catch (fun () ->
    let* _ = Urme_git.Ops.run_git ~cwd ["merge-base"; "--is-ancestor"; a; b] in
    Lwt.return true
  ) (fun _ -> Lwt.return false)

(* Does this SHA exist as a reachable commit in the repo? *)
let sha_exists ~cwd sha =
  Lwt.catch (fun () ->
    let* _ = Urme_git.Ops.run_git ~cwd ["rev-parse"; "--verify"; sha ^ "^{commit}"] in
    Lwt.return true
  ) (fun _ -> Lwt.return false)

(* Find a current branch that contains [sha] as an ancestor.  Returns
   the first match (preferring main/master if present). *)
let find_containing_branch ~cwd ~current sha =
  let ordered =
    let main_first = SMap.filter (fun n _ -> n = "main" || n = "master") current in
    let rest = SMap.filter (fun n _ -> n <> "main" && n <> "master") current in
    SMap.bindings main_first @ SMap.bindings rest in
  let rec find = function
    | [] -> Lwt.return_none
    | (name, tip) :: rest ->
      let* anc = is_ancestor ~cwd sha tip in
      if anc then Lwt.return_some (name, tip)
      else find rest
  in find ordered

(* ---------- Diff computation ---------- *)

let diff ~cwd ~saved =
  let* current = current_branches ~cwd in
  (* Changes from current branches *)
  let* changes_from_current = Lwt_list.filter_map_s (fun (name, new_tip) ->
    match Git_state.get_branch saved name with
    | None ->
      Lwt.return_some (NewBranch { name; tip = new_tip })
    | Some snap when snap.tip = new_tip ->
      Lwt.return_some (Unchanged { name; tip = new_tip })
    | Some snap ->
      let* anc = is_ancestor ~cwd snap.tip new_tip in
      if anc then
        Lwt.return_some (FastForward {
          name; old_tip = snap.tip; new_tip })
      else
        Lwt.return_some (Rebased {
          name; old_tip = snap.tip; new_tip })
  ) (SMap.bindings current) in
  (* Changes from deleted branches (in saved but not current) *)
  let saved_names = Git_state.current_branches saved in
  let deleted = List.filter (fun n -> not (SMap.mem n current)) saved_names in
  let* changes_from_deleted = Lwt_list.filter_map_s (fun name ->
    match Git_state.get_branch saved name with
    | None -> Lwt.return_none
    | Some snap ->
      let* exists = sha_exists ~cwd snap.tip in
      if not exists then
        (* SHA is GC'd or truly gone *)
        Lwt.return_some (Deleted { name; old_tip = snap.tip })
      else
        let* target = find_containing_branch ~cwd ~current snap.tip in
        match target with
        | Some (target_name, _) ->
          Lwt.return_some (Merged { name; old_tip = snap.tip; target = target_name })
        | None ->
          Lwt.return_some (Deleted { name; old_tip = snap.tip })
  ) deleted in
  Lwt.return (changes_from_current @ changes_from_deleted)
