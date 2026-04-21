(* Handlers for Git_walk: bridge the pure walking algorithm to
   real IO — Irmin for reads, ChromaDB for saves. *)

open Lwt.Syntax
open Git_link_types

(* ---------- Materialise: read from Irmin + apply patches ---------- *)

let materialise ~repo ~commit ~file ~patches =
  let* base = Lwt.catch (fun () ->
    Urme_store.Project_store.read_blob ~repo ~sha:commit ~path:file
  ) (fun _ -> Lwt.return_none) in
  Lwt.return (List.fold_left (fun acc p ->
    Patch.patch ~cleanly:false acc p
  ) base patches)

(* ---------- Save: write git_info to ChromaDB ---------- *)

let git_info_entry_of (edit, status) =
  match status with
  | Git_walk.Committed sha ->
    let dh = diff_hash_of_string (edit.old_string ^ "\x00" ^ edit.new_string) in
    Some (edit.edit_key, Some {
      commit_sha = sha; diff_hash = dh;
      turn_idx = edit.turn_idx; entry_idx = edit.entry_idx;
    })
  | Git_walk.Dead ->
    Some (edit.edit_key, None)
  | Git_walk.Pending -> None

let chromadb_id_of_entry (entry : Git_walk.stack_entry) =
  match entry.origin, entry.edits with
  | Git_walk.Claude, ((e, _) :: _) ->
    Printf.sprintf "%s_%d" e.session_id e.interaction_index
  | Git_walk.Human, ((e, _) :: _) ->
    Printf.sprintf "human_%s_%s" (Filename.basename e.file_base)
      (Digest.string (e.old_string ^ "\x00" ^ e.new_string) |> Digest.to_hex
       |> fun s -> String.sub s 0 16)
  | _, [] -> "empty"

let save ~port ~collection_id (entry : Git_walk.stack_entry) =
  let id = chromadb_id_of_entry entry in
  let new_entries = List.filter_map git_info_entry_of entry.edits in
  if new_entries = [] then Lwt.return_unit
  else
    let* existing_meta = Lwt.catch (fun () ->
      Urme_search.Chromadb.get_interaction_meta ~port ~collection_id ~id
    ) (fun _ -> Lwt.return_none) in
    let tbl = match existing_meta with
      | Some meta ->
        let open Yojson.Safe.Util in
        let gi = try meta |> member "git_info" |> to_string with _ -> "" in
        parse_git_info_json gi
      | None -> Hashtbl.create 8 in
    List.iter (fun (k, v) -> Hashtbl.replace tbl k v) new_entries;
    let* () = match entry.origin with
      | Git_walk.Human when existing_meta = None ->
        (match entry.edits with
         | (e, _) :: _ ->
           Urme_search.Chromadb.save_interaction ~port ~collection_id
             ~experience_id:"human"
             ~interaction_index:(Hashtbl.hash id land 0x7FFFFFFF)
             ~user_text:(Printf.sprintf "[human edit] %s" e.file_base)
             ~assistant_summary:""
             ~user_uuid:id
             ~timestamp:(Printf.sprintf "%.0f" e.timestamp)
             ()
         | [] -> Lwt.return_unit)
      | _ -> Lwt.return_unit in
    let+ _ok = Urme_search.Chromadb.update_interaction_git_info
        ~port ~collection_id ~id
        ~git_info:(serialize_git_info_json tbl) in ()

(* ---------- Build handlers record ---------- *)

let make ~repo ~port ~collection_id : Git_walk.handlers =
  { materialise = (fun ~commit ~file ~patches ->
      materialise ~repo ~commit ~file ~patches);
    save = (fun entry -> save ~port ~collection_id entry); }
