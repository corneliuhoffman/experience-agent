open Lwt.Syntax
open Urme_core.Types

let tenant = "default_tenant"
let database = "default_database"

let base_url port =
  Printf.sprintf "http://[::1]:%d/api/v2/tenants/%s/databases/%s"
    port tenant database

let ollama_url = "http://127.0.0.1:11434"

(* Strip invalid UTF-8 bytes to prevent ChromaDB JSON parse errors *)
let sanitize s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let c = Char.code s.[!i] in
    if c < 0x80 then (Buffer.add_char buf s.[!i]; incr i)
    else if c < 0xC0 then (Buffer.add_char buf '?'; incr i)
    else if c < 0xE0 then
      (if !i+1 < len && Char.code s.[!i+1] >= 0x80 && Char.code s.[!i+1] < 0xC0
       then (Buffer.add_string buf (String.sub s !i 2); i := !i+2)
       else (Buffer.add_char buf '?'; incr i))
    else if c < 0xF0 then
      (if !i+2 < len && Char.code s.[!i+1] >= 0x80 && Char.code s.[!i+2] >= 0x80
       then (Buffer.add_string buf (String.sub s !i 3); i := !i+3)
       else (Buffer.add_char buf '?'; incr i))
    else if c < 0xF8 then
      (if !i+3 < len && Char.code s.[!i+1] >= 0x80 && Char.code s.[!i+2] >= 0x80 && Char.code s.[!i+3] >= 0x80
       then (Buffer.add_string buf (String.sub s !i 4); i := !i+4)
       else (Buffer.add_char buf '?'; incr i))
    else (Buffer.add_char buf '?'; incr i)
  done;
  Buffer.contents buf

let safe_to_list j =
  match j with
  | `List l -> l
  | `Null -> []
  | _ -> []

let http_post_raw ~url ~body =
  let uri = Uri.of_string url in
  let headers = Cohttp.Header.of_list [
    "Content-Type", "application/json";
  ] in
  let body_str = Yojson.Safe.to_string body in
  let* _resp, body = Cohttp_lwt_unix.Client.post uri
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body_str) in
  let+ body_str = Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string body_str

let http_post ~port ~path ~body =
  http_post_raw ~url:(base_url port ^ path) ~body

let http_delete ~port ~path =
  let uri = Uri.of_string (base_url port ^ path) in
  let* _resp, body = Cohttp_lwt_unix.Client.delete uri in
  let+ body_str = Cohttp_lwt.Body.to_string body in
  ignore body_str

(* Ollama chat — summarize conversation *)
let summarize_text text =
  let prompt = Printf.sprintf
    "Summarize this coding conversation in 2-3 sentences. Focus on: what was the goal, what approach was taken, and what was the outcome. Be specific about technologies, files, and patterns.\n\n%s"
    text in
  let body = `Assoc [
    "model", `String "llama3.2";
    "prompt", `String prompt;
    "stream", `Bool false;
  ] in
  let+ resp = http_post_raw ~url:(ollama_url ^ "/api/generate") ~body in
  let open Yojson.Safe.Util in
  resp |> member "response" |> to_string_option |> Option.value ~default:text

(* Split text into chunks of at most n chars, breaking at newlines *)
let chunk_text ~max_chars text =
  let len = String.length text in
  if len <= max_chars then [text]
  else
    let chunks = ref [] in
    let pos = ref 0 in
    while !pos < len do
      let remaining = len - !pos in
      let chunk_end = if remaining <= max_chars then len
        else
          (* Look for last newline within range *)
          let limit = !pos + max_chars in
          let nl = ref limit in
          while !nl > !pos && String.get text !nl <> '\n' do decr nl done;
          if !nl > !pos then !nl + 1 else limit
      in
      chunks := String.sub text !pos (chunk_end - !pos) :: !chunks;
      pos := chunk_end
    done;
    List.rev !chunks

(* Embed a single chunk via Ollama *)
let embed_single chunk =
  let body = `Assoc [
    "model", `String "nomic-embed-text";
    "input", `String chunk;
  ] in
  let+ resp = http_post_raw ~url:(ollama_url ^ "/api/embed") ~body in
  let open Yojson.Safe.Util in
  match resp |> member "error" |> to_string_option with
  | Some err -> failwith (Printf.sprintf "Ollama embed error: %s" err)
  | None ->
    match resp |> member "embeddings" with
    | `Null | `Bool _ | `String _ ->
      failwith (Printf.sprintf "Ollama returned no embeddings (response: %s)"
        (Yojson.Safe.to_string resp |> fun s ->
          if String.length s > 200 then String.sub s 0 200 ^ "..." else s))
    | embeddings ->
      embeddings |> to_list |> List.hd |> to_list |> List.map to_float

(* Average a list of embedding vectors *)
let average_embeddings vecs =
  match vecs with
  | [] -> []
  | [v] -> v
  | first :: _ ->
    let n = Float.of_int (List.length vecs) in
    let sums = Array.make (List.length first) 0.0 in
    List.iter (fun v ->
      List.iteri (fun i x -> sums.(i) <- sums.(i) +. x) v
    ) vecs;
    Array.to_list (Array.map (fun s -> s /. n) sums)

(* Ollama embeddings — splits long text into chunks and averages *)
let embed_text text =
  let chunks = chunk_text ~max_chars:2000 text in
  let* embeddings = Lwt_list.map_s embed_single chunks in
  Lwt.return (average_embeddings embeddings)

(* Embed semantic chunks — each chunk is a meaningful unit, embedded separately and averaged *)
let embed_chunks chunks =
  let non_empty = List.filter (fun s -> String.trim s <> "") chunks in
  if non_empty = [] then embed_text ""
  else
    (* Each chunk may still be >2000 chars, so sub-chunk if needed *)
    let all_sub = List.concat_map (chunk_text ~max_chars:2000) non_empty in
    let* embeddings = Lwt_list.map_s embed_single all_sub in
    Lwt.return (average_embeddings embeddings)

(* Batch embed multiple texts at once via Ollama *)
let embed_texts texts =
  if texts = [] then Lwt.return []
  else
    (* Truncate each text to 2000 chars for embedding *)
    let truncated = List.map (fun t ->
      if String.length t <= 2000 then t
      else String.sub t 0 2000
    ) texts in
    let body = `Assoc [
      "model", `String "nomic-embed-text";
      "input", `List (List.map (fun t -> `String t) truncated);
    ] in
    let+ resp = http_post_raw ~url:(ollama_url ^ "/api/embed") ~body in
    let open Yojson.Safe.Util in
    match resp |> member "error" |> to_string_option with
    | Some err -> failwith (Printf.sprintf "Ollama batch embed error: %s" err)
    | None ->
      resp |> member "embeddings" |> to_list
      |> List.map (fun v -> v |> to_list |> List.map to_float)

(* Summarize then embed *)
let summarize_and_embed text =
  let* summary = summarize_text text in
  let+ embedding = embed_text summary in
  (summary, embedding)

(* Single collection per project *)
let get_or_create_collection ~port ~name =
  let body = `Assoc [
    "name", `String name;
    "get_or_create", `Bool true;
  ] in
  let+ resp = http_post ~port ~path:"/collections" ~body in
  let open Yojson.Safe.Util in
  match resp |> member "error" |> to_string_option with
  | Some err ->
    let msg = resp |> member "message" |> to_string_option
      |> Option.value ~default:err in
    failwith (Printf.sprintf "ChromaDB error creating collection %s: %s" name msg)
  | None ->
    resp |> member "id" |> to_string

let delete_collection ~port ~name =
  let* _id = get_or_create_collection ~port ~name in
  http_delete ~port ~path:(Printf.sprintf "/collections/%s" name)

let ensure_collection ~port ~project =
  get_or_create_collection ~port ~name:(project ^ "_experiences")

let ensure_interactions_collection ~port ~project =
  get_or_create_collection ~port ~name:(project ^ "_interactions")

(* Add document with embedding *)
let add_document ~port ~collection_id ~id ~document ~metadata =
  let* embedding = embed_text document in
  let body = `Assoc [
    "ids", `List [`String id];
    "documents", `List [`String document];
    "embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "metadatas", `List [metadata];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
  ()

(* Save an experience — summarize full conversation via Ollama, then embed the summary.
   Experience ID = commit SHA. Stores git metadata alongside conversation data. *)
let save_experience ~port ~collection_id (exp : experience) ~full_conversation =
  let* (summary, embedding) = summarize_and_embed full_conversation in
  let metadata = `Assoc [
    "label", `String exp.label;
    "intent", `String exp.intent;
    "session_id", `String exp.session_id;
    "interaction_uuids", `String (String.concat "," exp.interaction_uuids);
    "head_sha", `String exp.head_sha;
    "commit_sha", `String exp.commit_sha;
    "commit_message", `String exp.commit_message;
    "parent_sha", `String exp.parent_sha;
    "diff", `String (if String.length exp.diff <= 10000 then exp.diff
                      else String.sub exp.diff 0 10000 ^ "\n... (truncated)");
    "branch", `String exp.branch;
    "files_changed", `String (String.concat "," exp.files_changed);
    "timestamp", `Float exp.timestamp;
    "reverted", `Bool exp.reverted;
    "summary", `String summary;
    "conversation_text", `String full_conversation;
  ] in
  let body = `Assoc [
    "ids", `List [`String exp.id];
    "documents", `List [`String summary];
    "embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "metadatas", `List [metadata];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
  ()

(* Search experiences *)
let search ~port ~collection_id ~query ~n =
  let* embedding = embed_text query in
  let body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int n;
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids_outer = resp |> member "ids" |> safe_to_list in
  match ids_outer with
  | [] -> []
  | first_ids :: _ ->
    let ids = first_ids |> safe_to_list |> List.map to_string in
    let distances = (resp |> member "distances" |> safe_to_list
      |> function [] -> [] | d :: _ -> d |> safe_to_list |> List.map to_float) in
    let metadatas = (resp |> member "metadatas" |> safe_to_list
      |> function [] -> [] | m :: _ -> m |> safe_to_list) in
    if ids = [] then []
    else
      let results = List.combine (List.combine ids distances) metadatas
        |> List.map (fun ((id, dist), meta) ->
          let exp = experience_of_metadata id meta in
          (* Deprioritize reverted experiences *)
          let adjusted_dist = if exp.reverted then dist +. 0.5 else dist in
          { experience = exp; distance = adjusted_dist })
      in
      List.sort (fun a b -> Float.compare a.distance b.distance) results

(* Update metadata for an experience (for link_commit and go_back).
   Returns false if the document doesn't exist. *)
let update_metadata ~port ~collection_id ~id ~updates =
  let get_body = `Assoc [
    "ids", `List [`String id];
    "include", `List [`String "metadatas"];
  ] in
  let* resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body:get_body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  match ids with
  | [] -> Lwt.return false
  | _ ->
    let old_meta = resp |> member "metadatas" |> safe_to_list
      |> List.hd in
    let old_fields = match old_meta with `Assoc l -> l | _ -> [] in
    let new_fields = List.map (fun (k, v) ->
      match List.assoc_opt k updates with
      | Some new_v -> (k, new_v)
      | None -> (k, v)
    ) old_fields in
    let extra = List.filter (fun (k, _) ->
      not (List.mem_assoc k old_fields)
    ) updates in
    let merged = `Assoc (new_fields @ extra) in
    let update_body = `Assoc [
      "ids", `List [`String id];
      "metadatas", `List [merged];
    ] in
    let+ _resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/update" collection_id)
      ~body:update_body in
    true

(* List all experiences *)
let list_all ~port ~collection_id ~limit =
  let body = `Assoc [
    "limit", `Int limit;
    "include", `List [`String "documents"; `String "metadatas"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  let metadatas = resp |> member "metadatas" |> safe_to_list in
  if ids = [] then []
  else
    List.combine ids metadatas
    |> List.map (fun (id, meta) -> experience_of_metadata id meta)

(* List all experiences with their Ollama-generated summaries *)
let list_all_with_summaries ~port ~collection_id ~limit =
  let body = `Assoc [
    "limit", `Int limit;
    "include", `List [`String "documents"; `String "metadatas"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  let metadatas = resp |> member "metadatas" |> safe_to_list in
  if ids = [] then []
  else
    List.combine ids metadatas
    |> List.map (fun (id, meta) ->
      let exp = experience_of_metadata id meta in
      let summary = meta |> member "summary" |> to_string_option
        |> Option.value ~default:"" in
      (exp, summary))

(* Delete an experience *)
let delete ~port ~collection_id ~id =
  let body = `Assoc [
    "ids", `List [`String id];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/delete" collection_id) ~body in
  ()

(* Get metadata for an interaction by ID. Returns None if not found. *)
let get_interaction_meta ~port ~collection_id ~id =
  let body = `Assoc [
    "ids", `List [`String id];
    "include", `List [`String "metadatas"];
  ] in
  let* resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
  match ids with
  | [] -> Lwt.return_none
  | _ ->
    let meta = resp |> member "metadatas" |> safe_to_list |> List.hd in
    Lwt.return_some meta

(* Save a single interaction document into the interactions collection.
   Embeds semantic chunks (user text, assistant texts, files) separately and averages. *)
let save_interaction ~port ~collection_id ~experience_id ~interaction_index
    ~user_text ~assistant_summary ?(files_changed=[]) ~user_uuid ~timestamp
    ?(git_info="") () =
  let files_str = if files_changed = [] then ""
    else "Files: " ^ String.concat ", " (List.map Filename.basename files_changed) in
  (* Semantic chunks: user question, files, each assistant paragraph *)
  let assistant_chunks = String.split_on_char '\n' assistant_summary
    |> List.filter (fun s -> String.trim s <> "") in
  let chunks = [user_text; files_str] @ assistant_chunks in
  let document = String.concat "\n" (List.filter (fun s -> s <> "") chunks) in
  let* embedding = embed_chunks chunks in
  let id = Printf.sprintf "%s_%d" experience_id interaction_index in
  let git_fields = if git_info <> "" then [
    "git_info", `String git_info;
  ] else [] in
  let metadata = `Assoc ([
    "experience_id", `String experience_id;
    "interaction_index", `Int interaction_index;
    "user_uuid", `String user_uuid;
    "user_text", `String (sanitize user_text);
    "timestamp", `String timestamp;
  ] @ git_fields) in
  let body = `Assoc [
    "ids", `List [`String id];
    "documents", `List [`String document];
    "embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "metadatas", `List [metadata];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
  ()

(* Batch-save multiple interactions with semantic chunk embeddings.
   items: list of (id, experience_id, interaction_index, user_text,
                   assistant_summary, user_uuid, timestamp, files_changed) *)
let save_interactions_batch ~port ~collection_id items =
  if items = [] then Lwt.return_unit
  else
    (* Build full documents with files, then batch-embed *)
    let documents = List.map
      (fun (_id, _eid, _idx, user_text, assistant_summary, _uuid, _ts, files_changed) ->
        let files_str = if files_changed = [] then ""
          else "\nFiles: " ^ String.concat ", " (List.map Filename.basename files_changed) in
        sanitize (Printf.sprintf "User: %s%s\n%s" user_text files_str assistant_summary)
      ) items in
    let* embeddings = embed_texts documents in
    let ids = List.map (fun (id, _, _, _, _, _, _, _) -> `String id) items in
    let metadatas = List.map (fun (_id, eid, idx, user_text, _summary, uuid, ts, _files) ->
      `Assoc [
        "experience_id", `String eid;
        "interaction_index", `Int idx;
        "user_uuid", `String uuid;
        "user_text", `String (sanitize user_text);
        "timestamp", `String ts;
      ]
    ) items in
    let emb_json = List.map (fun emb ->
      `List (List.map (fun f -> `Float f) emb)
    ) embeddings in
    let body = `Assoc [
      "ids", `List ids;
      "documents", `List (List.map (fun d -> `String d) documents);
      "embeddings", `List emb_json;
      "metadatas", `List metadatas;
    ] in
    let+ resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/add" collection_id) ~body in
    let open Yojson.Safe.Util in
    (match resp |> member "error" |> to_string_option with
     | Some err ->
       let oc = open_out_gen [Open_append; Open_creat] 0o644 "/tmp/urme_debug.log" in
       let n = List.length items in
       let first_id = match items with (id,_,_,_,_,_,_,_)::_ -> id | [] -> "?" in
       let msg = resp |> member "message" |> to_string_option
         |> Option.value ~default:"" in
       Printf.fprintf oc "[%.2f] ChromaDB add FAILED (%d items, first=%s): %s: %s\n%!"
         (Unix.gettimeofday ()) n first_id err msg;
       close_out oc
     | None -> ())

(* Update git_info JSON on an existing interaction *)
let update_interaction_git_info ~port ~collection_id ~id ~git_info =
  let updates = [
    "git_info", `String git_info;
  ] in
  update_metadata ~port ~collection_id ~id ~updates

(* Search interactions within a specific experience *)
let search_interactions ~port ~collection_id ~experience_id ~query ~n =
  let* embedding = embed_text query in
  let body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int n;
    "where", `Assoc ["experience_id", `Assoc ["$eq", `String experience_id]];
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  let+ resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body in
  let open Yojson.Safe.Util in
  let ids_outer = resp |> member "ids" |> safe_to_list in
  match ids_outer with
  | [] -> []
  | first_ids :: _ ->
    let ids = first_ids |> safe_to_list |> List.map to_string in
    let distances = (resp |> member "distances" |> safe_to_list
      |> function [] -> [] | d :: _ -> d |> safe_to_list |> List.map to_float) in
    let metadatas = (resp |> member "metadatas" |> safe_to_list
      |> function [] -> [] | m :: _ -> m |> safe_to_list) in
    if ids = [] then []
    else
      List.combine (List.combine ids distances) metadatas
      |> List.map (fun ((_id, distance), meta) ->
        let idx = meta |> member "interaction_index" |> to_int_option
          |> Option.value ~default:0 in
        let user_uuid = meta |> member "user_uuid" |> to_string_option
          |> Option.value ~default:"" in
        (idx, user_uuid, distance))

(* Extract file-like tokens from query for text search *)
let extract_file_tokens query =
  String.split_on_char ' ' query
  |> List.filter_map (fun w ->
    let w = String.trim w in
    if String.contains w '.' && String.length w > 2 then Some w
    else None)

(* Parse ChromaDB query response into result tuples *)
let parse_query_results resp =
  let open Yojson.Safe.Util in
  let ids_outer = resp |> member "ids" |> safe_to_list in
  match ids_outer with
  | [] -> []
  | first_ids :: _ ->
    let ids = first_ids |> safe_to_list |> List.map to_string in
    let distances = (resp |> member "distances" |> safe_to_list
      |> function [] -> [] | d :: _ -> d |> safe_to_list |> List.map to_float) in
    let metadatas = (resp |> member "metadatas" |> safe_to_list
      |> function [] -> [] | m :: _ -> m |> safe_to_list) in
    let documents = (resp |> member "documents" |> safe_to_list
      |> function [] -> [] | d :: _ -> d |> safe_to_list
         |> List.map (fun j -> to_string_option j |> Option.value ~default:"")) in
    if ids = [] then []
    else
      let combine4 a b c d =
        List.map2 (fun (a,b) (c,d) -> (a,b,c,d))
          (List.combine a b) (List.combine c d) in
      combine4 ids distances metadatas documents
      |> List.map (fun (id, distance, meta, doc) ->
        let session_id = meta |> member "experience_id" |> to_string_option
          |> Option.value ~default:"" in
        let user_text = meta |> member "user_text" |> to_string_option
          |> Option.value ~default:"" in
        let timestamp = meta |> member "timestamp" |> to_string_option
          |> Option.value ~default:"" in
        let interaction_index = meta |> member "interaction_index" |> to_int_option
          |> Option.value ~default:0 in
        (id, session_id, user_text, doc, interaction_index, timestamp, distance))

(* Search all interactions: vector search + text search for file names, merged and deduped *)
let search_all_interactions ~port ~collection_id ~query ~n =
  let* embedding = embed_text query in
  (* 1. Vector similarity search *)
  let vector_body = `Assoc [
    "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
    "n_results", `Int (n * 2);
    "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
  ] in
  let p_vector = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body:vector_body in
  (* 2. Text search for file tokens (if query mentions files) *)
  let file_tokens = extract_file_tokens query in
  let p_text = if file_tokens = [] then Lwt.return []
    else
      (* Search for each file token via where_document $contains *)
      Lwt_list.map_s (fun tok ->
        let body = `Assoc [
          "query_embeddings", `List [`List (List.map (fun f -> `Float f) embedding)];
          "n_results", `Int n;
          "where_document", `Assoc ["$contains", `String tok];
          "include", `List [`String "documents"; `String "metadatas"; `String "distances"];
        ] in
        Lwt.catch (fun () ->
          let+ resp = http_post ~port
            ~path:(Printf.sprintf "/collections/%s/query" collection_id) ~body in
          parse_query_results resp
        ) (fun _ -> Lwt.return [])
      ) file_tokens in
  let* vector_resp = p_vector
  and* text_results = p_text in
  let vector_results = parse_query_results vector_resp in
  let text_results_flat = List.concat text_results in
  (* Merge: text matches get a distance bonus (lower = better) *)
  let text_ids = Hashtbl.create 32 in
  List.iter (fun (id, _, _, _, _, _, _) ->
    Hashtbl.replace text_ids id ()
  ) text_results_flat;
  let boosted = List.map (fun (id, sid, ut, doc, idx, ts, dist) ->
    let bonus = if Hashtbl.mem text_ids id then 0.8 else 0.0 in
    (sid, ut, doc, idx, ts, dist -. bonus)
  ) vector_results in
  (* Add text-only results not in vector results *)
  let vector_id_set = Hashtbl.create 64 in
  List.iter (fun (id, _, _, _, _, _, _) ->
    Hashtbl.replace vector_id_set id ()
  ) vector_results;
  let text_only = List.filter_map (fun (id, sid, ut, doc, idx, ts, dist) ->
    if Hashtbl.mem vector_id_set id then None
    else Some (sid, ut, doc, idx, ts, dist -. 0.5)
  ) text_results_flat in
  let all = boosted @ text_only in
  (* Dedupe by session_id+interaction_index, keep best score *)
  let seen = Hashtbl.create 64 in
  let deduped = List.filter (fun (sid, _, _, idx, _, dist) ->
    let key = Printf.sprintf "%s_%d" sid idx in
    match Hashtbl.find_opt seen key with
    | Some prev_dist when prev_dist <= dist -> false
    | _ -> Hashtbl.replace seen key dist; true
  ) all in
  let sorted = List.sort (fun (_, _, _, _, _, a) (_, _, _, _, _, b) ->
    Float.compare a b) deduped in
  Lwt.return (List.filteri (fun i _ -> i < n) sorted)

(* Fetch all interactions that have non-empty git_info, paged.
   Returns list of (id, git_info_string, experience_id, interaction_index). *)
let get_all_with_git_info ~port ~collection_id =
  let rec fetch_page ~offset ~acc =
    let body = `Assoc [
      "include", `List [`String "metadatas"];
      "limit", `Int 500;
      "offset", `Int offset;
    ] in
    let* resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
    let open Yojson.Safe.Util in
    let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
    let metadatas = resp |> member "metadatas" |> safe_to_list in
    let found = List.filter_map (fun (id, meta) ->
      let gi = meta |> member "git_info" |> to_string_option
        |> Option.value ~default:"" in
      if gi <> "" then
        let session_id = meta |> member "experience_id" |> to_string_option
          |> Option.value ~default:"" in
        let idx = meta |> member "interaction_index" |> to_int_option
          |> Option.value ~default:0 in
        let ts = meta |> member "timestamp" |> to_string_option
          |> Option.value ~default:"" in
        Some (id, gi, session_id, idx, ts)
      else None
    ) (List.combine ids metadatas) in
    let acc = acc @ found in
    if List.length ids < 500 then
      Lwt.return acc
    else
      fetch_page ~offset:(offset + 500) ~acc
  in
  fetch_page ~offset:0 ~acc:[]

(* Fetch all interaction IDs in the collection (paged). *)
let get_all_interaction_ids ~port ~collection_id =
  let rec fetch_page ~offset ~acc =
    let body = `Assoc [
      "include", `List [];
      "limit", `Int 5000;
      "offset", `Int offset;
    ] in
    let* resp = http_post ~port
      ~path:(Printf.sprintf "/collections/%s/get" collection_id) ~body in
    let open Yojson.Safe.Util in
    let ids = resp |> member "ids" |> safe_to_list |> List.map to_string in
    let acc = acc @ ids in
    if List.length ids < 5000 then Lwt.return acc
    else fetch_page ~offset:(offset + 5000) ~acc
  in
  fetch_page ~offset:0 ~acc:[]

(* Delete all interaction documents for an experience *)
let delete_interactions ~port ~collection_id ~experience_id =
  let body = `Assoc [
    "where", `Assoc ["experience_id", `Assoc ["$eq", `String experience_id]];
  ] in
  let+ _resp = http_post ~port
    ~path:(Printf.sprintf "/collections/%s/delete" collection_id) ~body in
  ()
