(* Thin helpers over the sqlite3 C bindings.

   We keep the API small on purpose: open, exec (for DDL and no-result
   statements), exec_params (for parameterised writes), query (for reads).

   All functions raise [Failure] with the sqlite error message on failure —
   the V2 pipeline treats DB errors as fatal and lets them propagate. *)

module S = Sqlite3

type t = S.db

let fail db msg =
  failwith (Printf.sprintf "sqlite: %s: %s" msg (S.errmsg db))

let check db rc msg =
  if rc <> S.Rc.OK && rc <> S.Rc.DONE then fail db msg

(* Run a statement that yields no rows (DDL, INSERT, UPDATE, DELETE,
   PRAGMA without result). Multiple statements separated by ';' are OK. *)
let exec db sql =
  match S.exec db sql with
  | S.Rc.OK -> ()
  | _ -> fail db (Printf.sprintf "exec failed: %s" sql)

let bind_all stmt params =
  List.iteri (fun i v ->
    match S.bind stmt (i + 1) v with
    | S.Rc.OK -> ()
    | _ -> failwith (Printf.sprintf "sqlite: bind index %d failed" (i + 1))
  ) params

let exec_params db sql params =
  let stmt = S.prepare db sql in
  bind_all stmt params;
  (match S.step stmt with
   | S.Rc.DONE | S.Rc.OK -> ()
   | _ -> fail db (Printf.sprintf "exec_params failed: %s" sql));
  ignore (S.finalize stmt)

(* Insert that returns last_insert_rowid. *)
let insert_returning_id db sql params =
  exec_params db sql params;
  S.last_insert_rowid db

(* Run a SELECT and fold over rows.
   [f] gets the column values as Data.t array; return value accumulates. *)
let query_fold db sql params ~init ~f =
  let stmt = S.prepare db sql in
  bind_all stmt params;
  let rec loop acc =
    match S.step stmt with
    | S.Rc.ROW ->
      let cols = Array.init (S.column_count stmt) (S.column stmt) in
      loop (f acc cols)
    | S.Rc.DONE -> acc
    | _ -> fail db (Printf.sprintf "query failed: %s" sql)
  in
  let result = loop init in
  ignore (S.finalize stmt);
  result

let query_list db sql params ~f =
  List.rev (query_fold db sql params ~init:[] ~f:(fun acc cols -> f cols :: acc))

(* Generic no-params query: returns column names and rows (Data.t arrays),
   capped at [max_rows]. For running caller-supplied read-only SELECTs. *)
let query_rows ?(max_rows = 500) db sql =
  let stmt = S.prepare db sql in
  let ncol = S.column_count stmt in
  let headers = Array.to_list (Array.init ncol (S.column_name stmt)) in
  let rec loop n acc =
    if n >= max_rows then (acc, true)
    else match S.step stmt with
      | S.Rc.ROW -> loop (n + 1) (Array.init ncol (S.column stmt) :: acc)
      | S.Rc.DONE -> (acc, false)
      | _ -> fail db (Printf.sprintf "query failed: %s" sql)
  in
  let (rows_rev, truncated) = loop 0 [] in
  ignore (S.finalize stmt);
  (headers, List.rev rows_rev, truncated)

(* Transaction wrapper — rolls back on exception. *)
let with_txn db f =
  exec db "BEGIN";
  match f () with
  | v -> exec db "COMMIT"; v
  | exception e -> (try exec db "ROLLBACK" with _ -> ()); raise e

(* Convenience accessors for Sqlite3.Data.t. *)
let data_to_string = function
  | S.Data.TEXT s | S.Data.BLOB s -> s
  | S.Data.INT i -> Int64.to_string i
  | S.Data.FLOAT f -> string_of_float f
  | S.Data.NULL | S.Data.NONE -> ""

let data_to_int = function
  | S.Data.INT i -> Int64.to_int i
  | S.Data.FLOAT f -> int_of_float f
  | S.Data.TEXT s -> (try int_of_string s with _ -> 0)
  | _ -> 0

let data_to_float = function
  | S.Data.FLOAT f -> f
  | S.Data.INT i -> Int64.to_float i
  | S.Data.TEXT s -> (try float_of_string s with _ -> 0.0)
  | _ -> 0.0

let data_to_string_opt = function
  | S.Data.NULL | S.Data.NONE -> None
  | d -> Some (data_to_string d)

let data_to_float_opt = function
  | S.Data.NULL | S.Data.NONE -> None
  | d -> Some (data_to_float d)
