(* Load an opengrep-callgraph/v1 export into the urme store, then compute
   SCC condensation + leaves-first topo order. JSON parsing lives here (in
   the engine) so the store stays free of a yojson dependency. *)

module Cg = Urme_store.Callgraph_store
module Schema = Urme_store.Schema

(* --- parse opengrep-callgraph/v1 --- *)

type parsed = {
  root : string;
  lang : string;
  nodes : Cg.node list;
  edges : Cg.edge list;
}

(* Store paths relative to the export's [root]. The opengrep node id
   embeds the file path mid-string (name|/abs/path|line|col), so this
   removes every occurrence of the root prefix, not just a leading one —
   shrinking every id, file, and edge endpoint the MCP tools later
   return. Readers resolve back against the [cg_root] meta value (see
   [extract_code] call sites). *)
let relativize ~root s =
  if root = "" then s
  else
    let pre = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
    let plen = String.length pre in
    let n = String.length s in
    let buf = Buffer.create n in
    let i = ref 0 in
    while !i < n do
      if !i + plen <= n && String.sub s !i plen = pre then i := !i + plen
      else begin Buffer.add_char buf s.[!i]; incr i end
    done;
    Buffer.contents buf

(* NB: [Yojson.Safe.Util] exports a [path] value, so don't name the
   parameter [path] under the local open. *)
let parse_file (json_path : string) : parsed =
  let open Yojson.Safe.Util in
  let j = Yojson.Safe.from_file json_path in
  let str ?(default = "") k v = try v |> member k |> to_string with _ -> default in
  let int_ ?(default = 0) k v = try v |> member k |> to_int with _ -> default in
  let root = str "root" j in
  let rel = relativize ~root in
  let node_of jn = {
    Cg.id = rel (jn |> member "id" |> to_string);
    name = str "name" jn;
    file = rel (str "file" jn);
    start_line = int_ "start_line" jn;
    end_line = int_ "end_line" jn;
    end_exact = (try jn |> member "end_exact" |> to_bool with _ -> false);
    kind = str ~default:"normal" "kind" jn;
  } in
  (* opengrep's Call_graph orients edges callee -> caller (taint
     signatures flow that way): the exported call_site always falls
     inside the TARGET's span. urme stores caller -> callee, so swap. *)
  let edge_of je =
    let cs = try je |> member "call_site" with _ -> `Null in
    {
      Cg.src = rel (je |> member "target" |> to_string);
      dst = rel (je |> member "source" |> to_string);
      kind = str ~default:"call" "kind" je;
      call_file = rel (str "file" cs);
      call_line = int_ "line" cs;
      call_col = int_ "col" cs;
    } in
  {
    root;
    lang = str "lang" j;
    nodes = j |> member "nodes" |> to_list |> List.map node_of;
    edges = j |> member "edges" |> to_list |> List.map edge_of;
  }

(* --- source extraction (spans -> code) --- *)

(* Decorators/annotations sit ABOVE a function's exported span
   (opengrep's start_line is the def itself), yet they carry facts the
   summaries need — @celery.task, @manager.option, route registration.
   Walk upward from [start_line] and return the first line of the
   contiguous decorator block, or [start_line] if there is none.
   Multi-line decorator calls are handled by bracket balance: scanning
   upward, unmatched closers mean we are inside a continuation; a line
   starting with '@' at balance 0 confirms the segment as a decorator.
   Unconfirmed segments (e.g. a dict literal above the span) are not
   included. *)
let decorator_start lines start_line =
  let count c s = String.fold_left (fun n ch -> if ch = c then n + 1 else n) 0 s in
  let opens s = count '(' s + count '[' s + count '{' s in
  let closes s = count ')' s + count ']' s + count '}' s in
  let rec up i depth confirmed =
    if i < 1 then confirmed
    else
      let s = String.trim lines.(i - 1) in
      let d = depth + closes s - opens s in
      if d <= 0 && String.length s > 0 && s.[0] = '@' then up (i - 1) 0 i
      else if d > 0 then up (i - 1) d confirmed
      else confirmed
  in
  up (start_line - 1) 0 start_line

(* Read lines [start_line, end_line] (1-based, inclusive) from [file],
   extended upward over any contiguous decorator block above the span.
   Returns "" if the file can't be read. *)
let extract_code ~file ~start_line ~end_line =
  if start_line <= 0 then ""
  else
    try
      let ic = open_in file in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
        let rec read i acc =
          if i >= end_line then Array.of_list (List.rev acc)
          else
            match input_line ic with
            | line -> read (i + 1) (line :: acc)
            | exception End_of_file -> Array.of_list (List.rev acc)
        in
        let lines = read 0 [] in
        let n = Array.length lines in
        let first = decorator_start lines (min start_line (n + 1)) in
        let last = min end_line n in
        if first > last then ""
        else
          String.concat "\n"
            (Array.to_list (Array.sub lines (first - 1) (last - first + 1))))
    with _ -> ""

let code_hash s = Digest.to_hex (Digest.string s)

(* --- build --- *)

(* Load [json_path] into the store, replacing any existing graph, then
   compute SCC + topo. Returns (n_nodes, n_edges). *)
let build ~db ~json_path =
  let p = parse_file json_path in
  Cg.insert_graph db ~lang:p.lang ~nodes:p.nodes ~edges:p.edges;
  Cg.compute_scc_topo db;
  (* Dispatch edges are derived from descriptions (populated when
     annotation completes — see handle_graph_set_descriptions); clear any
     from a prior graph here. *)
  (try Cg.populate_dispatch_edges db with _ -> ());
  Schema.set_meta db "cg_root" p.root;
  Schema.set_meta db "cg_lang" p.lang;
  Schema.bump_graph_version db;
  (List.length p.nodes, List.length p.edges)
