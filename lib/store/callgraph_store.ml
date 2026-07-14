(* Call-graph store.

   Nodes + edges from an opengrep-callgraph/v1 export, with SCC
   condensation and a leaves-first topological order, plus per-node
   descriptions filled in by the MCP describe-loop. Pure SQLite — the
   engine layer parses the JSON and hands us typed records.

   Ordering convention: edges are caller -> callee. We describe leaves
   first, so [topo] is assigned so that a node's cross-SCC callees always
   have a strictly smaller [topo] than the node itself. Recursion is
   handled by condensing each strongly-connected component into one unit
   that is described together. *)

module D = Db
module S = Sqlite3

type node = {
  id : string;
  name : string;
  file : string;
  start_line : int;
  end_line : int;
  end_exact : bool;
  kind : string;
}

type edge = {
  src : string;
  dst : string;
  kind : string;
  call_file : string;
  call_line : int;
  call_col : int;
}

let t s = S.Data.TEXT s
let ib i = S.Data.INT (Int64.of_int i)

(* ---------- insert ---------- *)

let clear db =
  D.exec db "DELETE FROM cg_edges";
  D.exec db "DELETE FROM cg_fts";
  D.exec db "DELETE FROM cg_nodes";
  (* SCC numbers are reassigned by the next compute_scc_topo, so leases
     from the old graph would name units that no longer exist. *)
  D.exec db "DELETE FROM cg_leases"

let insert_node db ~lang (n : node) =
  D.exec_params db
    "INSERT INTO cg_nodes\
     (id,name,file,start_line,end_line,end_exact,kind,lang) \
     VALUES(?,?,?,?,?,?,?,?) \
     ON CONFLICT(id) DO UPDATE SET \
       name=excluded.name, file=excluded.file, \
       start_line=excluded.start_line, end_line=excluded.end_line, \
       end_exact=excluded.end_exact, kind=excluded.kind, lang=excluded.lang"
    [ t n.id; t n.name; t n.file; ib n.start_line; ib n.end_line;
      ib (if n.end_exact then 1 else 0); t n.kind; t lang ];
  (* Seed the FTS row with the name; description filled in later. *)
  D.exec_params db "DELETE FROM cg_fts WHERE node_id=?" [ t n.id ];
  D.exec_params db
    "INSERT INTO cg_fts(node_id,name,description) VALUES(?,?,'')"
    [ t n.id; t n.name ]

let insert_edge db (e : edge) =
  D.exec_params db
    "INSERT OR IGNORE INTO cg_edges\
     (src,dst,kind,call_file,call_line,call_col) VALUES(?,?,?,?,?,?)"
    [ t e.src; t e.dst; t e.kind; t e.call_file; ib e.call_line; ib e.call_col ]

(* Replace the whole graph in one transaction. Edges whose endpoints are
   not real nodes are dropped. *)
let insert_graph db ~lang ~(nodes : node list) ~(edges : edge list) =
  let ids = Hashtbl.create (List.length nodes) in
  List.iter (fun (n : node) -> Hashtbl.replace ids n.id ()) nodes;
  D.with_txn db (fun () ->
    clear db;
    List.iter (insert_node db ~lang) nodes;
    List.iter (fun (e : edge) ->
      if Hashtbl.mem ids e.src && Hashtbl.mem ids e.dst then insert_edge db e)
      edges)

(* ---------- SCC + leaves-first topological order (ocamlgraph) ---------- *)

let all_node_ids db =
  D.query_list db "SELECT id FROM cg_nodes" []
    ~f:(fun c -> D.data_to_string c.(0))

let all_edges db =
  D.query_list db "SELECT src,dst FROM cg_edges" []
    ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string c.(1)))

module V = struct
  type t = string
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = String.equal
end

module Gr = Graph.Imperative.Digraph.Concrete (V)
module Comp = Graph.Components.Make (Gr)

(* Assign each node its SCC number and reuse it as the topo rank.
   ocamlgraph numbers SCCs in reverse topological order: for an edge from
   component [i] to component [j] (i<>j), [i > j]. Our edges are
   caller -> callee, so a callee's number is strictly smaller than its
   caller's — ascending [topo] is leaves-first, exactly what the
   describe-loop wants. Recursion collapses into one SCC, described as a
   unit. *)
let compute_scc_topo db =
  let ids = all_node_ids db in
  let g = Gr.create () in
  List.iter (fun id -> Gr.add_vertex g id) ids;
  List.iter (fun (s, d) ->
    if Gr.mem_vertex g s && Gr.mem_vertex g d then Gr.add_edge g s d)
    (all_edges db);
  let (_ncomp, comp_of) = Comp.scc g in
  D.with_txn db (fun () ->
    List.iter (fun id ->
      let c = comp_of id in
      D.exec_params db
        "UPDATE cg_nodes SET scc=?, topo=? WHERE id=?"
        [ ib c; ib c; t id ])
      ids)

(* ---------- leases ---------- *)

(* How long a claim from [acquire_ready_sccs] parks an SCC before it
   returns to the frontier. Sized for one annotator's batch: long enough
   to read a handful of functions and write their summaries, short enough
   that a crashed worker's units come back within one cycle rather than
   wedging the graph. Leases are advisory throughput control, never a
   correctness barrier — [set_description] never checks one. *)
let lease_ttl_seconds = 300.0

(* ---------- frontier (ready to describe) ---------- *)

(* An SCC is ready when it has at least one undescribed member and every
   cross-SCC callee of every member is already described. Kept as one
   string so the frontier query and its COUNT (see [status]) cannot
   drift apart. Written against the alias `n` over cg_nodes. *)
let ready_where =
  "EXISTS (SELECT 1 FROM cg_nodes u \
           WHERE u.scc=n.scc AND u.description IS NULL) \
   AND NOT EXISTS (SELECT 1 FROM cg_edges e \
         JOIN cg_nodes s ON s.id=e.src \
         JOIN cg_nodes d ON d.id=e.dst \
         WHERE s.scc=n.scc AND d.scc<>n.scc \
           AND d.description IS NULL)"

(* ...and not claimed by another annotator. An expired lease is invisible
   here, so a crashed worker's units free themselves. Takes one param:
   the current time. *)
let unleased_where =
  "NOT EXISTS (SELECT 1 FROM cg_leases l \
               WHERE l.scc=n.scc AND l.lease_until > ?)"

let now () = Unix.gettimeofday ()
let fl f = S.Data.FLOAT f

(* Ready units, leaves-first, excluding anything under a live lease. Pure
   read: it hands the same batch to every concurrent caller, so annotators
   should go through [acquire_ready_sccs] instead. *)
let ready_sccs db ~limit =
  D.query_list db
    (Printf.sprintf
       "SELECT n.scc FROM cg_nodes n WHERE %s AND %s \
        GROUP BY n.scc ORDER BY MIN(n.file), MIN(n.topo) ASC LIMIT ?"
       ready_where unleased_where)
    [ fl (now ()); ib limit ]
    ~f:(fun c -> D.data_to_int c.(0))

(* Take the next [limit] ready units AND claim them for [owner] in one
   transaction, so two concurrent annotators cannot both be handed the
   same SCC. BEGIN IMMEDIATE: the select must hold the write lock, or the
   claims race. Expired rows are reaped on the way past. *)
let acquire_ready_sccs db ~owner ~limit =
  D.with_immediate_txn db (fun () ->
    let t0 = now () in
    D.exec_params db "DELETE FROM cg_leases WHERE lease_until <= ?" [ fl t0 ];
    let sccs = ready_sccs db ~limit in
    let until = t0 +. lease_ttl_seconds in
    List.iter (fun scc ->
      D.exec_params db
        "INSERT INTO cg_leases(scc,owner,lease_until) VALUES(?,?,?) \
         ON CONFLICT(scc) DO UPDATE SET \
           owner=excluded.owner, lease_until=excluded.lease_until"
        [ ib scc; t owner; fl until ])
      sccs;
    sccs)

(* Drop the claims on whatever units these nodes belong to — called once
   their descriptions land, so the units don't sit leased until the TTL
   runs out. Unknown ids simply match no SCC. *)
let release_leases_for_nodes db ~ids =
  match ids with
  | [] -> ()
  | _ ->
    let ph = String.concat "," (List.map (fun _ -> "?") ids) in
    D.exec_params db
      (Printf.sprintf
         "DELETE FROM cg_leases WHERE scc IN \
          (SELECT scc FROM cg_nodes WHERE id IN (%s))" ph)
      (List.map t ids)

let node_of_cols c = {
  id = D.data_to_string c.(0);
  name = D.data_to_string c.(1);
  file = D.data_to_string c.(2);
  start_line = D.data_to_int c.(3);
  end_line = D.data_to_int c.(4);
  end_exact = D.data_to_int c.(5) = 1;
  kind = D.data_to_string c.(6);
}

let scc_members db ~scc =
  D.query_list db
    "SELECT id,name,file,start_line,end_line,end_exact,kind \
     FROM cg_nodes WHERE scc=? ORDER BY topo,start_line"
    [ ib scc ] ~f:node_of_cols

(* ---------- file-level annotation units ---------- *)

(* Condense FILES into file-SCCs (files that mutually call each other),
   leaves-first. A file-SCC is READY when every file-SCC it depends on is
   already fully described, so a whole module can be summarised in ONE
   self-contained pass: read the files once, describe all their functions
   with the cross-file callees' summaries as context. Each unit is fully
   self-describing — annotation needs no memory between units. *)
type file_unit = {
  ufiles   : string list;
  ufns     : node list;                       (* undescribed fns, topo order *)
  ucallees : (string * string option) list;   (* cross-file callee summaries *)
}

let all_files db =
  D.query_list db "SELECT DISTINCT file FROM cg_nodes" []
    ~f:(fun c -> D.data_to_string c.(0))

let cross_file_edges db =
  D.query_list db
    "SELECT DISTINCT s.file, d.file FROM cg_edges e \
       JOIN cg_nodes s ON s.id=e.src JOIN cg_nodes d ON d.id=e.dst \
       WHERE s.file<>d.file \
     UNION \
     SELECT DISTINCT s.file, d.file FROM cg_dispatch g \
       JOIN cg_nodes s ON s.id=g.src JOIN cg_nodes d ON d.id=g.dst \
       WHERE s.file<>d.file"
    [] ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string c.(1)))

let next_ready_file_units db ~limit =
  let files = all_files db in
  let cfe = cross_file_edges db in
  let g = Gr.create () in
  List.iter (fun f -> Gr.add_vertex g f) files;
  List.iter (fun (s, d) ->
    if Gr.mem_vertex g s && Gr.mem_vertex g d then Gr.add_edge g s d) cfe;
  let (_n, comp_of) = Comp.scc g in
  let undesc = Hashtbl.create 512 in
  List.iter (fun (f, n) -> Hashtbl.replace undesc f n)
    (D.query_list db
       "SELECT file, SUM(CASE WHEN description IS NULL THEN 1 ELSE 0 END) \
        FROM cg_nodes GROUP BY file" []
       ~f:(fun c -> (D.data_to_string c.(0), D.data_to_int c.(1))));
  let comp_undesc = Hashtbl.create 256 in
  List.iter (fun f ->
    let c = comp_of f in
    let u = try Hashtbl.find undesc f with Not_found -> 0 in
    Hashtbl.replace comp_undesc c
      ((try Hashtbl.find comp_undesc c with Not_found -> 0) + u)) files;
  let fully c = (try Hashtbl.find comp_undesc c with Not_found -> 0) = 0 in
  let out = Hashtbl.create 256 in
  List.iter (fun (sf, df) ->
    let cs = comp_of sf and cd = comp_of df in
    if cs <> cd then begin
      let s = try Hashtbl.find out cs with Not_found -> [] in
      if not (List.mem cd s) then Hashtbl.replace out cs (cd :: s)
    end) cfe;
  let comps = List.sort_uniq compare (List.map comp_of files) in
  (* ready: not fully described AND every downstream file-SCC fully described.
     ocamlgraph numbers callees below callers, so ascending comp id is
     leaves-first. *)
  let ready =
    List.filter (fun c ->
      (not (fully c))
      && List.for_all fully (try Hashtbl.find out c with Not_found -> []))
      comps
    |> List.sort compare in
  let rec take n = function
    | [] -> [] | _ when n <= 0 -> [] | x :: r -> x :: take (n - 1) r in
  List.map (fun c ->
    let cfiles = List.filter (fun f -> comp_of f = c) files in
    let ph = String.concat "," (List.map (fun _ -> "?") cfiles) in
    let ufns = D.query_list db
      (Printf.sprintf
        "SELECT id,name,file,start_line,end_line,end_exact,kind FROM cg_nodes \
         WHERE description IS NULL AND file IN (%s) ORDER BY file,topo,start_line"
        ph)
      (List.map t cfiles) ~f:node_of_cols in
    let ucallees = D.query_list db
      (Printf.sprintf
        "SELECT DISTINCT d.name, d.description FROM cg_edges e \
           JOIN cg_nodes s ON s.id=e.src JOIN cg_nodes d ON d.id=e.dst \
           WHERE s.file IN (%s) AND d.file NOT IN (%s) \
             AND d.description IS NOT NULL"
        ph ph)
      (List.map t cfiles @ List.map t cfiles)
      ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string_opt c.(1))) in
    { ufiles = cfiles; ufns; ucallees }) (take limit ready)

(* Cross-SCC callees (already described) of any member of this SCC. *)
let scc_callee_descriptions db ~scc =
  D.query_list db
    "SELECT DISTINCT d.name, d.description FROM cg_edges e \
     JOIN cg_nodes s ON s.id=e.src \
     JOIN cg_nodes d ON d.id=e.dst \
     WHERE s.scc=? AND d.scc<>? ORDER BY d.name"
    [ ib scc; ib scc ]
    ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string_opt c.(1)))

(* ---------- write-back ---------- *)

(* Returns whether a node actually took the description. An UPDATE on an
   id that doesn't exist matches 0 rows and raises nothing, so without
   this the caller cannot tell a write from a typo — and an annotator
   posting a malformed id would be told it succeeded while its work went
   nowhere. Ask SQLite what changed rather than assuming. *)
let set_description db ~id ~description ~code_hash =
  D.exec_params db
    "UPDATE cg_nodes SET description=?, code_hash=?, described_at=? WHERE id=?"
    [ t description;
      (match code_hash with Some h -> t h | None -> S.Data.NULL);
      fl (now ());
      t id ];
  let updated = D.changes db > 0 in
  if updated then begin
    (* Keep the FTS row in sync (delete + reinsert; FTS5 has no stable
       external rowid to UPDATE by here). *)
    D.exec_params db "DELETE FROM cg_fts WHERE node_id=?" [ t id ];
    D.exec_params db
      "INSERT INTO cg_fts(node_id,name,description) \
       SELECT id,name,? FROM cg_nodes WHERE id=?"
      [ t description; t id ]
  end;
  updated

(* ---------- status ---------- *)

let count db sql = D.query_fold db sql [] ~init:0 ~f:(fun _ c -> D.data_to_int c.(0))

(* Size of the frontier. COUNT over the same predicate [ready_sccs]
   selects on — this used to materialise every ready SCC into a list just
   to take its length, on a path that graph_status, graph_next_batch and
   graph_set_descriptions all hit.

   Leased units are deliberately EXCLUDED, matching [ready_sccs]: the
   number answers "how much work could another annotator pick up right
   now", which is what a caller uses to decide whether to spawn more
   annotators. Counting claimed units would advertise work that
   graph_next_batch would then refuse to hand out. Overall progress is
   [total]/[described], which leases don't touch. *)
let count_ready_sccs db =
  D.query_fold db
    (Printf.sprintf
       "SELECT COUNT(*) FROM (SELECT n.scc FROM cg_nodes n \
        WHERE %s AND %s GROUP BY n.scc)"
       ready_where unleased_where)
    [ fl (now ()) ] ~init:0 ~f:(fun _ c -> D.data_to_int c.(0))

let status db =
  let total = count db "SELECT COUNT(*) FROM cg_nodes" in
  let described =
    count db "SELECT COUNT(*) FROM cg_nodes WHERE description IS NOT NULL" in
  (total, described, count_ready_sccs db)

(* Is the session/turn memory index populated? Gates the memory-search
   tools out of the advertised surface on a graph-only project, so their
   schemas don't sit unused in every session's prefix. *)
let has_memory db =
  try count db "SELECT COUNT(*) FROM steps" > 0 with _ -> false

(* ---------- queries (ask questions of the graph) ---------- *)

type found = {
  fid : string;
  fname : string;
  ffile : string;
  fstart : int;
  fend : int;
  fkind : string;
  fdesc : string option;
}

let found_of_cols c = {
  fid = D.data_to_string c.(0);
  fname = D.data_to_string c.(1);
  ffile = D.data_to_string c.(2);
  fstart = D.data_to_int c.(3);
  fend = D.data_to_int c.(4);
  fkind = D.data_to_string c.(5);
  fdesc = D.data_to_string_opt c.(6);
}

(* Match by exact id first, else by function name. *)
let lookup db ~query =
  D.query_list db
    "SELECT id,name,file,start_line,end_line,kind,description FROM cg_nodes \
     WHERE id=? OR name=? ORDER BY topo LIMIT 25"
    [ t query; t query ] ~f:found_of_cols

(* Full-text search over the function summaries (name + description),
   ranked by BM25. The "searches in the summaries" half of a NL query;
   callers/callees supply the "db formula" half. *)
let search db ~fts ~limit =
  D.query_list db
    "SELECT n.id,n.name,n.file,n.start_line,n.end_line,n.kind,n.description \
     FROM cg_fts f JOIN cg_nodes n ON n.id = f.node_id \
     WHERE cg_fts MATCH ? ORDER BY bm25(cg_fts) LIMIT ?"
    [ t fts; ib limit ] ~f:found_of_cols

(* Functions whose *descriptions* mention [name] — dynamic-dispatch
   recovery. A Celery .delay, plugin-registry lookup, or signal has no
   static call edge, but the dispatcher's summary names its target
   (annotators read the dispatch site). Phrase-quoted and restricted to
   the description column, so the unicode61 tokenizer matches the
   identifier's token sequence exactly and a hit on the node's own name
   row can't qualify. *)
let mentioned_by db ~name ~exclude_id ~limit =
  let quoted =
    "description:\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\"" in
  D.query_list db
    "SELECT n.id,n.name,n.file,n.start_line,n.end_line,n.kind,n.description \
     FROM cg_fts f JOIN cg_nodes n ON n.id = f.node_id \
     WHERE cg_fts MATCH ? AND n.id <> ? \
     ORDER BY bm25(cg_fts) LIMIT ?"
    [ t quoted; t exclude_id; ib limit ] ~f:found_of_cols

(* Traversal direction: Callees (downstream — "what X reaches"), Callers
   (upstream — "what reaches X / change-impact"), or Both. *)
type direction = Callees | Callers | Both

(* Blast radius, correct by construction. The FULL transitive closure of
   callers (or callees) of a node, dispatch-inclusive — the traversal
   models kept composing wrong by hand (a one-hop COUNT, or a recursion
   that forgot cg_dispatch). Resolves the name to nodes so same-named
   methods across classes are reported SEPARATELY (no accidental merge),
   and returns both the one-hop [direct] count and the transitive count
   plus the closure grouped by file. Direction: Callers = upstream (what
   reaches X = change impact), Callees = downstream. *)
type blast = {
  bnode : found;
  direct : int;
  transitive : int;
  by_file : (string * int) list;
}

let blast_radius db ~name ~file ~(direction : direction) ~include_dispatch ~file_limit =
  let nodes =
    if file = "" then
      D.query_list db
        "SELECT id,name,file,start_line,end_line,kind,description FROM cg_nodes \
         WHERE name=? ORDER BY file,start_line"
        [ t name ] ~f:found_of_cols
    else
      D.query_list db
        "SELECT id,name,file,start_line,end_line,kind,description FROM cg_nodes \
         WHERE name=? AND file GLOB ? ORDER BY file,start_line"
        [ t name; t ("*" ^ file ^ "*") ] ~f:found_of_cols
  in
  let edges =
    if include_dispatch
    then "(SELECT src,dst FROM cg_edges UNION SELECT src,dst FROM cg_dispatch)"
    else "cg_edges" in
  (* Callers: add e.src for every edge whose dst is in the set. Callees: the
     mirror. Both edge tables are caller->callee, dispatcher->target. *)
  let nextc, onc = match direction with
    | Callees -> "e.dst", "e.src"
    | Callers | Both -> "e.src", "e.dst" in
  let count_p sql params =
    D.query_fold db sql params ~init:0 ~f:(fun _ c -> D.data_to_int c.(0)) in
  let cte = Printf.sprintf
    "WITH RECURSIVE reach(id) AS (SELECT ? \
     UNION SELECT %s FROM reach JOIN %s e ON %s=reach.id)" nextc edges onc in
  List.map (fun (m : found) ->
    let direct =
      count_p (Printf.sprintf
        "SELECT COUNT(DISTINCT %s) FROM %s e WHERE %s=?" nextc edges onc)
        [ t m.fid ] in
    let transitive =
      count_p (cte ^ " SELECT COUNT(*) FROM reach WHERE id<>?") [ t m.fid; t m.fid ] in
    let by_file =
      D.query_list db
        (cte ^ " SELECT n.file, COUNT(*) AS c FROM reach \
          JOIN cg_nodes n ON n.id=reach.id WHERE reach.id<>? \
          GROUP BY n.file ORDER BY c DESC LIMIT ?")
        [ t m.fid; t m.fid; ib file_limit ]
        ~f:(fun c -> (D.data_to_string c.(0), D.data_to_int c.(1))) in
    { bnode = m; direct; transitive; by_file }) nodes

(* Neighbour lists are name+file only: full summaries are read on demand
   via lookup/graph_describe, instead of re-sent with every hit. *)
(* Every node under any of several files/directories, ONE call — the
   coarse primitive for whole-subsystem comprehension. Each path is
   repo-relative: an exact file, or a directory whose contents match via
   GLOB (case-sensitive, treats '_' literally, unlike LIKE — important
   since paths contain underscores). [exclude_tests] drops test files so a
   directory pull isn't polluted (files under a tests/ dir or named
   test_*.py). Leaves-first within each file so callees precede callers. *)
let nodes_in_paths db ~paths ~exclude_tests ~limit =
  match paths with
  | [] -> []
  | _ ->
    let clause = "(file = ? OR file GLOB ?)" in
    let where = "(" ^ String.concat " OR " (List.map (fun _ -> clause) paths) ^ ")" in
    let params = List.concat_map (fun p -> [ t p; t (p ^ "/*") ]) paths in
    let excl =
      if exclude_tests then
        " AND NOT (file GLOB '*/tests/*' OR file GLOB '*/test_*.py' \
         OR file GLOB 'test_*.py' OR file GLOB '*/conftest.py')"
      else "" in
    D.query_list db
      (Printf.sprintf
         "SELECT id,name,file,start_line,end_line,kind,description \
          FROM cg_nodes WHERE %s%s ORDER BY file, topo, start_line LIMIT ?"
         where excl)
      (params @ [ ib limit ]) ~f:found_of_cols

(* Single-path convenience (kept for callers that pass one path). *)
let nodes_in_path db ~path ~limit =
  nodes_in_paths db ~paths:[path] ~exclude_tests:false ~limit

(* Exactly the named functions, one call — the surgical complement to
   nodes_in_path: when you know which functions you need, pull just those
   instead of a whole module. Name collisions (same name in several files)
   all come back; the caller disambiguates by file. *)
let nodes_by_names db ~names ~limit =
  match names with
  | [] -> []
  | _ ->
    let placeholders = String.concat "," (List.map (fun _ -> "?") names) in
    D.query_list db
      (Printf.sprintf
         "SELECT id,name,file,start_line,end_line,kind,description \
          FROM cg_nodes WHERE name IN (%s) ORDER BY file, topo, start_line \
          LIMIT ?" placeholders)
      (List.map t names @ [ ib limit ]) ~f:found_of_cols

(* Reachable subgraph from seed functions — the query that turns a
   question into exactly the nodes it needs, instead of dumping a whole
   package. Seeds are matched by name; the recursive walk follows call
   edges up to [depth] hops. Direction: Callees (downstream — "how does X
   work"), Callers (upstream — "what reaches X / change-impact"), or Both.
   Returns the closure's nodes (deduped, topo-ordered) with descriptions.
   Bounded by [depth] and [limit] so it stays small and one-call. *)

(* [follow_dispatch] also walks cg_dispatch (materialised dynamic-dispatch
   edges — see populate_dispatch_edges) so a single traversal recovers
   async/plugin/signal paths that have no static call edge. *)
let neighborhood db ~roots ~direction ~depth ~follow_dispatch ~limit ~seed_file =
  match roots with
  | [] -> []
  | _ ->
    let seeds = String.concat "," (List.map (fun _ -> "?") roots) in
    let tables = if follow_dispatch then ["cg_edges"; "cg_dispatch"] else ["cg_edges"] in
    let oriented tbl = match direction with
      | Callees -> Printf.sprintf "SELECT src AS a, dst AS b FROM %s" tbl
      | Callers -> Printf.sprintf "SELECT dst AS a, src AS b FROM %s" tbl
      | Both ->
        Printf.sprintf
          "SELECT src AS a, dst AS b FROM %s \
           UNION SELECT dst AS a, src AS b FROM %s" tbl tbl in
    let und = String.concat " UNION " (List.map oriented tables) in
    (* Optional file glob scopes the SEED set only — lets a trace pin an
       ambiguous method name (create_certificate exists in every issuer
       plugin) to one file, so the one-shot fires cleanly. *)
    let seed_filter = if seed_file = "" then "" else " AND file GLOB ?" in
    let sql = Printf.sprintf
      "WITH RECURSIVE und(a,b) AS (%s), \
       reach(id,depth) AS ( \
         SELECT id, 0 FROM cg_nodes WHERE name IN (%s)%s \
         UNION \
         SELECT und.b, reach.depth + 1 FROM reach JOIN und ON und.a = reach.id \
           WHERE reach.depth < ?) \
       SELECT n.id,n.name,n.file,n.start_line,n.end_line,n.kind,n.description \
       FROM reach JOIN cg_nodes n ON n.id = reach.id \
       GROUP BY n.id ORDER BY n.topo, n.file, n.start_line LIMIT ?"
      und seeds seed_filter in
    let seed_params =
      List.map t roots @ (if seed_file = "" then [] else [ t seed_file ]) in
    D.query_list db sql
      (seed_params @ [ ib depth; ib limit ]) ~f:found_of_cols

(* Materialise dynamic-dispatch edges into cg_dispatch from the FTS
   index. For every node with NO static caller (the dispatch-only
   functions — Celery tasks, plugin hooks, signal handlers) and a real
   (non-synthetic) name, record the functions whose descriptions name it
   as its dispatchers. Bounded per target so a generic name can't
   explode. Idempotent: clears and repopulates. *)
(* Call paths from any `from` function down to any `to` function, via
   callee edges (+ dispatch when following). Answers "how does X reach Y".
   Carries the id-path in a US-delimited string (0x1f — never in an id)
   with a cycle guard; returns the shortest few as ordered id lists. *)
let trace_paths db ~froms ~tos ~follow_dispatch ~max_depth ~max_paths =
  match froms, tos with
  | [], _ | _, [] -> []
  | _ ->
    let sep = "char(31)" in
    let edges =
      if follow_dispatch then
        "SELECT src AS a, dst AS b FROM cg_edges \
         UNION SELECT src, dst FROM cg_dispatch"
      else "SELECT src AS a, dst AS b FROM cg_edges" in
    let ph names = String.concat "," (List.map (fun _ -> "?") names) in
    let sql = Printf.sprintf
      "WITH RECURSIVE e(a,b) AS (%s), \
       walk(id, depth, path) AS ( \
         SELECT id, 0, %s||id||%s FROM cg_nodes WHERE name IN (%s) \
         UNION ALL \
         SELECT e.b, w.depth+1, w.path||e.b||%s \
           FROM walk w JOIN e ON e.a = w.id \
           WHERE w.depth < ? AND instr(w.path, %s||e.b||%s) = 0) \
       SELECT w.path FROM walk w JOIN cg_nodes n ON n.id = w.id \
       WHERE n.name IN (%s) ORDER BY w.depth LIMIT ?"
      edges sep sep (ph froms) sep sep sep (ph tos) in
    let paths =
      D.query_list db sql
        (List.map t froms @ [ ib max_depth ] @ List.map t tos @ [ ib max_paths ])
        ~f:(fun c -> D.data_to_string c.(0)) in
    List.map (fun p ->
      String.split_on_char '\031' p |> List.filter (fun s -> s <> ""))
      paths

let dispatch_edge_count db =
  count db "SELECT COUNT(*) FROM cg_dispatch"

let contains_sub hay needle =
  let hl = String.length hay and nl = String.length needle in
  if nl = 0 then true
  else
    let rec go i =
      if i + nl > hl then false
      else if String.sub hay i nl = needle then true
      else go (i + 1) in
    go 0

(* A dispatch edge requires more than a mention: the dispatcher's summary
   must name the target next to an actual dispatch token (Celery
   .delay/.apply_async/.s/.si/.apply). This keeps precision high — plain
   references ("similar to fetch_acme_cert") don't become edges. *)
let dispatch_cues tname =
  [ tname ^ ".delay"; tname ^ ".apply_async"; tname ^ ".apply(";
    tname ^ ".s("; tname ^ ".si("; tname ^ ".signature(" ]

let populate_dispatch_edges ?(per_target = 20) db =
  D.exec db "DELETE FROM cg_dispatch";
  let targets =
    D.query_list db
      "SELECT id, name FROM cg_nodes n \
       WHERE NOT EXISTS (SELECT 1 FROM cg_edges e WHERE e.dst = n.id) \
         AND name <> '' AND substr(name,1,1) <> '<' \
         AND substr(name,1,5) <> '_tmp_'"
      [] ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string c.(1))) in
  D.with_txn db (fun () ->
    List.iter (fun (tid, tname) ->
      let quoted =
        "description:\"" ^
        String.concat "\"\"" (String.split_on_char '"' tname) ^ "\"" in
      let candidates =
        try
          D.query_list db
            "SELECT n.id, n.description FROM cg_fts f \
             JOIN cg_nodes n ON n.id = f.node_id \
             WHERE cg_fts MATCH ? AND n.id <> ? ORDER BY bm25(cg_fts) LIMIT ?"
            [ t quoted; t tid; ib per_target ]
            ~f:(fun c -> (D.data_to_string c.(0),
                          match D.data_to_string_opt c.(1) with
                          | Some s -> s | None -> ""))
        with _ -> [] in
      let cues = dispatch_cues tname in
      List.iter (fun (sid, sdesc) ->
        if List.exists (contains_sub sdesc) cues then
          D.exec_params db
            "INSERT OR IGNORE INTO cg_dispatch(src,dst) VALUES(?,?)"
            [ t sid; t tid ])
        candidates)
      targets)

let callees db ~id =
  D.query_list db
    "SELECT DISTINCT d.name, d.file FROM cg_edges e \
     JOIN cg_nodes d ON d.id=e.dst WHERE e.src=? ORDER BY d.name"
    [ t id ]
    ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string c.(1)))

let callers db ~id =
  D.query_list db
    "SELECT DISTINCT s.name, s.file FROM cg_edges e \
     JOIN cg_nodes s ON s.id=e.src WHERE e.dst=? ORDER BY s.name"
    [ t id ]
    ~f:(fun c -> (D.data_to_string c.(0), D.data_to_string c.(1)))
