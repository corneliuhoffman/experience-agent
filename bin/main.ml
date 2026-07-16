open Cmdliner

(* Common options *)
let project_dir =
  Arg.(value & opt string "." & info ["project-dir"; "C"] ~docv:"DIR"
         ~doc:"Project directory (must be a git repo)")

(* --- Subcommand: ask --- *)

let ask_cmd =
  let prompt =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROMPT"
           ~doc:"Prompt to send to Claude") in
  let model =
    Arg.(value & opt (some string) None & info ["model"; "m"] ~docv:"MODEL"
           ~doc:"Model to use") in
  let run prompt model project_dir =
    let config = Urme_core.Config.load () in
    let opts = { Urme_claude.Process.default_opts with model } in
    Lwt_main.run begin
      let open Lwt.Syntax in
      let* proc = Urme_claude.Process.spawn_oneshot ~cwd:project_dir
          ~opts ~binary:config.claude_binary ~prompt () in
      let* () = Urme_claude.Process.iter_events proc ~f:(fun event ->
        (match event with
         | Urme_claude.Stream.Assistant_message { content; _ } ->
           let text = Urme_claude.Stream.text_of_content content in
           if text <> "" then print_string text
         | Urme_claude.Stream.Result { result; is_error; _ } ->
           if is_error then Printf.eprintf "Error: %s\n%!" result
         | _ -> ());
        Lwt.return_unit
      ) in
      let _ = Urme_claude.Process.wait proc in
      print_newline ();
      Lwt.return_unit
    end
  in
  Cmd.v (Cmd.info "ask" ~doc:"Send a one-shot prompt to Claude")
    Term.(const run $ prompt $ model $ project_dir)

(* --- Subcommand: init --- *)

let init_cmd =
  let skip_summaries =
    Arg.(value & flag & info ["skip-summaries"]
           ~doc:"Index steps but skip the Claude summarisation pass") in
  let parallel =
    Arg.(value & opt int 3 & info ["parallel"; "j"] ~docv:"N"
           ~doc:"Number of parallel Claude daemons for the summarisation pass \
                 (default 3; higher = faster but more RAM)") in
  let run skip_summaries parallel project_dir =
    let config = Urme_core.Config.load () in
    (* Normalise project_dir to an absolute path. Edit_extract uses
       the project_dir to strip the prefix from Claude's tool_use
       file_paths; those are absolute, so a relative project_dir
       (like the cmdliner default ".") leaves the stored file_path
       absolute while commits use relative paths — breaking path
       equality in [assign] and losing every Claude attribution. *)
    let project_dir =
      if project_dir = "." then Sys.getcwd ()
      else if Filename.is_relative project_dir
      then Filename.concat (Sys.getcwd ()) project_dir
      else project_dir in
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let n = Urme_engine.Indexer.index_all_sessions ~db ~project_dir in
    Printf.printf "indexed %d turns\n" n;
    (* Single Lwt_main.run: chaining summarise → run_once inside one
       Lwt loop avoids the state drift that a separate second
       Lwt_main.run seems to cause (the walker saved zero links when
       two Lwt_main.run calls ran back to back). *)
    (try
       Lwt_main.run begin
         let open Lwt.Syntax in
         let* () =
           if skip_summaries then Lwt.return_unit
           else begin
             Printf.printf "running Claude summarisation pass (%d daemons)...\n%!" parallel;
             Urme_engine.Summarise.summarise_pending
               ~pool_size:parallel
               ~binary:config.claude_binary ~db ()
           end
         in
         Printf.printf "building per-edit git links...\n%!";
         Urme_engine.Git_index.run_once ~project_dir ~db
       end
     with e -> Printf.eprintf "link error (non-fatal): %s\n%!"
                 (Printexc.to_string e));
    Urme_store.Schema.close db
  in
  Cmd.v (Cmd.info "init" ~doc:"Index Claude sessions into urme V2 SQLite store")
    Term.(const run $ skip_summaries $ parallel $ project_dir)

(* --- Subcommand: export --- *)

(* Resolve the commits reachable from [branch] but not from its
   merge-base with main/master. Same set `gh` uses for a PR's diff.
   Falls back to the whole branch history if the diff range is empty
   (e.g. when [branch] IS main, or has been fully merged). *)
let commits_for_branch ~cwd ~branch =
  let open Lwt.Syntax in
  let resolve_ref r =
    Lwt.catch
      (fun () ->
        let+ out = Urme_git.Ops.run_git ~cwd ["rev-parse"; "--verify"; r] in
        Some (String.trim out))
      (fun _ -> Lwt.return None) in
  let* base_ref =
    let* m = resolve_ref "main" in
    match m with
    | Some _ -> Lwt.return (Some "main")
    | None -> let* ms = resolve_ref "master" in
      (match ms with Some _ -> Lwt.return (Some "master") | None -> Lwt.return None) in
  let read_log args =
    let* out = Urme_git.Ops.run_git ~cwd args in
    Lwt.return (String.split_on_char '\n' out
                |> List.map String.trim
                |> List.filter (fun s -> s <> "")) in
  let* commits = match base_ref with
    | Some base ->
      read_log ["log"; "--format=%H"; Printf.sprintf "%s..%s" base branch]
    | None -> read_log ["log"; "--format=%H"; branch] in
  if commits <> [] then Lwt.return commits
  else read_log ["log"; "--format=%H"; branch]

let export_cmd =
  let branch =
    Arg.(value & opt (some string) None
           & info ["branch"; "b"] ~docv:"BRANCH"
               ~doc:"Export only the rows touching commits on this \
                     branch (resolves commits between the branch and \
                     main/master). When omitted, exports the whole DB.") in
  let out_path =
    Arg.(value & opt (some string) None
           & info ["out"; "o"] ~docv:"PATH"
               ~doc:"Output file (default: <branch>.urmedb or \
                     urme-snapshot.urmedb).") in
  let run branch out_path project_dir =
    match branch with
    | None ->
      let path = Option.value out_path ~default:"urme-snapshot.urmedb" in
      Urme_store.Export.export_project ~project_dir ~path;
      Printf.printf "exported (whole DB) → %s\n" path
    | Some br ->
      let path = Option.value out_path
        ~default:(Printf.sprintf "%s.urmedb"
                    (String.map (fun c -> if c = '/' then '-' else c) br)) in
      let commits =
        Lwt_main.run (commits_for_branch ~cwd:project_dir ~branch:br) in
      if commits = [] then begin
        Printf.eprintf
          "export: no commits found for branch %S (is it checked out?)\n" br;
        exit 1
      end;
      Printf.printf "export: %d commits on branch %s\n"
        (List.length commits) br;
      Urme_store.Export.export_scoped ~project_dir ~commits ~out_path:path;
      Printf.printf "exported (branch %s) → %s\n" br path
  in
  Cmd.v (Cmd.info "export"
           ~doc:"Write a snapshot of the urme store. With [--branch], \
                 writes only the rows for that branch's commits \
                 (intended for PR reviews).")
    Term.(const run $ branch $ out_path $ project_dir)

(* --- Subcommand: import --- *)

let import_cmd =
  let path =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH"
           ~doc:"Path to the .urmedb snapshot to load.") in
  let run path project_dir =
    if not (Sys.file_exists path) then begin
      Printf.eprintf "import: %s not found\n" path;
      exit 1
    end;
    (* Don't touch the reviewer's own [.urme/db.sqlite]. Instead,
       point this invocation at the snapshot via [URME_DB_PATH] and
       boot the TUI — the reviewer gets a normal URME where every
       view (Git / History / Search) is scoped to the imported data.
       When they quit, nothing is persisted to their own DB. *)
    let abs =
      if Filename.is_relative path then
        Filename.concat (Sys.getcwd ()) path
      else path in
    Unix.putenv "URME_DB_PATH" abs;
    Printf.printf "urme: loading %s (read-only review session)\n" abs;
    let _ = Urme_core.Config.load () in
    Lwt_main.run (Urme_tui.Reactive.run ~project_dir ())
  in
  Cmd.v (Cmd.info "import"
           ~doc:"Load a .urmedb snapshot and launch URME scoped to it. \
                 Leaves the project's own store untouched.")
    Term.(const run $ path $ project_dir)

(* --- Subcommand: graph-build --- *)

let graph_build_cmd =
  let json =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CALLGRAPH_JSON"
           ~doc:"Path to a call-graph JSON export (see the README section \
                 'Call-graph JSON format').") in
  let run json project_dir =
    let project_dir =
      if project_dir = "." then Sys.getcwd ()
      else if Filename.is_relative project_dir
      then Filename.concat (Sys.getcwd ()) project_dir
      else project_dir in
    if not (Sys.file_exists json) then begin
      Printf.eprintf "graph-build: %s not found\n" json;
      exit 1
    end;
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let (nn, ne) = Urme_engine.Callgraph_load.build ~db ~json_path:json in
    let (total, described, ready) = Urme_store.Callgraph_store.status db in
    Urme_store.Schema.close db;
    Printf.printf
      "loaded call graph: %d nodes, %d edges (described %d/%d, \
       %d SCC-units ready)\n" nn ne described total ready
  in
  Cmd.v (Cmd.info "graph-build"
           ~doc:"Load a call-graph JSON export into the urme store \
                 (nodes + edges + SCC/topo). The MCP describe-loop then \
                 fills in per-function descriptions.")
    Term.(const run $ json $ project_dir)

(* --- Subcommand: graph-init ---
   One-step bootstrap: run the opengrep-interfile-graph extractor, load
   the export, report status. Refuses to clobber an annotated graph
   unless --force (graph-build wipes descriptions). *)

let graph_init_cmd =
  let lang =
    Arg.(value & opt (some string) None & info ["lang"; "l"] ~docv:"LANG"
           ~doc:"Language to extract (default: auto-detect by file count).") in
  let jobs =
    Arg.(value & opt int 16 & info ["j"; "jobs"] ~docv:"N"
           ~doc:"Extractor parallelism (default 16).") in
  let extractor =
    Arg.(value & opt (some string) None & info ["extractor"]
           ~docv:"BIN"
           ~doc:"Path to a call-graph extractor binary (advanced; by \
                 default any supported extractor found on this machine \
                 is used).") in
  let force =
    Arg.(value & flag & info ["force"]
           ~doc:"Rebuild even if the existing graph has annotations \
                 (they are WIPED by a rebuild).") in
  let run lang jobs extractor force project_dir =
    let project_dir =
      if project_dir = "." then Sys.getcwd ()
      else if Filename.is_relative project_dir
      then Filename.concat (Sys.getcwd ()) project_dir else project_dir in
    (* Extractor candidates, tried in order until one succeeds:
       --extractor flag; `opengrep show dump-interfile-graph --json`
       (recent opengrep ships the exporter; older ones fail fast and we
       fall through); the standalone opengrep-interfile-graph dev binary,
       bundled next to urme or on PATH. *)
    let quiet_ok c = Sys.command (c ^ " >/dev/null 2>&1") = 0 in
    let candidates = match extractor with
      | Some p -> [ `Standalone p ]
      | None ->
        let opengrep =
          if quiet_ok "opengrep show supported-languages" then [ `Opengrep ]
          else [] in
        let self =
          try Unix.realpath Sys.executable_name
          with _ -> Sys.executable_name in
        let dir = Filename.dirname self in
        let parent = Filename.dirname dir in
        let bundled =
          [ Filename.concat dir "opengrep-interfile-graph";
            Filename.concat parent "libexec/urme/opengrep-interfile-graph";
            Filename.concat parent "libexec/opengrep-interfile-graph" ]
          |> List.filter Sys.file_exists in
        opengrep
        @ List.map (fun p -> `Standalone p) bundled
        @ [ `Standalone "opengrep-interfile-graph" ] in
    (* language auto-detection: dominant source extension *)
    let detect () =
      let exts = [ ".py", "python"; ".kt", "kotlin"; ".kts", "kotlin";
                   ".rb", "ruby"; ".go", "go"; ".java", "java";
                   ".ts", "typescript"; ".js", "javascript" ] in
      let skip = [ ".git"; "node_modules"; "_build"; "vendor"; "venv";
                   ".venv"; "dist"; "build" ] in
      let counts = Hashtbl.create 8 in
      let rec walk dir depth =
        if depth <= 8 then
          match Sys.readdir dir with
          | entries ->
            Array.iter (fun e ->
              if e <> "" && e.[0] <> '.' && not (List.mem e skip) then begin
                let p = Filename.concat dir e in
                if (try Sys.is_directory p with _ -> false) then
                  walk p (depth + 1)
                else
                  match List.assoc_opt (Filename.extension e) exts with
                  | Some l ->
                    Hashtbl.replace counts l
                      (1 + (try Hashtbl.find counts l with Not_found -> 0))
                  | None -> ()
              end) entries
          | exception _ -> () in
      walk project_dir 0;
      Hashtbl.fold (fun l n best -> match best with
        | Some (_, bn) when bn >= n -> best
        | _ -> Some (l, n)) counts None
      |> Option.map fst in
    let lang = match lang with
      | Some l -> l
      | None ->
        (match detect () with
         | Some l -> Printf.printf "detected language: %s\n%!" l; l
         | None ->
           prerr_endline "graph-init: could not detect a language; pass --lang";
           exit 1) in
    (* clobber guard *)
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let (_, described, _) =
      try Urme_store.Callgraph_store.status db with _ -> (0, 0, 0) in
    if described > 0 && not force then begin
      Printf.eprintf
        "graph-init: existing graph has %d annotated functions; a rebuild \
         WIPES them. Re-run with --force, or just run `urme annotate` to \
         fill in what's missing.\n" described;
      Urme_store.Schema.close db;
      exit 1
    end;
    Urme_store.Schema.close db;
    let urme_dir = Filename.concat project_dir ".urme" in
    (try Unix.mkdir urme_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (* An existing export in .urme/ wins: users without an extractor drop
       a callgraph-<lang>.json there (opengrep-callgraph/v1) and graph-init
       just loads it. Extraction only runs when there is nothing to load. *)
    let existing =
      (try Sys.readdir urme_dir with _ -> [||])
      |> Array.to_list
      |> List.filter (fun f ->
        String.length f > 15
        && String.sub f 0 10 = "callgraph-"
        && Filename.check_suffix f ".json")
      |> List.map (Filename.concat urme_dir)
      |> List.sort (fun a b ->
        compare (Unix.stat b).Unix.st_mtime (Unix.stat a).Unix.st_mtime) in
    let out = match existing with
      | j :: _ ->
        Printf.printf "using existing export %s\n%!" j;
        j
      | [] ->
        let out = Filename.concat urme_dir ("callgraph-" ^ lang ^ ".json") in
        let cmd_of = function
          | `Opengrep ->
            Printf.sprintf
              "opengrep show dump-interfile-graph %s %s --json > %s"
              (Filename.quote lang) (Filename.quote project_dir)
              (Filename.quote out),
            "opengrep show dump-interfile-graph --json"
          | `Standalone bin ->
            Printf.sprintf "%s export -l %s -r %s -o %s -j %d"
              (Filename.quote bin) (Filename.quote lang)
              (Filename.quote project_dir) (Filename.quote out) jobs,
            bin in
        let produced () =
          Sys.file_exists out
          && (try (Unix.stat out).Unix.st_size > 2 with _ -> false) in
        Printf.printf "extracting %s call graph...\n%!" lang;
        let ok =
          List.exists (fun cand ->
            let cmd, _ = cmd_of cand in
            Sys.command (cmd ^ " 2>/dev/null") = 0 && produced ())
            candidates in
        if not ok then begin
          Printf.eprintf
            "graph-init: no call-graph export found and no supported \
             extractor is available on this machine. Place a call-graph \
             JSON at .urme/callgraph-<lang>.json (see the README section \
             'Call-graph JSON format') and rerun.\n";
          exit 1
        end;
        out in
    let db = Urme_store.Schema.open_or_create ~project_dir in
    let (nn, ne) = Urme_engine.Callgraph_load.build ~db ~json_path:out in
    let (total, described, ready) = Urme_store.Callgraph_store.status db in
    Urme_store.Schema.close db;
    Printf.printf
      "loaded call graph: %d nodes, %d edges (described %d/%d, %d units \
       ready)\nnext: run `urme annotate` to write the summaries.\n"
      nn ne described total ready
  in
  Cmd.v (Cmd.info "graph-init"
           ~doc:"Set up the call graph: load .urme/callgraph-<lang>.json \
                 when present (see the README section 'Call-graph JSON \
                 format'), otherwise extract one if a supported extractor \
                 is installed. Follow with `urme annotate`.")
    Term.(const run $ lang $ jobs $ extractor $ force $ project_dir)

(* --- Subcommand: annotate ---
   urme drives the whole annotation loop itself: it pulls ready file-units
   leaves-first, prompts the model (headless, one file per prompt, N in
   parallel) purely to WRITE the summaries, parses the JSON back, and writes
   descriptions — until the graph is fully annotated. The model never sees a
   batch, a limit, or a tool, so it cannot flail; it only produces prose. *)

let annotate_system =
  "You annotate functions in a code call graph. For each function you are \
   given its `id` and full source, plus one-line summaries of the functions \
   it calls in OTHER files. For EVERY function write a description \
   (1-3 sentences; up to 5 when a caller genuinely needs more), covering: \
   (1) what it does, with the signature copied VERBATIM from the def line \
   — exact parameter names, ORDER, and defaults, never paraphrased or \
   reordered — and what it returns, including None/empty cases; \
   (2) everything non-obvious a caller must know: side effects, security \
   relevance, error/empty/edge behaviour; \
   (3) if it runs by REGISTRATION rather than a direct call (a decorator \
   like @task / @event.listens_for / a route, a plugin registry, a signal \
   or lifecycle hook), state exactly what triggers it; \
   (4) BUGS/GOTCHAS: only ones you can point to in the shown source — name \
   the construct (e.g. 'matches days == interval exactly, so a missed \
   daily run skips the cert'). NEVER claim a bug you merely suspect: a \
   wrong gotcha poisons every caller's summary above it; if unsure, leave \
   it out. \
   Fold in load-bearing callee gotchas: if behaviour depends on a callee \
   that silently fails, swallows errors, matches by strict equality, or \
   skips on a missed run, restate it so the summary stands alone. \
   Output ONLY a JSON array [{\"id\":\"<id>\",\"description\":\"<text>\"}], \
   one object per function, no prose and no markdown fences."

let cg_abs db file =
  if file <> "" && Filename.is_relative file then
    match Urme_store.Schema.get_meta db "cg_root" with
    | Some root when root <> "" -> Filename.concat root file
    | _ -> file
  else file

let build_annot_prompt db (u : Urme_store.Callgraph_store.file_unit) =
  let b = Buffer.create 4096 in
  (if u.ucallees <> [] then begin
     Buffer.add_string b
       "CALLEES (functions these call in OTHER files, already summarized):\n";
     List.iter (fun (n, d) ->
       Buffer.add_string b
         (Printf.sprintf "- %s: %s\n" n (match d with Some s -> s | None -> "")))
       u.ucallees;
     Buffer.add_char b '\n'
   end);
  Buffer.add_string b
    (Printf.sprintf "FUNCTIONS TO DESCRIBE (file: %s):\n"
       (String.concat ", " u.ufiles));
  List.iter (fun (m : Urme_store.Callgraph_store.node) ->
    let code = Urme_engine.Callgraph_load.extract_code
        ~file:(cg_abs db m.file) ~start_line:m.start_line ~end_line:m.end_line in
    Buffer.add_string b
      (Printf.sprintf "\n--- id=%s  name=%s  (%s:%d)\n%s\n"
         m.id m.name m.file m.start_line code)) u.ufns;
  Buffer.add_string b "\nReturn ONLY the JSON array of {id, description}.";
  Buffer.contents b

let parse_annot raw =
  match String.index_opt raw '[', String.rindex_opt raw ']' with
  | Some i, Some j when j > i ->
    (try
       match Yojson.Safe.from_string (String.sub raw i (j - i + 1)) with
       | `List items ->
         List.filter_map (fun it -> match it with
           | `Assoc a ->
             (match List.assoc_opt "id" a, List.assoc_opt "description" a with
              | Some (`String id), Some (`String d) when String.trim d <> "" ->
                Some (id, d)
              | _ -> None)
           | _ -> None) items
       | _ -> []
     with _ -> [])
  | _ -> []

let annotate_cmd =
  let model =
    Arg.(value & opt string "claude-haiku-4-5" & info ["model"; "m"]
           ~docv:"MODEL" ~doc:"Model to annotate with (default Haiku).") in
  let parallel =
    Arg.(value & opt int 6 & info ["parallel"; "j"] ~docv:"N"
           ~doc:"Parallel model calls / files in flight (default 6).") in
  let progress =
    Arg.(value & opt int 20 & info ["progress"] ~docv:"N"
           ~doc:"Print a progress line every ~N functions (default 20).") in
  let run model parallel progress project_dir =
    let progress = max 1 progress in
    let module Cg = Urme_store.Callgraph_store in
    let project_dir =
      if project_dir = "." then Sys.getcwd ()
      else if Filename.is_relative project_dir
      then Filename.concat (Sys.getcwd ()) project_dir else project_dir in
    let config = Urme_core.Config.load () in
    let db = Urme_store.Schema.open_or_create ~project_dir in
    Lwt_main.run begin
      let open Lwt.Syntax in
      (* One fresh claude process per file-unit (no daemon pool): a
         daemon accumulates conversation history, so ask N replays all
         N-1 earlier units — quadratic cost and eventual context blowup
         on big graphs. Fresh one-shots keep every call history-free;
         the constant system prompt still hits the server-side prompt
         cache across processes. *)
      let last = ref (-1) in
      let empties = ref 0 in
      let t0 = Unix.gettimeofday () in
      let d0 = ref (-1) in
      let fmt_dur s =
        let s = int_of_float s in
        if s >= 3600 then Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)
        else if s >= 60 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60)
        else Printf.sprintf "%ds" s in
      let print_progress described total =
        let el = Unix.gettimeofday () -. t0 in
        let fresh = described - (if !d0 < 0 then described else !d0) in
        let rate = if el > 1. && fresh > 0 then float fresh /. el *. 60. else 0. in
        let eta =
          if rate > 0. then fmt_dur (float (total - described) /. rate *. 60.)
          else "?" in
        let tm = Unix.localtime (Unix.gettimeofday ()) in
        Printf.printf "%02d:%02d:%02d  annotated %d/%d  [%s, %.0f fn/min, eta %s]\n%!"
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
          described total (fmt_dur el) rate eta in
      (* Continuous work queue: keep [parallel] units in flight and refill
         a slot the moment its worker finishes — no per-batch barrier, so
         one slow 1500-line unit no longer parks the other slots idle. *)
      let inflight : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      let active = ref 0 in
      let cond = Lwt_condition.create () in
      let unit_key (u : Cg.file_unit) =
        match u.Cg.ufns with
        | n :: _ -> n.Cg.id
        | [] -> String.concat "|" u.Cg.ufiles in
      let run_unit (u : Cg.file_unit) =
        let prompt = build_annot_prompt db u in
        (* Hard per-ask timeout: a wedged model call must never hold its
           slot (and its unit's in-flight key) forever — that starves the
           whole pump. On timeout the unit simply stays undescribed and is
           refetched. *)
        let* raw =
          Lwt.catch (fun () ->
              Lwt.pick [
                Urme_claude.Prompts.ask ~model
                  ~system_prompt:annotate_system ~no_tools:true
                  ~binary:config.claude_binary ~prompt ();
                (let* () = Lwt_unix.sleep 300.0 in Lwt.return "");
              ])
            (fun _ -> Lwt.return "") in
        (try
           List.iter (fun (id, desc) ->
             ignore (Cg.set_description db ~id ~description:desc ~code_hash:None))
             (parse_annot raw)
         with _ -> ());
        (* Back off after a failed call: the freed slot would otherwise be
           refilled with this same unit immediately, turning an API
           rate-limit blip into a tight retry loop that hammers it harder. *)
        if raw = "" then Lwt_unix.sleep 10.0 else Lwt.return_unit in
      let rec pump () =
        let (total, described, _) = try Cg.status db with _ -> (0, 0, 0) in
        if !d0 < 0 then d0 := described;
        if !last < 0 || described - !last >= progress || described >= total then
          (print_progress described total; last := described);
        if total > 0 && described >= total && !active = 0 then Lwt.return_unit
        else begin
          let want = max 0 (max 1 parallel - !active) in
          let fetched =
            if want = 0 then []
            else
              (try Cg.next_ready_file_units ~per_unit:true db
                     ~limit:(max 1 parallel * 2)
               with _ -> [])
              |> List.filter (fun u -> not (Hashtbl.mem inflight (unit_key u))) in
          let dispatch = List.filteri (fun i _ -> i < want) fetched in
          List.iter (fun u ->
            Hashtbl.replace inflight (unit_key u) ();
            incr active;
            Lwt.async (fun () ->
              Lwt.finalize (fun () -> run_unit u)
                (fun () ->
                   Hashtbl.remove inflight (unit_key u);
                   decr active;
                   Lwt_condition.signal cond ();
                   Lwt.return_unit)))
            dispatch;
          if dispatch = [] && !active = 0 then begin
            (* Could be transient (a lock made the query return nothing) or a
               real dead-end. Retry a few times before giving up, so a
               momentary hiccup doesn't abort the whole run. *)
            incr empties;
            if !empties > 8 then
              (Printf.printf "\nstuck: %d/%d described, no ready units after retries\n"
                 described total; Lwt.return_unit)
            else (let* () = Lwt_unix.sleep 2.0 in pump ())
          end else begin
            empties := 0;
            (* Wake when any worker finishes (or every 5s as a heartbeat)
               and top the queue back up. *)
            let* () = Lwt.pick [ Lwt_condition.wait cond;
                                 Lwt_unix.sleep 5.0 ] in
            pump ()
          end
        end in
      let* () = pump () in
      let (total, described, _) = Cg.status db in
      Printf.printf "\ndone: %d/%d annotated\n" described total;
      Lwt.return_unit
    end;
    Urme_store.Schema.close db in
  Cmd.v (Cmd.info "annotate"
           ~doc:"Annotate the call graph end-to-end: urme runs the loop, \
                 calling the model (default Haiku, headless, in parallel) \
                 only to write per-function summaries, leaves-first by file.")
    Term.(const run $ model $ parallel $ progress $ project_dir)

(* --- Default command: launch TUI on a TTY, MCP server otherwise.
       Claude Code spawns `urme` over stdio (no TTY), which trips the
       MCP branch. Humans running `urme` in a terminal get the TUI. *)

let default_run project_dir =
  let _ = Urme_core.Config.load () in
  if Unix.isatty Unix.stdin then
    Lwt_main.run (Urme_tui.Reactive.run ~project_dir ())
  else
    Lwt_main.run (Urme_mcp.Server.run ~project_dir)

let () =
  Printexc.record_backtrace true;
  (* Suppress stale-FD errors from background Lwt tasks during shutdown *)
  Lwt.async_exception_hook := (fun _exn -> ());
  (* Use libev (kqueue on macOS) instead of select() to avoid
     EINVAL when file descriptors exceed FD_SETSIZE (1024). *)
  (try Lwt_engine.set (new Lwt_engine.libev ())
   with Lwt_sys.Not_available _ -> ());
  let doc = "OCaml CLI orchestration layer for Claude + GitHub" in
  let info = Cmd.info "urme" ~doc ~version:"0.2.0" in
  let default = Term.(const default_run $ project_dir) in
  let cmd = Cmd.group ~default info [
    ask_cmd; init_cmd; export_cmd; import_cmd; graph_init_cmd;
    graph_build_cmd; annotate_cmd;
  ] in
  exit (Cmd.eval cmd)
