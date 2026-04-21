(* SQLite schema for urme V2.

   - steps      : one row per prompt→response turn, plus human-edit rows
                  with session_id = NULL.
   - steps_fts  : FTS5 external-content index over (summary, tags, prompt_text).
   - entities   : named concepts extracted by Claude during indexing.
   - relationships : edges between entities, with step_id provenance.
   - sessions   : per-JSONL metadata.
   - meta       : key/value for schema_version, graph_version, etc.

   Migrations are integer-versioned. [init] is idempotent: open an existing
   DB and it will apply any pending migrations. *)

module D = Db
module S = Sqlite3

let schema_version = 1

(* Migration 1: initial schema. *)
let migration_1 = [
  (* sessions *)
  {|CREATE TABLE IF NOT EXISTS sessions (
      id              TEXT PRIMARY KEY,
      started_at      REAL NOT NULL,
      jsonl_path      TEXT,
      turn_count      INTEGER NOT NULL DEFAULT 0,
      last_indexed_at REAL
    )|};

  (* steps — session_id NULL for human edits *)
  {|CREATE TABLE IF NOT EXISTS steps (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id     TEXT REFERENCES sessions(id),
      turn_index     INTEGER,
      timestamp      REAL NOT NULL,
      prompt_text    TEXT,
      files_touched  TEXT,      -- JSON array
      commands_run   TEXT,      -- JSON array
      tokens_in      INTEGER,
      tokens_out     INTEGER,
      commit_before  TEXT,
      commit_after   TEXT,
      summary        TEXT,
      tags           TEXT       -- space-separated, FTS-friendly
    )|};

  {|CREATE INDEX IF NOT EXISTS steps_session_idx   ON steps(session_id)|};
  {|CREATE INDEX IF NOT EXISTS steps_timestamp_idx ON steps(timestamp)|};
  {|CREATE INDEX IF NOT EXISTS steps_commit_after  ON steps(commit_after)|};

  (* FTS5 external-content index. Triggers keep it in sync with steps. *)
  {|CREATE VIRTUAL TABLE IF NOT EXISTS steps_fts USING fts5(
      summary, tags, prompt_text,
      content='steps', content_rowid='id',
      tokenize='porter unicode61'
    )|};

  {|CREATE TRIGGER IF NOT EXISTS steps_ai AFTER INSERT ON steps BEGIN
      INSERT INTO steps_fts (rowid, summary, tags, prompt_text)
      VALUES (new.id, new.summary, new.tags, new.prompt_text);
    END|};

  {|CREATE TRIGGER IF NOT EXISTS steps_ad AFTER DELETE ON steps BEGIN
      INSERT INTO steps_fts (steps_fts, rowid, summary, tags, prompt_text)
      VALUES ('delete', old.id, old.summary, old.tags, old.prompt_text);
    END|};

  {|CREATE TRIGGER IF NOT EXISTS steps_au AFTER UPDATE ON steps BEGIN
      INSERT INTO steps_fts (steps_fts, rowid, summary, tags, prompt_text)
      VALUES ('delete', old.id, old.summary, old.tags, old.prompt_text);
      INSERT INTO steps_fts (rowid, summary, tags, prompt_text)
      VALUES (new.id, new.summary, new.tags, new.prompt_text);
    END|};

  (* entities *)
  {|CREATE TABLE IF NOT EXISTS entities (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      kind        TEXT NOT NULL,   -- file|concept|decision|bug|pattern
      name        TEXT NOT NULL,
      aliases     TEXT,            -- JSON array
      description TEXT,
      created_at  REAL NOT NULL,
      UNIQUE(kind, name)
    )|};

  {|CREATE INDEX IF NOT EXISTS entities_name_idx ON entities(name)|};

  (* relationships *)
  {|CREATE TABLE IF NOT EXISTS relationships (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      src_entity_id INTEGER NOT NULL REFERENCES entities(id),
      dst_entity_id INTEGER NOT NULL REFERENCES entities(id),
      kind          TEXT NOT NULL,
      step_id       INTEGER REFERENCES steps(id),
      weight        REAL NOT NULL DEFAULT 1.0,
      created_at    REAL NOT NULL,
      UNIQUE(src_entity_id, dst_entity_id, kind, step_id)
    )|};

  {|CREATE INDEX IF NOT EXISTS rel_src_idx ON relationships(src_entity_id)|};
  {|CREATE INDEX IF NOT EXISTS rel_dst_idx ON relationships(dst_entity_id)|};

  (* meta: single-row key/value *)
  {|CREATE TABLE IF NOT EXISTS meta (
      key   TEXT PRIMARY KEY,
      value TEXT
    )|};
]

let migrations = [
  1, migration_1;
]

(* --- meta helpers --- *)

let get_meta db key =
  D.query_fold db "SELECT value FROM meta WHERE key = ?"
    [S.Data.TEXT key] ~init:None ~f:(fun _ cols -> D.data_to_string_opt cols.(0))

let set_meta db key value =
  D.exec_params db
    "INSERT INTO meta(key, value) VALUES(?, ?)\n\
     ON CONFLICT(key) DO UPDATE SET value = excluded.value"
    [S.Data.TEXT key; S.Data.TEXT value]

let current_schema_version db =
  match get_meta db "schema_version" with
  | Some s -> (try int_of_string s with _ -> 0)
  | None -> 0

(* --- pragmas --- *)

let apply_pragmas db =
  (* WAL concurrency, sane durability, referential integrity. *)
  D.exec db "PRAGMA journal_mode = WAL";
  D.exec db "PRAGMA synchronous = NORMAL";
  D.exec db "PRAGMA foreign_keys = ON";
  D.exec db "PRAGMA temp_store = MEMORY"

(* --- bootstrap + migrate --- *)

(* Ensure the meta table exists before reading schema_version. This is the
   only table we touch before migrations run, so we create it unconditionally
   (IF NOT EXISTS keeps it idempotent). *)
let ensure_meta db =
  D.exec db
    "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)"

let run_migrations db =
  ensure_meta db;
  let from_version = current_schema_version db in
  let pending = List.filter (fun (v, _) -> v > from_version) migrations in
  List.iter (fun (v, stmts) ->
    D.with_txn db (fun () ->
      List.iter (D.exec db) stmts;
      set_meta db "schema_version" (string_of_int v))
  ) pending;
  if from_version = 0 then set_meta db "graph_version" "0"

let init db =
  apply_pragmas db;
  run_migrations db

(* --- path helpers --- *)

(* Default DB path: <project_dir>/.urme/db.sqlite. Mirrors how other
   project-local state gets scoped to the repo. *)
let default_path ~project_dir =
  Filename.concat project_dir (Filename.concat ".urme" "db.sqlite")

let ensure_parent_dir path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

let open_or_create ~project_dir =
  let path = default_path ~project_dir in
  ensure_parent_dir path;
  let db = S.db_open path in
  init db;
  db

let open_at path =
  ensure_parent_dir path;
  let db = S.db_open path in
  init db;
  db

let close db =
  ignore (S.db_close db)

(* --- graph_version bump --- *)

let bump_graph_version db =
  let current =
    match get_meta db "graph_version" with
    | Some s -> (try int_of_string s with _ -> 0)
    | None -> 0
  in
  set_meta db "graph_version" (string_of_int (current + 1))

let graph_version db =
  match get_meta db "graph_version" with
  | Some s -> (try int_of_string s with _ -> 0)
  | None -> 0
