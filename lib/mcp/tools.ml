(* MCP tool definitions — JSON schemas for Claude Code integration *)

let tool_definitions = `List [
  `Assoc [
    "name", `String "search_history";
    "description", `String
      "Search past Claude Code session turns. Three-layer pipeline with \
       YOU (the calling Claude) doing the LLM steps:\n\
       (1) You translate the user's natural-language question into the \
       structured query spec below (`fts_terms`, `order_by`, temporal \
       bounds, …).\n\
       (2) The server runs SQLite FTS5 with that spec and returns up to \
       `limit` candidates. The top 5 include full `prompt_text` and \
       `assistant_text` as evidence — read them to inform your \
       ranking and synthesis.\n\
       (3) You rank the candidates yourself — drop tangential, \
       lexical-only, or opposite-direction hits (if the user asks when \
       a feature was ADDED, drop turns about REMOVING it, and vice-\
       versa). Prefer 0 or 1 clear citation over several weak ones. \
       Produce ONE or TWO sentences answering the question, citing the \
       best candidate as [session-prefix] turn N, date.\n\
       (4) Call `push_synthesis` with that answer text and the cited \
       {session_id, turn_index} pairs so the running URME TUI can show \
       the user your conclusion alongside the evidence.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "fts_terms", `Assoc [
          "type", `String "string";
          "description", `String
            "Space-separated keywords for a SQLite FTS5 MATCH expression. \
             Use concrete technical nouns, drop filler words. Examples: \
             \"sqlite fts5\", \"git link commit\", \"branch walker\". \
             Leave empty and pass `query` to fall back to straight \
             keyword match.";
        ];
        "order_by", `Assoc [
          "type", `String "string";
          "enum", `List [`String "earliest"; `String "latest"; `String "relevance"];
          "description", `String
            "\"earliest\" for origin queries (\"when did we first X?\", \
             \"when did X start?\"); \"latest\" for \"what did we last do \
             about X?\"; \"relevance\" otherwise (default).";
        ];
        "limit", `Assoc [
          "type", `String "integer";
          "description", `String "Max candidates to return, 10-200 (default 20).";
        ];
        "require_summary", `Assoc [
          "type", `String "boolean";
          "description", `String
            "True for analytical queries (default). False for meta / \
             question-style queries where matching the prompt text is more \
             useful than matching a pre-computed summary.";
        ];
        "after", `Assoc [
          "type", `String "string";
          "description", `String
            "ISO date YYYY-MM-DD or omit. Only turns on/after this date.";
        ];
        "before", `Assoc [
          "type", `String "string";
          "description", `String
            "ISO date YYYY-MM-DD or omit. Only turns before this date.";
        ];
        "query", `Assoc [
          "type", `String "string";
          "description", `String
            "Optional raw natural-language query — used as a plain-FTS5 \
             fallback if `fts_terms` is empty or returns nothing.";
        ];
      ];
      "required", `List [];
    ];
  ];
  `Assoc [
    "name", `String "push_synthesis";
    "description", `String
      "Push your final answer to the running URME TUI. Call this once \
       after you have produced your synthesis from `search_history` \
       candidates, so the user can see the conclusion and the evidence \
       behind it. The TUI will fetch the cited turns' full text from \
       disk — you do not need to send it.\n\
       \n\
       CRITICAL: `synthesis` and `cited` are SIBLING tool arguments. \
       `synthesis` must be plain prose ONLY — do not embed the cited \
       array inside it as `<parameter name=\"cited\">[...]</parameter>` \
       or any other XML/JSON fragment. Pass the citations as the \
       separate `cited` array argument. If you concatenate them into \
       one string, the TUI will show no Results pane hits.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "synthesis", `Assoc [
          "type", `String "string";
          "description", `String
            "Your one-or-two-sentence answer to the user's question, \
             same text you are about to give them.";
        ];
        "cited", `Assoc [
          "type", `String "array";
          "description", `String
            "The 0-2 candidates your synthesis cites. Each is \
             {session_id, turn_index} as returned by `search_history`. \
             Empty if nothing answered.";
          "items", `Assoc [
            "type", `String "object";
            "properties", `Assoc [
              "session_id", `Assoc ["type", `String "string"];
              "turn_index", `Assoc ["type", `String "integer"];
            ];
            "required", `List [`String "session_id"; `String "turn_index"];
          ];
        ];
      ];
      "required", `List [`String "synthesis"];
    ];
  ];
  `Assoc [
    "name", `String "get_turn";
    "description", `String
      "Fetch one turn's full user prompt + assistant response. Use \
       this for the 1–2 candidates from `search_history` that you \
       actually want to cite or quote in your synthesis — avoids \
       dumping every hit's full text into context.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "session_id", `Assoc [
          "type", `String "string";
          "description", `String "Session ID as returned by search_history.";
        ];
        "turn_index", `Assoc [
          "type", `String "integer";
          "description", `String "Turn index within the session.";
        ];
      ];
      "required", `List [`String "session_id"; `String "turn_index"];
    ];
  ];
  `Assoc [
    "name", `String "file_history";
    "description", `String "Get the full git + Claude edit history for a file. Shows which commits touched the file and which Claude edits explain each change.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "file_path", `Assoc [
          "type", `String "string";
          "description", `String "File path relative to project root";
        ];
      ];
      "required", `List [`String "file_path"];
    ];
  ];
  `Assoc [
    "name", `String "region_blame";
    "description", `String "Git blame for a line range with Claude edit attribution. Shows which commits and Claude edits introduced each line.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "file_path", `Assoc [
          "type", `String "string";
          "description", `String "File path relative to project root";
        ];
        "start_line", `Assoc [
          "type", `String "integer";
          "description", `String "Start line (1-based)";
        ];
        "end_line", `Assoc [
          "type", `String "integer";
          "description", `String "End line (1-based)";
        ];
      ];
      "required", `List [`String "file_path"; `String "start_line"; `String "end_line"];
    ];
  ];
  `Assoc [
    "name", `String "explain_change";
    "description", `String "Decompose a commit's changes to a file: which parts are Claude edits, which are human edits, and what session/turn they came from.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "commit_sha", `Assoc [
          "type", `String "string";
          "description", `String "Git commit SHA (full or abbreviated)";
        ];
        "file_path", `Assoc [
          "type", `String "string";
          "description", `String "File path relative to project root";
        ];
      ];
      "required", `List [`String "commit_sha"; `String "file_path"];
    ];
  ];
  `Assoc [
    "name", `String "commit_links";
    "description", `String "Get all Claude session links for a commit. Shows which sessions and turns contributed edits to each file in the commit.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "commit_sha", `Assoc [
          "type", `String "string";
          "description", `String "Git commit SHA (full or abbreviated)";
        ];
      ];
      "required", `List [`String "commit_sha"];
    ];
  ];
  `Assoc [
    "name", `String "search_by_file";
    "description", `String "Find all interactions that changed a specific file. Combines vector search with text matching on file names.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "file_path", `Assoc [
          "type", `String "string";
          "description", `String "File path or basename to search for";
        ];
        "n", `Assoc [
          "type", `String "integer";
          "description", `String "Number of results (default 10)";
        ];
      ];
      "required", `List [`String "file_path"];
    ];
  ];
  (* ---- annotated call graph ---- *)
  `Assoc [
    "name", `String "graph_init";
    "description", `String
      "Initialise the annotated call graph for this repo (\"using urme, \
       initialise the callgraph\"). Runs the opengrep interfile analyzer \
       (static analysis — can take minutes on big repos), loads the \
       resulting nodes+edges into .urme/db.sqlite with SCC/topo order, \
       and reports how many functions await description. YOU then \
       produce the annotations: loop graph_next_batch -> write \
       short-but-comprehensive descriptions -> graph_set_descriptions until \
       remaining = 0 (parallel subagents are safe: ready units are \
       independent). Requires the opengrep-interfile-graph binary on \
       PATH or via URME_OPENGREP_EXPORTER. Replaces any existing graph \
       for the project.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "lang", `Assoc [
          "type", `String "string";
          "description", `String
            "Language to analyze (opengrep id: \"ts\", \"js\", \
             \"python\", \"java\", \"go\", ...). One graph per language.";
        ];
        "root", `Assoc [
          "type", `String "string";
          "description", `String
            "Project root to analyze (default: this project). Must be a \
             standalone repo root — paths nested inside another git repo \
             yield 0 files.";
        ];
        "ncores", `Assoc [
          "type", `String "integer";
          "description", `String "Parallelism for the analyzer (default 4).";
        ];
        "exporter", `Assoc [
          "type", `String "string";
          "description", `String
            "Path to the opengrep-interfile-graph binary (overrides PATH \
             / URME_OPENGREP_EXPORTER).";
        ];
      ];
      "required", `List [`String "lang"];
    ];
  ];
  `Assoc [
    "name", `String "graph_status";
    "description", `String
      "Progress of the annotated call graph for this repo. Returns \
       {total, described, remaining, ready_units}. `ready_units` is how \
       many SCC-units can be described right now (all their cross-SCC \
       callees already have descriptions). remaining=0 means fully \
       annotated.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [];
      "required", `List [];
    ];
  ];
  `Assoc [
    "name", `String "graph_next_batch";
    "description", `String
      "Drive the leaves-first describe-loop. Returns up to `limit` \
       SCC-units that are READY to describe: every unit's cross-SCC \
       callees already have descriptions (included, so you can write \
       informed summaries). For each function in `functions`, write a \
       SHORT BUT COMPREHENSIVE description (usually 1-2 sentences) — what \
       it does and its type/signature, plus anything non-obvious a caller \
       must know (side effects, security relevance, error behaviour). \
       Favour completeness over brevity when they conflict. A unit \
       with `recursive:true` holds mutually-recursive functions: describe \
       them together. Functions with synthetic names (e.g. _tmp_lambda) \
       are lambdas: START their description with the real binding name \
       visible in the code (e.g. \"notSolved: (challenge) => ...\") so \
       name searches can find them. Then call `graph_set_descriptions` \
       with a {id, description} for every function returned, and call \
       this again. Repeat until `graph_status` shows remaining=0.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "limit", `Assoc [
          "type", `String "integer";
          "description", `String "Max SCC-units to return (default 5).";
        ];
      ];
      "required", `List [];
    ];
  ];
  `Assoc [
    "name", `String "graph_set_descriptions";
    "description", `String
      "Write function descriptions back into the graph. Pass \
       `descriptions` as an array of {id, description}, using the exact \
       `id` values from `graph_next_batch`. Returns updated progress \
       counts.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "descriptions", `Assoc [
          "type", `String "array";
          "description", `String "One {id, description} per function.";
          "items", `Assoc [
            "type", `String "object";
            "properties", `Assoc [
              "id", `Assoc ["type", `String "string"];
              "description", `Assoc ["type", `String "string"];
            ];
            "required", `List [`String "id"; `String "description"];
          ];
        ];
      ];
      "required", `List [`String "descriptions"];
    ];
  ];
  `Assoc [
    "name", `String "graph_describe";
    "description", `String
      "Ask the annotated call graph about a function instead of \
       re-reading the repo. Given a function `query` (its name), returns \
       each match's stored description, file:line, and its callers as \
       compact strings — bare \"name\" when in the same file as the \
       match, \"name (file)\" otherwise; describe a caller by name when \
       you need its summary. Callee knowledge is already embedded in the \
       description (summaries are written with callee context and name \
       the ones that matter); pass include_callees for the exhaustive \
       list. Use it to \
       answer 'what does X do?', 'what calls X?', and to scope \
       change-impact ('to change X, what else is affected?'). When a \
       match has no static callers, a `mentioned_by` list is attached: \
       functions whose summaries name it — usually its dynamic \
       dispatchers (task queues, plugin registries, event/signal \
       systems). Treat those as probable callers and keep walking from \
       them.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Function name or exact node id.";
        ];
        "include_code", `Assoc [
          "type", `String "boolean";
          "description", `String
            "Include the function source in each match (default false).";
        ];
        "include_callees", `Assoc [
          "type", `String "boolean";
          "description", `String
            "Include each match's exhaustive callee list (default false \
             — the description already names the callees that matter).";
        ];
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "graph_search";
    "description", `String
      "START HERE for any 'how does X work' / debug / review / \
       understand / where-should-I question. YOU turn the question into \
       `fts_terms` (concrete technical nouns); FTS5 runs over the stored \
       summaries and returns the top functions with their descriptions. \
       KEY: each summary was written leaves-first, so it already folds in \
       what its callees do — a high-level function's description is a \
       self-contained account of the whole downstream flow. So the top \
       few hits usually already contain the answer: read them and answer, \
       DO NOT reflexively pull the neighborhood, dump the file, or open \
       the source. Use a SMALL `limit` (5-8) and `neighbors:false` unless \
       you need callers. Only if a SPECIFIC detail is genuinely missing \
       from the summaries do one more targeted graph_search or a \
       graph_describe. Dynamic dispatch (task queues, plugin registries, \
       signals) has no static edge — if a function's callers look \
       empty, search its NAME; dispatch sites are named in the summaries. \
       For a count / ranking / caller-set / transitive closure (a \
       structural FACT, not an explanation), use graph_query instead.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "fts_terms", `Assoc [
          "type", `String "string";
          "description", `String
            "Space-separated FTS5 keywords over the function summaries. \
             Concrete technical nouns, drop filler words.";
        ];
        "limit", `Assoc [
          "type", `String "integer";
          "description", `String
            "Max functions to return, 1-100 (default 6). The top summaries \
             embed their callees, so a few is usually enough — only raise \
             it if the answer is genuinely spread wider.";
        ];
        "neighbors", `Assoc [
          "type", `String "boolean";
          "description", `String
            "Include each hit's callers as name strings (default false). \
             Leave off — the summaries are self-contained; set true only \
             when walking upward for change-impact.";
        ];
      ];
      "required", `List [`String "fts_terms"];
    ];
  ];
  `Assoc [
    "name", `String "graph_neighborhood";
    "description", `String
      "Get the actual reachable SUBGRAPH from seed functions — the set of \
       nodes and how they connect, in ONE call. Use this when you need \
       the STRUCTURE itself, not an explanation: an exhaustive caller/ \
       callee set for change-impact ('everything that breaks if I change \
       X' -> roots [X], direction callers), a specific dispatch-aware \
       closure, or the exact source of a flow (`include_code:true`). \
       NOTE: for a plain 'how does X work' / understand / debug question, \
       DON'T start here — graph_search is cheaper, because each summary \
       already embeds its callees, so a few top summaries usually answer \
       the question without walking the whole subgraph. Reach for \
       graph_neighborhood only when the summaries aren't enough and you \
       need the concrete node set or source. Seed from `roots` (function \
       names; scope an ambiguous name with `file`), walk `depth` hops. \
       Returns a bounded, task-shaped set (auto-briefs if wide, briefs \
       the prose when code is included). (For a custom shape, write a \
       recursive CTE with graph_query.)";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "roots", `Assoc [
          "type", `String "array";
          "items", `Assoc [ "type", `String "string" ];
          "description", `String
            "Seed function names to start the walk from (entry points / \
             anchors). Bare name collisions ALL seed the walk — scope them \
             with `file` when a method name (e.g. create_certificate) \
             exists in many files.";
        ];
        "file", `Assoc [
          "type", `String "string";
          "description", `String
            "Optional glob (e.g. \"*lemur_digicert*\") restricting which \
             seed nodes match `roots` — pins an ambiguous seed to one \
             file/plugin so the trace stays on the intended flow. Only \
             filters the seeds, not the reachable subgraph.";
        ];
        "direction", `Assoc [
          "type", `String "string";
          "description", `String
            "\"callees\" (downstream — how the seeds work; default), \
             \"callers\" (upstream — what reaches the seeds / \
             change-impact), or \"both\".";
        ];
        "depth", `Assoc [
          "type", `String "integer";
          "description", `String
            "Max hops from the seeds (default 3, max 8). Start small; \
             widen if the slice is missing pieces.";
        ];
        "follow_dispatch", `Assoc [
          "type", `String "boolean";
          "description", `String
            "Also traverse dynamic-dispatch edges (Celery .delay/ \
             .apply_async, plugin registries, signals) recovered from the \
             summaries — so async paths are in the closure without a \
             separate mentioned_by chase (default true).";
        ];
        "detail", `Assoc [
          "type", `String "string";
          "description", `String
            "\"full\" or \"brief\" (first-sentence gists); default auto \
             (brief above 60 functions).";
        ];
        "include_code", `Assoc [
          "type", `String "boolean";
          "description", `String "Include each function's source (default false).";
        ];
        "limit", `Assoc [
          "type", `String "integer";
          "description", `String "Max functions to return (default 250).";
        ];
      ];
      "required", `List [`String "roots"];
    ];
  ];
  `Assoc [
    "name", `String "graph_query";
    "description", `String
      "Run a read-only SQL query over the annotated call graph and get \
       back rows — the general primitive. YOU write the SELECT for your \
       question; one query does what would otherwise be several \
       graph_search/describe/neighborhood calls. Use it whenever the \
       other tools don't fit the exact shape (filter, join, aggregate, \
       recursive closure, multi-condition). Read-only (writes are \
       rejected); rows capped — add your own LIMIT.\n\n\
       SCHEMA (paths are repo-relative):\n\
       - cg_nodes(id, name, file, start_line, end_line, kind, scc, topo, \
       description) — one row per function; `description` is the \
       annotator summary; `kind` is normal|<top_level>|lambda.\n\
       - cg_edges(src, dst) — a static call edge, src=caller id, \
       dst=callee id (join to cg_nodes.id).\n\
       - cg_dispatch(src, dst) — a dynamic-dispatch edge (Celery \
       .delay/.apply_async etc.), src=dispatcher, dst=target.\n\
       - cg_fts(node_id, name, description) — FTS5 full-text index; match \
       with  WHERE cg_fts MATCH 'description:\"send email\"'  and join \
       node_id to cg_nodes.id, ORDER BY bm25(cg_fts).\n\n\
       RECIPES:\n\
       - Search summaries:  SELECT n.name,n.file,n.start_line,n.description \
       FROM cg_fts f JOIN cg_nodes n ON n.id=f.node_id WHERE cg_fts MATCH \
       'description:\"revoke certificate\"' ORDER BY bm25(cg_fts) LIMIT 15;\n\
       - Direct callers of X:  SELECT s.name,s.file FROM cg_edges e JOIN \
       cg_nodes s ON s.id=e.src JOIN cg_nodes d ON d.id=e.dst WHERE \
       d.name='revoke';\n\
       - Everything in a file/dir:  SELECT name,start_line,description FROM \
       cg_nodes WHERE file='lemur/certificates/service.py' ORDER BY topo;\n\
       - Transitive callers incl. dynamic dispatch (blast radius):\n\
       \  WITH RECURSIVE up(id) AS (\n\
       \    SELECT id FROM cg_nodes WHERE name='send_default_notification'\n\
       \    UNION SELECT e.src FROM up JOIN (SELECT src,dst FROM cg_edges \
       UNION SELECT src,dst FROM cg_dispatch) e ON e.dst=up.id)\n\
       \  SELECT n.name,n.file,n.description FROM up JOIN cg_nodes n ON \
       n.id=up.id;\n\
       Swap e.dst=up.id -> e.src=... and select e.dst for downstream \
       (callees). Filter entry points with e.g. description LIKE \
       '%@celery.task%'.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "sql", `Assoc [
          "type", `String "string";
          "description", `String
            "A single read-only SELECT/WITH query over the schema above.";
        ];
        "max_rows", `Assoc [
          "type", `String "integer";
          "description", `String "Row cap (default 300, max 1000).";
        ];
      ];
      "required", `List [`String "sql"];
    ];
  ];
]
