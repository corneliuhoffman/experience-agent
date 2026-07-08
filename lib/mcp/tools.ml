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
       re-reading the repo. Given a function `query` (its name, or exact \
       node id), returns each match's stored description, source span, \
       callees and callers (each with their descriptions). Use it to \
       answer 'what does X do?', 'what calls X?', and to scope \
       change-impact ('to change X, what else is affected?').";
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
      ];
      "required", `List [`String "query"];
    ];
  ];
  `Assoc [
    "name", `String "graph_search";
    "description", `String
      "Answer a natural-language question about the code by searching the \
       function summaries and traversing the call graph — no repo re-read. \
       Same pattern as `search_history`: YOU turn the question into \
       `fts_terms` (concrete technical nouns), the server runs FTS5 over \
       the stored descriptions and returns the top functions, each with \
       its description and (optionally) callers/callees so you can follow \
       the structure. Rank the hits yourself and answer in 1-2 sentences. \
       For change-impact ('to do X, what do I touch?'), start from the \
       hits and walk callers/callees.";
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
          "description", `String "Max functions to return, 1-100 (default 15).";
        ];
        "neighbors", `Assoc [
          "type", `String "boolean";
          "description", `String
            "Include each hit's callers and callees with descriptions \
             (default true) — needed for change-impact reasoning.";
        ];
      ];
      "required", `List [`String "fts_terms"];
    ];
  ];
]
