(* MCP tool definitions — JSON schemas for Claude Code integration *)

let tool_definitions = `List [
  `Assoc [
    "name", `String "search_history";
    "description", `String "Semantic search across all Claude Code session interactions. Use to find past conversations about a topic, file, or task.";
    "inputSchema", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "query", `Assoc [
          "type", `String "string";
          "description", `String "Natural language search query";
        ];
        "n", `Assoc [
          "type", `String "integer";
          "description", `String "Number of results (default 5)";
        ];
      ];
      "required", `List [`String "query"];
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
]
