# urme (romanian for traces)

OCaml TUI + MCP server for linking git history to Claude Code conversations.
Browse commits, see which Claude edits explain each diff, detect human
modifications, and search past interactions.

## Install

```sh
brew tap corneliuhoffman/urme
brew install urme
```

For source builds, see [Build from source](#build-from-source) below.

## Quickstart

```sh
cd ~/some/repo-where-you-use-claude-code   # any repo with .claude/ session JSONLs

urme init -j 8                                  # one-time: index every JSONL into .urme/db.sqlite
                                                # (optionally in parallel)
                                                # (also walks git, links Claude edits to commits)

urme                                        # open the TUI — search, replay, view per-turn diffs
```

That's it for the standalone tool. The TUI's three modes (cycle with `g` / `h` / `s`):

- **Git** — commit log with Claude / human attribution per file.
  <img width="1918" height="1055" alt="Screenshot 2026-04-27 at 08 56 56" src="https://github.com/user-attachments/assets/c64fe2ae-703e-4482-9366-7a1de1ee12f0" />

- **History** — every session and turn, navigable.
<img width="1910" height="1078" alt="Screenshot 2026-04-27 at 09 01 08" src="https://github.com/user-attachments/assets/ce24edaa-751a-4738-b5d4-743a6371828e" />

- **Search** — type a query, hit Enter, get ranked candidates plus a Claude-synthesised answer.
<img width="1919" height="1070" alt="Screenshot 2026-04-27 at 09 02 40" src="https://github.com/user-attachments/assets/2c6cbd4c-f285-4777-adbc-26b92afce6ed" />


Re-indexing later: just run `urme init` again. It skips JSONLs whose mtime hasn't changed and only re-walks branches with new commits or unlinked Claude edits.

## Use as a Claude Code MCP server

`urme` auto-detects how it was invoked: with a TTY on stdin it opens the TUI, without one it speaks JSON-RPC over stdio (MCP server). So the same `urme` binary works as a CLI for you and as an MCP server for Claude Code — no separate subcommand needed.

One-time registration:

```sh
claude mcp add -s user urme urme
```

Or per-project in `.mcp.json`:

```json
{
  "mcpServers": {
    "urme": {
      "type": "stdio",
      "command": "urme",
      "args": []
    }
  }
}
```

Tools Claude can then call: `search_history`, `get_turn`, `push_synthesis`, `file_history`, `explain_change`, `commit_links`, `search_by_file`.

## Suggested use

It is meant to be used in a split pane with Claude code. Questions with "using urme ...." will push stuff into the urme pane and therefore you will be able to follow the interaction with Claude
that generated the changes.
<img width="1910" height="1069" alt="Screenshot 2026-04-27 at 08 54 43" src="https://github.com/user-attachments/assets/0cd9c628-b3d1-4da5-9a65-1b512375252a" />


## How it works

urme is a single binary backed by a local SQLite store (`.urme/db.sqlite` at the project root).

1. **Indexing** — reads Claude Code session logs under `~/.claude/projects/<encoded-project-path>/*.jsonl` and writes one `steps` row per turn with deterministic metadata (files touched, commands run, tokens, `commit_before` / `commit_after`).
2. **Summarisation** — runs the `claude` CLI (Haiku 4.5, one spawn per batch of 8 turns) to produce a one-sentence summary plus 3–8 tags for each step. Indexed via FTS5.
3. **Git linkage** — branch-aware `Git_walk` algorithm walks each branch's commits against the Claude Edit / Write tool_use history and populates the `edit_links` table with per-edit → commit linkage. Human edits (content in a commit that no Claude edit explains) are detected by reconciliation.
4. **Search** — FTS5 + BM25 over `summary`, `tags`, `prompt_text`. The `--smart` and `--deep` modes have Claude rewrite sparse queries and rerank the shortlist with a one-sentence synthesis.

Claude access goes exclusively through the `claude` CLI subprocess — no `ANTHROPIC_API_KEY`, no direct API calls. Uses your subscription.

## Code navigation: the annotated call graph

Besides session history, urme can serve an **annotated call graph** of a
codebase over MCP: every function gets a summary written from its source
(leaves-first, so callers' summaries fold in what their callees do), and
`graph_*` tools answer structure questions — blast radius, flows,
rankings, dead code — without grep or file reading.

```sh
cd my-repo
urme graph-init         # loads .urme/callgraph-<lang>.json (a call-graph export is needed — see below)
urme annotate           # writes per-function summaries via the claude CLI (Haiku by default, -m to change)
```

Then add urme as an MCP server and the `graph_search` / `graph_describe` /
`graph_query` / `graph_blast_radius` / `graph_neighborhood` tools become
available, with usage rules served in the MCP instructions.

`graph-init` needs a call-graph export of the repo. If a supported
extractor is installed it runs it automatically; otherwise place a JSON
with the shape below at `.urme/callgraph-<lang>.json` and re-run — any
tool that can enumerate functions and resolved calls can produce it.

### Call-graph JSON format

One JSON object:

```jsonc
{
  "schema": "…",            // informational; ignored by the loader
  "lang": "python",
  "root": "/abs/path/to/repo",   // prefix stripped from all paths on load
  "nodes": [
    {
      "id": "b|/abs/path/to/repo/src/x.py|4|4",  // name|file|line|col of the def's name token
      "name": "b",
      "file": "/abs/path/to/repo/src/x.py",
      "start_line": 4,          // 1-based; the `def` line (decorators handled at read time)
      "start_col": 4,
      "end_line": 5,            // last line of the body
      "end_exact": true,        // false if end_line is a best-effort bound
      "kind": "normal"          // "normal" | "lambda" | "toplevel"
    }
  ],
  "edges": [
    {
      "source": "a|…|1|4",      // the CALLEE's node id
      "target": "b|…|4|4",      // the CALLER's node id
      "kind": "call",
      "call_site": { "file": "/abs/…/x.py", "line": 5, "col": 11 }  // inside the caller
    }
  ]
}
```

Note the edge orientation: exports are **callee → caller** (`source` is
the callee, `target` the caller, and `call_site` falls inside the
caller's span); urme swaps them on load and stores caller → callee.
Node ids must be unique and stable — same-named functions are told apart
by file/line/col. `urme graph-build <file.json>` loads an export
directly if you keep it somewhere other than `.urme/`.

## One-shot questions: `urme ask`

```sh
urme ask "what does this repo do?"
urme ask "summarise the public API of lib/engine" --model sonnet
```

A thin convenience wrapper around the `claude` CLI: spawns Claude as a one-shot subprocess in the project directory, streams the assistant's text reply to stdout, exits. Doesn't read or write the urme index — useful when you just want a Claude answer with project cwd set, without opening a full Claude Code session.

## Export / import

```sh
urme export /tmp/backup.sqlite
urme import /tmp/backup.sqlite --project-dir /other/repo --force
```

Uses SQLite's `VACUUM INTO` under the hood — produces a standard `.sqlite` file anyone can open. WAL-safe; can run alongside the summariser.

## Build from source

```sh
make setup   # install OCaml dependencies via opam
make build   # build + copy binary to bin/urme
make clean   # remove build artifacts
```

## Author

Corneliu Hoffman, 2026
