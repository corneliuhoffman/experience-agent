# urme

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

urme init                                   # one-time: index every JSONL into .urme/db.sqlite
                                            # (also walks git, links Claude edits to commits)

urme                                        # open the TUI — search, replay, view per-turn diffs
```

That's it for the standalone tool. The TUI's three modes (cycle with `g` / `h` / `s`):

- **Git** — commit log with Claude / human attribution per file.
- **History** — every session and turn, navigable.
- **Search** — type a query, hit Enter, get ranked candidates plus a Claude-synthesised answer.

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

Tools Claude can then call: `search_history`, `get_turn`, `push_synthesis`, `file_history`, `region_blame`, `explain_change`, `commit_links`, `search_by_file`.

## How it works

urme is a single binary backed by a local SQLite store (`.urme/db.sqlite` at the project root). No external services — no ChromaDB, no Ollama, no vector embeddings.

1. **Indexing** — reads Claude Code session logs under `~/.claude/projects/<encoded-project-path>/*.jsonl` and writes one `steps` row per turn with deterministic metadata (files touched, commands run, tokens, `commit_before` / `commit_after`).
2. **Summarisation** — runs the `claude` CLI (Haiku 4.5, one spawn per batch of 8 turns) to produce a one-sentence summary plus 3–8 tags for each step. Indexed via FTS5.
3. **Git linkage** — branch-aware `Git_walk` algorithm walks each branch's commits against the Claude Edit / Write tool_use history and populates the `edit_links` table with per-edit → commit linkage. Human edits (content in a commit that no Claude edit explains) are detected by reconciliation.
4. **Search** — FTS5 + BM25 over `summary`, `tags`, `prompt_text`. The `--smart` and `--deep` modes have Claude rewrite sparse queries and rerank the shortlist with a one-sentence synthesis.

Claude access goes exclusively through the `claude` CLI subprocess — no `ANTHROPIC_API_KEY`, no direct API calls. Uses your Max subscription.

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

Dependencies: `ocaml >= 4.14`, `opam`. See `scripts/setup.sh` for the full package list. Once built, `bin/urme` is the same binary `brew install urme` ships as `urme`.

## Author

Corneliu Hoffman, 2026
