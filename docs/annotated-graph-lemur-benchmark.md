# The annotated call graph, and the lemur benchmark

This document describes the system as it stands — how the annotated call
graph is built and served — and the benchmark used to evaluate it on
**lemur** (Netflix's certificate-management service: Flask REST API,
Celery tasks, and a slug-keyed plugin registry for issuers, destinations,
and notifications), including the full question set and a careful
comparison of the three final configurations.

---

## 1. The annotated call graph

### 1.1 Static extraction

`opengrep` exports a whole-repo call graph (`opengrep-callgraph/v1` JSON):
one node per function/method with its exact source span, one edge per
resolved call, caller→callee. Resolution covers module-qualified calls,
class-qualified calls, and `self.method()` calls resolved through the
class hierarchy to inherited definitions (including staticmethods called
via an instance). The export is fast — **0.62 s for all of lemur**.

For lemur this yields **2,292 functions across 308 files and 3,713 call
edges** (test files included). Genuinely dynamic dispatch — a registry
lookup keyed by a database value, an ORM event listener — is not
representable as a static edge; it is recovered at the summary layer
(below).

### 1.2 Loading

`urme graph-build callgraph.json` loads the export into SQLite
(`.urme/db.sqlite`):

- `cg_nodes(id, name, file, start_line, end_line, kind, description)`
- `cg_edges(src, dst)` — caller→callee
- `cg_dispatch(src, dst)` — dynamic-dispatch edges
- `cg_fts` — full-text index over names and summaries

and computes the file-level SCC condensation plus a leaves-first
topological order, which drives annotation.

### 1.3 Annotation

`urme annotate` writes a summary for every function. It is a
deterministic pipeline, not an agent: urme itself runs the loop and calls
the model only to produce prose.

- **Order: leaves-first.** A function is annotated only after the
  functions it calls (in other files) are annotated, so each prompt can
  include its cross-file callees' finished summaries and each summary can
  *fold in* callee behaviour. A summary is therefore self-contained: a
  high-level function's description already accounts for what happens
  downstream.
- **Units.** Work is batched into units of ≤1,500 source lines and ≤40
  functions; several small ready files share one unit, and a huge file is
  split into parallel chunks. Each unit is sent to a fresh, tool-less,
  MCP-less, history-less model process (`-j` units in flight; a hard
  timeout and failure backoff make the loop self-healing).
- **The summary contract** (the annotation prompt's rules): 1–5 sentences
  per function covering (1) what it does, with the **signature copied
  verbatim** from the def line — exact parameter names, order, defaults —
  and what it returns including None/empty cases; (2) side effects,
  security relevance, error/edge behaviour; (3) if the function runs by
  **registration** rather than a direct call (decorator, route, plugin
  registry, event listener), exactly what triggers it; (4) bugs/gotchas
  **only when pointable-to in the shown source** — never suspected; plus
  folding of load-bearing callee gotchas so the summary stands alone.

Cost for lemur, with Haiku 4.5 at 8-way parallelism: **~16 minutes and
~$1.50 for all 2,292 functions** (~110 functions/minute). This is a
one-time cost per repo revision, amortized over every subsequent query
session.

### 1.4 Serving: the MCP tools

The graph is served over MCP:

| tool | what it does |
|---|---|
| `graph_search` | FTS over names + summaries; the entry point for "how does X work" |
| `graph_describe` | one function's summary; `code_only:true` returns exactly its source span |
| `graph_query` | read-only SQL over the tables — counts, rankings, caller sets in one call |
| `graph_blast_radius` | the dispatch-inclusive transitive caller/callee closure for a name: direct + transitive counts + by-file breakdown, computed per same-named definition, with a loud `ambiguous:true` flag when a name resolves to many distinct functions |
| `graph_neighborhood` | the reachable subgraph from seed functions, optionally with code |

Model guidance is eight compact rules (weak models follow short
mechanical imperatives; explanatory prose measurably degrades them):

1. **Route**: how-does-X-work → `graph_search` + summaries; blast radius →
   `graph_blast_radius`; other structural facts → `graph_query`; one
   function → `graph_describe`.
2. **Never compute structure yourself** — no scripts, no grep for
   callers/dead code/flows; the graph holds these precomputed.
3. **Trust the summaries**; cite `file:line` from them without opening
   files.
4. **Read code only via `graph_describe code_only`**, in two cases: a
   literal token no summary carries, or a **diagnosis question** ("find
   the bug", "is it safe", guard audits) — triage with the graph, then
   read the implicated functions; bugs live in lines summaries compress
   away.
5. **Never state what a file registers/raises/returns** unless a summary
   says it or you read that code.
6. **Always run `graph_blast_radius` on the named symbol** for
   centrality/safety questions — a bare name like `get` can be 70+
   different functions; never report one merged number.
7. **Exact contracts** (signatures, return shapes) are re-quoted from a
   fresh `graph_describe`, never reconstructed from memory.
8. Otherwise **reuse** results already in context.

---

## 2. The benchmark

### 2.1 Design

Three configurations answer the same 21 questions about lemur, in one
session each, printing a `---DONE <ID>---` marker per question:

- **Haiku + graph** — Claude Haiku 4.5 with the urme MCP tools.
- **Opus + graph** — Claude Opus 4.8 with the urme MCP tools.
- **Opus + grep** — Claude Opus 4.8 with *no* graph: shell, grep/sed/cat,
  file reads, subagents. (In practice it writes its own throwaway AST
  call-graph script each session — name-based, unresolved — and queries
  that plus raw source.)

**Hygiene.** The grep arm runs with an empty MCP config
(`--strict-mcp-config`) and with every graph artifact stashed out of the
repo (`.urme/`, `CLAUDE.md`, exports); the graph arms run with them
restored. Several pilot runs were invalidated by leaked artifacts before
this protocol was fixed.

**Cost.** Computed from per-message token usage × rate card, deduplicated
by message id, subagents included; cache writes are split 5-minute
($6.25/MTok Opus) vs 1-hour ($10/MTok) via `usage.cache_creation`.
Validated against the CLI's `/usage` to the cent on single-agent runs;
multi-agent runs read ~5–10% low (subagent transcripts expose only a flat
cache-write field).

**Scoring.** Independent adjudication agents put the answer sets side by
side and verify every disputed claim against the source tree (grep/read as
tiebreaker) and, for the graph arms, against the SQLite db — checking both
that a number is *true* and that it is *faithful* to what the tool
returned.

### 2.2 The 21 questions

Structure and rankings:

- **R1.** Which 3–5 functions are depended on by the largest portion of the codebase (widest blast radius)?
- **R2.** Which 6–8 modules/files have the most functions that nothing else in the codebase calls (genuine dead code)?
- **R3.** Trace the `certificate_reissue` Celery task from entry down to the database — functions in order, how deep.
- **R4.** Which 5 shared helper/utility functions are called from the most different places?

Flow traces:

- **Q5.** Trace certificate revocation from the REST endpoint to an issuer plugin's `revoke_certificate`, incl. dispatch hops.
- **Q6.** How does the DigiCert plugin issue a certificate? Entry point → outbound HTTP.
- **Q7.** How does the SFTP destination plugin upload a certificate? Entry → SSH/SFTP write.
- **Q8.** In the ACME plugin, how is a DNS-01 challenge created and propagated before finalize? Key functions.
- **Q9.** Add a Slack notification plugin: which files/functions define the interface & registration, what to implement.

Blast radius:

- **BR1.** Change impact of `parse_certificate` (common/utils.py): direct callers + rough transitive count.
- **BR2.** DigiCert `create_certificate` return change: what depends on it up to REST/CLI entry points.
- **BR3.** Blast radius of `session_query` (database.py): count + entry-point categories.
- **BR5.** Change impact of `create_certificate_roles` (certificates/service.py): callers + transitive reach.

Diagnosis:

- **G1.** Expiration notifications sometimes don't send — diagnose: flow, failure points, where to log.
- **G2.** Safe to move issuance to a background Celery task? What assumes sync, what changes, what breaks silently.
- **G3.** Revocation endpoint: is authz enforced before the CA revoke? Walk checks; flag gaps.
- **G4.** Best single place to throttle every outbound DigiCert call, why there vs call sites.

Name-collision traps (each question names a symbol that is ambiguous or
whose obvious reading is wrong):

- **N1.** A teammate wants to change the return value of the DigiCert plugin's `create_certificate`. What's the change impact — which functions and REST/CLI entry points actually depend on it, and how is it reached? *(the plugin has 1 static caller — a test; the real path is dynamic dispatch through `mint`)*
- **N2.** Among the certificate-authority issuer plugins, which one's certificate-issuance code is invoked from the most places in the codebase?
- **N3.** `send` shows up all over this codebase. Which `send` is the high-traffic one, what does it do, and what would break if its signature changed? *(metrics `send` vs notification `send` vs signal `.send`)*
- **N4.** There's a `get` accessor used throughout the code. How central is it — what's its blast radius, and is it safe to change? *(72 distinct functions named `get`)*

---

## 3. The three final runs

| | **Haiku + graph** | **Opus + graph** | **Opus + grep** |
|---|---|---|---|
| Cost | **$0.33** | $4.75 | $5.35 |
| Wall time | **3.0 min** | 8.6 min | ~10 min |
| Completed | 21/21 | 21/21 | 21/21 |
| Tool profile | 40 graph calls, parallel-batched | 63 graph calls incl. 5 targeted code reads | self-built AST graph + grep/sed + 4 subagents |

### 3.1 Counting questions (R1, R2, R4, BR1–BR5, N-counts)

Both graph arms report **database-exact numbers that independent grep
corroborates**: `metrics.send` 156 distinct callers (223 call sites),
`get_option` 104 edges / 41 callers, `parse_certificate` 25 direct,
`validate_schema` 75, `database.get` 30 — every sampled figure matched
the db to the digit, and the db matched source.

The grep arm's disposable name-based graph is its structural weakness,
and it showed exactly where its own docstring predicted
("same-named methods across classes collapse together"):

- `database.filter` credited with **63 direct callers; ground truth 13**
  — drowned by ~200 generic SQLAlchemy `.filter(` sites. Two of its five
  R1 "widest blast radius" rows and one R4 row were artifacts of this
  inflation.
- A **false dead-code row**: `human_time`/`interval`/`unit` declared
  "verified dead" — they are registered as Jinja template filters three
  lines below their definitions.
- **BR5**: claimed "reachable entry points are just 4", omitting both
  REST endpoints — contradicting its own BR2 answer.
- **N2**: opened with "they're all invoked from exactly one place",
  false for ACME, then contradicted itself.

Verdict: on anything that is fundamentally *counting* — rankings, dead
code, caller sets, blast radius — the persistent resolved graph wins
outright, at every model tier. The traps behaved the same way: all three
arms disambiguated N3/N4 explicitly (the graph arms led with "multiple
`send`/`get` functions, ambiguous name" — the tool flags collisions —
and the grep arm disambiguated by hand), and all three got BR2/N1's real
contract (a 3-tuple `(end_entity, intermediate, certificate_id)`
unpacked inside `mint`, reached only by dynamic dispatch — the static
caller count of 1 is a test).

### 3.2 Diagnosis questions (G1–G4, and bug-finding generally)

This is where model capability and the willingness to read raw code
dominate, and where the three arms genuinely differ:

- **Haiku + graph** answers at flow level: correct chains, correct
  `file:line`, generic failure points. It never exercises the read-the-code
  rule; in repeated trials it has not once produced a line-level bug
  discovery. That is a model ceiling, not an instruction problem.
- **Opus + graph** triages with the graph, then pulls the implicated
  functions' source (five targeted `code_only` reads in the final run) and
  grounds the diagnosis in quoted code: the `days == interval`
  exact-equality bug in `needs_notification` (a missed daily run silently
  skips that notification forever), the **90-day query window** in
  `get_certificates` that filters out any longer-interval notification
  before eligibility is even tested, and — as dead-code corroboration — a
  latent `TypeError` in an uncalled ultradns path. It also correctly
  reports registration-driven flows (the SQLAlchemy
  `@event.listens_for(Certificate.destinations, "append")` trigger behind
  the SFTP upload) from the summaries, which record triggers by contract.
- **Opus + grep** reads the most source of the three and finds real,
  verified bugs the summaries smooth over — in its final run: a
  notification success-counter reset inside a loop, a `RoleNeed("creator")`
  that authorizes any creator-role holder to revoke anything, a dead
  `RoleNeed(owner)` comparison that can never match, an ignored `False`
  return from ACME revocation (a failed CA revoke still marks the
  certificate revoked), and an email plugin that counts an empty
  recipient list as success.

In the same-model adjudication (Opus + graph vs Opus + grep, same tree,
every disputed claim source-verified) the score was **grep 8, graph 5,
even 8**: the graph took the counting questions, grep took the reading
questions. The graph arm's subsequent runs under the read-on-diagnosis
rule closed part of that gap (the 90-day window had previously been a
grep-only find), but grep's raw-source appetite still finds bugs the
graph arm does not. Bug-finding is also high-variance for everyone:
several verified bugs found in one grep session were re-found by *neither*
arm in the next.

### 3.3 Errors

- **Graph arms, final runs: no verified false claims.** Every sampled
  citation was exact and every number tool-faithful. (The failure mode
  that produces graph-arm errors — asserting mechanics of a file the
  model never read — is specifically forbidden by rule 5, and stopped
  appearing once that rule and the contract-re-quoting rule were in
  place.)
- **Grep arm: four verified false claims** in its final run (the
  `filter` inflation, the Jinja false-dead row, the BR5 entry-point
  claim, the N2 claim), all traceable to its two structural handicaps:
  name collision in its throwaway graph, and re-deriving structure from
  scratch under time pressure.

### 3.4 Economics

- The graph costs **~$1.50 + 16 minutes once** per repo revision
  (annotation; the static export is under a second).
- After that, a **$0.33 / 3-minute Haiku session** answers
  structure/fact/contract questions at the same verified accuracy as a
  ~$5 Opus session — a 15× price separation with a clean routing rule.
- Between the two Opus arms cost is a wash ($4.75 vs $5.35 — within
  run-to-run variance). The choice between them is not price but failure
  mode: the graph arm's numbers and citations are reliable and its
  errors are absent; the grep arm reads more and finds more bugs, but
  ships collision-inflated rankings and false dead-code alongside them —
  and re-pays its entire derivation cost, and re-risks its errors, every
  session.

### 3.5 Bottom line

| question class | best arm | why |
|---|---|---|
| counts, rankings, blast radius, dead code | **Haiku + graph** | db-exact at 6% of the cost |
| flow traces, contracts, collision-prone lookups | **Haiku + graph** | summaries carry verbatim signatures + triggers; tool flags collisions |
| diagnosis, audits, bug-hunting | **Opus + graph**, with grep-style raw reading as a complement | graph triage + targeted code reads; grep still surfaces bugs nothing else does |

The division of labor is the result: the graph makes *facts* cheap and
trustworthy; the strong model makes *judgment* available when a question
needs it; and the one thing neither fully replaces is an adversarial
reader with the patience to read the code it indicts.

---

## 4. Known limitations

- **Dynamic dispatch is summary-level, not edge-level.** Registry-by-slug
  and ORM-event edges exist as recorded *triggers in prose*, not rows in
  `cg_dispatch` (3 rows on this graph). Traversals can't cross them;
  models must notice them in summaries. Emitting these edges from the
  annotator (which reads the registration code anyway) is the top open
  item.
- **Tests are in the graph.** Counts include test callers unless a query
  filters them; rankings should (and the strong model does) report both
  bases. A first-class prod/test split in the tools is open item two.
- **Summaries compress.** Line-level defects (an ignored return value, a
  counter reset) are legitimately absent from good summaries; that is what
  the read-on-diagnosis rule and the raw-reading complement are for.
