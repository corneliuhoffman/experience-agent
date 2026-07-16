# The lemur benchmark campaign (July 2026)

A record of the urme call-graph benchmark arc on **lemur** (Netflix's
certificate-management service: Flask + Celery + a slug-keyed plugin
architecture; ~2,300 functions), with side excursions into **leakcanary**
(Kotlin). What was measured, what broke, what got fixed, and where the
design landed.

## Setup

- **21 questions** (`~/bench-questions.md`): structure/rankings (R1–R4),
  flow traces (Q5–Q9), blast radius (BR1–BR5), diagnosis/gotchas (G1–G4),
  and name-collision traps (N1–N4, e.g. "the `get` accessor" — 72 distinct
  functions named `get`).
- **Arms**: model ∈ {Haiku 4.5, Opus 4.8} × tooling ∈ {urme graph via MCP,
  no-tooling (grep/read/self-built scripts)}.
- **Clean-arm protocol** (several early runs were invalidated without it):
  the no-tooling arm must run with `--strict-mcp-config --mcp-config
  empty.json` and with `.urme/`, `CLAUDE.md`, `*.dot`, `.opengrep/`
  stashed out of the repo. The graph arm needs them restored.
- **Cost**: computed from per-message usage × rate card, deduped by
  message id, subagents included. Cache writes must be split 5m ($6.25/MTok
  Opus) vs 1h ($10/MTok) via `usage.cache_creation` — pricing all writes at
  the 5m rate undercounts ~10–15%. Validated to the cent against `/usage`
  on single-agent runs; runs with subagents still read ~5–10% low (subagent
  transcripts expose only the flat cache-write field).

## Arc 1: the graph loses, and why

Early adjudications (independent agents verifying both arms' answers
against source) were bad for the graph: on the original graph,
**grep-Opus was more correct on ~15/21** vs Haiku+urme, and beat
Opus+urme on leakcanary too. Root causes, all verified at the SQL/source
level:

1. **Static edge recall.** opengrep resolved module-qualified calls
   (`metrics.send` → 164 edges, exact) but dropped:
   - `self.method()` resolving to *inherited* methods — `IPlugin.get_option`
     had **5 of 92** call sites (87 dropped; every plugin subclass calls it
     via `self.`, and it's a staticmethod-called-via-instance). Cost the R4
     question outright.
   - (Kotlin) calls inside **lambda/enum-entry bodies** — `SharkLog.d {…}`
     undercounted 3.6× (35 vs 127 sites); live functions false-flagged dead.
2. **`cg_dispatch` was Celery-only.** `dispatch_cues` matched
   `.delay/.apply_async/...` and nothing else: **3 dispatch edges in the
   whole graph**. The plugin registry (`plugins.get(slug).upload()`),
   SQLAlchemy `@event.listens_for`, and blinker signals were structurally
   invisible. `update_destinations` was an island severed at both joints
   (no inbound event edge, no outbound registry edge) — lost Q7 every time.
3. **Summary quality (Haiku annotations).** Paraphrased/reordered
   signatures (`get(model, field, value)` vs real `get(model, value,
   field="id")`), one invented gotcha, and terseness that forced follow-up
   queries (the Opus-summary graph was *cheaper to query*: $1.95 vs $3.81).
4. **Weak-model behaviors** (each later cured by a mechanical rule):
   punting to earlier answers instead of querying (the N4 "already
   answered" miss), merging adjacent facts into confident hybrids (the
   BR2 4-tuple), hand-writing recursive SQL alongside `graph_blast_radius`
   (~70% cost inflation).

A decidability analysis sorted the edge gaps: `self.`-method resolution is
**fully static** (opengrep should do it — and was fixed); registry-by-slug
resolves only to a **candidate set** (CHA over-approximation); ORM-event
edges need **framework knowledge or a reader** (the annotator). LSP
servers (pyright, pyrefly-with-config) resolve the static class perfectly
(both found ~90 of get_option's 92 refs; probe: `documentSymbol` +
`references` is a sufficient extractor contract), but opengrep exports the
lemur graph in 0.62s vs ~30s for an LSP sweep — verdict: keep opengrep as
extractor, use LSP references as a recall *oracle* for regression gates.

## Arc 2: the fixes

| Fix | Where | Effect (verified) |
|---|---|---|
| MRO/self-method resolution | opengrep (user) | `get_option` 5 → 104 edges / 41 callers |
| Hardened annotation prompt: verbatim signatures, returns incl. None, registration triggers, evidence-gated gotchas | `bin/main.ml` `annotate_system` (4053b61) | N3/N4 signatures exact; Q7 trigger described in summary; no invented gotchas |
| `ambiguous:true` + loud note on name collisions | `graph_blast_radius` (33fdf75) | 72-way `get` can't be reported as one number |
| Recursive-SQL redirect to blast_radius | `graph_query` (f09f399) | redundant closures eliminated |
| "Always run blast_radius on the named symbol" + collision warning | server_instructions | N4-class punts stopped |
| Contracts must be re-quoted from a fresh `graph_describe`, never recomposed from memory | server_instructions (7c158ab) | BR2 4-tuple hybrid gone (17 describes in the verifying run) |
| Diagnosis-class questions triage via graph then **read the implicated code** (`code_only`); never assert file mechanics without summary-or-code backing | server_instructions + CLAUDE.md (9d0a86c) | targets the fabrication + G-depth class |

**`urme annotate` pipeline** (the "stupid workflows" problem → a
deterministic loop) accumulated its own fix stack, each found by a real
failure: fresh one-shot process per unit — daemons accumulate conversation
history, quadratic cost (c890590); continuous work queue + elapsed/rate/ETA
progress (fc66e23, 2713d1f); **fd leak** — spawn never closed stdout/stderr
channels, EMFILE after ~100 units = the silent stalls (66236d3); 300s ask
timeout; 10s failure backoff (72656b7); 40-fn chunk cap + max-turns 4 —
100+ tiny test fns overflowed one output turn (037cf75); **strip MCP from
workers** — `--tools ""` doesn't stop the repo's own urme MCP server
attaching, and Haiku burned turns calling graph tools (`error_max_turns`)
(56d4b66); pack multiple ready SCCs per call (a522cfd). Result: lemur
2,292 fns from scratch in **16 min, ~$1.50** at `-j 8` (~110 fn/min;
Haiku). GitLab projection (graph already built: 84,552 fns / 22,330
files): ~5–6 h at `-j 16`, ~$55–75 — needs condensation caching in the
fetcher first (it currently recomputes the file-SCC DAG per refill).

macOS operational trap: never `cp` over an installed binary in place — the
kernel SIGKILLs on stale code signature. `rm` + copy + `codesign -s - -f`.
Shell resolves urme from the 5.5 opam switch; dune installs to 5.3.

## Arc 3: the rematch (same tree, all fixes in)

Rebuilt graph: fixed edges + hardened Haiku summaries, 2,292 nodes
(tests now included). Corrected costs.

| arm | cost | notes |
|---|---|---|
| **Haiku + urme** | **$0.71** | every number db-exact (156/41/25 verified); N3/N4 signatures verbatim; 72-way collision disambiguated; contract re-quoting fixed BR2. Loses only judgment depth. |
| **Opus + urme** | **$4.73–6.04** | citations ~30/30 exact; wins every counting question; found Q7's event-listener via summaries; run 3 checked wiring instead of fabricating and found the `g.user.username` Celery crash |
| **Opus + grep (fresh)** | **$5.35** | built its own name-based AST graph ("same-named methods collapse — good enough"), then paid for it: `filter` inflated 63 vs 13 real, false dead-code row; but found 5 verified unique bugs by reading |

**Final same-tree adjudication (Opus vs Opus): grep 8 — urme 5 — even 8.**
The split is exact and is the campaign's central finding:

- **The graph wins every *counting* problem** (rankings, dead code,
  caller sets, blast radius): its numbers are provably faithful; grep's
  disposable graph inflates on name collisions.
- **Reading wins every *diagnosis* problem**: five verified bugs (counter
  reset, `RoleNeed("creator")` revoke bypass, dead `RoleNeed(owner)`
  comparison, ignored ACME revoke failure, email silent success) lived in
  lines summaries legitimately compress away — and "trust the summaries,
  don't read" produced one repeated fabrication (a lemur_slack
  `__init__.py` registration story).
- Cost between Opus arms is **noise** ($4.73–6.04 vs $5.35). The real
  economics: the graph's $1.50 build converts routine queries into
  **$0.71 Haiku sessions at Opus-grep factual quality** — ~7.5× cheaper —
  while grep re-derives (and re-errs) from scratch every session.

Bug-finding is high-variance for everyone: four live bugs found by one
grep run were refound by **neither** arm in the next round.

## Where the design landed

- Structure/counts/impact → graph, always. Diagnosis → graph triage,
  then read the implicated functions (`code_only`). Contracts → re-quote,
  never recompose. Collisions → the tool flags them.
- Annotation: Haiku with the hardened prompt is the default; summaries
  carry verbatim signatures and registration triggers (which is how Q7
  became findable without an edge).

## Coda: guidance consolidation (c378d93)

The instruction block had accreted a paragraph per lesson (~700 words).
On Haiku that length itself became the failure: a run under the full
block ignored the new diagnosis rule entirely, dropped 4 questions, lost
previously-correct answers, asked a mid-run "token constraints" question,
and cost $1.06. Rewritten as 8 numbered rules (~300 words, every lesson
kept) plus a slimmed CLAUDE.md (the doctrine now lives once, not twice):

- **Haiku: $0.33, 3 min, 21/21** — cheapest run of the campaign, full
  compliance (collision-led N3/N4, exact BR2 contract, get_option in
  rankings), parallel-batched tool calls. `code_only` still never fires:
  diagnosis-by-reading is Haiku's ceiling, not an instruction problem.
- **Opus: $4.75, 21/21** — rule 4b (diagnosis triage → read the code)
  fires reliably (5 code_only pulls on exactly the implicated functions),
  the fabrication class is gone, and it landed the 90-day notification
  query ceiling — a find that had been grep-exclusive in every prior
  round — plus a new dead-code-corroborating bug (ultradns
  get_zone_name missing-arg TypeError).

Final tiering: route facts/structure/contracts to Haiku+graph ($0.33);
route diagnosis/audit questions to Opus+graph ($4.75), which now does
reading-class discovery the summaries-only doctrine used to forbid.

## Open items, by payoff

1. **Annotator-emitted dispatch edges** — the model reads
   `@event.listens_for` / `setup.py` entry-points during annotation
   anyway; have it emit the resolved edge instead of leaving prose. Kills
   the Q7-class island structurally. (`dispatch_cues` extension is a
   treadmill; CHA over-approximation is the static fallback.)
2. **Prod/test split in tools** — tests in the graph polluted R1/R2 for
   the weak model and inflate every closure; `by_file`/blast_radius
   should report both bases.
3. **leakcanary rebuild** with fixed opengrep — retest Kotlin (its losses
   were all lambda/enum + interface-dispatch recall; unknown how much the
   fix covers).
4. **Condensation caching** in `next_ready_file_units` before any
   GitLab-scale annotate.
5. **LSP oracle harness** — pyright/pyrefly `references` diffed against
   opengrep edges as a per-repo recall regression gate
   (`get_option == 92` is the canonical check).
