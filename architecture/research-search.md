# Research-search contract

The experimental interface has four operations:

```text
bg_research_init    establish the question, goal, and policy
bg_research_state   read candidates, evidence, comparisons, decisions, and history
bg_research_propose validate an action and report applicable findings
bg_research_apply   revalidate a current proposal and atomically record it
```

Glade and agents call the same interface as R users. No web server, client-owned
copy of the research state, or agent-specific research operations are required.
The Glade restart review and its Positron prototype informed this choice: the
prototype already uses the researcher's R session and rejects stale submissions.
No open PRs were present in either repository when this implementation began.

## Scope and compatibility

This is an experimental search primitive alongside the existing graph-based
workflow. It does not reinterpret old branches as candidates, alter `bg_run()`,
or migrate existing decision records. Callers can attach evidence obtained with
the existing executors or with other tools. They must specify what P, A, and D
actually produced that evidence. Bayesgrove cannot verify those assertions by
reading an arbitrary Stan program or a caller-supplied result.

A candidate is data, not executable source. Store source contents, data versions,
and approximation settings explicitly when needed to reconstruct the candidate.
A path alone does not preserve a file. Large fit artifacts can remain in the
existing artifact store; a reference in a research result is a reference, not an
additional copy. Bundles preserve the research document, but existing rules for
excluding fits and external files still apply.

## Actions

Every proposal requires a declared actor (`id`, `type`) and a non-empty rationale.
The `action` is a named list. Unknown fields are errors. Every action may also
supply `basis`, a named list of `evidence_ids`, `comparison_ids`, or `decision_ids`
that motivated it. These must refer to existing records and remain in history.

| kind | Required fields besides kind | Meaning |
| --- | --- | --- |
| `create` | `label`, `components` | Create a root candidate. |
| `revise` | `candidate_id`, `label`, `components` | Replace the supplied P/A/D components in a descendant. Other components are inherited. |
| `evidence` | `candidate_id`, `label`, `check`, `utility`, `result` | Attach a result to an exact candidate. |
| `compare` | `candidate_ids`, `evidence_ids`, `criteria`, `result` | Record a comparison covering at least two candidates, including closed ones. |
| `review` | `candidate_id`, `evidence_ids` | Record review of specific evidence. |
| `accept`, `reject` | `candidate_id`, `evidence_ids` | Record a judgment for the investigation's goal. Rejection does not close a lineage. |
| `note` | `candidate_id` | Record reasoning without changing a candidate. |
| `close`, `reopen` | `candidate_id` | Close or reopen a lineage with the proposal's rationale. |
| `configure` | `mode`, `rules` | Replace intervention settings. Requires a declared human actor. |

Each successful operation returns the full state. The last history entry carries
the generated `id`; for create, revise, evidence, compare, and decisions, this is
also the new record's ID. A root requires P. A and D can be absent or explicitly
cleared in a revision. Components are replaced whole, not recursively merged.
A revision that changes nothing is rejected.

Plain lists, strings, finite numbers, logical values, and NULL are supported.
Named fields must be unique. Arrays are lists on reading. Executable functions,
environments, classed objects, matrices, arrays, other attributes except names,
NA, and infinite values are rejected instead of
being silently converted by JSON. Convert results into an explicit record first.

Closed ancestors prevent evidence collection, revision, and acceptance beneath
them. Historical notes, reviews, rejections, and comparisons remain possible.
Reopening a descendant cannot bypass a closed ancestor. Review evidence must
belong to the candidate named by the decision; reviews do not transfer across
revisions. New evidence is not covered by earlier reviews.

## Policy

Record preserves actions without generating policy findings. Guide returns
findings but allows actions. Enforce returns the same findings and prevents an
operation when any applicable rule remains unmet. Structural invariants, such
as closed-lineage and stale-proposal checks, apply in all modes.

Rules have `id`, `actions`, `type`, and `message`. Optional `explanation`,
`suggestion`, `interpretation`, and `references` provide guidance without a UI.
References are source strings; the engine does not fetch or verify them.

| type | Additional fields | Requirement |
| --- | --- | --- |
| `require_values` | `path`, `values` | The candidate field contains every specified value. |
| `forbid_values` | `path`, `values` | The candidate field contains none of the specified values. |
| `require_evidence` | `check` | This candidate has evidence with that check name. |
| `require_review` | `check` | All recorded evidence for that check has human review. Missing evidence also leaves the requirement unmet. |

A path is a sequence such as `c("P", "covariates")`. Specification rules inspect
the proposed descendant for a revision. Evidence rules refer to exact candidate
IDs, so parent evidence cannot satisfy a requirement for a new revision.
Comparison rules inspect all selected candidates. Evidence presence does not
mean a check passed, and review does not certify scientific validity. There is
no default model ranking, scalar reward, or automatic acceptance threshold.

Mode and rule changes are historical operations. They do not resolve concerns
or rewrite earlier decisions. `allowed` and `findings` in a submitted proposal
are never trusted: apply recomputes them against the current state and policy.

Actors are caller-declared provenance. Human-review requirements and human-only policy changes are
protocol checks, not authentication. A host exposing this interface to untrusted
agents must bind identity and authorize policy changes itself. Direct R access
can also call legacy graph operations; this API is not a sandbox.

## Persistence and handoff

The state lives at `.bayesgrove/workflow/research.json`. A write requires the
project writer lock, a current proposal version, and an atomic replacement under
a document lock. A failure leaves the prior research state intact. Replaying a
proposal after a successful write is stale. After an ambiguous response, inspect
the history before proposing anything again.

`bg_snapshot()` includes research state. Project bundles include it, and exported
reports contain candidate ancestry, operation history, and the exact research
records. Older projects return NULL research state and need no migration.

The vocabulary draws on the PAD taxonomy and goal-dependent utilities in
[Bürkner, Scholz, and Radev](https://arxiv.org/abs/2209.02439). The action and
storage contracts here are software design decisions, not claims that the paper
prescribes this implementation.
