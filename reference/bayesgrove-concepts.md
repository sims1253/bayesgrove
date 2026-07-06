# Concepts: what asks, what answers, what blocks

bayesgrove's question surfaces — gates, obligations, and decisions —
overlap by design. This topic is the one-table disambiguation; the
[`vignette("concepts")`](https://sims1253.github.io/bayesgrove/articles/concepts.md)
article carries the full narrative and diagram.

## Details

|  |  |  |
|----|----|----|
| Concept | Who produces it | What it does |
| **Node** | The user | Pairs code with a graph slot. |
| **Summary** | A built-in executor | Typed evidence (HMC, ppc, loo_pit, sbc, ...). |
| **Obligation** | A workflow pack | A demand raised from a summary (severity: info / warning / blocking). |
| **Action** | A workflow pack | A copy-pasteable call resolving the obligation. |
| **Decision** | The user | The recorded answer (choice + rationale). |
| **Hold** | The engine | Stops downstream execution until a blocking decision lands. |
| **Gate** | The user | A pre-planned checkpoint on an edge. |

Obligations and gates both block, but they are not the same: gates are
*pre-planned* checkpoints the user authors on an edge ("review before
fitting the hierarchical model"); obligations are *evidence-driven* and
arise dynamically from summaries.

## Scopes

Decisions and obligations carry a scope: `project` (whole project) or
`branch:<id>` (one branch). A decision recorded on a branch does not
satisfy a project-scope obligation, and vice versa. Use
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
to see what is outstanding at the relevant scope.

## Evidence freshness

A summary is *fresh* when its fingerprint matches the node's current
configuration, *stale* when the node's params, upstream inputs, or
environment changed since it was written. Packs consume fresh summaries
by default; a stale summary produces no obligation until the node is
rerun.

## See also

[`vignette("concepts")`](https://sims1253.github.io/bayesgrove/articles/concepts.md),
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md),
[`bg_record_decision()`](https://sims1253.github.io/bayesgrove/reference/bg_record_decision.md),
[`bg_add_gate()`](https://sims1253.github.io/bayesgrove/reference/bg_add_gate.md),
[`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md)
