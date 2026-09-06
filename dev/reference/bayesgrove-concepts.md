# Concepts: what asks, what answers, what blocks

Workflow packs request reviews based on diagnostic summaries and project
context. Blocking obligations hold downstream computation until
resolved. See
[`vignette("concepts")`](https://sims1253.github.io/bayesgrove/dev/articles/concepts.md)
for the review flow and glossary.

## Details

|  |  |  |
|----|----|----|
| Concept | Who produces it | What it does |
| **Node** | The user | A computation and its dependencies. |
| **Summary** | An executor (built-in or user-registered) | Typed evidence (HMC, ppc, loo_pit, sbc, ...). |
| **Obligation** | A workflow pack | Work the analysis needs, such as reviewing divergences. |
| **Action** | A workflow pack | A suggested step, such as recording a decision or adding a check. |
| **Decision** | The user | The recorded answer (choice + rationale). |
| **Hold** | The engine | Prevents a node from running. |
| **Gate** | The user | A pre-planned checkpoint on an edge. |

Use a gate when you know in advance that a step needs sign-off. Workflow
packs raise obligations as the analysis develops. A decision can resolve
a review; other obligations need new evidence. Advisory obligations do
not hold execution.

## Scopes

Decisions and obligations carry a scope: `project` (whole project) or
`branch:<id>` (one branch). A decision recorded on a branch does not
satisfy a project-scope obligation, and vice versa. Use
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_next_actions.md)
to see what is outstanding at the relevant scope.

## Evidence freshness

A summary is *fresh* when its fingerprint matches the node's current
configuration, *stale* when the node's params, upstream inputs, or
environment changed since it was written. Packs consume fresh summaries
by default; a stale summary produces no obligation until the node is
rerun.

## See also

[`vignette("concepts")`](https://sims1253.github.io/bayesgrove/dev/articles/concepts.md),
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_next_actions.md),
[`bg_record_decision()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_record_decision.md),
[`bg_add_gate()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_add_gate.md),
[`bg_run()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_run.md)
