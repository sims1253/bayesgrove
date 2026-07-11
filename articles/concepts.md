# Concepts

This vignette is the single mental-model entry point for bayesgrove: it
lays out the moving parts once and shows how they connect. It is aimed
at all audiences (practitioners, researchers, students); the case-study
and reproducibility vignettes build on it.

## The loop in one diagram

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgOTAwIDE1MCIgcm9sZT0iaW1nIiBhcmlhLWxhYmVsbGVkYnk9Imxvb3AtdGl0bGUgbG9vcC1kZXNjIiBzdHlsZT0id2lkdGg6MTAwJTttYXgtd2lkdGg6OTAwcHgiPjx0aXRsZSBpZD0ibG9vcC10aXRsZSI+ClRoZSBiYXllc2dyb3ZlIHJldmlldyBsb29wCjwvdGl0bGU+CjxkZXNjIGlkPSJsb29wLWRlc2MiPk5vZGUgYW5kIGV4ZWN1dG9yIHByb2R1Y2UgYSBzdW1tYXJ5LCB3aGljaCBwcm9kdWNlcwphbiBvYmxpZ2F0aW9uIGFuZCBhY3Rpb24uIEEgcmVjb3JkZWQgZGVjaXNpb24gcmVzb2x2ZXMgdGhlIGhvbGQgYW5kCmFsbG93cyBleGVjdXRpb24gdG8gY29udGludWUuPC9kZXNjPjxkZWZzPjxtYXJrZXIgaWQ9ImFycm93IiB2aWV3Ym94PSIwIDAgMTAgMTAiIHJlZng9IjkiIHJlZnk9IjUiIG1hcmtlcndpZHRoPSI2IiBtYXJrZXJoZWlnaHQ9IjYiIG9yaWVudD0iYXV0by1zdGFydC1yZXZlcnNlIj48cGF0aCBkPSJNIDAgMCBMIDEwIDUgTCAwIDEwIHoiIGZpbGw9IiM1MDYwNzAiIC8+PC9tYXJrZXI+PC9kZWZzPjxnIGZpbGw9IiNmM2Y2ZjgiIHN0cm9rZT0iIzUwNjA3MCIgc3Ryb2tlLXdpZHRoPSIyIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgdGV4dC1hbmNob3I9Im1pZGRsZSI+PHJlY3QgeD0iMTAiIHk9IjM1IiB3aWR0aD0iMTMwIiBoZWlnaHQ9IjUwIiByeD0iOCIgLz48dGV4dCB4PSI3NSIgeT0iNjUiIGZpbGw9IiMxZjI5MzMiIHN0cm9rZT0ibm9uZSI+Tm9kZQorIGV4ZWN1dG9yPC90ZXh0PjxyZWN0IHg9IjE2NSIgeT0iMzUiIHdpZHRoPSIxMTAiIGhlaWdodD0iNTAiIHJ4PSI4IiAvPjx0ZXh0IHg9IjIyMCIgeT0iNjUiIGZpbGw9IiMxZjI5MzMiIHN0cm9rZT0ibm9uZSI+U3VtbWFyeTwvdGV4dD48cmVjdCB4PSIzMDAiIHk9IjM1IiB3aWR0aD0iMTIwIiBoZWlnaHQ9IjUwIiByeD0iOCIgLz48dGV4dCB4PSIzNjAiIHk9IjY1IiBmaWxsPSIjMWYyOTMzIiBzdHJva2U9Im5vbmUiPk9ibGlnYXRpb248L3RleHQ+PHJlY3QgeD0iNDQ1IiB5PSIzNSIgd2lkdGg9IjEwMCIgaGVpZ2h0PSI1MCIgcng9IjgiIC8+PHRleHQgeD0iNDk1IiB5PSI2NSIgZmlsbD0iIzFmMjkzMyIgc3Ryb2tlPSJub25lIj5BY3Rpb248L3RleHQ+PHJlY3QgeD0iNTcwIiB5PSIzNSIgd2lkdGg9IjExMCIgaGVpZ2h0PSI1MCIgcng9IjgiIC8+PHRleHQgeD0iNjI1IiB5PSI2NSIgZmlsbD0iIzFmMjkzMyIgc3Ryb2tlPSJub25lIj5EZWNpc2lvbjwvdGV4dD48cmVjdCB4PSI3MDUiIHk9IjM1IiB3aWR0aD0iOTAiIGhlaWdodD0iNTAiIHJ4PSI4IiAvPjx0ZXh0IHg9Ijc1MCIgeT0iNjUiIGZpbGw9IiMxZjI5MzMiIHN0cm9rZT0ibm9uZSI+SG9sZDwvdGV4dD48L2c+PGcgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjNTA2MDcwIiBzdHJva2Utd2lkdGg9IjIiIG1hcmtlci1lbmQ9InVybCgjYXJyb3cpIj48cGF0aCBkPSJNMTQwIDYwIEgxNjUiIC8+PHBhdGggZD0iTTI3NSA2MCBIMzAwIiAvPjxwYXRoIGQ9Ik00MjAgNjAgSDQ0NSIgLz48cGF0aCBkPSJNNTQ1IDYwIEg1NzAiIC8+PHBhdGggZD0iTTY4MCA2MCBINzA1IiAvPjxwYXRoIGQ9Ik03NTAgODUgVjEyNSBINzUgVjg1IiAvPjwvZz48L3N2Zz4=)

Bayesgrove is a *guided* loop, not a fire-and-forget runner:

1.  A **node** pairs code (an *executor*) with a place in a dependency
    graph.
2.  Running a node produces an artifact and one or more **summaries**
    (typed evidence: an HMC diagnostic, a posterior-predictive check, a
    LOO-PIT calibration, an SBC rank histogram, …).
3.  Workflow **packs** scan summaries and raise **obligations** — “this
    fit has divergences, please review it”, “this posterior-predictive
    p-value is extreme”, “this model has not been compared to its
    alternatives”.
4.  Each obligation becomes an **action** the user can take (record a
    decision, run a comparison node, add a branch).
5.  A **decision** is the user’s recorded answer (choice + rationale).
    Until a decision resolves a *blocking* obligation, the obligation
    places a **hold** on downstream nodes.
6.  A **hold** stops a node from executing. Resolve the obligation and
    the hold lifts; the loop advances.

The defining claim of bayesgrove is step 3 → step 5 → step 6: **evidence
forces a recorded judgment, and the judgment gates execution**. A node
cannot run “past” a warning its operator never acknowledged.

## What asks, what answers, what blocks

| Concept | Who produces it | What it does |
|:---|:---|:---|
| **Node** | The user | Pairs code with a graph slot. |
| **Summary** | An executor (built-in or user-registered) | Typed evidence (HMC, ppc, loo_pit, sbc, …). |
| **Obligation** | A workflow pack | A demand raised from a summary (severity: advisory or blocking). |
| **Action** | A workflow pack | A copy-pasteable call resolving the obligation. |
| **Decision** | The user | The recorded answer (choice + rationale). |
| **Hold** | The engine | Stops downstream execution until a blocking decision lands. |
| **Gate** | The user | A pre-planned checkpoint on an edge (distinct from evidence-driven obligations). |

Obligations and gates overlap but are not the same: gates are
*pre-planned* checkpoints the user authors on an edge (“review before
fitting the hierarchical model”); obligations are *evidence-driven* and
arise dynamically from summaries. Both can block.

## Scopes

Decisions and obligations carry a scope:

- `project` — applies to the whole project.
- `branch:<id>` — applies to a branch.

A decision recorded on a branch does not satisfy a project-scope
obligation, and vice versa. Use `bg_next_actions(handle)` to see what is
outstanding at the relevant scope.

## Evidence freshness

A summary is *fresh* when its fingerprint matches the node’s current
configuration; it is *stale* when the node’s params, upstream inputs, or
environment changed since the summary was written. Packs consume fresh
summaries by default; a stale summary produces no obligation until the
node is rerun. Fingerprints pin the environment (Stan file hash, package
versions, seeds) so “same node” is meaningful across sessions and
machines.

## Glossary

| Term | Meaning |
|:---|:---|
| Artifact | The persisted (content-addressed) output of running a node. |
| Executor | The function bayesgrove calls with `(node, inputs)` to produce an artifact. |
| Fingerprint | A deterministic hash of a node’s params + upstream + environment. |
| Pack | A bundle of (matcher, evaluator) pairs that raise obligations. |
| Wave | The set of nodes ready to execute simultaneously (inputs satisfied). |
| Hold | A block on a node from an unresolved blocking obligation. |

See also: `?bayesgrove-concepts` (the help-topic version of this table),
[`vignette("getting-started")`](https://sims1253.github.io/bayesgrove/articles/getting-started.md),
and
[`vignette("eight-schools")`](https://sims1253.github.io/bayesgrove/articles/eight-schools.md).

## Where bayesgrove ends and dagriculture begins

bayesgrove does not implement its own graph engine. Node and edge
storage, state recomputation, and the structural planning primitives
live in the `dagriculture` package, behind a thin internal adapter
layer. Everything in this vignette that carries *meaning* — branches,
goals, scopes, summaries, obligations, decisions, holds, gates,
freshness — is bayesgrove’s own layer on top: those are
analysis-workflow concepts, not graph concepts, and pushing them down
into a generic graph package would leak Bayesian review policy into
structural machinery. The public editing verbs
([`bg_add_node()`](https://sims1253.github.io/bayesgrove/reference/bg_add_node.md),
[`bg_connect()`](https://sims1253.github.io/bayesgrove/reference/bg_connect.md))
stay in bayesgrove because API shape, persistence, and project commit
semantics belong here even when the structural mutation is delegated.
Contributors can find the full ownership table in
`architecture/dagriculture-boundary.md` in the source repository.

## Why not targets or workflowr?

A common question, answered here once:

- **`targets`** caches computation (a build system for R analyses) but
  records no *decisions*. It will happily re-run a model whose
  diagnostics you never read. bayesgrove’s claim is the enforced link
  between *evidence* (typed summaries) and *recorded judgment*
  (decisions), so a node cannot run past a warning its operator never
  acknowledged.

- **`workflowr`** versions notebooks and renders reproducible HTML, but
  it does not *read* diagnostics — it organizes a manual workflow.
  bayesgrove reads the diagnostics, raises obligations from them, and
  gates execution on the responses.

bayesgrove composes with both (a `targets` pipeline can call
[`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md);
`workflowr` can render a bayesgrove report), but its distinguishing job
is *decision provenance*: an audit trail that ties every prior change to
a rationale and every warning to a review.
