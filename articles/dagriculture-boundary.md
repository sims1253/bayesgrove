# Dagriculture Boundary

This page mirrors the contributor-facing architecture note at
`architecture/dagriculture-boundary.md` so pkgdown can expose it as a
first-class site page.

| Concern                                                                                                                                                                                                  | Package owner                                                      | Reason                                                                                                                                                                 |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Graph node and edge storage, state recomputation, and structural planning primitives                                                                                                                     | `dagriculture`                                                     | These are graph-generic execution concerns with no Bayesian or workflow policy semantics.                                                                              |
| Internal adapter helpers in `R/22-dagri-adapters.R`                                                                                                                                                      | bayesgrove, with extraction candidates called out helper-by-helper | The adapter layer keeps the current dependency explicit while concentrating the graph-generic surface bayesgrove actually relies on.                                   |
| Descendant, incoming-edge, outgoing-edge, ordered-edge, and structural graph-diff helpers                                                                                                                | Candidate to move to `dagriculture`                                | These helpers express pure topology queries over graph snapshots and do not mention branches, goals, decisions, or summaries.                                          |
| Public graph editing verbs such as [`bg_add_node()`](https://sims1253.github.io/bayesgrove/reference/bg_add_node.md) and [`bg_connect()`](https://sims1253.github.io/bayesgrove/reference/bg_connect.md) | bayesgrove                                                         | User-facing API shape, ID generation, persistence, and project commit semantics belong to this package even when they delegate structural mutations.                   |
| Policy-hold aware planning and external hold propagation                                                                                                                                                 | bayesgrove                                                         | Holds come from workflow obligations and protocol severity, so they depend on bayesgrove semantics rather than pure graph structure.                                   |
| Branch lineage, workflow scope resolution, and active/inactive branch filtering                                                                                                                          | bayesgrove                                                         | Branch identity and scope semantics are analysis-workflow concepts layered above the raw graph.                                                                        |
| Summary freshness, decision coverage, and workflow protocol guidance                                                                                                                                     | bayesgrove                                                         | These concerns depend on Bayesian review semantics, artifact freshness, and decision provenance.                                                                       |
| Decision logs, gates, and provenance records                                                                                                                                                             | bayesgrove                                                         | Provenance is domain-specific context on top of the structural graph, not a generic graph primitive.                                                                   |
| A cross-repo migration in this phase                                                                                                                                                                     | Explicitly not in scope                                            | This repository does not carry the `dagriculture` source, so Phase 7 stops at boundary documentation, internal adapters, and tests that clarify what is graph-generic. |

## Why this split

bayesgrove already uses `dagriculture` as its structural execution
engine, but Phase 7 makes that boundary easier to defend:

- graph-generic queries now sit behind a small internal adapter layer;
- candidate extraction points are visible in one place instead of being
  scattered inline; and
- workflow semantics stay local, avoiding an architectural leak where
  Bayesian review rules would be pushed into a generic graph package.
