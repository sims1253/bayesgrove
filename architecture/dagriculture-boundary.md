# `dagriculture` Boundary

This note records which concerns are intentionally owned by bayesgrove and
which are owned by `dagriculture` as graph-generic primitives.

| Concern | Package owner | Reason |
| --- | --- | --- |
| Graph node and edge storage, state recomputation, and structural planning primitives | `dagriculture` | These are graph-generic execution concerns with no Bayesian or workflow policy semantics. |
| Internal adapter helpers in [`R/dagri-adapters.R`](../R/dagri-adapters.R) | bayesgrove, thin pass-throughs | The adapter layer keeps a stable internal name and concentrates the graph-generic surface bayesgrove relies on; each `bg_dagri_*` wrapper delegates to the canonical `dagriculture::dagri_*` implementation. |
| Descendant, incoming-edge, outgoing-edge, ordered-edge, edge-id, and structural graph-diff helpers | `dagriculture` (>=0.3.0) | These express pure topology queries over graph snapshots and do not mention branches, goals, decisions, or summaries. Moved out of bayesgrove in M5; bayesgrove keeps only pass-through adapters and a smoke test (semantic coverage lives in dagriculture). |
| Public graph editing verbs such as [`bg_add_node()`](../R/graph.R) and [`bg_connect()`](../R/graph.R) | bayesgrove | User-facing API shape, ID generation, persistence, and project commit semantics belong to this package even when they delegate structural mutations. |
| Policy-hold aware planning and external hold propagation | bayesgrove | Holds come from workflow obligations and protocol severity, so they depend on bayesgrove semantics rather than pure graph structure. |
| Branch lineage, workflow scope resolution, and active/inactive branch filtering | bayesgrove | Branch identity and scope semantics are analysis-workflow concepts layered above the raw graph. |
| Summary freshness, decision coverage, and workflow protocol guidance | bayesgrove | These concerns depend on Bayesian review semantics, artifact freshness, and decision provenance. |
| Decision logs, gates, and provenance records | bayesgrove | Provenance is domain-specific context on top of the structural graph, not a generic graph primitive. |

## Decision note (Milestone 5)

Milestone 5 moved the edge-traversal and graph-diff helpers
(`dagri_incoming_edges`, `dagri_outgoing_edges`, `dagri_order_edges`,
`dagri_edge_ids`, `dagri_graph_diff`) from bayesgrove's local implementations
in `bg_dagri_*` to the canonical `dagriculture::dagri_*` exports. The move was
safe because those helpers are pure topology queries with no workflow
semantics; their bayesgrove tests were duplicate coverage of dagriculture's
own suite and were slimmed to a single pass-through smoke test. The
`dagriculture` version pin was raised to `>=0.3.0` to require the moved
exports.

## Decision note: a generic execution layer? (recorded, not implemented)

The larger open question is whether the execution substrate —
fingerprinting, the content-addressed artifact store, run-plan state
derivation, and the jobs log — should also migrate into dagriculture as a
generic execution layer, leaving bayesgrove purely semantic (summaries,
obligations, decisions, holds).

Revisit extraction when a second package needs cached, fingerprinted graph
execution without the review protocol. Until then, keep these interfaces
local to bayesgrove.
