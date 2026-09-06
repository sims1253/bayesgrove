# `dagriculture` boundary

`dagriculture` owns graph structure and planning. bayesgrove owns project
persistence and the review workflow.

| Concern | Package owner | Reason |
| --- | --- | --- |
| Graph node and edge storage, state recomputation, and structural planning | `dagriculture` | These operations do not depend on Bayesian models or review rules. |
| Internal adapters in [`R/dagri-adapters.R`](../R/dagri-adapters.R) | bayesgrove | Each `bg_dagri_*` wrapper calls a `dagriculture::dagri_*` function. The wrappers give bayesgrove stable internal names for these dependencies. |
| Descendant, incoming-edge, outgoing-edge, ordered-edge, edge-id, and graph-diff helpers | `dagriculture` (>=0.3.0) | These query graph topology without using branches, goals, decisions, or summaries. dagriculture tests their behavior; bayesgrove tests the adapters. |
| Public editing functions such as [`bg_add_node()`](../R/graph.R) and [`bg_connect()`](../R/graph.R) | bayesgrove | bayesgrove defines the public API, generates IDs, and persists edits through project commits. |
| Planning with policy holds and propagating external holds | bayesgrove | Holds depend on workflow obligations and protocol severity. |
| Branch lineage, scope resolution, and active/inactive branch filtering | bayesgrove | Branch identity and scope belong to the analysis workflow. |
| Summary freshness, decision coverage, and workflow guidance | bayesgrove | These use review rules, artifact freshness, and decision records. |
| Decision logs, gates, and provenance records | bayesgrove | These record analysis decisions rather than graph structure. |

## Decision note (Milestone 5)

Milestone 5 moved the edge-traversal and graph-diff helpers
(`dagri_incoming_edges`, `dagri_outgoing_edges`, `dagri_order_edges`,
`dagri_edge_ids`, `dagri_graph_diff`) from bayesgrove's local implementations
in `bg_dagri_*` to the `dagriculture::dagri_*` exports. Those helpers query
topology without using workflow rules. Their tests duplicated dagriculture's
tests, so bayesgrove retains one adapter smoke test. The
`dagriculture` version pin was raised to `>=0.3.0` to require the moved
exports.

## Decision note: a generic execution layer? (recorded, not implemented)

Fingerprinting, the content-addressed artifact store, run-plan state
derivation, and the jobs log could also move to dagriculture. bayesgrove
would retain summaries, obligations, decisions, and holds.

Revisit extraction when a second package needs cached, fingerprinted graph
execution without the review protocol. Until then, keep these interfaces
local to bayesgrove.
