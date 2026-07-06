# Adapter layer for graph-generic operations.
#
# The edge-diff and edge-traversal helpers below are thin pass-throughs to
# `dagriculture` (>=0.3.0), which now owns them as the canonical `dagri_*`
# implementations. Bayesgrove keeps the `bg_dagri_*` wrappers so internal call
# sites read consistently and so a future dagriculture API change has one
# adapter to update rather than N call sites.

# --- Node Operations ---

#' @keywords internal
bg_dagri_add_node <- function(
  graph,
  id,
  kind,
  label = NULL,
  params = list(),
  metadata = list()
) {
  dagriculture::dagri_add_node(
    graph = graph,
    id = id,
    kind = kind,
    label = label,
    params = params,
    metadata = metadata
  )
}

# --- Edge Operations ---

#' @keywords internal
bg_dagri_add_edge <- function(
  graph,
  from,
  to,
  id,
  type = "data",
  metadata = list()
) {
  dagriculture::dagri_add_edge(
    graph = graph,
    from = from,
    to = to,
    type = type,
    id = id,
    metadata = metadata
  )
}

# --- Node Update / Removal ---

#' @keywords internal
bg_dagri_update_node <- function(
  graph,
  node_id,
  label = NULL,
  params = NULL,
  metadata = NULL
) {
  dagriculture::dagri_update_node(
    graph = graph,
    id = node_id,
    label = label,
    params = params,
    metadata = metadata
  )
}

#' @keywords internal
bg_dagri_remove_node <- function(graph, node_id) {
  dagriculture::dagri_remove_node(
    graph = graph,
    id = node_id
  )
}

# --- Edge Traversal & Diffing (owned by dagriculture >=0.3.0) ---

#' @keywords internal
bg_dagri_incoming_edges <- function(graph, node_id) {
  dagriculture::dagri_incoming_edges(graph, node_id)
}

#' @keywords internal
bg_dagri_outgoing_edges <- function(graph, node_id) {
  dagriculture::dagri_outgoing_edges(graph, node_id)
}

#' @keywords internal
bg_dagri_order_edges <- function(edges) {
  dagriculture::dagri_order_edges(edges)
}

# --- Graph Traversal ---

#' @keywords internal
bg_dagri_descendants <- function(graph, node_id) {
  dagriculture::dagri_descendants(graph, node_id)
}

# --- Graph State & Planning ---

#' @keywords internal
bg_dagri_recompute_state <- function(graph) {
  dagriculture::dagri_recompute_state(graph)
}

#' @keywords internal
bg_dagri_plan <- function(graph, targets = NULL, external_holds = list()) {
  dagriculture::dagri_plan(
    graph,
    targets,
    external_holds = external_holds
  )
}

#' @keywords internal
bg_dagri_edge_ids <- function(edges) {
  dagriculture::dagri_edge_ids(edges)
}

# --- Graph Diffing ---

#' @keywords internal
bg_dagri_graph_diff <- function(graph_before, graph_after) {
  dagriculture::dagri_graph_diff(graph_before, graph_after)
}
