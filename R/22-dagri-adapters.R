# Internal adapter layer for graph-generic operations that are plausible
# migration candidates for `dagriculture`.

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

bg_dagri_add_edge <- function(
  graph,
  from,
  to,
  type = "data",
  id,
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

bg_dagri_update_node <- function(
  graph,
  node_id,
  label = NULL,
  params = NULL,
  metadata = NULL
) {
  dagriculture::dagri_update_node(
    graph = graph,
    node_id = node_id,
    label = label,
    params = params,
    metadata = metadata
  )
}

bg_dagri_remove_node <- function(graph, node_id) {
  dagriculture::dagri_remove_node(
    graph = graph,
    node_id = node_id
  )
}

bg_dagri_incoming_edges <- function(graph, node_id) {
  Filter(function(edge) identical(edge$to, node_id), graph$edges %||% list())
}

bg_dagri_outgoing_edges <- function(graph, node_id) {
  Filter(function(edge) identical(edge$from, node_id), graph$edges %||% list())
}

bg_dagri_order_edges <- function(edges) {
  if (length(edges) <= 1) {
    return(edges)
  }

  edge_ids <- vapply(
    edges,
    function(edge) edge$id %||% "",
    character(1)
  )
  edges[order(edge_ids)]
}

bg_dagri_descendants <- function(graph, node_id) {
  dagriculture::dagri_descendants(graph, node_id)
}

bg_dagri_recompute_state <- function(graph) {
  dagriculture::dagri_recompute_state(graph)
}

bg_dagri_plan <- function(graph, targets = NULL, external_holds = list()) {
  dagriculture::dagri_plan(
    graph,
    targets,
    external_holds = external_holds
  )
}

bg_dagri_graph_diff <- function(graph_before, graph_after) {
  before_nodes <- names(graph_before$nodes %||% list())
  after_nodes <- names(graph_after$nodes %||% list())
  before_edges <- names(graph_before$edges %||% list())
  after_edges <- names(graph_after$edges %||% list())

  # Migration candidate: pure structural diff with no workflow semantics.
  list(
    added_nodes = setdiff(after_nodes, before_nodes),
    removed_nodes = setdiff(before_nodes, after_nodes),
    added_edges = setdiff(after_edges, before_edges),
    removed_edges = setdiff(before_edges, after_edges)
  )
}
