# Internal adapter layer for graph-generic operations that are plausible
# migration candidates for `dagriculture`.

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

#' @keywords internal
bg_dagri_incoming_edges <- function(graph, node_id) {
  Filter(function(edge) identical(edge$to, node_id), graph$edges %||% list())
}

#' @keywords internal
bg_dagri_outgoing_edges <- function(graph, node_id) {
  Filter(function(edge) identical(edge$from, node_id), graph$edges %||% list())
}

#' @keywords internal
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

# Prefer container names because `dagriculture` stores edges keyed by id today,
# but fall back to the embedded `edge$id` so diffs still work if edges become
# unnamed lists.
#' @keywords internal
bg_dagri_edge_ids <- function(edges) {
  if (length(edges) == 0) {
    return(character())
  }

  edge_names <- names(edges) %||% rep("", length(edges))
  if (all(nzchar(edge_names))) {
    return(sort(unique(edge_names)))
  }

  edge_ids <- vapply(
    edges,
    function(edge) edge$id %||% "",
    character(1)
  )
  if (!all(nzchar(edge_ids))) {
    cli::cli_abort(
      "Graph edges must be named or carry non-empty `id` fields for diffing."
    )
  }

  sort(unique(edge_ids))
}

# --- Graph Diffing ---

#' @keywords internal
bg_dagri_graph_diff <- function(graph_before, graph_after) {
  before_nodes <- names(graph_before$nodes %||% list())
  after_nodes <- names(graph_after$nodes %||% list())
  before_edges <- bg_dagri_edge_ids(graph_before$edges %||% list())
  after_edges <- bg_dagri_edge_ids(graph_after$edges %||% list())

  # Migration candidate: pure structural diff with no workflow semantics.
  list(
    added_nodes = setdiff(after_nodes, before_nodes),
    removed_nodes = setdiff(before_nodes, after_nodes),
    added_edges = setdiff(after_edges, before_edges),
    removed_edges = setdiff(before_edges, after_edges)
  )
}
