#' Add a node to the bayesgrove project graph
#'
#' @param project A `bg_handle`.
#' @param kind The node kind string.
#' @param label Optional label.
#' @param params Named list of parameters.
#' @param inputs Optional character vector of upstream node IDs to connect.
#' @param metadata Optional metadata list.
#'
#' @return The generated node ID.
#' @export
bg_add_node <- function(
  project,
  kind,
  label = NULL,
  params = list(),
  inputs = NULL,
  metadata = list()
) {
  S7::check_is_S7(project, bg_handle)

  node_id <- bg_new_id("node")

  graph <- bg_read_graph(project)

  # Add the node
  graph <- bg_dagri_add_node(
    graph = graph,
    id = node_id,
    kind = kind,
    label = label,
    params = params,
    metadata = metadata
  )

  # Connect inputs if provided
  if (!is.null(inputs)) {
    for (input in inputs) {
      edge_id <- bg_new_id("edge")
      graph <- bg_dagri_add_edge(
        graph = graph,
        from = input,
        to = node_id,
        type = "data",
        id = edge_id
      )
    }
  }

  # Commit
  bg_commit_graph(project, graph)

  node_id
}

#' Connect two nodes in the bayesgrove project graph
#'
#' @param project A `bg_handle`.
#' @param from The upstream node ID.
#' @param to The downstream node ID.
#' @param edge_type The type of edge (default: "data").
#' @param metadata Optional metadata list.
#'
#' @return The generated edge ID.
#' @export
bg_connect <- function(
  project,
  from,
  to,
  edge_type = "data",
  metadata = list()
) {
  S7::check_is_S7(project, bg_handle)

  edge_id <- bg_new_id("edge")

  graph <- bg_read_graph(project)

  graph <- bg_dagri_add_edge(
    graph = graph,
    from = from,
    to = to,
    type = edge_type,
    id = edge_id,
    metadata = metadata
  )

  bg_commit_graph(project, graph)

  edge_id
}

#' Update a node in the bayesgrove project graph
#'
#' @param project A `bg_handle`.
#' @param node_id The node ID to update.
#' @param label Optional new label.
#' @param params Optional new parameters.
#' @param metadata Optional new metadata.
#'
#' @return The node ID.
#' @export
bg_update_node <- function(
  project,
  node_id,
  label = NULL,
  params = NULL,
  metadata = NULL
) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)

  graph <- bg_dagri_update_node(
    graph = graph,
    node_id = node_id,
    label = label,
    params = params,
    metadata = metadata
  )

  bg_commit_graph(project, graph)

  node_id
}

#' Remove a node from the bayesgrove project graph
#'
#' @param project A `bg_handle`.
#' @param node_id The node ID to remove.
#'
#' @return The removed node ID, invisibly.
#' @export
bg_remove_node <- function(project, node_id) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)

  graph <- bg_dagri_remove_node(
    graph = graph,
    node_id = node_id
  )

  bg_commit_graph(project, graph)

  invisible(node_id)
}
