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
#' `params` and `metadata` default to MERGING into the node's existing values
#' via [utils::modifyList], so `bg_update_node(handle, id, params = list(stan_file = f))`
#' updates just `stan_file` and preserves `chains`, `seed`, etc. Pass
#' `replace = TRUE` for intentional wholesale replacement (the historical
#' behavior of `dagriculture::dagri_update_node`).
#'
#' @param project A `bg_handle`.
#' @param node_id The node ID to update.
#' @param label Optional new label.
#' @param params Optional new parameters (merged by default).
#' @param metadata Optional new metadata (merged by default).
#' @param replace Logical scalar, default `FALSE`. When `TRUE`, `params` and
#'   `metadata` REPLACE the existing lists wholesale instead of merging.
#'
#' @return The node ID.
#' @export
bg_update_node <- function(
  project,
  node_id,
  label = NULL,
  params = NULL,
  metadata = NULL,
  replace = FALSE
) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)

  if (!isTRUE(replace)) {
    existing <- graph$nodes[[node_id]]
    if (
      !is.null(params) &&
        is.list(params) &&
        is.list(existing$params %||% list())
    ) {
      params <- utils::modifyList(
        existing$params %||% list(),
        params,
        keep.null = TRUE
      )
    }
    if (
      !is.null(metadata) &&
        is.list(metadata) &&
        is.list(existing$metadata %||% list())
    ) {
      metadata <- utils::modifyList(
        existing$metadata %||% list(),
        metadata,
        keep.null = TRUE
      )
    }
  }

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

#' Attach a data object to a node via the content-addressed store
#'
#' Serializes `data` through the project's CAS store and records a
#' `data_ref = "cas:sha256:..."` param on the node, so the `stan_data` executor
#' can fetch it at run time without an embedded code channel. Existing params
#' are preserved (merged, not replaced). The data object must be serializable
#' via [base::saveRDS()].
#'
#' @param project A `bg_handle`.
#' @param node_id The node to attach data to (typically a `stan_data` node).
#' @param data An R object to attach.
#' @return The node ID, invisibly.
#' @export
bg_set_node_data <- function(project, node_id, data) {
  S7::check_is_S7(project, bg_handle)

  ref <- bg_store_cas_blob(project, data)

  graph <- bg_read_graph(project)
  node <- graph$nodes[[node_id]]
  if (is.null(node)) {
    cli::cli_abort("No node with id {.val {node_id}}.")
  }
  merged_params <- utils::modifyList(
    node$params %||% list(),
    list(data_ref = ref)
  )
  graph <- bg_dagri_update_node(
    graph = graph,
    node_id = node_id,
    params = merged_params
  )
  bg_commit_graph(project, graph)

  invisible(node_id)
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
