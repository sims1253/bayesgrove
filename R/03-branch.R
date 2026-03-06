#' Branch a node in the BayesGrove project graph
#'
#' Clones an existing node and its upstream dependencies (edges) to create a new branch.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to branch.
#' @param label Optional label for the new branched node.
#' @param copy_params Whether to copy the parameters of the branched node (default: TRUE).
#'
#' @return The generated node ID of the new branch.
#' @export
bg_branch <- function(project, node_id, label = NULL, copy_params = TRUE) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)

  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  old_node <- graph$nodes[[node_id]]
  new_id <- sprintf("node_%s", digest::digest(runif(1), algo = "xxhash32"))

  new_label <- label %||% paste0(old_node$label %||% old_node$kind, " (branch)")
  new_params <- if (copy_params) old_node$params else list()

  # Create the new node
  graph <- dagriculture::dagri_add_node(
    graph = graph,
    id = new_id,
    kind = old_node$kind,
    label = new_label,
    params = new_params,
    metadata = old_node$metadata
  )

  # Find upstream edges and duplicate them for the new node
  upstream_edges <- Filter(function(e) e$to == node_id, graph$edges)

  for (e in upstream_edges) {
    new_edge_id <- sprintf(
      "edge_%s",
      digest::digest(runif(1), algo = "xxhash32")
    )
    graph <- dagriculture::dagri_add_edge(
      graph = graph,
      from = e$from,
      to = new_id,
      type = e$type,
      id = new_edge_id,
      metadata = e$metadata
    )
  }

  bg_commit_graph(project, graph)
  bg_register_branch(
    project = project,
    root_node_id = new_id,
    source_node_id = node_id,
    label = new_label,
    metadata = list(copy_params = isTRUE(copy_params))
  )

  new_id
}

#' Invalidate a node's result
#'
#' Invalidation is an explicit escape hatch for intentional recomputation.
#' It does not alter the structural graph state.
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node to invalidate.
#' @param recursive Whether to recursively invalidate downstream nodes (default: TRUE).
#'
#' @export
bg_invalidate <- function(project, node_id, recursive = TRUE) {
  S7::check_is_S7(project, bg_handle)

  graph <- bg_read_graph(project)
  if (!node_id %in% names(graph$nodes)) {
    cli::cli_abort("Node {.val {node_id}} not found in graph.")
  }

  nodes_to_invalidate <- c(node_id)

  if (recursive) {
    descendants <- dagriculture::dagri_descendants(graph, node_id)
    nodes_to_invalidate <- unique(c(nodes_to_invalidate, descendants))
  } else {
    cli::cli_warn(
      paste0(
        "Non-recursive invalidation leaves downstream cache entries untouched. ",
        "Use {.code recursive = TRUE} for the consistency-preserving path."
      )
    )
  }

  index <- bg_read_artifact_index(project)
  changed <- FALSE
  superseded_count <- 0L

  for (target_id in nodes_to_invalidate) {
    updated <- bg_supersede_artifacts_for_node(index, target_id)
    index <- updated$index
    changed <- changed || updated$changed
    superseded_count <- superseded_count + updated$count
  }

  if (changed) {
    bg_write_artifact_index(project, index)
  }

  cli::cli_inform(
    "Invalidated {length(nodes_to_invalidate)} node{?s}; superseded {superseded_count} cache binding{?s}."
  )

  invisible(TRUE)
}
