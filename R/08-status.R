#' Get workflow status
#'
#' @param project A `bg_handle`.
#'
#' @return A `bg_status` list summarizing the project.
#' @export
bg_status <- function(project) {
  S7::check_is_S7(project, bg_handle)

  plan <- bg_plan(project)
  gates <- bg_pending_gates(project)

  list(
    workflow_state = if (length(gates) > 0) {
      "blocked"
    } else if (length(plan$to_execute) == 0) {
      "idle"
    } else {
      "ready"
    },
    runnable_nodes = length(plan$to_execute),
    blocked_nodes = length(plan$blocked),
    cached_nodes = length(plan$cache_hits),
    pending_gates = length(gates),
    total_nodes = length(plan$graph_plan$topo_order)
  )
}

#' Retrieve a result artifact from the workflow
#'
#' @param project A `bg_handle`.
#' @param node_id The ID of the node whose result you want.
#'
#' @return The R object produced by the node.
#' @export
bg_result <- function(project, node_id) {
  S7::check_is_S7(project, bg_handle)

  plan <- bg_plan(project)
  if (!node_id %in% plan$cache_hits) {
    cli::cli_abort(
      "No cached result available for node {.val {node_id}}. It may need to be run, or its upstream dependencies changed."
    )
  }

  fp <- plan$metadata$fingerprints[[node_id]]
  ref <- bg_check_artifact(project, fp)

  bg_fetch_artifact(project, ref)
}
