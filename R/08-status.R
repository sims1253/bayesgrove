#' Get workflow status
#'
#' @param project A `bg_handle`.
#' @param auto_advance Whether to automatically submit newly eligible nodes if the last run was async (default TRUE).
#'
#' @return A `bg_status` list summarizing the project.
#' @export
bg_status <- function(project, auto_advance = TRUE) {
  S7::check_is_S7(project, bg_handle)

  bg_reconcile_daemon_jobs(project)

  plan <- bg_plan(project)
  gates <- bg_pending_gates(project)
  jobs <- bg_jobs(project)
  active_jobs <- Filter(function(j) j$status %in% c("queued", "running"), jobs)
  num_active <- length(active_jobs)
  paused <- bg_workflow_paused(project)

  if (
    auto_advance &&
      length(plan$to_execute) > 0 &&
      length(gates) == 0 &&
      num_active == 0 &&
      !paused
  ) {
    # Find if the most recent finished job was from an async run
    # If so, we might auto-submit here. For the MVP, we let the user explicitly call bg_submit()
    # or bg_wait() to chain jobs.
  }

  list(
    workflow_state = if (num_active > 0) {
      "running"
    } else if (paused) {
      "paused"
    } else if (length(gates) > 0) {
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
    active_jobs = num_active,
    total_nodes = length(plan$graph_plan$topo_order),
    paused = paused
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
      paste0(
        "No cached result available for node {.val {node_id}}. ",
        "It may need to be run, or its upstream dependencies changed."
      )
    )
  }

  fp <- plan$metadata$fingerprints[[node_id]]
  ref <- bg_check_artifact(project, fp, node_id = node_id)

  bg_fetch_artifact(project, ref)
}
