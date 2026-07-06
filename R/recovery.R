#' Retrieve a complete snapshot of the project state
#'
#' This aggregates the project configuration, the structural graph, the active
#' runtime registries, the pending decision gates, the active jobs, and the
#' overall workflow status into a single nested list.
#'
#' @param project A `bg_handle`.
#'
#' @return A list representing the `bg_project_snapshot` schema.
#' @export
bg_snapshot <- function(project) {
  S7::check_is_S7(project, bg_handle)

  config <- bg_read_project_config(project)

  graph <- bg_read_graph(project)
  jobs <- bg_jobs(project)
  gates <- bg_pending_gates(project)
  decisions <- bg_read_decisions(project)
  artifacts <- bg_read_artifact_index(project)
  status <- bg_status(project)

  list(
    project_id = project@project_id,
    name = config$project_name %||% basename(project@path),
    path = project@path,
    graph = graph,
    decisions = decisions,
    gate_specs = gates,
    artifacts = artifacts,
    jobs = jobs,
    config = config,
    status = status
  )
}

#' Pause the workflow execution
#'
#' Pausing the workflow stops auto-advancing and marks the workflow state
#' so that no new jobs are dispatched. It does not cancel currently running jobs.
#'
#' In a synchronous runtime this is a near-no-op: it only gates the next
#' [bg_run] call. Under parallel execution (Milestone 3) pause takes effect at
#' WAVE BOUNDARIES — the in-flight wave finishes, then the run stops before the
#' next wave dispatches, and [bg_resume] clears the flag. Reclassified
#' `experimental` while the wave-boundary semantics settle.
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_pause <- function(project) {
  S7::check_is_S7(project, bg_handle)

  config <- bg_read_project_config(project)

  config$paused <- TRUE

  bg_write_project_config_path(project@path, config)
  cli::cli_inform(
    "Workflow paused. No new jobs will be dispatched until the workflow is resumed."
  )

  invisible(list(state = "paused", workflow_state = "paused"))
}

#' Resume the workflow execution
#'
#' Removes the pause marker, allowing the workflow to resume execution. Pair
#' with [bg_pause]; reclassified `experimental` along with it (the
#' wave-boundary pause semantics are new in Milestone 3).
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_resume <- function(project) {
  S7::check_is_S7(project, bg_handle)

  config <- bg_read_project_config(project)

  config$paused <- FALSE

  bg_write_project_config_path(project@path, config)
  cli::cli_inform("Workflow resumed.")

  invisible(bg_status(project))
}

bg_read_project_config <- function(project) {
  bg_read_project_config_path(project@path)
}

bg_workflow_paused <- function(project) {
  isTRUE(bg_read_project_config(project)$paused)
}
