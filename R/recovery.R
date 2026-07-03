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

  # Ensure background jobs are reconciled first
  bg_reconcile_daemon_jobs(project)

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
    registry_bindings = lapply(project@registries$backends, function(b) {
      if (is.function(b$backend_runtime_signature)) {
        b$backend_runtime_signature(list())
      } else {
        list()
      }
    }),
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
#' @param project A `bg_handle`.
#'
#' @export
bg_pause <- function(project) {
  S7::check_is_S7(project, bg_handle)

  config <- bg_read_project_config(project)

  config$paused <- TRUE

  bg_write_project_config_path(project@path, config)
  cli::cli_inform(
    "Workflow paused. Currently running background jobs will continue to run, but no new jobs will be dispatched."
  )

  invisible(list(state = "paused", workflow_state = "paused"))
}

#' Resume the workflow execution
#'
#' Removes the pause marker, allowing the workflow to resume execution.
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
