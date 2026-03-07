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

  external_holds <- bg_workflow_external_holds(project)
  plan <- bg_plan(project, external_holds = external_holds)
  gates <- bg_pending_gates(project)
  jobs <- bg_jobs(project)
  active_jobs <- Filter(function(j) j$status %in% c("queued", "running"), jobs)
  failed_jobs <- Filter(
    function(j) j$status %in% c("failed", "orphaned"),
    jobs
  )
  num_active <- length(active_jobs)
  paused <- bg_workflow_paused(project)
  held_nodes <- plan$held_by_policy %||% list()
  total_nodes <- length(plan$graph_plan$topo_order)

  last_run_id <- NULL
  if (length(jobs) > 0) {
    submitted_at <- vapply(
      jobs,
      function(job) job$submitted_at %||% "",
      character(1)
    )
    latest_idx <- order(submitted_at, decreasing = TRUE)[[1]]
    run_ids <- vapply(
      jobs,
      function(job) job$run_id %||% "",
      character(1)
    )
    last_run_id <- run_ids[[latest_idx]]
    if (!nzchar(last_run_id)) {
      last_run_id <- NULL
    }
  }

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

  messages <- character(0)
  if (paused) {
    messages <- c(messages, "Workflow is paused.")
  }
  if (length(gates) > 0) {
    messages <- c(
      messages,
      sprintf("%d pending gate(s) block downstream work.", length(gates))
    )
  }
  if (length(held_nodes) > 0) {
    messages <- c(
      messages,
      sprintf(
        "%d node(s) are held by workflow obligations.",
        length(held_nodes)
      )
    )
  }
  if (length(failed_jobs) > 0) {
    messages <- c(
      messages,
      sprintf(
        "%d job(s) are in a failed or orphaned state.",
        length(failed_jobs)
      )
    )
  }
  if (num_active == 0 && length(plan$to_execute) > 0 && length(messages) == 0) {
    messages <- c(
      messages,
      sprintf("%d node(s) are ready to run.", length(plan$to_execute))
    )
  }

  workflow_state <- if (total_nodes == 0 && num_active == 0 && !paused) {
    "open"
  } else if (num_active > 0) {
    "running"
  } else if (paused) {
    "paused"
  } else if (length(failed_jobs) > 0) {
    "degraded"
  } else if (length(gates) > 0 || length(held_nodes) > 0) {
    "blocked"
  } else {
    "idle"
  }

  health <- if (length(failed_jobs) > 0) {
    "error"
  } else if (paused || length(gates) > 0 || length(held_nodes) > 0) {
    "warning"
  } else {
    "ok"
  }

  list(
    workflow_state = workflow_state,
    runnable_nodes = length(plan$to_execute),
    blocked_nodes = length(plan$blocked) + length(held_nodes),
    pending_gates = length(gates),
    active_jobs = num_active,
    last_run_id = last_run_id,
    health = health,
    messages = messages
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

  plan <- bg_plan(project, include_inactive = TRUE)
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
