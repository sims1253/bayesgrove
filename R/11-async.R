#' Submit a project workflow for asynchronous execution
#'
#' @param project A `bg_handle`.
#' @param targets Optional character vector of target node IDs.
#' @param mode Execution mode: 'async'.
#' @param backend The async backend to use (`"mirai"`). `"auto"` resolves to
#'   `"mirai"` and errors if the package is not installed.
#'
#' @return A `bg_run_handle` list.
#' @export
bg_submit <- function(
  project,
  targets = NULL,
  mode = c("async"),
  backend = c("auto", "mirai")
) {
  mode <- match.arg(mode)
  backend <- match.arg(backend)
  S7::check_is_S7(project, bg_handle)

  if (bg_workflow_paused(project)) {
    cli::cli_abort(
      "Workflow is paused. Call {.fn bg_resume} before dispatching new work."
    )
  }

  external_holds <- bg_workflow_external_holds(project)
  plan <- bg_plan(
    project,
    targets,
    external_holds = external_holds,
    mode = "async"
  )

  run_id <- bg_new_id("run")

  if (length(plan$to_execute) == 0) {
    bg_cli_inform("No nodes require execution.")
    res <- list(
      run_id = run_id,
      status = "succeeded",
      mode = "async",
      targets = plan$targets %||% character(0),
      job_ids = character(0),
      submitted_at = NULL,
      started_at = NULL,
      finished_at = NULL,
      summary = list(total_jobs = 0L),
      error = NULL,
      metadata = list()
    )
    class(res) <- "bg_run_handle"
    return(res)
  }

  if (backend == "auto") {
    backend <- "mirai"
  }

  if (!requireNamespace("mirai", quietly = TRUE)) {
    cli::cli_abort("The {.pkg mirai} package is required for async execution.")
  }

  mirai_ready <- tryCatch(
    {
      if (mirai::status(.compute = project@project_id)$daemons == 0) {
        mirai::daemons(1, .compute = project@project_id)
      }
      TRUE
    },
    error = function(e) {
      e
    }
  )

  if (inherits(mirai_ready, "error")) {
    cli::cli_abort(
      paste0(
        "Unable to start the {.pkg mirai} daemon pool in this environment: ",
        mirai_ready$message
      )
    )
  }

  bg_cli_inform(
    "Starting async run {.val {run_id}} with {length(plan$to_execute)} node{?s}."
  )

  job_ids <- character(0)

  for (node_id in plan$to_execute) {
    job <- bg_create_job(project, run_id, node_id, backend = backend)
    job_ids <- c(job_ids, job$job_id)

    # Update state to running before fork
    bg_update_job(
      project,
      job$job_id,
      status = "running",
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )

    # We pass the absolute path and identifiers to the background worker
    worker_args <- list(
      project_path = normalizePath(project@path),
      node_id = node_id,
      job_id = job$job_id,
      fingerprint = plan$metadata$fingerprints[[node_id]],
      input_bindings = plan$input_bindings[[node_id]],
      lib_paths = .libPaths(),
      registries = project@registries
    )

    m <- mirai::mirai(
      {
        if (!is.null(args$lib_paths)) {
          .libPaths(args$lib_paths)
        }
        library(bayesgrove)
        fn <- get("bg_worker_process", envir = asNamespace("bayesgrove"))
        fn(args)
      },
      args = worker_args,
      .compute = project@project_id
    )

    if (is.null(project@metadata$mirai_procs)) {
      project@metadata$mirai_procs <- list()
    }
    project@metadata$mirai_procs[[job$job_id]] <- m
  }

  res <- list(
    run_id = run_id,
    status = "running",
    mode = "async",
    targets = plan$targets,
    job_ids = job_ids,
    submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    finished_at = NULL,
    summary = list(total_jobs = length(job_ids)),
    error = NULL,
    metadata = list()
  )
  class(res) <- "bg_run_handle"
  res
}

#' Cancel an asynchronous run
#'
#' @param project A `bg_handle`.
#' @param run_id The ID of the run to cancel.
#'
#' @export
bg_cancel <- function(project, run_id) {
  S7::check_is_S7(project, bg_handle)

  jobs <- bg_jobs(project)
  run_jobs <- Filter(
    function(j) j$run_id == run_id && j$status %in% c("queued", "running"),
    jobs
  )

  if (length(run_jobs) == 0) {
    bg_cli_inform("No active jobs found for run {.val {run_id}}.")
    return(invisible(TRUE))
  }

  for (job in run_jobs) {
    if (!is.null(project@metadata$mirai_procs[[job$job_id]])) {
      m <- project@metadata$mirai_procs[[job$job_id]]
      mirai::stop_mirai(m)
      project@metadata$mirai_procs[[job$job_id]] <- NULL
    }

    # 2. Update job status
    bg_update_job(
      project,
      job$job_id,
      status = "cancelled",
      finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  }

  bg_cli_inform(
    "Cancelled {length(run_jobs)} job{?s} for run {.val {run_id}}."
  )
  invisible(TRUE)
}
#' Wait for asynchronous jobs to complete
#'
#' @param project A `bg_handle`.
#' @param run_id The ID of the run to wait for.
#' @param timeout Timeout in seconds.
#'
#' @export
bg_wait <- function(project, run_id, timeout = Inf) {
  start_time <- Sys.time()

  cli::cli_progress_bar("Waiting for jobs to complete", type = "tasks")

  repeat {
    jobs <- bg_jobs(project)
    run_jobs <- Filter(function(j) j$run_id == run_id, jobs)

    if (length(run_jobs) == 0) {
      cli::cli_progress_done()
      break
    }

    statuses <- sapply(run_jobs, function(j) j$status)
    done_count <- sum(
      statuses %in% c("succeeded", "failed", "cancelled", "orphaned")
    )

    cli::cli_progress_update(set = done_count, total = length(run_jobs))

    if (done_count == length(run_jobs)) {
      cli::cli_progress_done()
      break
    }

    if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {
      cli::cli_abort("Timeout reached while waiting for run {.val {run_id}}.")
    }

    Sys.sleep(0.5)
  }

  invisible(TRUE)
}

#' The internal worker process that executes a single node
#'
#' @param args List of worker arguments.
#' @keywords internal
#' @export
bg_worker_process <- function(args) {
  # This function runs in a completely separate R process
  cat(
    "Worker started\n",
    file = file.path(
      args$project_path,
      ".bayesgrove",
      "runs",
      paste0(args$job_id, "_debug.txt")
    ),
    append = TRUE
  )

  if (!is.null(args$lib_paths)) {
    .libPaths(args$lib_paths)
  }

  library(bayesgrove)

  cat(
    "Package loaded\n",
    file = file.path(
      args$project_path,
      ".bayesgrove",
      "runs",
      paste0(args$job_id, "_debug.txt")
    ),
    append = TRUE
  )

  handle <- bg_open(args$project_path, readonly = FALSE)

  if (!is.null(args$registries)) {
    handle@registries <- args$registries
  }

  graph <- bg_read_graph(handle)
  node <- graph$nodes[[args$node_id]]

  kind_reg <- handle@registries$node_kinds[[node$kind]]

  # Fetch inputs
  resolved_inputs <- list()
  for (b in args$input_bindings) {
    resolved_inputs[[b$from_node_id]] <- bg_fetch_artifact(
      handle,
      b$artifact_ref
    )
  }

  cat(
    "Inputs resolved, executing\n",
    file = file.path(
      args$project_path,
      ".bayesgrove",
      "runs",
      paste0(args$job_id, "_debug.txt")
    ),
    append = TRUE
  )

  # Execute
  result <- tryCatch(
    {
      execution <- kind_reg$executor(node, resolved_inputs)
      normalized <- bg_normalize_execution_result(execution)
      cat(
        "Executor finished\n",
        file = file.path(
          args$project_path,
          ".bayesgrove",
          "runs",
          paste0(args$job_id, "_debug.txt")
        ),
        append = TRUE
      )

      # Store artifact
      ref <- bg_store_artifact(
        handle,
        args$node_id,
        args$fingerprint,
        normalized$artifact
      )
      bg_write_summaries(
        project = handle,
        node_id = args$node_id,
        artifact_ref = ref,
        execution_fingerprint = args$fingerprint,
        summaries = normalized$summaries
      )
      cat(
        "Artifact stored\n",
        file = file.path(
          args$project_path,
          ".bayesgrove",
          "runs",
          paste0(args$job_id, "_debug.txt")
        ),
        append = TRUE
      )

      # Update job
      bg_update_job(
        handle,
        args$job_id,
        status = "succeeded",
        result_ref = ref,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
      cat(
        "Job updated\n",
        file = file.path(
          args$project_path,
          ".bayesgrove",
          "runs",
          paste0(args$job_id, "_debug.txt")
        ),
        append = TRUE
      )

      normalized$artifact
    },
    error = function(e) {
      cat(
        "Error: ",
        e$message,
        "\n",
        file = file.path(
          args$project_path,
          ".bayesgrove",
          "runs",
          paste0(args$job_id, "_debug.txt")
        ),
        append = TRUE
      )
      bg_update_job(
        handle,
        args$job_id,
        status = "failed",
        error = list(message = e$message),
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
      stop(e)
    }
  )

  invisible(TRUE)
}
