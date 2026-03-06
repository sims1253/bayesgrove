#' @importFrom rlang %||%
NULL

#' Log a job entry
#'
#' @param project A `bg_handle`.
#' @param job_record A list representing a `bg_job` entry.
#'
#' @keywords internal
#' @export
bg_log_job <- function(project, job_record) {
  # Enforce schema fields
  required <- c(
    "job_id",
    "run_id",
    "project_id",
    "node_id",
    "status",
    "submitted_at",
    "started_at",
    "finished_at",
    "progress",
    "result_ref",
    "error",
    "backend",
    "metadata"
  )

  missing <- setdiff(required, names(job_record))
  if (length(missing) > 0) {
    cli::cli_abort("Job record missing required fields: {.val {missing}}")
  }

  job_record$schema_name <- "bg_job_entry"
  job_record$schema_version <- 1L

  log_path <- file.path(project@path, ".bayesgrove", "runs", "jobs.jsonl")

  # Ensure runs directory exists
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  bg_append_jsonl(log_path, job_record)

  invisible(job_record)
}

#' Read current state of all jobs
#'
#' @param project A `bg_handle`.
#' @param status Optional character vector of statuses to filter by.
#' @param detailed Logical, whether to return detailed job information (default FALSE).
#'
#' @return A named list of job records.
#' @export
bg_jobs <- function(project, status = NULL, detailed = FALSE) {
  S7::check_is_S7(project, bg_handle)

  log_path <- file.path(project@path, ".bayesgrove", "runs", "jobs.jsonl")
  if (!file.exists(log_path)) {
    return(list())
  }

  # Read JSONL
  lines <- readLines(log_path, warn = FALSE)
  if (length(lines) == 0) {
    return(list())
  }

  jobs <- list()
  for (line in lines) {
    if (trimws(line) == "") {
      next
    }
    record <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    jobs[[record$job_id]] <- record
  }

  if (!is.null(status)) {
    jobs <- Filter(function(j) j$status %in% status, jobs)
  }

  jobs
}

#' Create a new job record
#'
#' @param project A `bg_handle`.
#' @param run_id The run this job belongs to.
#' @param node_id The node this job executes.
#' @param backend The backend identifier.
#'
#' @return The generated job record.
#' @keywords internal
#' @export
bg_create_job <- function(project, run_id, node_id, backend = "local") {
  job_id <- sprintf("job_%s", digest::digest(runif(1), algo = "xxhash32"))

  record <- list(
    job_id = job_id,
    run_id = run_id,
    project_id = project@project_id,
    node_id = node_id,
    status = "queued",
    submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    started_at = NULL,
    finished_at = NULL,
    progress = list(stage = "queued", percent = 0),
    result_ref = NULL,
    error = NULL,
    backend = backend,
    metadata = list()
  )

  bg_log_job(project, record)
  record
}

#' Update an existing job record
#'
#' @param project A `bg_handle`.
#' @param job_id The ID of the job to update.
#' @param ... Fields to update.
#'
#' @return The updated job record.
#' @keywords internal
#' @export
bg_update_job <- function(project, job_id, ...) {
  jobs <- bg_jobs(project)
  if (!job_id %in% names(jobs)) {
    cli::cli_abort("Job {.val {job_id}} not found.")
  }

  record <- jobs[[job_id]]
  updates <- list(...)

  for (nm in names(updates)) {
    record[[nm]] <- updates[[nm]]
  }

  # Remove schema fields before re-logging to avoid duplication, bg_log_job re-adds them
  record$schema_name <- NULL
  record$schema_version <- NULL

  bg_log_job(project, record)
}
