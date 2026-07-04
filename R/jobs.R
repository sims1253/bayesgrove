#' @importFrom rlang %||%
NULL

#' File-system path to the jobs JSONL log for a project.
#'
#' @param project A `bg_handle`.
#' @return Character path (the file may not yet exist).
#' @keywords internal
#' @noRd
bg_jobs_log_path <- function(project) {
  file.path(project@path, ".bayesgrove", "runs", "jobs.jsonl")
}

#' Read all jobs from disk, bypassing the cache.
#'
#' Used by [bg_jobs_cache_get] to (re)populate the cache and as the
#' authoritative fallback when the cache is stale or absent. Returns the
#' deduplicated, last-record-wins view. The raw on-disk line count (number of
#' append events) is read separately by the cache layer to avoid the O(lines)
#' recount on subsequent appends.
#' @param project A `bg_handle`.
#' @return A named list of job records keyed by `job_id`.
#' @keywords internal
#' @noRd
bg_jobs_read_disk <- function(project) {
  log_path <- bg_jobs_log_path(project)
  if (!file.exists(log_path)) {
    return(list())
  }

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
    # Append-only snapshot: last record for a given job_id wins.
    jobs[[record$job_id]] <- record
  }
  jobs
}

#' Count non-blank lines in the jobs log (the number of append events).
#'
#' This is the value passed to `bg_append_jsonl(known_line_count =)` so the
#' next append's seq stamp skips its own O(lines) recount. Called only when the
#' cache is (re)populated from disk, so it runs O(N) times total across a run
#' rather than O(N^2).
#' @param project A `bg_handle`.
#' @return Integer line count (0 if the file does not exist).
#' @keywords internal
#' @noRd
bg_jobs_count_lines <- function(project) {
  log_path <- bg_jobs_log_path(project)
  if (!file.exists(log_path)) {
    return(0L)
  }
  length(count.fields(log_path, sep = "\n", blank.lines.skip = FALSE))
}

#' Read or refresh the per-handle jobs cache.
#'
#' The cache lives on the handle's `.state` environment
#' (`project@.state$jobs_cache`) so it survives across calls within a session
#' and is invalidated by file mtime+size: a changed mtime OR a changed size
#' forces a re-parse of the jobs JSONL. This keeps a run of N nodes at O(N)
#' file parses instead of O(N^2): each `bg_jobs()` call after the first hits
#' the cache, and `bg_update_job()`/`bg_log_job()` update it in place.
#'
#' @param project A `bg_handle`.
#' @return A named list of job records keyed by `job_id` (last record wins).
#' @keywords internal
#' @noRd
bg_jobs_cache_get <- function(project) {
  log_path <- bg_jobs_log_path(project)
  state <- project@.state
  cache <- state$jobs_cache

  if (file.exists(log_path)) {
    fi <- file.info(log_path)
    mtime <- fi$mtime
    size <- as.numeric(fi$size)
    # bayesgrove is the sole writer of jobs.jsonl and only appends, so a
    # changed mtime or size reliably signals new content (the same-size +
    # same-mtime blind spot is unreachable under append-only, single-writer).
    if (
      is.null(cache) ||
        !identical(cache$mtime, mtime) ||
        !identical(cache$size, size)
    ) {
      jobs <- bg_jobs_read_disk(project)
      state$jobs_cache <- list(
        jobs = jobs,
        line_count = bg_jobs_count_lines(project),
        mtime = mtime,
        size = size
      )
      return(jobs)
    }
    return(cache$jobs)
  }

  # No file on disk: cache an empty list so the missing-file branch is hit
  # once, not on every call.
  if (is.null(cache)) {
    state$jobs_cache <- list(
      jobs = list(),
      line_count = 0L,
      mtime = NA,
      size = NA_real_
    )
  }
  state$jobs_cache$jobs
}

#' Log a job entry
#'
#' Appends a job snapshot to the JSONL log and refreshes the handle's jobs
#' cache so subsequent reads do not re-parse the file.
#'
#' @param project A `bg_handle`.
#' @param job_record A list representing a `bg_job` entry.
#'
#' @keywords internal
#' @noRd
bg_log_job <- function(project, job_record) {
  # Enforce schema fields
  required <- c(
    "job_id",
    "run_id",
    "project_id",
    "node_id",
    "status",
    "submitted_at",
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

  log_path <- bg_jobs_log_path(project)

  # Ensure runs directory exists
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  # Pass the cache's known line count so bg_append_jsonl can stamp the seq
  # number without an O(lines) recount. The cache may be stale (external
  # writes); bg_append_jsonl trusts the count under its file lock, which is
  # safe because bayesgrove is the single writer for jobs.jsonl.
  cache <- project@.state$jobs_cache
  known_lines <- if (!is.null(cache)) cache$line_count else NULL

  seq <- bg_append_jsonl(
    log_path,
    job_record,
    known_line_count = known_lines
  )
  # bg_append_jsonl stamps seq on its in-memory copy; mirror it onto the record
  # we cache so callers reading bg_jobs() see the same seq as the on-disk line.
  job_record$seq <- seq

  # Fold the new record into the cache without a full re-parse. mtime+size and
  # the line count are refreshed so the next bg_jobs()/append see a cache hit.
  fi <- file.info(log_path)
  state <- project@.state
  # When the cache was NULL (first write on this handle), re-read from disk so
  # any pre-existing records are merged before folding in the new one; when the
  # cache is populated, reuse it directly (no re-parse).
  jobs <- if (is.null(cache)) {
    bg_jobs_read_disk(project)
  } else {
    cache$jobs
  }
  jobs[[job_record$job_id]] <- job_record
  state$jobs_cache <- list(
    jobs = jobs,
    line_count = (known_lines %||% 0L) + 1L,
    mtime = fi$mtime,
    size = as.numeric(fi$size)
  )

  invisible(job_record)
}

#' Read current state of all jobs
#'
#' Reads the jobs log, consulting a per-handle cache that is invalidated by
#' file mtime+size. Within a session, repeated calls after the first parse
#' return the cached snapshot, so a run of N nodes does O(N) rather than O(N^2)
#' full log parses.
#'
#' @param project A `bg_handle`.
#' @param status Optional character vector of statuses to filter by.
#' @param detailed Logical, whether to return detailed job information (default FALSE).
#'
#' @return A named list of job records.
#' @export
bg_jobs <- function(project, status = NULL, detailed = FALSE) {
  S7::check_is_S7(project, bg_handle)

  jobs <- bg_jobs_cache_get(project)

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
#' @noRd
bg_create_job <- function(project, run_id, node_id, backend = "local") {
  job_id <- bg_new_id("job")

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
#' Reads the current jobs (from the cache), applies the field updates, appends
#' the updated snapshot to the log, and refreshes the cache in place — without
#' a second full parse of the log.
#'
#' @param project A `bg_handle`.
#' @param job_id The ID of the job to update.
#' @param ... Fields to update.
#'
#' @return The updated job record.
#' @keywords internal
#' @noRd
bg_update_job <- function(project, job_id, ...) {
  jobs <- bg_jobs_cache_get(project)
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
