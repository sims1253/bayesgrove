#' Reconcile background daemon jobs
#'
#' This checks the status of background jobs (e.g. from callr or mirai)
#' and updates the job logs if they have finished or failed unexpectedly.
#'
#' @param project A `bg_handle`.
#'
#' @export
bg_reconcile_daemon_jobs <- function(project) {
  S7::check_is_S7(project, bg_handle)

  jobs <- bg_jobs(project)
  active_jobs <- Filter(function(j) j$status %in% c("queued", "running"), jobs)

  if (length(active_jobs) == 0) {
    return(invisible(TRUE))
  }

  for (job in active_jobs) {
    if (job$backend == "callr") {
      p <- project@metadata$callr_procs[[job$job_id]]
      if (!is.null(p)) {
        if (!p$is_alive()) {
          # Process died or finished
          status <- p$get_status()
          # Note: If it finished successfully, the worker process should have updated the JSONL log
          # to "succeeded". If it's still "running" in the log but dead, it probably crashed.

          # Re-read jobs to get latest status in case worker updated it right before dying
          latest_jobs <- bg_jobs(project)
          latest <- latest_jobs[[job$job_id]]

          if (latest$status %in% c("queued", "running")) {
            bg_update_job(
              project,
              job$job_id,
              status = "failed",
              error = list(
                message = "Background process terminated unexpectedly."
              ),
              finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            )
          }

          project@metadata$callr_procs[[job$job_id]] <- NULL
        }
      } else {
        # We don't have the process handle in memory. Maybe it was started in a previous session.
        # Without process tracking, we mark it orphaned if it's been running too long without heartbeat.
        # For MVP, we leave it alone or mark orphaned.
      }
    } else if (job$backend == "mirai") {
      m <- project@metadata$mirai_procs[[job$job_id]]
      if (!is.null(m)) {
        if (mirai::unresolved(m)) {
          # Still running
        } else {
          # Finished or error
          latest_jobs <- bg_jobs(project)
          latest <- latest_jobs[[job$job_id]]

          if (latest$status %in% c("queued", "running")) {
            res <- m$data
            if (inherits(res, "errorValue")) {
              bg_update_job(
                project,
                job$job_id,
                status = "failed",
                error = list(message = as.character(res)),
                finished_at = format(
                  Sys.time(),
                  "%Y-%m-%dT%H:%M:%SZ",
                  tz = "UTC"
                )
              )
            }
          }

          project@metadata$mirai_procs[[job$job_id]] <- NULL
        }
      }
    }
  }

  invisible(TRUE)
}
