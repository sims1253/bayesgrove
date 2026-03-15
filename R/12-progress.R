#' Reconcile background daemon jobs
#'
#' This checks the status of background jobs coordinated via `mirai`
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
    if (job$backend == "mirai") {
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
            if (mirai::is_error_value(res)) {
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
