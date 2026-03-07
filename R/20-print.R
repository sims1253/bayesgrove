#' @method print bg_decision_record
#' @export
print.bg_decision_record <- function(x, ...) {
  cli::cli_text("{cli::col_grey('<bg_decision_record>')} {x$decision_id}")
  cli::cli_bullets(c(
    "*" = "{cli::col_yellow('Prompt:')} {x$prompt}",
    "*" = "{cli::col_cyan('Choice:')} {x$choice}",
    "*" = "{cli::col_grey('Rationale:')} {x$rationale}"
  ))
  invisible(x)
}

#' @method print bg_run_handle
#' @export
print.bg_run_handle <- function(x, ...) {
  cli::cli_text("{cli::col_grey('<bg_run_handle>')} {x$run_id}")

  status_text <- x$status %||% ""

  status_color <- switch(
    status_text,
    "succeeded" = cli::col_green,
    "failed" = cli::col_red,
    "running" = cli::col_yellow,
    "queued" = cli::col_blue,
    "blocked" = cli::col_magenta,
    cli::col_grey
  )

  summary_value <- x$summary$total_executed %||% x$summary$total_jobs %||% 0L
  summary_label <- if (!is.null(x$summary$total_executed)) {
    "Executed Nodes"
  } else {
    "Jobs"
  }

  cli::cli_bullets(c(
    "*" = "{cli::col_grey('Status:')} {status_color(status_text)}",
    "*" = "{cli::col_grey('Mode:')} {x$mode}",
    "*" = "{cli::col_grey(summary_label, ':')} {summary_value}"
  ))

  if (!is.null(x$error)) {
    error_message <- x$error$message %||% x$error$type %||% "Unknown error"
    cli::cli_alert_danger("Error: {error_message}")
  }
  invisible(x)
}
