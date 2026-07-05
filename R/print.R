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

  # When blocked, surface the held nodes + their reason and the resolving call,
  # so the console UI carries the protocol forward.
  if (identical(status_text, "blocked")) {
    held <- x$metadata$held_by_policy %||% list()
    if (length(held) > 0L) {
      cli::cli_text("{cli::col_magenta('Held nodes:')}")
      for (node_id in names(held)) {
        reason <- held[[node_id]]
        why <- reason$reason %||% "held by protocol"
        cli::cli_bullets(c(
          "*" = "{cli::col_grey(node_id)}: {why}"
        ))
      }
      cli::cli_text(
        "{cli::col_grey('Next:')} call {.fn bg_next_actions} on the handle to see obligations."
      )
    }
  }
  invisible(x)
}

#' @method print bg_next_actions_result
#' @export
print.bg_next_actions_result <- function(x, ...) {
  cli::cli_text("{cli::col_grey('<bg_next_actions_result>')}")

  obligations <- x$obligations %||% list()
  if (length(obligations) == 0L) {
    cli::cli_alert_success("No outstanding obligations.")
    return(invisible(x))
  }

  cli::cli_text("{cli::col_grey('Obligations:')}")
  for (i in seq_along(obligations)) {
    ob <- obligations[[i]]
    severity <- ob$severity %||% "info"
    glyph <- switch(
      severity,
      "blocking" = cli::col_red("x"),
      "warning" = cli::col_yellow("!"),
      "info" = cli::col_blue("i"),
      cli::col_grey("*")
    )
    title <- ob$title %||% ob$kind %||% "obligation"
    kind <- ob$kind %||% "unknown"
    why <- ob$why %||% ob$description %||% ""
    cli::cli_text("{glyph} {i}. [{kind}] {title}")
    if (nzchar(why)) {
      cli::cli_text("     {cli::col_grey(why)}")
    }
    resolving <- ob$resolving_call %||%
      sprintf('bg_record_decision(handle, kind = "%s", ...)', kind)
    cli::cli_text("     {cli::col_cyan(resolving)}")
  }
  invisible(x)
}
