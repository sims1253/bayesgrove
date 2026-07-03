#' @keywords internal
bg_new_id <- function(prefix) {
  sprintf(
    "%s_%s",
    prefix,
    digest::digest(runif(1), algo = "xxhash32")
  )
}

#' @keywords internal
bg_revised_label <- function(node) {
  paste0(node$label %||% node$kind, " (revised)")
}

#' @keywords internal
bg_require_rationale <- function(
  rationale,
  message = "Rationale is required."
) {
  if (is.null(rationale) || !nzchar(trimws(rationale))) {
    cli::cli_abort(message)
  }

  invisible(trimws(rationale))
}

#' @keywords internal
bg_find_obligation <- function(obligations, kind, scope = NULL) {
  matches <- Filter(
    function(obligation) {
      identical(obligation$kind %||% NULL, kind) &&
        (is.null(scope) || identical(obligation$scope %||% NULL, scope))
    },
    obligations %||% list()
  )

  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[1]]
}

#' @keywords internal
bg_cli_quiet <- function() {
  isTRUE(getOption("bayesgrove.quiet_inform")) ||
    tolower(Sys.getenv("TESTTHAT")) %in% c("true", "yes", "1")
}

#' @keywords internal
bg_cli_inform <- function(..., .envir = parent.frame()) {
  if (!bg_cli_quiet()) {
    cli::cli_inform(..., .envir = .envir)
  }

  invisible(TRUE)
}
