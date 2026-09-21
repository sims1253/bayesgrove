#' @keywords internal
bg_drop_null_fields <- function(x) {
  if (!is.list(x)) {
    return(x)
  }

  keep <- !vapply(x, is.null, logical(1))
  x <- x[keep]

  lapply(x, bg_drop_null_fields)
}

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
  # nzchar(NA) is TRUE, so NA must be rejected explicitly; a length-2 (or
  # non-character) rationale previously died in a coercion error inside the
  # length check itself.
  if (
    is.null(rationale) ||
      (length(rationale) == 1L && is.na(rationale)) ||
      (is.character(rationale) &&
        length(rationale) == 1L &&
        !nzchar(trimws(rationale)))
  ) {
    cli::cli_abort(message)
  }

  if (!is.character(rationale) || length(rationale) != 1L) {
    cli::cli_abort(
      "{.arg rationale} must be a single non-empty string, not {.obj_type_friendly {rationale}} of length {length(rationale)}."
    )
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
