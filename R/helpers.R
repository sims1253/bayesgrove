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
    # runif() draws have 32-bit granularity under R's default RNG, so a
    # 32-bit digest of one draw leaves at most ~2^32 distinct ids (#27).
    # Four draws feed xxhash64 a full 64 bits of entropy, which keeps the
    # birthday collision probability at n = 10,000 ids near 1e-11.
    digest::digest(runif(4), algo = "xxhash64")
  )
}

# Draw ids from bg_new_id() until one is absent from `taken`, so an unlucky
# collision regenerates instead of aliasing or aborting (#27). The bound only
# guards against a pathological generator; with 64-bit ids it is unreachable
# in practice.
#' @keywords internal
bg_new_unique_id <- function(prefix, taken = character(), max_attempts = 64L) {
  for (attempt in seq_len(max_attempts)) {
    id <- bg_new_id(prefix)
    if (!id %in% taken) {
      return(id)
    }
  }

  cli::cli_abort(
    "Failed to draw a unique {.val {prefix}} id after {max_attempts} attempts."
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
  # nzchar(NA) is TRUE, so NA must be rejected explicitly; wrong-length input
  # would otherwise die in a coercion below rather than with this message.
  if (
    is.null(rationale) ||
      !is.character(rationale) ||
      length(rationale) != 1 ||
      is.na(rationale) ||
      !nzchar(trimws(rationale))
  ) {
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
