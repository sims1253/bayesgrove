# Built-in executors --------------------------------------------------------
#
# Built-in executors are persisted as "builtin:<id>" references
# and fingerprinted by id + package version. This file holds the
# package-internal registry; the cmdstanr/brms executor implementations are
# registered in R/backends-cmdstanr.R and R/backends-brms.R.

#' Package-internal registry of built-in executors.
#'
#' Returns a named list mapping built-in ids to executor functions. Populated
#' by bg_register_builtin_executors() at load time; returns an empty list
#' until the cmdstanr/brms backend files add their entries.
#' @keywords internal
#' @noRd
bg_builtin_executors <- function() {
  .bg_builtin_executor_registry
}

.bg_builtin_executor_registry <- new.env(parent = emptyenv())

#' Register a built-in executor by id.
#'
#' Used by backend files (R/backends-cmdstanr.R, R/backends-brms.R) to install
#' their executor functions into the package-internal registry at load time.
#' @param id Built-in id (without the "builtin:" prefix).
#' @param fn Executor function taking (node, inputs) and returning
#'   list(result=, summaries=).
#' @keywords internal
#' @noRd
bg_register_builtin_executor <- function(id, fn) {
  .bg_builtin_executor_registry[[id]] <- fn
  invisible(fn)
}

#' Select one executor input by preferred id, else by position.
#'
#' The single selection rule shared by every built-in executor that picks an
#' input (fit data, the fit a check evaluates, observed data), so the input it
#' uses and the input id it reports as consumed can never disagree. Inputs are
#' keyed by upstream node id; `id` is `NULL` when the inputs are unnamed.
#' @param inputs Named list of upstream artifacts.
#' @param preferred Optional input id (a node param such as `data_input`).
#' @param position Fallback position when `preferred` does not name an input.
#' @return `list(id, value)`, or `NULL` when there is no such input.
#' @keywords internal
#' @noRd
bg_select_input <- function(inputs, preferred = NULL, position = 1L) {
  ids <- names(inputs)
  if (
    is.character(preferred) &&
      length(preferred) == 1L &&
      !is.null(ids) &&
      preferred %in% ids
  ) {
    return(list(id = preferred, value = inputs[[preferred]]))
  }
  if (length(inputs) < position) {
    return(NULL)
  }

  id <- if (is.null(ids) || !nzchar(ids[[position]])) NULL else ids[[position]]
  list(id = id, value = inputs[[position]])
}

#' Extract draws from a cached fit artifact.
#'
#' Adapter that handles both CmdStanMCMC and brmsfit objects so the shared
#' loo/compare/ppc executors can operate on either.
#' @param artifact A cached fit artifact (CmdStanMCMC or brmsfit).
#' @param as Return shape: `"matrix"` (draws x variables, chain structure
#'   flattened) or `"array"` (iteration x chain x variable, preserving chain
#'   structure, e.g. for loo::relative_eff).
#' @return A posterior::draws_matrix or posterior::draws_array.
#' @keywords internal
#' @noRd
bg_extract_draws <- function(artifact, as = c("matrix", "array")) {
  # One guard covers every caller (loo, loo_pit, ppc) so a missing
  # posterior aborts here with a named package instead of the raw
  # "there is no package called 'posterior'" from ::.
  if (!requireNamespace("posterior", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg posterior} package is required for extracting draws."
    )
  }

  as <- match.arg(as)
  to <- if (identical(as, "array")) {
    posterior::as_draws_array
  } else {
    posterior::as_draws_matrix
  }

  if (inherits(artifact, "CmdStanMCMC")) {
    return(to(artifact$draws()))
  }
  if (inherits(artifact, "brmsfit")) {
    return(to(posterior::as_draws(artifact)))
  }
  if (inherits(artifact, "draws_matrix") || inherits(artifact, "draws")) {
    return(to(artifact))
  }

  # Fallback: assume it's already a matrix or coercible.
  to(artifact)
}

#' Select draw-variable columns matching a Stan variable name.
#'
#' Matches a Stan variable exactly, including its indexed form. A variable
#' named `log_lik` matches `log_lik` and `log_lik[1]`, `log_lik[2]`, ... but
#' not `log_lik_saturated[1]` or `log_lik_extra`. Substring matching
#' (`grepl(fixed = TRUE)`) would incorrectly match those names.
#'
#' A column matches when its name equals `var` or starts with `paste0(var, "[")`
#' (Stan's indexed-variable form). `var` is matched literally, not as a regex,
#' so no escaping is needed.
#'
#' @param var Bare Stan variable name (e.g. `"log_lik"`, `"yrep"`).
#' @param col_names Character vector of draw-variable names.
#' @return Logical vector the same length as `col_names`.
#' @keywords internal
#' @noRd
bg_indexed_var_cols <- function(var, col_names) {
  col_names == var | startsWith(col_names, paste0(var, "["))
}
