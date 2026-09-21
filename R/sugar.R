# Practitioner convenience helpers ----------------------------------------
#
# Each helper creates a data node and a fit node, runs the fit, and returns
# the fit node id invisibly while printing the run handle. Extra `...` params
# pass through to the fit node.

#' Require backend kinds before a sugar helper starts mutating the graph.
#' @keywords internal
#' @noRd
bg_require_backend_kinds <- function(handle, kinds, setup_function) {
  available <- names(handle@registries$node_kinds %||% list())
  missing <- setdiff(kinds, available)
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "Required backend node kind{?s} {?is/are} not registered: {.val {missing}}.",
      "i" = "Call {.fn {setup_function}} before using this fit helper."
    ))
  }
  invisible(TRUE)
}

# Tracks which sugar helpers have already warned about the deprecated
# `handle` argument so the warning fires once per session.
#' @keywords internal
#' @noRd
.bg_deprecated_handle_env <- new.env(parent = emptyenv())

#' Warn once per session that the `handle` argument name is deprecated.
#' @keywords internal
#' @noRd
bg_warn_deprecated_handle_arg <- function(fn_name) {
  key <- paste0(fn_name, "-handle")
  if (isTRUE(get0(key, envir = .bg_deprecated_handle_env))) {
    return(invisible(NULL))
  }
  assign(key, TRUE, envir = .bg_deprecated_handle_env)
  cli::cli_warn(c(
    "{.arg handle} is deprecated for {.fn {fn_name}}; use {.arg project} instead.",
    "Positional passing is unaffected."
  ))
  invisible(NULL)
}

#' Fit a Stan model in one call
#'
#' Creates a data node (via [bg_set_node_data]), a `cmdstanr_fit` node consuming
#' it, dispatches [bg_run], and returns the fit node id invisibly while printing
#' the run handle. Extra
#' arguments in `...` become the fit node's params (e.g. `chains`, `seed`,
#' `iter_warmup`, `iter_sampling`).
#'
#' Call [bg_use_cmdstanr()] first so the `cmdstanr_fit` kind is registered.
#'
#' @param project A `bg_handle`.
#' @param stan_file Path to the Stan model file. Relative paths resolve
#'   against the project root when the file exists there, and from the working
#'   directory otherwise.
#' @param data A named list (or environment) of Stan data.
#' @param label Optional label for the fit node.
#' @param ... Passed to the fit node as params (e.g. `chains = 4`).
#' @param handle Deprecated synonym for `project`; accepted with a
#'   once-per-session warning.
#' @return The fit node id, invisibly. The run handle is printed.
#' @export
bg_fit_stan <- function(
  project,
  stan_file,
  data,
  label = NULL,
  ...,
  handle = NULL
) {
  if (!is.null(handle)) {
    if (!missing(project)) {
      cli::cli_abort(c(
        "Pass the project via {.arg project} or the deprecated {.arg handle},",
        "not both."
      ))
    }
    bg_warn_deprecated_handle_arg("bg_fit_stan")
    project <- handle
  }
  S7::check_is_S7(project, bg_handle)
  bg_require_backend_kinds(
    project,
    c("stan_data", "cmdstanr_fit"),
    "bg_use_cmdstanr"
  )

  data_node <- bg_add_node(
    project,
    kind = "stan_data",
    label = paste0(label %||% "fit", "_data")
  )
  bg_set_node_data(project, data_node, data)

  fit_params <- utils::modifyList(
    list(stan_file = stan_file),
    list(...)
  )
  fit_node <- bg_add_node(
    project,
    kind = "cmdstanr_fit",
    label = label %||% "fit",
    params = fit_params,
    inputs = data_node
  )

  run_handle <- bg_run(project, targets = fit_node)
  print(run_handle)
  invisible(fit_node)
}

#' Fit a brms model in one call
#'
#' Creates a data node (via [bg_set_node_data]), a `brms_fit` node consuming
#' it, dispatches [bg_run], and returns the fit node id invisibly while printing
#' the run handle. Extra arguments in `...`
#' become the fit node's params (e.g. `chains`, `seed`, `iter`).
#'
#' Call [bg_use_brms()] first so the `brms_fit` kind is registered.
#'
#' @param project A `bg_handle`.
#' @param formula A `brmsformula` or formula object.
#' @param data A data frame of observations.
#' @param label Optional label for the fit node.
#' @param ... Passed to the fit node as params (e.g. `chains = 4`).
#' @param handle Deprecated synonym for `project`; accepted with a
#'   once-per-session warning.
#' @return The fit node id, invisibly. The run handle is printed.
#' @export
bg_fit_brms <- function(
  project,
  formula,
  data,
  label = NULL,
  ...,
  handle = NULL
) {
  if (!is.null(handle)) {
    if (!missing(project)) {
      cli::cli_abort(c(
        "Pass the project via {.arg project} or the deprecated {.arg handle},",
        "not both."
      ))
    }
    bg_warn_deprecated_handle_arg("bg_fit_brms")
    project <- handle
  }
  S7::check_is_S7(project, bg_handle)
  bg_require_backend_kinds(
    project,
    c("data", "brms_fit"),
    "bg_use_brms"
  )

  data_node <- bg_add_node(
    project,
    kind = "data",
    label = paste0(label %||% "fit", "_data")
  )
  bg_set_node_data(project, data_node, data)

  fit_params <- utils::modifyList(
    list(formula = formula),
    list(...)
  )
  fit_node <- bg_add_node(
    project,
    kind = "brms_fit",
    label = label %||% "fit",
    params = fit_params,
    inputs = data_node
  )

  run_handle <- bg_run(project, targets = fit_node)
  print(run_handle)
  invisible(fit_node)
}
