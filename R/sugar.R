# Practitioner convenience helpers (Milestone 6 item 9) ---------------------
#
# Pure sugar over the existing graph verbs. Each one collapses the minimal
# fit into a single call: create the data node, create the fit node, run it,
# and return the fit node id (invisibly) with the run handle printed. No new
# semantics; all extra `...` params pass through to the fit node.

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

#' Fit a Stan model in one call
#'
#' Creates a data node (via [bg_set_node_data]), a `cmdstanr_fit` node consuming
#' it, dispatches [bg_run], and returns the fit node id invisibly while printing
#' the run handle. Pure sugar over the existing verbs — no new semantics. Extra
#' arguments in `...` become the fit node's params (e.g. `chains`, `seed`,
#' `iter_warmup`, `iter_sampling`).
#'
#' Call [bg_use_cmdstanr()] first so the `cmdstanr_fit` kind is registered.
#'
#' @param handle A `bg_handle`.
#' @param stan_file Path to the Stan model file.
#' @param data A named list (or environment) of Stan data.
#' @param label Optional label for the fit node.
#' @param ... Passed to the fit node as params (e.g. `chains = 4`).
#' @return The fit node id, invisibly. The run handle is printed.
#' @export
bg_fit_stan <- function(handle, stan_file, data, label = NULL, ...) {
  S7::check_is_S7(handle, bg_handle)
  bg_require_backend_kinds(
    handle,
    c("stan_data", "cmdstanr_fit"),
    "bg_use_cmdstanr"
  )

  # Create + populate the data node, then the fit node consuming it.
  data_node <- bg_add_node(
    handle,
    kind = "stan_data",
    label = paste0(label %||% "fit", "_data")
  )
  bg_set_node_data(handle, data_node, data)

  fit_params <- utils::modifyList(
    list(stan_file = stan_file),
    list(...)
  )
  fit_node <- bg_add_node(
    handle,
    kind = "cmdstanr_fit",
    label = label %||% "fit",
    params = fit_params,
    inputs = data_node
  )

  run_handle <- bg_run(handle, targets = fit_node)
  print(run_handle)
  invisible(fit_node)
}

#' Fit a brms model in one call
#'
#' Creates a data node (via [bg_set_node_data]), a `brms_fit` node consuming
#' it, dispatches [bg_run], and returns the fit node id invisibly while printing
#' the run handle. Pure sugar over the existing verbs. Extra arguments in `...`
#' become the fit node's params (e.g. `chains`, `seed`, `iter`).
#'
#' Call [bg_use_brms()] first so the `brms_fit` kind is registered.
#'
#' @param handle A `bg_handle`.
#' @param formula A `brmsformula` or formula object.
#' @param data A data frame of observations.
#' @param label Optional label for the fit node.
#' @param ... Passed to the fit node as params (e.g. `chains = 4`).
#' @return The fit node id, invisibly. The run handle is printed.
#' @export
bg_fit_brms <- function(handle, formula, data, label = NULL, ...) {
  S7::check_is_S7(handle, bg_handle)
  bg_require_backend_kinds(
    handle,
    c("data", "brms_fit"),
    "bg_use_brms"
  )

  data_node <- bg_add_node(
    handle,
    kind = "data",
    label = paste0(label %||% "fit", "_data")
  )
  bg_set_node_data(handle, data_node, data)

  fit_params <- utils::modifyList(
    list(formula = formula),
    list(...)
  )
  fit_node <- bg_add_node(
    handle,
    kind = "brms_fit",
    label = label %||% "fit",
    params = fit_params,
    inputs = data_node
  )

  run_handle <- bg_run(handle, targets = fit_node)
  print(run_handle)
  invisible(fit_node)
}
