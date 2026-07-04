# Wave scheduler --------------------------------------------------------------
#
# Drives bg_run() in topological waves: each wave is the set of nodes currently
# ready to execute (in plan$to_execute AND with all upstream artifacts already
# available, i.e. present in plan$input_bindings). Nodes within a wave are
# mutually independent by construction, so they can be dispatched in parallel
# (Milestone 3 Step 3); the sequential fallback executes them in topo order.
#
# Protocol holds and the pause flag are evaluated at WAVE BOUNDARIES, not
# between individual nodes within a wave. This is the documented semantic: a
# hold discovered from a fresh summary in wave N takes effect before wave N+1
# is dispatched, never mid-wave.

#' Compute the current execution wave.
#'
#' The wave is the set of nodes that are (a) scheduled for execution
#' (`plan$to_execute`) and (b) have all upstream artifact inputs resolved
#' (`plan$input_bindings[[node_id]]` is non-empty, OR the node has no upstream
#' edges — base nodes). These nodes are mutually independent by construction
#' (no edges run between them within the wave), so they may run concurrently.
#'
#' `plan$input_bindings[[node_id]]` is populated by `bg_derive_run_plan_state`
#' only when every upstream `artifact_ref` is available, so presence in
#' `names(plan$input_bindings)` is the ready signal.
#'
#' @param plan A run plan (from [bg_plan] / [bg_refresh_run_plan]).
#' @return Character vector of node IDs ready to execute now, in topological
#'   order. Empty when nothing is ready (run done, or remaining nodes are all
#'   held/blocked).
#' @keywords internal
#' @noRd
bg_compute_wave <- function(plan) {
  to_execute <- plan$to_execute %||% character()
  if (length(to_execute) == 0) {
    return(character())
  }
  ready <- names(plan$input_bindings %||% list())
  # Base nodes (no upstream edges) have an empty-but-present input_bindings
  # entry; nodes awaiting unresolved inputs are absent entirely. Intersect with
  # to_execute, then preserve topological order for deterministic execution.
  wave <- intersect(to_execute, ready)
  topo <- plan$graph_plan$topo_order %||% character()
  intersect(topo, wave)
}

#' Detect whether parallel dispatch should be used for the given mode.
#'
#' `parallel = "never"` always returns FALSE. `"auto"` returns TRUE only when
#' mirai reports active daemons (so a wave can be dispatched in parallel without
#' the caller having to wire anything up). `"always"` requires mirai daemons and
#' errors if they are unavailable. Soft dependency: returns FALSE (never/auto) or
#' errors cleanly (always) when mirai is not installed.
#'
#' @param parallel One of `"auto"`, `"never"`, `"always"`.
#' @return TRUE if the current wave should be dispatched in parallel.
#' @keywords internal
#' @noRd
bg_parallel_active <- function(parallel) {
  if (identical(parallel, "never")) {
    return(FALSE)
  }
  if (!requireNamespace("mirai", quietly = TRUE)) {
    if (identical(parallel, "always")) {
      cli::cli_abort(c(
        "{.arg parallel = \"always\"} requires the {.pkg mirai} package.",
        "i" = "Install it with {.run install.packages(\"mirai\")}, or use {.arg parallel = \"auto\"} to fall back to sequential execution when mirai is absent."
      ))
    }
    return(FALSE)
  }
  daemons_up <- bg_mirai_daemons_active()
  if (identical(parallel, "always") && !daemons_up) {
    cli::cli_abort(c(
      "{.arg parallel = \"always\"} requires active mirai daemons, but none are set.",
      "i" = "Start daemons with {.run mirai::daemons(4)} before calling {.fn bg_run}."
    ))
  }
  daemons_up
}

#' Report whether mirai daemons are currently set.
#'
#' mirai exposes daemon status via `mirai::status()` (a data frame with one row
#' per daemon) in current versions; older/newer builds may use
#' `mirai::daemons()` returning a non-NULL value when set. This helper tries the
#' modern check first and falls back gracefully so a future API change degrades
#' to "no daemons" (sequential) rather than erroring.
#' @return TRUE if at least one mirai daemon is active.
#' @keywords internal
#' @noRd
bg_mirai_daemons_active <- function() {
  status <- tryCatch(mirai::status(), error = function(e) NULL)
  if (is.data.frame(status) && nrow(status) > 0) {
    return(TRUE)
  }
  daemons <- tryCatch(mirai::daemons(), error = function(e) NULL)
  !is.null(daemons) &&
    (!is.numeric(daemons) || length(daemons) == 0 || daemons[[1]] > 0)
}
