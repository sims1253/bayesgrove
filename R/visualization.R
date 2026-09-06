# Visualization --------------------------------------------------------------
#
# bg_graph_mermaid renders the active graph as a Mermaid flowchart using
# dagriculture's dagri_mermaid, extended with bayesgrove state coloring
# (cached/ready/held/blocked/failed). bg_plot dispatches on a node's
# artifact/summary kind to a bayesplot method.

#' Mermaid flowchart of the active project graph
#'
#' Emits Mermaid flowchart text for the active graph: node label + kind, state
#' coloring (cached/ready/held/blocked/failed), and branch grouping via
#' subgraphs. Produces text without additional dependencies. The output renders
#' on GitHub and in Quarto. Embed it in [bg_export_report].
#'
#' @param project A `bg_handle`.
#' @param direction Mermaid graph direction (default `"TD"`; also `"LR"`).
#' @return A character scalar of Mermaid flowchart text with class
#'   `bg_mermaid`, whose print method renders the raw text (so the result is
#'   copy-pasteable at the console and embeddable as a string).
#' @export
bg_graph_mermaid <- function(project, direction = "TD") {
  S7::check_is_S7(project, bg_handle)

  # The graph carries recomputed state (cached/ready/failed); protocol holds
  # and structural blockers come from the refreshed run plan. The workflow
  # bundle computes graph + plan + holds once (same cost as one bg_status()
  # call, which builds the same bundle internally).
  graph <- bg_dagri_recompute_state(bg_read_graph(project))
  bundle <- tryCatch(bg_workflow_bundle(project), error = function(e) NULL)

  held_nodes <- character()
  blocked_nodes <- character()
  if (!is.null(bundle)) {
    held_nodes <- names(bundle$plan$held_by_policy %||% list())
    blocked_nodes <- names(bundle$plan$blocked %||% list())
  }

  node_label <- function(node) {
    kind <- node$kind %||% "?"
    label <- node$label %||% node$id
    sprintf("%s<br/>(%s)", label, kind)
  }

  node_class <- function(node) {
    id <- node$id
    if (!is.null(id) && id %in% held_nodes) {
      return("held")
    }
    if (!is.null(id) && id %in% blocked_nodes) {
      return("blocked")
    }
    state <- node$state %||% "ready"
    switch(
      state,
      "cached" = "cached",
      "ready" = "ready",
      "failed" = "failed",
      state
    )
  }

  text <- dagriculture::dagri_mermaid(
    graph,
    node_label = node_label,
    node_class = node_class,
    direction = direction
  )

  structure(text, class = c("bg_mermaid", "character"))
}

#' @method print bg_mermaid
#' @export
print.bg_mermaid <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}

#' Plot a node's artifact or diagnostics
#'
#' Selects a plot based on the node's kind:
#' - `ppc`: [bayesplot::ppc_dens_overlay()] on the stored plot-ready data.
#' - `loo_pit`: a base-graphics PIT ECDF plot against the Uniform CDF.
#' - `sbc`: a base-graphics rank histogram.
#' - `cmdstanr_fit` / `brms_fit`: [bayesplot::mcmc_trace()] over the draws.
#'
#' The bayesplot-backed kinds need the `bayesplot` package (a soft
#' dependency); the function aborts with an informative message when it is
#' missing or the node kind has no plot.
#'
#' For posterior-predictive checking, this graphical view is the primary
#' diagnostic: the p-values in a `ppc` node's summary are conservative
#' checks that flag substantial misfit. An unremarkable p-value is weak
#' evidence of adequacy. Inspect the overlay alongside the p-values.
#'
#' @param project A `bg_handle`.
#' @param node_id The node to plot.
#' @param ... Passed to the underlying bayesplot function (ignored for the
#'   base-graphics kinds).
#' @return For `ppc` and fit kinds, the bayesplot ggplot object. For
#'   `loo_pit` and `sbc`, `NULL` invisibly (the plot is drawn as a side
#'   effect).
#' @export
bg_plot <- function(project, node_id, ...) {
  S7::check_is_S7(project, bg_handle)

  node <- bg_read_graph(project)$nodes[[node_id]]
  if (is.null(node)) {
    cli::cli_abort("No node with id {.val {node_id}}.")
  }

  artifact <- tryCatch(
    bg_result(project, node_id),
    error = function(e) NULL
  )
  if (is.null(artifact)) {
    cli::cli_abort(
      "Node {.val {node_id}} has no artifact to plot. Run it first with {.fn bg_run}."
    )
  }

  kind <- node$kind
  plottable <- c("cmdstanr_fit", "brms_fit", "ppc", "loo_pit", "sbc")
  if (!kind %in% plottable) {
    cli::cli_abort(
      "No plot method for node kind {.val {kind}}. Kinds with plots: {.field {plottable}}."
    )
  }

  if (kind %in% c("cmdstanr_fit", "brms_fit")) {
    if (!requireNamespace("bayesplot", quietly = TRUE)) {
      cli::cli_abort(c(
        "Plotting fits requires the {.pkg bayesplot} package.",
        "i" = "Install it with {.run install.packages(\"bayesplot\")}."
      ))
    }
    fit <- artifact
    draws <- tryCatch(
      posterior::as_draws_matrix(fit$draws()),
      error = function(e) NULL
    )
    if (is.null(draws)) {
      cli::cli_abort(
        "Could not extract draws from the fit at {.val {node_id}} for plotting."
      )
    }
    return(bayesplot::mcmc_trace(draws, ...))
  }

  if (identical(kind, "ppc")) {
    if (!requireNamespace("bayesplot", quietly = TRUE)) {
      cli::cli_abort(c(
        "Plotting ppc nodes requires the {.pkg bayesplot} package.",
        "i" = "Install it with {.run install.packages(\"bayesplot\")}."
      ))
    }
    plot_data <- artifact$plot_data
    if (is.null(plot_data)) {
      cli::cli_abort(
        "Node {.val {node_id}} has no plot-ready ppc data (its artifact predates Milestone 4)."
      )
    }
    return(bayesplot::ppc_dens_overlay(
      plot_data$observed_y,
      plot_data$yrep,
      ...
    ))
  }

  if (identical(kind, "loo_pit")) {
    plot_data <- artifact$plot_data
    if (is.null(plot_data)) {
      cli::cli_abort("Node {.val {node_id}} has no plot-ready LOO-PIT data.")
    }
    # PIT ECDF overlay: the empirical CDF of the PIT values vs the Uniform CDF.
    pit <- sort(plot_data$pit_values)
    n <- length(pit)
    emp <- seq_len(n) / n
    graphics::plot(
      pit,
      emp,
      type = "s",
      xlim = c(0, 1),
      ylim = c(0, 1),
      xlab = "LOO-PIT value",
      ylab = "ECDF",
      main = "LOO-PIT calibration"
    )
    graphics::abline(0, 1, col = "grey", lty = 2)
    return(invisible(NULL))
  }

  if (identical(kind, "sbc")) {
    plot_data <- artifact$plot_data
    if (is.null(plot_data)) {
      cli::cli_abort("Node {.val {node_id}} has no plot-ready SBC rank data.")
    }
    graphics::barplot(
      plot_data$rank_histogram,
      main = "SBC rank histogram",
      xlab = "rank bin",
      ylab = "count"
    )
    graphics::abline(
      h = sum(plot_data$rank_histogram) / length(plot_data$rank_histogram),
      col = "grey",
      lty = 2
    )
    return(invisible(NULL))
  }

  cli::cli_abort("No plot method for node kind {.val {kind}}.")
}
