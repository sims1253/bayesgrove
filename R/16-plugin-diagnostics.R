#' Baseline diagnostics plugin
#'
#' This provides a minimal MVP implementation of standard Bayesian
#' diagnostic summaries.
#'
#' @return A backend implementation list
#' @export
bg_diagnostics_plugin <- function() {
  list(
    backend_diagnose = function(fit_result, params) {
      if (!requireNamespace("posterior", quietly = TRUE)) {
        cli::cli_abort(
          "The {.pkg posterior} package is required for diagnostics."
        )
      }

      # In a real implementation:
      # draws <- posterior::as_draws(fit_result)
      # posterior::summarise_draws(draws)

      list(
        diagnostic = TRUE,
        summary = "Mock diagnostic summary: Rhat < 1.05",
        status = "ok",
        fit_ref = fit_result$chains
      )
    },
    backend_compare = function(fits, params) {
      if (!requireNamespace("loo", quietly = TRUE)) {
        cli::cli_abort(
          "The {.pkg loo} package is required for model comparison."
        )
      }

      # In a real implementation:
      # loos <- lapply(fits, loo::loo)
      # loo::loo_compare(loos)

      list(
        comparison = TRUE,
        summary = "Mock loo comparison: model 2 is better",
        method = params$method %||% "loo"
      )
    }
  )
}
