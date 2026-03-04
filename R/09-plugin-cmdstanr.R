#' Baseline cmdstanr backend plugin
#'
#' This provides a minimal MVP implementation of the cmdstanr backend
#' handling the compile/sample split as outlined in the design docs.
#'
#' @return A backend implementation list
#' @export
bg_cmdstanr_plugin <- function() {
  list(
    backend_compile = function(params) {
      if (!requireNamespace("cmdstanr", quietly = TRUE)) {
        cli::cli_abort("The {.pkg cmdstanr} package is required.")
      }

      # In a real implementation this would compile the model
      # model <- cmdstanr::cmdstan_model(params$stan_file)
      # return(model)

      list(compiled = TRUE, backend = "cmdstanr")
    },
    backend_fit = function(compiled_model, data, params) {
      if (!requireNamespace("cmdstanr", quietly = TRUE)) {
        cli::cli_abort("The {.pkg cmdstanr} package is required.")
      }

      # model$sample(data = data, chains = params$chains, iter_sampling = params$iter_sampling)
      list(fitted = TRUE, backend = "cmdstanr", chains = params$chains)
    },
    backend_source_hash = function(params) {
      if (is.null(params$stan_file) || !file.exists(params$stan_file)) {
        return("no_source_file")
      }
      digest::digest(file = params$stan_file, algo = "sha256")
    },
    backend_runtime_signature = function(params) {
      list(
        fingerprint_fields = list(
          backend = "cmdstanr",
          version = "0.1.0" # cmdstanr::cmdstan_version()
        ),
        compatibility_fields = list(
          os = Sys.info()[["sysname"]]
        )
      )
    }
  )
}
