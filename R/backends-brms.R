# brms built-in executors ---------------------------------------------------
#
# Registers brms-based node kinds. loo/compare/ppc are shared with the
# cmdstanr executors (they operate on draws via bg_extract_draws).

#' Register brms node kinds on a project.
#'
#' Registers `brms_fit` and `brms_prior_fit` built-in executors. The
#' `loo`, `compare`, and `ppc` kinds are shared with [bg_use_cmdstanr()] and
#' are registered here too so a brms-only project can use them without calling
#' both setup functions.
#'
#' @param project A `bg_handle`.
#' @return Invisibly, the project handle.
#' @export
bg_use_brms <- function(project) {
  S7::check_is_S7(project, bg_handle)

  kind_specs <- list(
    stan_data = list(
      executor = bg_executor_stan_data,
      output_type = "list"
    ),
    brms_fit = list(
      executor = bg_executor_brms_fit,
      output_type = "brmsfit"
    ),
    brms_prior_fit = list(
      executor = bg_executor_brms_prior_fit,
      output_type = "brmsfit"
    ),
    loo = list(
      executor = bg_executor_loo,
      output_type = "loo"
    ),
    compare = list(
      executor = bg_executor_compare,
      output_type = "comparison"
    ),
    ppc = list(
      executor = bg_executor_ppc,
      output_type = "ppc"
    )
  )

  for (id in names(kind_specs)) {
    spec <- kind_specs[[id]]
    bg_register_builtin_executor(id, spec$executor)
    bg_register_node_kind_builtin(
      project = project,
      kind = id,
      executor_ref = paste0("builtin:", id),
      input_contract = spec$input_contract,
      output_type = spec$output_type
    )
  }

  invisible(project)
}

# --- Executor implementations ----------------------------------------------

#' @keywords internal
#' @noRd
bg_executor_brms_fit <- function(node, inputs) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg brms} package is required for brms_fit nodes.",
      "i" = "Install it with {.code install.packages('brms')}."
    ))
  }

  formula <- node$params$formula %||% NULL
  if (is.null(formula)) {
    cli::cli_abort("{.field formula} is required for brms_fit nodes.")
  }
  if (is.character(formula)) {
    formula <- stats::as.formula(formula)
  }

  data <- bg_resolve_fit_data(node, inputs)
  if (is.null(data)) {
    cli::cli_abort("brms_fit requires a data input node.")
  }

  priors <- bg_brms_build_priors(node$params$priors)

  # Assemble the brm() argument list and drop NULL entries before do.call, so a
  # missing seed does not pass seed = NULL (brms' default is NA, not NULL, and
  # NULL errors inside brm()). Mirrors the cmdstanr executor's arg pattern.
  brm_args <- list(
    formula = formula,
    data = data,
    family = node$params$family %||% stats::gaussian(),
    prior = priors,
    chains = node$params$chains %||% 4,
    iter = (node$params$iter_warmup %||% 1000) +
      (node$params$iter_sampling %||% 1000),
    warmup = node$params$iter_warmup %||% 1000,
    seed = node$params$seed %||% NULL,
    control = list(
      adapt_delta = node$params$adapt_delta %||% 0.8,
      max_treedepth = node$params$max_treedepth %||% 10
    ),
    silent = 2,
    refresh = 0
  )
  brm_args <- brm_args[!vapply(brm_args, is.null, logical(1))]
  fit <- do.call(brms::brm, brm_args)

  metrics <- bg_brms_hmc_metrics(
    fit,
    max_treedepth = node$params$max_treedepth %||% 10
  )
  thresholds <- node$params$thresholds %||% list()
  severity <- bg_hmc_severity(metrics, thresholds)

  summary <- list(
    summary_kind = "hmc_diagnostics",
    passed = identical(severity, "ok"),
    severity = severity,
    metrics = metrics
  )

  list(result = fit, summaries = list(summary))
}

#' @keywords internal
#' @noRd
bg_executor_brms_prior_fit <- function(node, inputs) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg brms} package is required for brms_prior_fit nodes."
    )
  }

  formula <- node$params$formula %||% NULL
  if (is.null(formula)) {
    cli::cli_abort("{.field formula} is required for brms_prior_fit nodes.")
  }
  if (is.character(formula)) {
    formula <- stats::as.formula(formula)
  }

  data <- bg_resolve_fit_data(node, inputs, default = data.frame())

  brm_args <- list(
    formula = formula,
    data = data,
    sample_prior = "only",
    chains = node$params$chains %||% 4,
    iter = (node$params$iter_warmup %||% 1000) +
      (node$params$iter_sampling %||% 1000),
    warmup = node$params$iter_warmup %||% 1000,
    seed = node$params$seed %||% NULL,
    silent = 2,
    refresh = 0
  )
  brm_args <- brm_args[!vapply(brm_args, is.null, logical(1))]
  fit <- do.call(brms::brm, brm_args)

  # Match the cmdstanr prior-fit executor: carry the draws summary as evidence.
  # Severity stays "ok" absent a check function (that hook arrives with the
  # trusted-executor mechanism); evidence must still be non-empty.
  draws_summary <- tryCatch(
    posterior::summarise_draws(posterior::as_draws(fit)),
    error = function(e) NULL
  )

  summary <- list(
    summary_kind = "prior_predictive_check",
    passed = TRUE,
    severity = "ok",
    metrics = list(draws_summary = draws_summary)
  )

  list(result = fit, summaries = list(summary))
}

#' Extract HMC diagnostic metrics from a brmsfit.
#'
#' Uses posterior::summarise_draws and brms::nuts_params to compute the same
#' metric shape as the cmdstanr helper.
#' @keywords internal
#' @noRd
bg_brms_hmc_metrics <- function(fit, max_treedepth = 10) {
  requireNamespace("posterior", quietly = TRUE)

  draws_df <- posterior::summarise_draws(posterior::as_draws(fit))
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)

  nuts_metrics <- bg_brms_nuts_metrics(np, max_treedepth = max_treedepth)

  rhats <- draws_df$rhat[is.finite(draws_df$rhat)]
  max_rhat <- if (length(rhats) > 0) max(rhats) else 1.0

  bulk_ess <- draws_df$ess_bulk[is.finite(draws_df$ess_bulk)]
  min_bulk_ess <- if (length(bulk_ess) > 0) min(bulk_ess) else Inf

  tail_ess <- draws_df$ess_tail[is.finite(draws_df$ess_tail)]
  min_tail_ess <- if (length(tail_ess) > 0) min(tail_ess) else Inf

  list(
    divergences = nuts_metrics$divergences,
    max_treedepth_hits = nuts_metrics$max_treedepth_hits,
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk_ess,
    min_tail_ess = min_tail_ess,
    e_bfmi = nuts_metrics$e_bfmi,
    num_transitions = tryCatch(
      nrow(posterior::as_draws_matrix(posterior::as_draws(fit))),
      error = function(e) NA_integer_
    )
  )
}

#' Derive HMC counts/divergences/E-BFMI from a brms::nuts_params() data frame.
#'
#' Pure helper split out of [bg_brms_hmc_metrics] so it can be unit-tested
#' against a synthetic data frame without faking a brmsfit. Expects columns
#' `Chain`, `Parameter`, `Value`. Returns NULL-valued/zero metrics when `np`
#' is NULL.
#' @keywords internal
#' @noRd
bg_brms_nuts_metrics <- function(np, max_treedepth = 10) {
  if (is.null(np)) {
    return(list(
      divergences = 0L,
      max_treedepth_hits = 0L,
      e_bfmi = Inf
    ))
  }

  divergences <- 0L
  max_treedepth_hits <- 0L
  e_bfmi_values <- NULL

  div_rows <- np[
    grepl("divergent__", np$Parameter, fixed = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(div_rows) > 0) {
    divergences <- as.integer(sum(div_rows$Value > 0, na.rm = TRUE))
  }

  td_rows <- np[
    grepl("treedepth__", np$Parameter, fixed = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(td_rows) > 0) {
    max_treedepth_hits <- as.integer(
      sum(td_rows$Value >= max_treedepth, na.rm = TRUE)
    )
  }

  # E-BFMI is derived per chain from the energy__ sampler parameter, not read
  # as a raw value: energy is on an arbitrary scale, so min(energy) is never
  # comparable to the 0.3 threshold. Per chain with energies E,
  #   ebfmi = sum(diff(E)^2) / ((length(E) - 1) * var(E)).
  eb_rows <- np[grepl("energy__", np$Parameter, fixed = TRUE), , drop = FALSE]
  if (nrow(eb_rows) > 0) {
    # Order within each chain matters: E-BFMI uses diff(E), which is
    # iteration-order sensitive. nuts_params() normally returns sorted rows,
    # but sort explicitly so the result never depends on input row order.
    eb_rows <- eb_rows[order(eb_rows$Chain, eb_rows$Iteration), , drop = FALSE]
    e_bfmi_values <- vapply(
      split(eb_rows$Value, eb_rows$Chain),
      function(e) {
        if (length(e) < 3L) {
          return(NA_real_)
        }
        v <- stats::var(e)
        if (!is.finite(v) || v <= 0) {
          return(NA_real_)
        }
        sum(diff(e)^2) / ((length(e) - 1) * v)
      },
      numeric(1)
    )
  }

  finite_e_bfmi <- e_bfmi_values[is.finite(e_bfmi_values)]
  min_e_bfmi <- if (length(finite_e_bfmi) > 0) min(finite_e_bfmi) else Inf

  list(
    divergences = as.integer(divergences),
    max_treedepth_hits = as.integer(max_treedepth_hits),
    e_bfmi = min_e_bfmi
  )
}

#' Translate a node priors param into a brmsprior object (or list thereof).
#'
#' Accepts either a single prior spec (a named list of `brms::set_prior`
#' arguments, a character string, or a brmsprior) or an unnamed list of such
#' specs. Returns NULL when `priors` is NULL, a single `brmsprior`, or a
#' combined `c(...)` of brmsprior objects for multiple priors.
#' @keywords internal
#' @noRd
bg_brms_build_priors <- function(priors) {
  if (is.null(priors)) {
    return(NULL)
  }

  build_one <- function(spec) {
    if (inherits(spec, "brmsprior")) {
      spec
    } else if (is.character(spec)) {
      brms::set_prior(spec)
    } else if (is.list(spec)) {
      do.call(brms::set_prior, spec)
    } else {
      cli::cli_abort(
        "Unsupported prior spec of class {.cls {class(spec)[1]}} in {.field priors}."
      )
    }
  }

  # An unnamed list of specs: each element is itself a list/character/brmsprior.
  if (
    is.list(priors) &&
      is.null(names(priors)) &&
      all(vapply(
        priors,
        function(x) {
          is.list(x) || is.character(x) || inherits(x, "brmsprior")
        },
        logical(1)
      ))
  ) {
    built <- lapply(priors, build_one)
    return(do.call(c, built))
  }

  build_one(priors)
}
