# cmdstanr built-in executors -----------------------------------------------
#
# Registers node kinds that wrap cmdstanr for real model fitting and compute
# HMC diagnostics themselves. Every executor takes (node, inputs) and returns
# list(result=, summaries=).

#' Compute HMC diagnostic severity from sampler metrics.
#'
#' Thresholds follow Vehtari et al. (2021) for R-hat/ESS. Defaults are
#' overridable via node params (`rhat_error`, `rhat_warn`, `ess_warn`,
#' `ebfmi_warn`, `divergence_rate_error`).
#'
#' The ESS warning threshold scales with the chain count: Vehtari et al.
#' (2021) recommend ~100 effective samples per chain. When `metrics` carries
#' an `n_chains` entry, `ess_warn` defaults to `100 * n_chains`; otherwise it
#' stays 400 (the historical default). An explicit `thresholds$ess_warn`
#' always wins over both.
#'
#' @param metrics Named list with: divergences, max_treedepth_hits,
#'   max_rhat, min_bulk_ess, min_tail_ess, e_bfmi, num_transitions.
#'   `num_transitions` is the total post-warmup transitions across all
#'   chains (the denominator for the per-transition divergence rate),
#'   since divergences are summed across chains. May optionally carry
#'   `n_chains` to scale the default ESS warning threshold.
#' @param thresholds Optional list overriding default thresholds.
#' @return One of "ok", "warning", "error".
#' @export
bg_hmc_severity <- function(metrics, thresholds = list()) {
  n_chains <- metrics$n_chains
  ess_warn_default <- if (
    !is.null(n_chains) &&
      is.finite(n_chains) &&
      n_chains > 0
  ) {
    100 * n_chains
  } else {
    400
  }
  defaults <- list(
    rhat_error = 1.05,
    rhat_warn = 1.01,
    ess_warn = ess_warn_default,
    ebfmi_warn = 0.3,
    divergence_rate_error = 0.01
  )
  t <- utils::modifyList(defaults, thresholds %||% list())

  divergences <- metrics$divergences %||% 0L
  divergence_rate <- bg_hmc_divergence_rate(metrics)$value

  max_rhat <- metrics$max_rhat %||% 1.0
  min_bulk_ess <- metrics$min_bulk_ess %||% Inf
  min_tail_ess <- metrics$min_tail_ess %||% Inf
  e_bfmi <- metrics$e_bfmi %||% Inf

  if (
    max_rhat > t$rhat_error ||
      (divergences > 0 &&
        !is.na(divergence_rate) &&
        divergence_rate > t$divergence_rate_error)
  ) {
    return("error")
  }
  if (
    max_rhat > t$rhat_warn ||
      divergences > 0 ||
      e_bfmi < t$ebfmi_warn ||
      min_bulk_ess < t$ess_warn ||
      min_tail_ess < t$ess_warn ||
      (metrics$max_treedepth_hits %||% 0L) > 0
  ) {
    return("warning")
  }

  "ok"
}

#' Compute the divergence rate without inventing a zero denominator.
#'
#' @keywords internal
#' @noRd
bg_hmc_divergence_rate <- function(metrics) {
  num_transitions <- metrics$num_transitions %||% NA_integer_
  available <- length(num_transitions) == 1L &&
    !is.na(num_transitions) &&
    num_transitions > 0

  list(
    value = if (available) {
      (metrics$divergences %||% 0L) / num_transitions
    } else {
      NA_real_
    },
    available = available
  )
}

#' Extract HMC diagnostic metrics from a CmdStanMCMC fit.
#'
#' @param fit A CmdStanMCMC object with draws already materialized.
#' @return Named list of metrics for severity computation.
#' @keywords internal
#' @noRd
bg_cmdstanr_hmc_metrics <- function(fit) {
  requireNamespace("posterior", quietly = TRUE)

  diag <- fit$diagnostic_summary()
  draws_df <- posterior::summarise_draws(fit$draws())

  divergences <- sum(diag$num_divergent %||% 0L, na.rm = TRUE)
  max_treedepth_hits <- sum(diag$num_max_treedepth %||% 0L, na.rm = TRUE)

  rhats <- draws_df$rhat[is.finite(draws_df$rhat)]
  max_rhat <- if (length(rhats) > 0) max(rhats) else 1.0

  bulk_ess <- draws_df$ess_bulk[is.finite(draws_df$ess_bulk)]
  min_bulk_ess <- if (length(bulk_ess) > 0) min(bulk_ess) else Inf

  tail_ess <- draws_df$ess_tail[is.finite(draws_df$ess_tail)]
  min_tail_ess <- if (length(tail_ess) > 0) min(tail_ess) else Inf

  # E-BFMI per chain (if available); take the minimum over finite chains.
  # A single NA chain must not disable the whole check (filter first).
  e_bfmi_values <- diag$ebfmi %||% NULL
  finite_e_bfmi <- e_bfmi_values[is.finite(e_bfmi_values)]
  min_e_bfmi <- if (length(finite_e_bfmi) > 0) min(finite_e_bfmi) else Inf

  # Total post-warmup transitions across all chains, used for the divergence
  # RATE (divergences / num_transitions). divergences are summed across
  # chains, so the denominator must be the total transition count too;
  # otherwise the rate is inflated by the chain count. Coerce to a
  # draws_matrix first so nrow() is the true total draw count (as.matrix on a
  # draws_array flattens to a single column of draws * variables rows).
  num_transitions <- tryCatch(
    nrow(posterior::as_draws_matrix(fit$draws())),
    error = function(e) NA_integer_
  )

  metrics <- list(
    divergences = as.integer(divergences),
    max_treedepth_hits = as.integer(max_treedepth_hits),
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk_ess,
    min_tail_ess = min_tail_ess,
    e_bfmi = min_e_bfmi,
    num_transitions = as.integer(num_transitions),
    n_chains = as.integer(fit$num_chains() %||% NA_integer_)
  )
  rate <- bg_hmc_divergence_rate(metrics)
  metrics$divergence_rate <- rate$value
  metrics$divergence_rate_available <- rate$available
  metrics
}

#' Register cmdstanr node kinds on a project.
#'
#' Registers the built-in cmdstanr executor node kinds: `stan_data`,
#' `cmdstanr_fit`, `prior_fit`, `loo`, `compare`, `ppc`, `loo_pit`, and `sbc`.
#' Each executor is a built-in resolved by reference (no source text
#' persisted). The fit executors compute HMC diagnostics themselves and emit
#' `hmc_diagnostics`
#' summaries with severity rules; no user-written diagnostic code is required.
#'
#' @param project A `bg_handle`.
#' @return Invisibly, the project handle.
#' @export
bg_use_cmdstanr <- function(project) {
  S7::check_is_S7(project, bg_handle)

  kind_specs <- list(
    stan_data = list(executor = bg_executor_stan_data, output_type = "list"),
    cmdstanr_fit = list(
      executor = bg_executor_cmdstanr_fit,
      output_type = "CmdStanMCMC"
    ),
    prior_fit = list(
      executor = bg_executor_cmdstanr_prior_fit,
      output_type = "CmdStanMCMC"
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
    ),
    loo_pit = list(
      executor = bg_executor_loo_pit,
      output_type = "loo_pit"
    ),
    sbc = list(
      executor = bg_executor_sbc,
      output_type = "sbc"
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

#' Register a node kind backed by a built-in executor reference.
#'
#' Registers the kind in the structural graph (so it can be added as a node)
#' and in the runtime handle with executor_ref = "builtin:<id>" so it restores
#' safely on open (Phase 2.2) and fingerprints by id + version (Phase 2.1).
#' @keywords internal
#' @noRd
bg_register_node_kind_builtin <- function(
  project,
  kind,
  executor_ref,
  input_contract = NULL,
  output_type = NULL
) {
  # Structural graph registration (so bg_add_node accepts the kind).
  graph <- bg_read_graph(project)
  if (is.null(graph$registry$kinds[[kind]])) {
    new_kind <- dagriculture::dagri_kind(
      name = kind,
      input_contract = input_contract,
      output_type = output_type
    )
    graph$registry$kinds[[kind]] <- new_kind
    graph$version <- graph$version + 1L
    bg_commit_graph(project, graph)
  }

  # Runtime handle registration with the built-in executor.
  if (is.null(project@registries$node_kinds)) {
    project@registries$node_kinds <- list()
  }
  executor <- bg_resolve_builtin_executor(executor_ref)
  project@registries$node_kinds[[kind]] <- list(
    name = kind,
    executor = executor,
    executor_ref = executor_ref
  )

  # Persist structurally (executor_ref only, no source text).
  config <- bg_read_project_config(project)
  manifest <- config$runtime_manifest %||% bg_empty_runtime_manifest()
  manifest$node_kinds[[kind]] <- bg_node_kind_manifest_entry(
    kind = kind,
    input_contract = input_contract,
    output_type = output_type,
    executor_ref = executor_ref
  )
  config$runtime_manifest <- manifest
  bg_write_project_config_path(project@path, config)

  invisible(TRUE)
}

# --- Executor implementations ----------------------------------------------

#' Resolve the data input for a fit executor.
#'
#' `data_input` may name an upstream node id directly; if it does not match a
#' node id, the first input is used. This bridges the gap between bayesgrove's
#' node-id-keyed inputs and the user-facing `data_input` param.
#' @keywords internal
#' @noRd
bg_resolve_fit_data <- function(node, inputs, default = NULL) {
  if (length(inputs) == 0) {
    return(default)
  }

  data_input <- node$params$data_input %||% NULL
  if (!is.null(data_input) && data_input %in% names(inputs)) {
    return(inputs[[data_input]])
  }

  inputs[[1]]
}

#' @keywords internal
#' @noRd
bg_executor_stan_data <- function(node, inputs) {
  # Resolution order: resolved$data (resolved by bg_execute_node from a cas:
  # data_ref) > inline data param > abort. Params are data, never code: the
  # former data_fn_source eval channel has been removed; use bg_set_node_data()
  # to attach a data object.
  if (!is.null(node$resolved$data)) {
    list(
      result = node$resolved$data,
      summaries = list()
    )
  } else if (!is.null(node$params$data)) {
    list(result = node$params$data, summaries = list())
  } else {
    cli::cli_abort(c(
      "No data attached to this {.field stan_data} node.",
      "i" = "Attach a data object with {.fn bg_set_node_data} before running."
    ))
  }
}

#' @keywords internal
#' @noRd
bg_executor_cmdstanr_fit <- function(node, inputs) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg cmdstanr} package is required for cmdstanr_fit nodes.",
      "i" = paste0(
        "Install it with {.code install.packages('cmdstanr', repos = ",
        "c('https://stan-dev.r-universe.dev', getOption('repos')))} and ",
        "run {.code cmdstanr::install_cmdstan()}."
      )
    ))
  }

  stan_file <- node$params$stan_file %||% NULL
  if (is.null(stan_file) || !file.exists(stan_file)) {
    cli::cli_abort("{.field stan_file} must point to an existing Stan program.")
  }

  data <- bg_resolve_fit_data(node, inputs)

  model <- cmdstanr::cmdstan_model(stan_file = stan_file)

  sampler_args <- list(
    data = data,
    chains = node$params$chains %||% 4,
    iter_warmup = node$params$iter_warmup %||% 1000,
    iter_sampling = node$params$iter_sampling %||% 1000,
    seed = node$params$seed %||% NULL,
    adapt_delta = node$params$adapt_delta %||% NULL,
    max_treedepth = node$params$max_treedepth %||% NULL
  )
  sampler_args <- sampler_args[!vapply(sampler_args, is.null, logical(1))]

  fit <- do.call(model$sample, sampler_args)

  # Materialize draws so the CmdStanMCMC object is self-contained under saveRDS.
  fit$draws()

  metrics <- bg_cmdstanr_hmc_metrics(fit)
  thresholds <- node$params$thresholds %||% list()
  severity <- bg_hmc_severity(metrics, thresholds)

  passed <- identical(severity, "ok")

  summary <- list(
    summary_kind = "hmc_diagnostics",
    passed = passed,
    severity = severity,
    metrics = metrics
  )

  list(result = fit, summaries = list(summary))
}

#' @keywords internal
#' @noRd
bg_executor_cmdstanr_prior_fit <- function(node, inputs) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg cmdstanr} package is required for prior_fit nodes."
    )
  }

  stan_file <- node$params$stan_file %||% NULL
  if (is.null(stan_file) || !file.exists(stan_file)) {
    cli::cli_abort("{.field stan_file} must point to an existing Stan program.")
  }

  data <- bg_resolve_fit_data(node, inputs, default = list())

  model <- cmdstanr::cmdstan_model(stan_file = stan_file)

  fit <- model$sample(
    data = data,
    chains = 1,
    iter_warmup = node$params$iter_warmup %||% 1000,
    iter_sampling = node$params$iter_sampling %||% 1000,
    seed = node$params$seed %||% NULL,
    fixed_param = TRUE
  )
  fit$draws()

  draws_summary <- tryCatch(
    posterior::summarise_draws(fit$draws()),
    error = function(e) NULL
  )

  # Prior-predictive checking with a custom function now returns as a
  # trusted-executor pattern: users register their own node kind wrapping
  # prior_fit, which goes through the bg_restore_executors trust gate. The
  # former check_fn_source params-as-code channel has been removed.
  summary <- list(
    summary_kind = "prior_predictive_check",
    passed = NA,
    severity = "ok",
    metrics = list(draws_summary = draws_summary, graded = FALSE)
  )

  list(result = fit, summaries = list(summary))
}

#' @keywords internal
#' @noRd
bg_executor_loo <- function(node, inputs) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    cli::cli_abort("The {.pkg loo} package is required for loo nodes.")
  }

  fit_artifact <- inputs[[1]]
  draws_matrix <- bg_extract_draws(fit_artifact, as = "matrix")
  draws_array <- bg_extract_draws(fit_artifact, as = "array")

  log_lik_var <- node$params$log_lik_var %||% "log_lik"
  ll_cols <- bg_indexed_var_cols(log_lik_var, colnames(draws_matrix))
  ll_draws <- draws_matrix[, ll_cols, drop = FALSE]
  ll_array <- draws_array[,, ll_cols, drop = FALSE]

  if (ncol(ll_draws) == 0) {
    cli::cli_abort(
      "No draws matching {.val {log_lik_var}} found in the fit for {.field loo}."
    )
  }

  # loo warns about missing r_eff when given a flattened matrix. Compute it
  # from the chain-shaped draws_array so per-chain effective sample sizes are
  # used; loo::relative_eff expects the pointwise log-lik on the exp scale.
  r_eff <- tryCatch(
    loo::relative_eff(exp(ll_array)),
    error = function(e) NULL
  )
  loo_obj <- loo::loo(ll_draws, r_eff = r_eff)
  ks <- loo_obj$diagnostics$pareto_k

  high_k <- sum(ks > 0.7 & ks <= 1, na.rm = TRUE)
  bad_k <- sum(ks > 1, na.rm = TRUE)

  severity <- if (bad_k > 0) {
    "error"
  } else if (high_k > 0) {
    "warning"
  } else {
    "ok"
  }

  summary <- list(
    summary_kind = "loo_diagnostics",
    passed = identical(severity, "ok"),
    severity = severity,
    metrics = list(
      pareto_k_high = as.integer(high_k),
      pareto_k_bad = as.integer(bad_k)
    )
  )

  list(result = loo_obj, summaries = list(summary))
}

#' @keywords internal
#' @noRd
bg_executor_compare <- function(node, inputs) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    cli::cli_abort("The {.pkg loo} package is required for compare nodes.")
  }

  loo_objs <- inputs
  if (length(loo_objs) < 2) {
    cli::cli_abort("{.field compare} requires at least two loo input nodes.")
  }

  comparison <- loo::loo_compare(loo_objs)
  comparison_df <- as.data.frame(comparison)
  comparison_table <- list(
    model = rownames(comparison_df),
    elpd_diff = unname(comparison_df$elpd_diff),
    se_diff = unname(comparison_df$se_diff)
  )
  # loo_compare orders best-first, so comparison[1, 1] (the best model's
  # elpd_diff) is always 0 by construction. Report the runner-up's gap as the
  # headline metric so it is usable; NA when fewer than two models compare.
  runner_up_elpd_diff <- if (nrow(comparison_df) >= 2) {
    comparison_df$elpd_diff[[2]]
  } else {
    NA_real_
  }

  # loo::stacking_weights expects a pointwise lpd matrix, not a list of loo
  # objects; use loo_model_weights(method = "stacking") instead. On error,
  # surface the message in the summary metadata rather than dropping it.
  stacking_error <- NULL
  stacking <- tryCatch(
    loo::loo_model_weights(loo_objs, method = "stacking"),
    error = function(e) {
      stacking_error <<- conditionMessage(e)
      NULL
    }
  )

  summaries <- list(list(
    summary_kind = "comparison_results",
    passed = TRUE,
    severity = "ok",
    metrics = list(
      elpd_diff = runner_up_elpd_diff,
      comparison_table = comparison_table
    )
  ))

  summaries <- c(
    summaries,
    list(list(
      summary_kind = "stacking_weights",
      passed = !is.null(stacking),
      severity = if (is.null(stacking)) "warning" else "ok",
      metrics = as.list(stacking),
      metadata = if (!is.null(stacking_error)) {
        list(stacking_error = stacking_error)
      } else {
        list()
      }
    ))
  )

  list(result = comparison, summaries = summaries)
}

#' @keywords internal
#' @noRd
bg_executor_ppc <- function(node, inputs) {
  if (length(inputs) < 2) {
    cli::cli_abort(
      paste0(
        "{.field ppc} requires two inputs: a fit node and a data node ",
        "supplying the observed {.field {node$params$y_var %||% 'y'}}."
      )
    )
  }

  # The fit artifact supplies posterior-predictive draws (yrep); the data
  # node supplies the observed y. Resolve by param name, falling back to
  # fit-first / data-second ordering.
  fit_input <- node$params$fit_input %||% NULL
  data_input <- node$params$data_input %||% NULL

  fit_artifact <- if (!is.null(fit_input) && fit_input %in% names(inputs)) {
    inputs[[fit_input]]
  } else {
    inputs[[1]]
  }
  observed_data <- if (!is.null(data_input) && data_input %in% names(inputs)) {
    inputs[[data_input]]
  } else {
    inputs[[2]]
  }

  draws <- bg_extract_draws(fit_artifact)

  y_var <- node$params$y_var %||% "y"
  yrep_var <- node$params$yrep_var %||% "yrep"
  stats <- node$params$stats %||% c("mean", "sd", "min", "max")

  # Observed y comes from the data node, not from the fit's draws.
  y <- observed_data[[y_var]] %||% NULL
  if (is.null(y)) {
    # Also check the data node itself (e.g. if it is a bare vector).
    if (is.numeric(observed_data)) {
      y <- observed_data
    } else {
      cli::cli_abort(
        "Observed variable {.val {y_var}} not found in the ppc data input."
      )
    }
  }

  # Posterior-predictive draws (yrep) come from the fit.
  yrep_cols <- bg_indexed_var_cols(yrep_var, colnames(draws))
  yrep <- draws[, yrep_cols, drop = FALSE]

  if (ncol(yrep) == 0) {
    cli::cli_abort(
      "Could not find {.val {yrep_var}} draws in the fit for ppc."
    )
  }

  # Restrict ppc statistics to an allowlist. Custom statistics wait for the
  # trusted-hook mechanism; params are data, never code, so match.fun over an
  # unbounded search path would reopen a params-as-code channel.
  allowed_stats <- c("mean", "sd", "median", "min", "max", "mad")

  p_values <- list()
  for (stat_name in stats) {
    if (!stat_name %in% allowed_stats) {
      cli::cli_abort(
        "Unsupported ppc statistic {.val {stat_name}}. Allowed: {.val {allowed_stats}}."
      )
    }
    stat_fn <- match.fun(stat_name)
    y_stat <- stat_fn(y)
    # One statistic per draw, computed across observations (rows are draws in a
    # posterior draws matrix). Margin 2 would give the per-observation
    # distribution, not the posterior-predictive distribution of T(yrep).
    yrep_stats <- apply(yrep, 1, stat_fn)
    p_values[[stat_name]] <- mean(yrep_stats >= y_stat)
  }

  threshold_low <- node$params$p_value_low %||% 0.02
  threshold_high <- node$params$p_value_high %||% 0.98

  any_extreme <- any(vapply(
    p_values,
    function(p) p < threshold_low || p > threshold_high,
    logical(1)
  ))

  severity <- if (any_extreme) "warning" else "ok"

  summary <- list(
    summary_kind = "posterior_predictive_check",
    passed = !any_extreme,
    severity = severity,
    metrics = p_values
  )

  # Plot-ready data: observed y plus a capped subsample of yrep rows, so a
  # downstream plot (Milestone 7) can render ppc_dens_overlay without re-running
  # the fit. Cap to max_yrep_rows draws (default 100), evenly spaced across the
  # posterior to keep the subsample representative. Stored alongside the p-values
  # in the artifact; the summary carries only the scalar metrics.
  max_yrep_rows <- node$params$max_yrep_rows %||% 100L
  yrep_rows <- if (nrow(yrep) > max_yrep_rows) {
    # Evenly-spaced integer row indices across the posterior draws (round,
    # don't rely on fractional-index truncation).
    unique(round(seq.int(1L, nrow(yrep), length.out = max_yrep_rows)))
  } else {
    seq_len(nrow(yrep))
  }
  plot_data <- list(
    observed_y = y,
    yrep = unname(yrep[yrep_rows, , drop = FALSE]),
    stats = stats
  )

  result <- list(
    p_values = p_values,
    plot_data = plot_data
  )

  list(result = result, summaries = list(summary))
}

#' Compute LOO-PIT (probability integral transform) calibration values.
#'
#' For each observation `y_i`, PIT_i is the value of the leave-one-out
#' posterior-predictive CDF at `y_i`, computed from the PSIS-LOO weights applied
#' to the posterior-predictive draws (`yrep`). If the model is well calibrated,
#' the PIT values are uniformly distributed on \[0, 1\]. The executor emits a
#' `loo_pit_calibration` summary whose severity reflects posterior's
#' dependence-aware PIET uniformity test (with the KS distance retained as an
#' effect-size metric), and stores the
#' plot-ready PIT vector + observed `y` in the artifact (for a PIT ECDF or
#' histogram plot).
#'
#' Inputs: a fit node (carrying `log_lik` and `yrep` draws) and a data node
#' supplying the observed `y_var` (default `"y"`).
#' @keywords internal
#' @noRd
bg_executor_loo_pit <- function(node, inputs) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    cli::cli_abort("The {.pkg loo} package is required for loo_pit nodes.")
  }
  if (length(inputs) < 2) {
    cli::cli_abort(
      paste0(
        "{.field loo_pit} requires two inputs: a fit node and a data node ",
        "supplying the observed {.field {node$params$y_var %||% 'y'}}."
      )
    )
  }

  fit_input <- node$params$fit_input %||% NULL
  data_input <- node$params$data_input %||% NULL
  fit_artifact <- if (!is.null(fit_input) && fit_input %in% names(inputs)) {
    inputs[[fit_input]]
  } else {
    inputs[[1]]
  }
  observed_data <- if (!is.null(data_input) && data_input %in% names(inputs)) {
    inputs[[data_input]]
  } else {
    inputs[[2]]
  }

  draws_matrix <- bg_extract_draws(fit_artifact, as = "matrix")
  draws_array <- bg_extract_draws(fit_artifact, as = "array")
  log_lik_var <- node$params$log_lik_var %||% "log_lik"
  ll_cols <- bg_indexed_var_cols(log_lik_var, colnames(draws_matrix))
  ll_draws <- draws_matrix[, ll_cols, drop = FALSE]
  ll_array <- draws_array[,, ll_cols, drop = FALSE]
  if (ncol(ll_draws) == 0) {
    cli::cli_abort(
      "No draws matching {.val {log_lik_var}} found in the fit for {.field loo_pit}."
    )
  }

  yrep_var <- node$params$yrep_var %||% "yrep"
  yrep_cols <- bg_indexed_var_cols(yrep_var, colnames(draws_matrix))
  yrep <- draws_matrix[, yrep_cols, drop = FALSE]
  if (ncol(yrep) == 0) {
    cli::cli_abort(
      "Could not find {.val {yrep_var}} draws in the fit for loo_pit."
    )
  }

  y_var <- node$params$y_var %||% "y"
  y <- observed_data[[y_var]] %||% NULL
  if (is.null(y)) {
    if (is.numeric(observed_data)) {
      y <- observed_data
    } else {
      cli::cli_abort(
        "Observed variable {.val {y_var}} not found in the loo_pit data input."
      )
    }
  }

  if (ncol(ll_draws) != length(y) || ncol(yrep) != length(y)) {
    cli::cli_abort(
      "log_lik/yrep column count ({ncol(ll_draws)}) must match the number of observed values ({length(y)})."
    )
  }

  # PSIS-LOO weights per observation (draws x observations). loo::psis takes the
  # importance ratios for LOO (=-log_lik) and returns smoothed log weights.
  # r_eff follows loo::loo()'s convention (relative ESS of exp(log_lik) from
  # chain-shaped draws); without it psis() warns on every run.
  r_eff <- tryCatch(
    loo::relative_eff(exp(ll_array)),
    error = function(e) NULL
  )
  psis_obj <- loo::psis(-ll_draws, r_eff = r_eff)
  log_w <- psis_obj$log_weights
  # Normalize columns: exp(log_w - logsumexp) per observation.
  weights <- apply(log_w, 2L, function(col) {
    m <- max(col)
    exp(col - m) / sum(exp(col - m))
  })
  # If psis() returns a single column, apply() drops dims; restore.
  if (!is.matrix(weights)) {
    weights <- matrix(weights, ncol = 1L)
  }

  # posterior::pit() is the maintained weighted-PIT implementation. It uses
  # Pr(yrep < y) for continuous outcomes and randomized PIT for discrete ones,
  # avoiding the upward bias from the former unconditional `<=` calculation.
  # A fixed default seed keeps the executor deterministic when callers do not
  # supply one; node params remain data, not executable code.
  n_obs <- length(y)
  discrete <- bg_is_discrete_pit_data(y, yrep)
  pit_seed <- as.integer(node$params$pit_seed %||% node$params$seed %||% 1L)
  pit <- bg_with_preserved_seed(
    pit_seed,
    posterior::pit(
      posterior::as_draws_matrix(yrep),
      y,
      weights = weights
    )
  )

  diagnostics <- bg_loo_pit_diagnostics(
    pit,
    p_warn = node$params$pit_p_warn %||% 0.05,
    p_error = node$params$pit_p_error %||% 0.025
  )
  severity <- diagnostics$severity

  summary <- list(
    summary_kind = "loo_pit_calibration",
    passed = if (diagnostics$graded) identical(severity, "ok") else NA,
    severity = severity,
    metrics = list(
      ks_statistic = diagnostics$ks_statistic,
      p_value = diagnostics$p_value,
      uniformity_test = diagnostics$test,
      graded = diagnostics$graded,
      n_obs = as.integer(n_obs),
      discrete = discrete,
      pit_method = if (discrete) "randomized" else "continuous",
      pit_seed = pit_seed
    )
  )

  result <- list(
    pit_values = pit,
    observed_y = y,
    plot_data = list(
      pit_values = pit,
      observed_y = y
    )
  )

  list(result = result, summaries = list(summary))
}

#' Kolmogorov–Smirnov distance of a sample from Uniform(0, 1).
#'
#' The KS statistic is the maximum absolute difference between the empirical CDF
#' of the sample and the Uniform(0,1) CDF. Used to grade LOO-PIT calibration: a
#' well-calibrated model has PIT values ~ Uniform(0,1), so KS near 0.
#' @param x Numeric vector of values in \[0, 1\].
#' @return The KS statistic (numeric scalar).
#' @keywords internal
#' @noRd
bg_ks_uniform_stat <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  n <- length(x)
  sorted <- sort(x)
  # Empirical CDF: at sorted value x_(i), F_n jumps from (i-1)/n to i/n. The
  # KS distance to F(x)=x is max over both edges: |i/n - x_(i)| and |x_(i) - (i-1)/n|.
  i <- seq_len(n)
  d_plus <- i / n - sorted
  d_minus <- sorted - (i - 1L) / n
  max(abs(d_plus), abs(d_minus))
}

#' Run a dependence-aware uniformity diagnostic for LOO-PIT values.
#'
#' @param pit PIT values in \[0, 1\].
#' @param p_warn Warn below this p-value (default 0.05).
#' @param p_error Error below this p-value (default 0.025).
#' @return Diagnostic list with severity, p-value, test, and KS effect size.
#' @keywords internal
#' @noRd
bg_loo_pit_diagnostics <- function(pit, p_warn = 0.05, p_error = 0.025) {
  pit <- pit[is.finite(pit)]
  ks <- bg_ks_uniform_stat(pit)
  if (length(pit) == 0L) {
    return(list(
      severity = "ok",
      p_value = NA_real_,
      test = "PIET",
      ks_statistic = ks,
      graded = FALSE
    ))
  }

  test_result <- tryCatch(
    posterior::uniformity_test(pit, test = "PIET"),
    error = function(e) NULL
  )
  p_value <- test_result$pvalue %||% NA_real_
  graded <- length(p_value) == 1L && is.finite(p_value)
  severity <- if (!graded) {
    "ok"
  } else if (p_value < p_error) {
    "error"
  } else if (p_value < p_warn) {
    "warning"
  } else {
    "ok"
  }

  list(
    severity = severity,
    p_value = as.numeric(p_value),
    test = "PIET",
    ks_statistic = ks,
    graded = graded
  )
}

#' Detect outcomes for which randomized PIT is required.
#' @keywords internal
#' @noRd
bg_is_discrete_pit_data <- function(y, yrep) {
  values <- c(y, yrep)
  length(values) > 0L &&
    all(is.finite(values)) &&
    all(values == round(values))
}

#' Evaluate an expression under a deterministic seed without leaking RNG state.
#' @keywords internal
#' @noRd
bg_with_preserved_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  set.seed(seed)
  eval.parent(substitute(code))
}

# Simulation-based calibration (SBC) ----------------------------------------
#
# Talts et al. (2018): if a model is correctly specified, repeated draws of
# theta_true from the prior, data generated from the prior-predictive, and the
# model fit to that data, produce posterior ranks of theta_true that are
# uniformly distributed. The executor runs the SBC loop, bin-aggregates ranks,
# and grades calibration with a chi-squared uniformity test.
#
# Simulation cases arrive as plain data from an upstream generator executor:
# `list(simulations = list(list(theta = <scalar>, data = <Stan data>), ...))`.
# Project-defined generator code therefore executes only through the ordinary
# executor registry and its explicit `bg_restore_executors(trust = TRUE)` gate;
# no persisted node param is ever parsed or evaluated by this built-in.
#
# Cost: this fits n_sims models. Run it under mirai daemons (Milestone 3) for
# scale — each sim is independent and the whole loop parallelizes cleanly.

#' Run simulation-based calibration.
#'
#' @param node Params: `stan_file`, `theta_name` (Stan parameter name to rank,
#'   default `"theta"`), optional `generator_input`, `n_sims` (defaults to all
#'   supplied cases), `chains`, `iter_warmup`, `iter_sampling`, `seed`,
#'   `rank_draws` (default 100), and optional `n_bins`.
#' @param inputs An upstream plain-data artifact containing `simulations`, a
#'   list of `list(theta=<scalar>, data=<Stan data list>)` cases.
#' @return `list(result=, summaries=)` with an `sbc_result` summary carrying the
#'   rank histogram counts, chi-squared statistic, and severity.
#' @keywords internal
#' @noRd
bg_executor_sbc <- function(node, inputs) {
  cases <- bg_sbc_simulation_cases(node, inputs)

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg cmdstanr} package is required for sbc nodes. Run it under mirai daemons for scale (Milestone 3)."
    )
  }

  stan_file <- node$params$stan_file %||% NULL
  if (is.null(stan_file) || !file.exists(stan_file)) {
    cli::cli_abort(
      "{.field stan_file} must point to an existing Stan program for sbc."
    )
  }

  theta_name <- node$params$theta_name %||% "theta"
  n_sims <- as.integer(node$params$n_sims %||% length(cases))
  if (n_sims < 1L || n_sims > length(cases)) {
    cli::cli_abort(
      "{.field n_sims} must be between 1 and the {length(cases)} supplied simulation case{?s}."
    )
  }
  cases <- cases[seq_len(n_sims)]
  base_seed <- node$params$seed %||% NULL

  model <- cmdstanr::cmdstan_model(stan_file = stan_file)

  ranks <- rep(NA_integer_, n_sims)
  theta_draws_by_sim <- vector("list", n_sims)
  theta_true_by_sim <- rep(NA_real_, n_sims)
  effective_draws <- rep(NA_integer_, n_sims)
  for (i in seq_len(n_sims)) {
    sim_seed <- if (!is.null(base_seed)) as.integer(base_seed) + i else NULL
    theta_true <- cases[[i]]$theta
    sim_data <- cases[[i]]$data

    sampler_args <- list(
      data = sim_data,
      chains = node$params$chains %||% 1L,
      iter_warmup = node$params$iter_warmup %||% 200L,
      iter_sampling = node$params$iter_sampling %||% 200L,
      seed = sim_seed,
      refresh = 0L
    )
    sampler_args <- sampler_args[!vapply(sampler_args, is.null, logical(1))]
    fit <- tryCatch(
      do.call(model$sample, sampler_args),
      error = function(e) {
        cli::cli_warn("SBC sim {i} failed to fit: {.err {conditionMessage(e)}}")
        NULL
      }
    )
    if (is.null(fit)) {
      ranks[[i]] <- NA_integer_
      next
    }

    # Posterior draws of theta_name; rank theta_true among them.
    theta_draws <- tryCatch(
      posterior::extract_variable(fit$draws(), theta_name),
      error = function(e) {
        posterior::extract_variable(
          fit$draws(),
          paste0(theta_name, "[1]")
        )
      }
    )
    theta_draws <- theta_draws[is.finite(theta_draws)]
    if (length(theta_draws) == 0L) {
      next
    }
    theta_draws_by_sim[[i]] <- theta_draws
    theta_true_by_sim[[i]] <- theta_true
    ess <- tryCatch(posterior::ess_bulk(theta_draws), error = function(e) NA)
    effective_draws[[i]] <- if (is.finite(ess) && ess >= 1) {
      as.integer(floor(ess))
    } else {
      length(theta_draws)
    }
  }

  valid_simulations <- which(vapply(
    theta_draws_by_sim,
    function(draws) length(draws) > 0L,
    logical(1)
  ))
  if (length(valid_simulations) == 0L) {
    summary <- list(
      summary_kind = "sbc_result",
      passed = FALSE,
      severity = "error",
      metrics = list(
        n_sims = n_sims,
        n_valid = 0L,
        chi_sq = NA_real_,
        n_bins = 0L,
        graded = FALSE
      )
    )
    result <- list(
      ranks = ranks,
      rank_histogram = integer(),
      n_sims = n_sims
    )
    return(list(result = result, summaries = list(summary)))
  }

  # Raw MCMC draws are autocorrelated. Use a common number no larger than the
  # minimum bulk ESS, then deterministically spread those draws across each
  # chain history. Every rank consequently has the same support 0..rank_draws.
  rank_draws_target <- as.integer(node$params$rank_draws %||% 100L)
  if (rank_draws_target < 1L) {
    cli::cli_abort("{.field rank_draws} must be a positive integer.")
  }
  rank_draws <- min(c(
    rank_draws_target,
    effective_draws[valid_simulations],
    vapply(theta_draws_by_sim[valid_simulations], length, integer(1))
  ))
  rank_draws <- as.integer(max(1L, floor(rank_draws)))
  for (i in valid_simulations) {
    draws <- theta_draws_by_sim[[i]]
    keep <- unique(round(seq.int(1L, length(draws), length.out = rank_draws)))
    ranks[[i]] <- as.integer(sum(draws[keep] <= theta_true_by_sim[[i]]))
  }

  valid_ranks <- ranks[!is.na(ranks)]
  n_valid <- length(valid_ranks)
  histogram <- bg_sbc_rank_histogram(
    valid_ranks,
    n_draws = rank_draws,
    n_bins = node$params$n_bins %||% NULL
  )
  chi_sq <- if (histogram$graded) {
    sum((histogram$counts - histogram$expected)^2 / histogram$expected)
  } else {
    NA_real_
  }
  p_value <- if (histogram$graded) {
    stats::pchisq(chi_sq, df = histogram$n_bins - 1L, lower.tail = FALSE)
  } else {
    NA_real_
  }

  # Severity: warn/error by p-value thresholds (low p => poor calibration).
  p_warn <- node$params$sbc_p_warn %||% 0.05
  p_error <- node$params$sbc_p_error %||% 0.01
  severity <- if (!histogram$graded) {
    "ok"
  } else if (!is.na(p_value) && p_value < p_error) {
    "error"
  } else if (!is.na(p_value) && p_value < p_warn) {
    "warning"
  } else {
    "ok"
  }

  summary <- list(
    summary_kind = "sbc_result",
    passed = if (histogram$graded) identical(severity, "ok") else NA,
    severity = severity,
    metrics = list(
      n_sims = n_sims,
      n_valid = as.integer(n_valid),
      n_bins = histogram$n_bins,
      expected_per_bin = as.numeric(histogram$expected),
      graded = histogram$graded,
      smoke_test = n_valid < 100L,
      rank_draws = rank_draws,
      chi_sq = as.numeric(chi_sq),
      p_value = as.numeric(p_value)
    )
  )

  result <- list(
    ranks = ranks,
    rank_histogram = histogram$counts,
    theta_name = theta_name,
    n_sims = n_sims,
    plot_data = list(
      rank_histogram = histogram$counts,
      breaks = histogram$breaks
    )
  )

  list(result = result, summaries = list(summary))
}

#' Resolve and validate plain-data SBC simulation cases.
#' @keywords internal
#' @noRd
bg_sbc_simulation_cases <- function(node, inputs) {
  if (!is.null(node$params$data_fn)) {
    cli::cli_abort(c(
      "SBC node params must not contain executable source in {.field data_fn}.",
      "i" = paste0(
        "Register a project-specific generator node, connect it upstream, ",
        "and have it return {.code list(simulations = list(...))}. Restored ",
        "generator executors require {.code bg_restore_executors(trust = TRUE)}."
      )
    ))
  }

  input_name <- node$params$generator_input %||% NULL
  artifact <- if (!is.null(input_name) && input_name %in% names(inputs)) {
    inputs[[input_name]]
  } else if (length(inputs) > 0L) {
    inputs[[1]]
  } else {
    node$params$simulations %||% NULL
  }
  cases <- if (
    is.list(artifact) &&
      !is.null(names(artifact)) &&
      "simulations" %in% names(artifact)
  ) {
    artifact$simulations
  } else {
    artifact
  }
  if (!is.list(cases) || length(cases) == 0L) {
    cli::cli_abort(c(
      "An sbc node requires plain simulation cases from an upstream input.",
      "i" = "Return {.code list(simulations = list(list(theta = 0, data = list(...)), ...))} from a trusted generator executor."
    ))
  }
  if (!is.null(cases$theta) && !is.null(cases$data)) {
    cases <- list(cases)
  }

  for (i in seq_along(cases)) {
    case <- cases[[i]]
    valid <- is.list(case) &&
      is.numeric(case$theta) &&
      length(case$theta) == 1L &&
      is.finite(case$theta) &&
      is.list(case$data)
    if (!valid) {
      cli::cli_abort(
        "SBC simulation case {i} must contain a finite numeric scalar {.field theta} and a named-list {.field data}."
      )
    }
  }
  cases
}

#' Bin discrete SBC ranks with their exact null probabilities.
#' @keywords internal
#' @noRd
bg_sbc_rank_histogram <- function(ranks, n_draws, n_bins = NULL) {
  ranks <- as.integer(ranks[is.finite(ranks)])
  n_draws <- as.integer(n_draws)
  if (is.null(n_bins)) {
    n_bins <- min(n_draws + 1L, max(1L, floor(length(ranks) / 5L)))
    repeat {
      support_bins <- floor((0:n_draws) * n_bins / (n_draws + 1L)) + 1L
      expected <- length(ranks) *
        tabulate(support_bins, nbins = n_bins) /
        (n_draws + 1L)
      if (n_bins <= 1L || all(expected >= 5)) {
        break
      }
      n_bins <- n_bins - 1L
    }
  } else {
    n_bins <- as.integer(n_bins)
  }
  if (n_bins < 1L || n_bins > n_draws + 1L) {
    cli::cli_abort(
      "{.field n_bins} must be between 1 and the {n_draws + 1L} possible ranks."
    )
  }

  support <- 0:n_draws
  support_bins <- floor(support * n_bins / (n_draws + 1L)) + 1L
  observed_bins <- floor(ranks * n_bins / (n_draws + 1L)) + 1L
  counts <- tabulate(observed_bins, nbins = n_bins)
  expected <- length(ranks) *
    tabulate(support_bins, nbins = n_bins) /
    (n_draws + 1L)
  breaks <- vapply(
    seq_len(n_bins),
    function(bin) {
      min(support[support_bins == bin])
    },
    integer(1)
  )

  list(
    counts = as.integer(counts),
    expected = as.numeric(expected),
    n_bins = as.integer(n_bins),
    breaks = breaks,
    graded = n_bins >= 2L && length(ranks) > 0L && all(expected >= 5)
  )
}
