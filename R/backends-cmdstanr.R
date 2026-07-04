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

  num_transitions <- metrics$num_transitions %||% NA_integer_
  divergences <- metrics$divergences %||% 0L
  divergence_rate <- if (!is.na(num_transitions) && num_transitions > 0) {
    divergences / num_transitions
  } else {
    0
  }

  max_rhat <- metrics$max_rhat %||% 1.0
  min_bulk_ess <- metrics$min_bulk_ess %||% Inf
  min_tail_ess <- metrics$min_tail_ess %||% Inf
  e_bfmi <- metrics$e_bfmi %||% Inf

  if (
    max_rhat > t$rhat_error ||
      (divergences > 0 && divergence_rate > t$divergence_rate_error)
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

  list(
    divergences = as.integer(divergences),
    max_treedepth_hits = as.integer(max_treedepth_hits),
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk_ess,
    min_tail_ess = min_tail_ess,
    e_bfmi = min_e_bfmi,
    num_transitions = as.integer(num_transitions),
    n_chains = as.integer(fit$num_chains() %||% NA_integer_)
  )
}

#' Register cmdstanr node kinds on a project.
#'
#' Registers the built-in cmdstanr executor node kinds: `stan_data`,
#' `cmdstanr_fit`, `prior_fit`, `loo`, `compare`, and `ppc`. Each executor is
#' a built-in resolved by reference (no source text persisted). The fit
#' executors compute HMC diagnostics themselves and emit `hmc_diagnostics`
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
      "i" = "Install it with {.code install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))} and run {.code cmdstanr::install_cmdstan()}."
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
  severity <- "ok"

  summary <- list(
    summary_kind = "prior_predictive_check",
    passed = identical(severity, "ok"),
    severity = severity,
    metrics = list(draws_summary = draws_summary)
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
      "{.field ppc} requires two inputs: a fit node and a data node supplying the observed {.field {node$params$y_var %||% 'y'}}."
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
  stats <- node$params$stats %||% c("mean", "sd")

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

  list(result = p_values, summaries = list(summary))
}
