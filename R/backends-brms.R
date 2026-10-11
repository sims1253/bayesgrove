# brms built-in executors ---------------------------------------------------
#
# Registers brms-based node kinds. loo/compare/ppc are shared with the
# cmdstanr executors (they operate on draws via bg_extract_draws).

#' Register brms node kinds on a project.
#'
#' Registers `brms_fit` and `brms_prior_fit` built-in executors. The
#' `loo`, `compare`, `ppc`, `loo_pit`, and `sbc` kinds are shared with
#' [bg_use_cmdstanr()] and are registered here too so a brms-only project can
#' use them without calling both setup functions. A neutral `data` kind (an
#' alias of the `stan_data` executor) is registered as well, so brms-backed
#' graphs need not label plain data frames with a Stan-specific kind.
#'
#' The brms fit executors pass `backend` and `algorithm` to `brms::brm()`
#' explicitly, from node params or else the `brms.backend` and
#' `brms.algorithm` options resolved at execution time. Their job records'
#' provenance (see [bg_node_pad()]) carries the family, the applied priors,
#' and the sampler settings read back from the fit.
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
    data = list(
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

  pad_declarations <- bg_builtin_pad_declarations()
  for (id in names(kind_specs)) {
    spec <- kind_specs[[id]]
    bg_register_builtin_executor(id, spec$executor)
    bg_register_node_kind_builtin(
      project = project,
      kind = id,
      executor_ref = paste0("builtin:", id),
      input_contract = spec$input_contract,
      output_type = spec$output_type,
      pad = pad_declarations[[id]] %||% NULL
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

  selection <- bg_resolve_fit_data(node, inputs)
  if (is.null(selection$data)) {
    cli::cli_abort("brms_fit requires a data input node.")
  }

  priors <- bg_brms_build_priors(node$params$priors)
  engine <- bg_brms_engine(node)

  # Assemble the brm() argument list and drop NULL entries before do.call, so a
  # missing seed does not pass seed = NULL (brms' default is NA, not NULL, and
  # NULL errors inside brm()). Mirrors the cmdstanr executor's arg pattern.
  # backend and algorithm are passed explicitly, so the recorded values are
  # the ones used.
  brm_args <- list(
    formula = formula,
    data = selection$data,
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
    backend = engine$backend,
    algorithm = engine$algorithm,
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

  list(
    result = fit,
    summaries = list(summary),
    metadata = bg_fit_executor_report(
      selection,
      bg_try_readback(bg_brms_effective(fit))
    )
  )
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

  selection <- bg_resolve_fit_data(node, inputs, default = data.frame())
  engine <- bg_brms_engine(node)

  brm_args <- list(
    formula = formula,
    data = selection$data,
    sample_prior = "only",
    chains = node$params$chains %||% 4,
    iter = (node$params$iter_warmup %||% 1000) +
      (node$params$iter_sampling %||% 1000),
    warmup = node$params$iter_warmup %||% 1000,
    seed = node$params$seed %||% NULL,
    backend = engine$backend,
    algorithm = engine$algorithm,
    silent = 2,
    refresh = 0
  )
  brm_args <- brm_args[!vapply(brm_args, is.null, logical(1))]
  fit <- do.call(brms::brm, brm_args)

  # Match the cmdstanr prior-fit executor: carry the draws summary as evidence.
  # Severity stays "ok" without a check function; the summary still includes
  # the draws as evidence.
  draws_summary <- tryCatch(
    posterior::summarise_draws(posterior::as_draws(fit)),
    error = function(e) NULL
  )

  summary <- list(
    summary_kind = "prior_predictive_check",
    passed = NA,
    severity = "ok",
    metrics = list(draws_summary = draws_summary, graded = FALSE)
  )

  list(
    result = fit,
    summaries = list(summary),
    metadata = bg_fit_executor_report(
      selection,
      bg_try_readback(bg_brms_effective(fit))
    )
  )
}

#' Resolve the brms backend and algorithm for one execution.
#'
#' Node params win; otherwise the session options brms itself would consult
#' are resolved now, so the executor can pass both explicitly and the recorded
#' values are the ones used.
#' @keywords internal
#' @noRd
bg_brms_engine <- function(node) {
  list(
    backend = node$params$backend %||% getOption("brms.backend", "rstan"),
    algorithm = node$params$algorithm %||%
      getOption("brms.algorithm", "sampling")
  )
}

# --- Effective-settings read-back ------------------------------------------

#' Plain-data view of a brms family object.
#' @keywords internal
#' @noRd
bg_brms_family_readback <- function(family) {
  if (!is.list(family) || is.null(family$family)) {
    return("unavailable")
  }
  list(
    family = bg_readback_value(family$family),
    link = bg_readback_value(family$link)
  )
}

#' Plain-data rows of `brms::prior_summary()` output.
#'
#' One named list per applied prior, with the columns brms reports
#' (`prior`, `class`, `coef`, `group`, `resp`, `dpar`, `nlpar`, `lb`, `ub`,
#' `source`) as strings.
#' @keywords internal
#' @noRd
bg_brms_prior_readback <- function(priors) {
  if (!is.data.frame(priors)) {
    return("unavailable")
  }
  columns <- intersect(
    c(
      "prior",
      "class",
      "coef",
      "group",
      "resp",
      "dpar",
      "nlpar",
      "lb",
      "ub",
      "source"
    ),
    names(priors)
  )
  lapply(seq_len(nrow(priors)), function(i) {
    row <- lapply(columns, function(column) {
      value <- as.character(priors[[column]][[i]])
      if (length(value) == 0L || is.na(value)) "" else value
    })
    names(row) <- columns
    row
  })
}

#' Sampler settings from an rstan `stanfit` (`@sim` and `@stan_args`).
#'
#' `sim` and `stan_args` are the slot contents, so this helper stays pure.
#' A control setting that was not passed is recorded as unavailable.
#' @keywords internal
#' @noRd
bg_rstan_sampler_settings <- function(sim, stan_args) {
  if (!is.list(sim)) {
    sim <- list()
  }
  args <- if (is.list(stan_args) && length(stan_args) > 0L) {
    stan_args[[1]]
  } else {
    list()
  }
  if (!is.list(args)) {
    args <- list()
  }
  control <- if (is.list(args$control)) args$control else list()

  iter <- sim$iter %||% args$iter %||% NULL
  warmup <- sim$warmup %||% args$warmup %||% NULL
  iter_sampling <- if (
    is.numeric(iter) &&
      is.numeric(warmup) &&
      length(iter) == 1L &&
      length(warmup) == 1L
  ) {
    iter - warmup
  } else {
    NULL
  }

  list(
    sampler = bg_readback_value(args$algorithm),
    chains = bg_readback_value(sim$chains),
    iter_warmup = bg_readback_value(warmup),
    iter_sampling = bg_readback_value(iter_sampling),
    adapt_delta = bg_readback_value(control$adapt_delta),
    max_treedepth = bg_readback_value(control$max_treedepth),
    seed = bg_readback_value(args$seed)
  )
}

#' Sampler settings of a brms fit, by backend.
#'
#' With the cmdstanr backend they come from
#' `attr(fit$fit, "metadata")$metadata`; with rstan from `fit$fit@sim` and
#' `fit$fit@stan_args`. For algorithms other than `"sampling"` the HMC
#' adaptation settings do not apply.
#' @param backend,algorithm `fit$backend` and `fit$algorithm`.
#' @param stanfit `fit$fit`.
#' @keywords internal
#' @noRd
bg_brms_sampler_settings <- function(backend, algorithm, stanfit) {
  sim <- bg_try_readback(stanfit@sim)
  settings <- if (identical(backend, "cmdstanr")) {
    meta <- bg_try_readback(attr(stanfit, "metadata")$metadata)
    bg_cmdstan_sampler_settings(meta, chains = sim$chains %||% NULL)
  } else if (identical(backend, "rstan")) {
    bg_rstan_sampler_settings(
      sim,
      bg_try_readback(stanfit@stan_args)
    )
  } else {
    return("unavailable")
  }

  if (!identical(algorithm, "sampling")) {
    settings$adapt_delta <- "inapplicable"
    settings$max_treedepth <- "inapplicable"
  }
  settings
}

#' Effective settings of a brms fit.
#'
#' `effective$P` holds the family (`stats::family(fit)`) and the applied
#' priors (`brms::prior_summary(fit)`); `effective$A` holds `fit$backend`,
#' `fit$algorithm`, `fit$version`, and the sampler settings.
#' @keywords internal
#' @noRd
bg_brms_effective <- function(fit) {
  backend <- bg_readback_value(bg_try_readback(fit$backend))
  algorithm <- bg_readback_value(bg_try_readback(fit$algorithm))
  version <- bg_try_readback(lapply(fit$version, as.character))

  list(
    P = list(
      family = bg_brms_family_readback(bg_try_readback(stats::family(fit))),
      priors = bg_brms_prior_readback(
        bg_try_readback(as.data.frame(brms::prior_summary(fit)))
      )
    ),
    A = list(
      backend = backend,
      algorithm = algorithm,
      version = if (length(version) > 0L) version else "unavailable",
      sampler = bg_try_readback(
        bg_brms_sampler_settings(backend, algorithm, fit$fit)
      ) %||%
        "unavailable"
    )
  )
}

#' Extract HMC diagnostic metrics from a brmsfit.
#'
#' Uses posterior::summarise_draws and brms::nuts_params to compute the same
#' metric shape as the cmdstanr helper.
#' @keywords internal
#' @noRd
bg_brms_hmc_metrics <- function(fit, max_treedepth = 10) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg posterior} package is required for HMC diagnostics."
    )
  }

  draws_df <- posterior::summarise_draws(posterior::as_draws(fit))
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)

  nuts_metrics <- bg_brms_nuts_metrics(np, max_treedepth = max_treedepth)

  rhats <- draws_df$rhat[is.finite(draws_df$rhat)]
  max_rhat <- if (length(rhats) > 0) max(rhats) else 1.0

  bulk_ess <- draws_df$ess_bulk[is.finite(draws_df$ess_bulk)]
  min_bulk_ess <- if (length(bulk_ess) > 0) min(bulk_ess) else Inf

  tail_ess <- draws_df$ess_tail[is.finite(draws_df$ess_tail)]
  min_tail_ess <- if (length(tail_ess) > 0) min(tail_ess) else Inf

  metrics <- list(
    divergences = nuts_metrics$divergences,
    max_treedepth_hits = nuts_metrics$max_treedepth_hits,
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk_ess,
    min_tail_ess = min_tail_ess,
    e_bfmi = nuts_metrics$e_bfmi,
    num_transitions = tryCatch(
      nrow(posterior::as_draws_matrix(posterior::as_draws(fit))),
      error = function(e) NA_integer_
    ),
    n_chains = tryCatch(
      posterior::nchains(posterior::as_draws(fit)),
      error = function(e) NA_integer_
    )
  )
  rate <- bg_hmc_divergence_rate(metrics)
  metrics$divergence_rate <- rate$value
  metrics$divergence_rate_available <- rate$available
  metrics
}

#' Derive HMC counts/divergences/E-BFMI from a brms::nuts_params() data frame.
#'
#' Accepts a data frame without requiring a brmsfit object. Expects columns
#' `Chain`, `Iteration`, `Parameter`, `Value` (the `brms::nuts_params()`
#' shape). Returns zero/`Inf` metrics when `np` is NULL.
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
