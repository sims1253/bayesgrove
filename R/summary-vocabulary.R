# Summary vocabulary ----------------------------------------------------------
#
# The summary-kind vocabulary is the declared, validated interface between
# executors (which emit summary records) and workflow packs (which match on
# summary_kind). Unknown kinds cause a warning at write time, since packs
# cannot match a misspelled kind.

#' Built-in summary-kind vocabulary
#'
#' Returns a named list of summary-kind descriptors. Each descriptor has:
#' `kind`, `title`, `expected_metrics` (named list of type strings, informative
#' not enforced), `emitted_by` (free text), `consumed_by_packs` (character).
#'
#' ## Evidence coverage
#'
#' Every summary kind a built-in workflow pack matches on is emitted by at
#' least one shipped executor (or is documented as requiring a user executor). The
#' table below maps the kinds packs most commonly ask for to the built-in node
#' kind that produces them:
#'
#' | summary kind            | produced by built-in kind     | notes                          |
#' |:------------------------|:------------------------------|:-------------------------------|
#' | `hmc_diagnostics`       | `cmdstanr_fit` / `brms_fit`   | HMC sampler diagnostics        |
#' | `prior_predictive_check`| `prior_fit` / `brms_prior_fit`| prior-predictive draws         |
#' | `posterior_predictive_check` | `ppc`                    | observed y + capped yrep draws |
#' | `loo_diagnostics`       | `loo`                         | PSIS-LOO Pareto-k              |
#' | `loo_pit_calibration`   | `loo_pit`                     | randomized PIT + PIET severity|
#' | `sbc_result`            | `sbc`                         | ESS-bounded ranks + valid bins |
#' | `comparison_results` / `stacking_weights` | `compare`        | model comparison               |
#' | `prior_spec`            | user executor                 | record the prior               |
#' | `projpred_selection`    | user executor                 | use `projpred`                 |
#' | `causal_*` / `pad_*`    | user executors                | supply causal/PAD evidence     |
#'
#' A test (`test-summary-vocabulary-coverage.R`) asserts every literal
#' `summary_kinds = "<kind>"` referenced by a pack source is present here.
#' New summary kinds used by packs require a vocabulary entry that identifies
#' a built-in or user executor as the source.
#'
#' @return Named list of summary-kind descriptors.
#' @export
bg_summary_vocabulary <- function() {
  kinds <- list(
    list(
      kind = "hmc_diagnostics",
      title = "Hamiltonian Monte Carlo sampler diagnostics",
      expected_metrics = list(
        divergences = "integer",
        max_treedepth_hits = "integer",
        max_rhat = "numeric",
        min_bulk_ess = "numeric",
        min_tail_ess = "numeric",
        e_bfmi = "numeric"
      ),
      emitted_by = "cmdstanr_fit / brms_fit executors",
      consumed_by_packs = c(
        "bayesgrove.default_bayesian",
        "bayesgrove.model_checks"
      )
    ),
    list(
      kind = "optimizer_diagnostics",
      title = "Optimization-based sampler diagnostics",
      expected_metrics = list(converged = "logical"),
      emitted_by = "optimizer-based executors",
      consumed_by_packs = "bayesgrove.default_bayesian"
    ),
    list(
      kind = "prior_spec",
      title = "Recorded prior specification",
      expected_metrics = list(),
      emitted_by = "user executors",
      consumed_by_packs = "bayesgrove.prior_workflow"
    ),
    list(
      kind = "prior_predictive_check",
      title = "Prior predictive check",
      expected_metrics = list(),
      emitted_by = "prior_fit / brms_prior_fit executors",
      consumed_by_packs = "bayesgrove.prior_workflow"
    ),
    list(
      kind = "posterior_predictive_check",
      title = "Posterior predictive check",
      expected_metrics = list(
        p_values = "numeric"
      ),
      emitted_by = "ppc executor",
      consumed_by_packs = "bayesgrove.model_checks"
    ),
    list(
      kind = "sbc_result",
      title = "Simulation-based calibration result",
      expected_metrics = list(),
      emitted_by = "sbc executor",
      consumed_by_packs = "bayesgrove.model_checks"
    ),
    list(
      kind = "loo_diagnostics",
      title = "PSIS-LOO diagnostics (Pareto-k)",
      expected_metrics = list(
        pareto_k_high = "integer",
        pareto_k_bad = "integer"
      ),
      emitted_by = "loo executor",
      consumed_by_packs = "bayesgrove.model_selection"
    ),
    list(
      kind = "loo_pit_calibration",
      title = "LOO-PIT calibration",
      expected_metrics = list(),
      emitted_by = "loo_pit executor",
      consumed_by_packs = "bayesgrove.model_checks"
    ),
    list(
      kind = "pareto_k_diagnostics",
      title = "Pareto-k diagnostic summary",
      expected_metrics = list(),
      emitted_by = "loo executor",
      consumed_by_packs = "bayesgrove.model_selection"
    ),
    list(
      kind = "comparison_results",
      title = "Model comparison results (ELPD differences)",
      expected_metrics = list(),
      emitted_by = "compare executor",
      consumed_by_packs = "bayesgrove.model_selection"
    ),
    list(
      kind = "model_comparison",
      title = "Model comparison decision context",
      expected_metrics = list(),
      emitted_by = "compare executor / decision recording",
      consumed_by_packs = c(
        "bayesgrove.model_selection",
        "bayesgrove.default_bayesian"
      )
    ),
    list(
      kind = "projpred_selection",
      title = "Projection-predictive variable selection result",
      expected_metrics = list(),
      emitted_by = "projpred executor",
      consumed_by_packs = "bayesgrove.model_selection"
    ),
    list(
      kind = "stacking_weights",
      title = "Stacking model-averaging weights",
      expected_metrics = list(),
      emitted_by = "compare executor",
      consumed_by_packs = "bayesgrove.model_selection"
    ),
    list(
      kind = "causal_selection_contract",
      title = "Causal selection contract (adjustment set + formula terms)",
      expected_metrics = list(),
      emitted_by = "derive_causal_selection_contract executor",
      consumed_by_packs = c(
        "bayesgrove.causal_minimal",
        "bayesgrove.pad_scaffold"
      )
    ),
    list(
      kind = "causal_framing",
      title = "Causal question framing",
      expected_metrics = list(),
      emitted_by = "frame_causal_question executor",
      consumed_by_packs = "bayesgrove.causal_minimal"
    ),
    list(
      kind = "dagitty_adjustment",
      title = "dagitty-derived adjustment set",
      expected_metrics = list(),
      emitted_by = "derive_causal_adjustment executor",
      consumed_by_packs = "bayesgrove.causal_minimal"
    ),
    list(
      kind = "dagitty_implications",
      title = "dagitty-derived conditional independence implications",
      expected_metrics = list(),
      emitted_by = "check_causal_implications executor",
      consumed_by_packs = "bayesgrove.causal_minimal"
    ),
    list(
      kind = "pad_annotation",
      title = "PAD (Prior/Assumption/Decision) annotation",
      expected_metrics = list(),
      emitted_by = "pad annotation executors",
      consumed_by_packs = "bayesgrove.pad_scaffold"
    )
  )

  stats::setNames(kinds, vapply(kinds, `[[`, character(1), "kind"))
}

#' Known summary-kind identifiers
#'
#' Returns the character vector of built-in summary-kind names.
#' @param project Optional handle; project-registered kinds are appended.
#' @return Character vector of summary-kind names.
#' @keywords internal
#' @noRd
bg_known_summary_kinds <- function(project = NULL) {
  kinds <- names(bg_summary_vocabulary())

  if (!is.null(project)) {
    registered <- names(project@registries$summary_kinds %||% list())
    kinds <- union(kinds, registered)
  }

  kinds
}

#' Register a custom summary kind
#'
#' Adds a summary-kind descriptor to the project's runtime registry so that
#' executors emitting it are recognized by [bg_write_summaries()] validation.
#'
#' @param project A `bg_handle`.
#' @param kind Summary-kind name (a non-empty string).
#' @param title Optional human-readable title.
#' @param expected_metrics Optional named list of metric type strings.
#' @param emitted_by Optional free-text description of the emitter.
#' @param consumed_by_packs Optional character vector of consuming pack ids.
#' @return Invisibly, the registered descriptor.
#' @export
bg_register_summary_kind <- function(
  project,
  kind,
  title = NULL,
  expected_metrics = list(),
  emitted_by = NULL,
  consumed_by_packs = character()
) {
  S7::check_is_S7(project, bg_handle)

  if (!is.character(kind) || length(kind) != 1 || !nzchar(kind)) {
    cli::cli_abort("{.arg kind} must be a single non-empty string.")
  }

  descriptor <- list(
    kind = kind,
    title = title %||% kind,
    expected_metrics = expected_metrics %||% list(),
    emitted_by = emitted_by %||% "",
    consumed_by_packs = consumed_by_packs %||% character()
  )

  if (is.null(project@registries$summary_kinds)) {
    registries <- project@registries
    registries$summary_kinds <- list()
    project@registries <- registries
  }
  project@registries$summary_kinds[[kind]] <- descriptor

  # Persist to the runtime manifest so custom kinds survive reopen.
  config <- bg_read_project_config(project)
  manifest <- config$runtime_manifest %||% bg_empty_runtime_manifest()
  manifest$summary_kinds[[kind]] <- descriptor
  config$runtime_manifest <- manifest
  bg_write_project_config_path(project@path, config)

  invisible(descriptor)
}

#' Suggest the nearest known summary kind for an unknown one.
#'
#' Uses approximate string distance to find the closest known kind, for use in
#' typo warnings.
#' @param kind The unknown kind string.
#' @param known Character vector of known kinds.
#' @return The nearest known kind, or NULL if none.
#' @keywords internal
#' @noRd
bg_nearest_summary_kind <- function(kind, known) {
  if (length(known) == 0) {
    return(NULL)
  }

  distances <- utils::adist(kind, known)
  nearest_idx <- which.min(distances)
  known[[nearest_idx]]
}
