bg_workflow_source_registry <- function() {
  list(
    workflow_core = c(
      "Gelman et al. (2020) — Bayesian Workflow"
    ),
    prior_predictive = c(
      "Gabry et al. (2019) — Visualization in Bayesian Workflow"
    ),
    posterior_predictive = c(
      paste(
        "Gelman, Meng, and Stern (1996) — Posterior Predictive Assessment",
        "of Model Fitness via Realized Discrepancies"
      )
    ),
    sbc = c(
      paste(
        "Talts et al. (2018) — Validating Bayesian Inference Algorithms",
        "with Simulation-Based Calibration"
      )
    ),
    model_comparison = c(
      paste(
        "Vehtari, Gelman, and Gabry (2017) — Practical Bayesian model",
        "evaluation using leave-one-out cross-validation and WAIC"
      )
    ),
    stacking = c(
      "Yao et al. (2018) — Using Stacking to Average Bayesian Predictive Distributions"
    ),
    loo_pit = c(
      "Gabry et al. (2019) — Visualization in Bayesian Workflow"
    ),
    stan_diagnostics = c(
      "Betancourt and Girolami (2015) — Hamiltonian Monte Carlo for Hierarchical Models",
      "Betancourt (2017) — A Conceptual Introduction to Hamiltonian Monte Carlo"
    ),
    taxonomy = c(
      paste(
        "Bürkner, Scholz, and Radev (2023) — Some models are useful, but how",
        "do we know which ones? Towards a unified Bayesian model taxonomy"
      )
    ),
    causal_scaffold = c(
      "McElreath (2020) — Statistical Rethinking (2nd ed.)"
    ),
    causal_identification = c(
      "Pearl (2009) — Causality",
      paste(
        "Textor et al. (2016) — Robust causal inference using directed acyclic",
        "graphs: the R package dagitty"
      )
    ),
    projection_predictive = c(
      paste(
        "Piironen, Paasiniemi, and Vehtari (2020) — Projective Inference in",
        "High-Dimensional Problems: Prediction and Feature Selection"
      )
    )
  )
}

bg_workflow_references <- function(source_keys = NULL, references = NULL) {
  registry <- bg_workflow_source_registry()

  keyed_refs <- unlist(lapply(source_keys %||% character(), function(key) {
    registry[[key]] %||% key
  }), use.names = FALSE)

  unique(c(keyed_refs, references %||% character()))
}
