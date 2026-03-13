# Simulation Study Workflow

## Motivation

This vignette shows how the current bayesgrove MVP can support a small
simulation study with explicit workflow state, branching, cached
execution, and decision logging.

It focuses on structural branching and explicit gate decisions.
Summary-driven workflow guidance is also available in the current
package through
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
and `bg_plan(..., external_holds = ...)`, including the optional
phase-10 packs for prior workflow, model checks, model selection, causal
framing, and PAD annotations, but that layer is kept out of this
vignette so the simulation example stays focused.

It is intentionally modest relative to the full vision for the package.
The workflow is inspired by the simulation-oriented perspective of
<https://arxiv.org/abs/2408.06504> and
<https://arxiv.org/abs/2210.06927>, but it stays inside the package’s
current capabilities and uses base R model fitting so that the example
remains light.

The study asks a simple question: when treatment assignment is
confounded, does an adjustment-aware model outperform a naive model that
omits the confounder?

## Setup

``` r
library(bayesgrove)

project_root <- file.path(tempdir(), "bg-simulation-study")
unlink(project_root, recursive = TRUE, force = TRUE)

handle <- bg_init(project_root, project_name = "simulation-study")
```

## Register node kinds

Each node kind below is deliberately simple, but together they already
resemble a structured workflow:

- a scenario node defines the data-generating condition,
- simulation nodes generate train/test data,
- a descriptive node summarizes imbalance,
- two fit nodes branch into competing model choices,
- a comparison node ranks the branches after an explicit decision gate.

``` r
bg_register_node_kind(handle, "scenario", executor = function(node, inputs) {
  node$params
})

bg_register_node_kind(handle, "simulate_data", executor = function(node, inputs) {
  scenario <- inputs[[1]]
  set.seed(scenario$seed + node$params$seed_offset)

  n <- scenario$n
  z <- rnorm(n)
  treatment_prob <- plogis(scenario$assignment_intercept + scenario$confounding * z)
  treatment <- rbinom(n, size = 1, prob = treatment_prob)
  outcome <- 0.5 + scenario$beta_treat * treatment + scenario$beta_z * z + rnorm(n, sd = scenario$sigma)

  data <- data.frame(
    z = z,
    treatment = treatment,
    outcome = outcome
  )

  list(
    data = data,
    truth = list(
      beta_treat = scenario$beta_treat,
      beta_z = scenario$beta_z,
      confounding = scenario$confounding
    ),
    split = node$params$split
  )
})

bg_register_node_kind(handle, "descriptive_check", executor = function(node, inputs) {
  sim <- inputs[[1]]
  data <- sim$data

  treated <- data[data$treatment == 1, , drop = FALSE]
  untreated <- data[data$treatment == 0, , drop = FALSE]
  pooled_sd <- stats::sd(data$z)

  list(
    split = sim$split,
    treated_share = mean(data$treatment),
    mean_z_treated = mean(treated$z),
    mean_z_untreated = mean(untreated$z),
    z_smd = (mean(treated$z) - mean(untreated$z)) / pooled_sd,
    cor_treatment_z = stats::cor(data$treatment, data$z)
  )
})

bg_register_node_kind(handle, "fit_lm", executor = function(node, inputs) {
  train <- inputs[[1]]
  test <- inputs[[2]]
  formula <- stats::as.formula(node$params$formula)

  fit <- stats::lm(formula, data = train$data)
  predictions <- stats::predict(fit, newdata = test$data)
  rmse <- sqrt(mean((test$data$outcome - predictions)^2))

  estimate <- unname(stats::coef(fit)[["treatment"]])

  list(
    model_label = node$label,
    formula = node$params$formula,
    estimate = estimate,
    absolute_effect_error = abs(estimate - train$truth$beta_treat),
    predictive_rmse = rmse
  )
})

bg_register_node_kind(handle, "compare_models", executor = function(node, inputs) {
  naive <- inputs[[1]]
  adjusted <- inputs[[2]]
  descriptive <- inputs[[3]]

  ranking <- data.frame(
    model = c(naive$model_label, adjusted$model_label),
    absolute_effect_error = c(
      naive$absolute_effect_error,
      adjusted$absolute_effect_error
    ),
    predictive_rmse = c(naive$predictive_rmse, adjusted$predictive_rmse)
  )

  ranking <- ranking[order(ranking$absolute_effect_error, ranking$predictive_rmse), ]

  list(
    imbalance = descriptive,
    ranking = ranking,
    recommended = ranking$model[[1]]
  )
})
```

## Build two scenario branches

We construct two branches:

- a near-randomized assignment scenario,
- a confounded assignment scenario.

``` r
build_branch <- function(handle, label, confounding, seed) {
  scenario <- bg_add_node(
    handle,
    kind = "scenario",
    label = sprintf("%s scenario", label),
    params = list(
      n = 250,
      beta_treat = 1.0,
      beta_z = 0.8,
      sigma = 1.0,
      assignment_intercept = 0,
      confounding = confounding,
      seed = seed
    )
  )

  train <- bg_add_node(
    handle,
    kind = "simulate_data",
    label = sprintf("%s train", label),
    inputs = scenario,
    params = list(split = "train", seed_offset = 1)
  )

  test <- bg_add_node(
    handle,
    kind = "simulate_data",
    label = sprintf("%s test", label),
    inputs = scenario,
    params = list(split = "test", seed_offset = 1001)
  )

  descriptive <- bg_add_node(
    handle,
    kind = "descriptive_check",
    label = sprintf("%s descriptive check", label),
    inputs = train
  )

  naive_fit <- bg_add_node(
    handle,
    kind = "fit_lm",
    label = sprintf("%s naive model", label),
    inputs = c(train, test),
    params = list(formula = "outcome ~ treatment")
  )

  adjusted_fit <- bg_add_node(
    handle,
    kind = "fit_lm",
    label = sprintf("%s adjusted model", label),
    inputs = c(train, test),
    params = list(formula = "outcome ~ treatment + z")
  )

  compare <- bg_add_node(
    handle,
    kind = "compare_models",
    label = sprintf("%s comparison", label),
    inputs = c(naive_fit, adjusted_fit, descriptive)
  )

  gate <- bg_add_gate(
    handle,
    from = descriptive,
    to = compare,
    prompt = sprintf(
      "Does the descriptive imbalance in the %s scenario justify relying on the adjustment-aware model?",
      label
    ),
    options = c("yes", "no"),
    refs = list(
      list(
        citekey = "scholz2024causalproxy",
        url = "https://arxiv.org/abs/2210.06927"
      )
    )
  )

  list(
    scenario = scenario,
    train = train,
    test = test,
    descriptive = descriptive,
    naive_fit = naive_fit,
    adjusted_fit = adjusted_fit,
    compare = compare,
    gate = gate
  )
}

randomized <- build_branch(handle, "Randomized", confounding = 0.0, seed = 101)
confounded <- build_branch(handle, "Confounded", confounding = 1.1, seed = 202)
```

## First run: execute everything up to the decision gates

The comparison nodes are blocked, but the descriptive summaries and both
model branches can already run.

``` r
first_run <- bg_run(handle, mode = "sync")
#> Starting run "run_01566f27" with 2 nodes to execute.
#> Running node "node_8cfeb1ca"...
#> Running node "node_1e86b9ad"...
#> Running node "node_7ecd6878"...
#> Running node "node_cf82482e"...
#> Running node "node_12d04737"...
#> Running node "node_86892a1e"...
#> Running node "node_2fcde878"...
#> Running node "node_8d060132"...
#> Running node "node_da036e34"...
#> Running node "node_28679b29"...
#> Running node "node_9813439a"...
#> Running node "node_3607d7ef"...
first_run$summary
#> $total_executed
#> [1] 12

bg_status(handle)
#> $workflow_state
#> [1] "blocked"
#> 
#> $runnable_nodes
#> [1] 0
#> 
#> $blocked_nodes
#> [1] 2
#> 
#> $pending_gates
#> [1] 2
#> 
#> $active_jobs
#> [1] 0
#> 
#> $last_run_id
#> [1] "run_01566f27"
#> 
#> $health
#> [1] "warning"
#> 
#> $messages
#> [1] "2 pending gate(s) block downstream work."
```

Inspect the descriptive summaries that motivated the gate answers:

``` r
bg_result(handle, randomized$descriptive)
#> $split
#> [1] "train"
#> 
#> $treated_share
#> [1] 0.46
#> 
#> $mean_z_treated
#> [1] 0.1046877
#> 
#> $mean_z_untreated
#> [1] -0.03377925
#> 
#> $z_smd
#> [1] 0.1345947
#> 
#> $cor_treatment_z
#> [1] 0.06721623
bg_result(handle, confounded$descriptive)
#> $split
#> [1] "train"
#> 
#> $treated_share
#> [1] 0.5
#> 
#> $mean_z_treated
#> [1] 0.6228693
#> 
#> $mean_z_untreated
#> [1] -0.3738931
#> 
#> $z_smd
#> [1] 0.9888632
#> 
#> $cor_treatment_z
#> [1] 0.4954234
```

## Resolve the gates with explicit rationale

The randomized branch should not show systematic imbalance, whereas the
confounded branch usually does. We record that distinction as an
explicit decision rather than letting the workflow silently continue.

``` r
bg_answer_gate(
  handle,
  randomized$gate$id,
  choice = "no",
  rationale = paste(
    "The standardized mean difference for z is small, so the adjustment branch",
    "is not required to justify interpretation here."
  ),
  evidence = randomized$descriptive,
  refs = list(
    list(
      citekey = "scholz2024causalproxy",
      url = "https://arxiv.org/abs/2210.06927"
    )
  )
)
#> <bg_decision_record> dec_5dbff162
#> • Prompt: Does the descriptive imbalance in the Randomized scenario justify
#>   relying on the adjustment-aware model?
#> • Choice: no
#> • Rationale: The standardized mean difference for z is small, so the adjustment
#>   branch is not required to justify interpretation here.

bg_answer_gate(
  handle,
  confounded$gate$id,
  choice = "yes",
  rationale = paste(
    "Treatment assignment is visibly associated with z, so the adjusted branch",
    "is the only trustworthy explanatory candidate."
  ),
  evidence = confounded$descriptive,
  refs = list(
    list(
      citekey = "fazio2025primedpriors",
      url = "https://arxiv.org/abs/2408.06504"
    )
  )
)
#> <bg_decision_record> dec_151d02d3
#> • Prompt: Does the descriptive imbalance in the Confounded scenario justify
#>   relying on the adjustment-aware model?
#> • Choice: yes
#> • Rationale: Treatment assignment is visibly associated with z, so the adjusted
#>   branch is the only trustworthy explanatory candidate.
```

## Second run: execute the comparison nodes

``` r
second_run <- bg_run(handle, mode = "sync")
#> Starting run "run_62402479" with 2 nodes to execute.
#> Running node "node_dab33fa3"...
#> Running node "node_066eb3c3"...
second_run$summary
#> $total_executed
#> [1] 2
```

Now the comparison nodes can summarize the two branches:

``` r
bg_result(handle, randomized$compare)$ranking
#>                       model absolute_effect_error predictive_rmse
#> 2 Randomized adjusted model            0.02457046        0.964850
#> 1    Randomized naive model            0.13661777        1.294119
bg_result(handle, confounded$compare)$ranking
#>                       model absolute_effect_error predictive_rmse
#> 2 Confounded adjusted model            0.02950755        1.062609
#> 1    Confounded naive model            0.72121614        1.264224
```

In a typical run, the randomized scenario keeps the two models close,
while the confounded scenario penalizes the naive model more strongly.
That is the kind of structured depth-first search bayesgrove is well
positioned to support: each branch is explicit, caches are reused
automatically, and the rationale for the next step is stored alongside
the computational state.

## Export an audit report

``` r
report_path <- bg_export_report(handle, path = "simulation-study-report.md", format = "md")
#> Report exported to
#> /tmp/RtmpL8n4bW/bg-simulation-study/simulation-study-report.md
report_path
#> [1] "/tmp/RtmpL8n4bW/bg-simulation-study/simulation-study-report.md"
```

The generated report now includes graph topology, decision provenance,
gate context, evidence nodes, and the artifact index for the current
run.

## How the richer workflow packs fit on top

This vignette stays intentionally lightweight, but the package can now
layer a richer Bayesian review vocabulary on top of the same graph and
decision infrastructure. The optional built-in packs add:

- `bayesgrove.process_guidance` for workflow preflight, iterative
  repair, and out-of-sample stability review,
- `bayesgrove.model_taxonomy` for PAD classification and utility
  trade-offs,
- `bayesgrove.prior_workflow` for prior rationale and prior predictive
  review,
- `bayesgrove.model_checks` for posterior predictive checks and, on
  `latent_inference` branches, SBC and LOO-PIT calibration review,
- `bayesgrove.model_selection` for fresh comparison evidence, including
  `comparison_results` and stacking weights,
- `bayesgrove.stan_workflow` for Stan diagnostics and
  projection-predictive review, and
- `bayesgrove.causal_dagitty` for DAG-based adjustment review,
  implication review, and causal selection contracts that can constrain
  formulas or projection-based variable selection.

What still remains domain-specific is the layer above those semantics:
rich prior object systems, causal DAG classes, graph-aware suggestion
engines, and package-specific executors for particular model families.
bayesgrove now has the review vocabulary for those workflows, but you
still supply the node kinds and summaries that make the workflow
concrete.
