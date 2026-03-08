# Extending bayesgrove

## Why this vignette exists

bayesgrove keeps the runtime boundary small on purpose. You bring your
own node kinds, executors, and domain conventions. bayesgrove handles
the graph, cache, decision log, and workflow protocol.

This vignette uses the following vocabulary:

| Term          | Meaning in bayesgrove today                                                                                                                |
|---------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| Node set      | A group of node kinds plus executors you register on a handle                                                                              |
| Ruleset       | A chosen set of `workflow_packs` plus the summary vocabulary they consume                                                                  |
| Domain module | An R helper or package that bundles node registration, pack selection, and project conventions                                             |
| Plugin        | A backend implementation registered with [`bg_register_backend()`](https://sims1253.github.io/bayesgrove/reference/bg_register_backend.md) |
| Extension     | Any combination of the four items above                                                                                                    |

The stable public hooks are node-kind registration, backend
registration, and summary emission from executors. Workflow-pack
selection is also public. Authoring brand new workflow-pack providers is
still an internal extension surface, so external packages should
currently target the built-in summary contracts instead of reaching into
unexported provider internals.

## Activate the built-in workflow packs you need

The default pack stays narrow. If your workflow needs richer Bayesian
review, activate the optional built-in packs at project creation time:

``` r
library(bayesgrove)

project_root <- file.path(tempdir(), "bg-extensions")
unlink(project_root, recursive = TRUE, force = TRUE)

handle <- bg_init(
  path = project_root,
  project_name = "Extensions",
  workflow_packs = list(
    "bayesguide.default_bayesian",
    "bayesgrove.prior_workflow",
    "bayesgrove.model_checks",
    "bayesgrove.model_selection",
    "bayesgrove.causal_minimal",
    "bayesgrove.pad_scaffold"
  )
)

vapply(bg_workflow_packs(handle), function(pack) pack$pack_id, character(1))
#> [1] "bayesguide.default_bayesian" "bayesgrove.prior_workflow"  
#> [3] "bayesgrove.model_checks"     "bayesgrove.model_selection" 
#> [5] "bayesgrove.causal_minimal"   "bayesgrove.pad_scaffold"
```

Those packs add review vocabulary without changing the graph engine:

- `bayesguide.default_bayesian`: computation review, fit criticism,
  candidate comparison, and branch disposition.
- `bayesgrove.prior_workflow`: prior rationale and prior predictive
  review.
- `bayesgrove.model_checks`: posterior predictive checks and, for
  `latent_inference` branches, SBC review.
- `bayesgrove.model_selection`: model-comparison evidence from fresh
  comparison summaries, with optional stacking weights.
- `bayesgrove.causal_minimal`: an advisory causal-question scaffold for
  `latent_inference` branches.
- `bayesgrove.pad_scaffold`: an advisory PAD annotation scaffold once a
  branch has an inferential goal.

The scope matters. `bayesgrove.model_selection` only acts at project
scope when there are at least two clean fit candidates. The causal and
PAD packs are branch-scoped prompts, not full domain systems, and they
do not fire until the branch has the right goal context.

## Build a node set

In practice, a node set is usually a helper that registers several node
kinds for one modeling domain. The only hard contract is that each
executor receives `(node, inputs)` and returns plain R data.

``` r
register_normal_model_nodes <- function(handle) {
  bg_register_node_kind(handle, "study_data", executor = function(node, inputs) {
    data.frame(y = rnorm(50), x = rnorm(50))
  })

  bg_register_node_kind(handle, "prior_note", executor = function(node, inputs) {
    list(
      result = node$params,
      summaries = list(list(
        summary_kind = "prior_spec",
        passed = TRUE,
        severity = "ok",
        metrics = list(scale = node$params$scale)
      ))
    )
  })

  bg_register_node_kind(handle, "fit_normal", executor = function(node, inputs) {
    data <- inputs[[1]]

    list(
      result = list(mean_y = mean(data$y)),
      summaries = list(
        list(
          summary_kind = "prior_predictive_check",
          passed = TRUE,
          severity = "ok",
          metrics = list(simulated_mean = 0.0)
        ),
        list(
          summary_kind = "posterior_predictive_check",
          passed = TRUE,
          severity = "ok",
          metrics = list(ppc_p_value = 0.46)
        ),
        list(
          summary_kind = "pad_annotation",
          passed = TRUE,
          severity = "ok",
          metrics = list(
            model_class = "PAD",
            primary_utility = "predictive_performance"
          )
        )
      )
    )
  })
}
```

Two practical rules make these node sets easy to compose:

1.  Keep executors domain-specific and return plain data.
2.  Emit summaries as plain-data records so workflow packs can react to
    them.

## Emit summaries that workflow packs understand

The optional packs are summary-driven. They do not require special node
kinds; they only care about summary records attached to fresh results.

| Summary kind                                                   | Consumed by                  | Typical use                                                                     |
|----------------------------------------------------------------|------------------------------|---------------------------------------------------------------------------------|
| `prior_spec`                                                   | `bayesgrove.prior_workflow`  | Record the current prior specification                                          |
| `prior_predictive_check`                                       | `bayesgrove.prior_workflow`  | Review whether priors generate plausible observables                            |
| `posterior_predictive_check`                                   | `bayesgrove.model_checks`    | Review fit-to-data mismatch after conditioning                                  |
| `sbc_result`                                                   | `bayesgrove.model_checks`    | Review parameter recoverability and calibration for `latent_inference` branches |
| `comparison_results` / `model_comparison` / `stacking_weights` | `bayesgrove.model_selection` | Compare clean candidates and review weighting evidence                          |
| `causal_framing`                                               | `bayesgrove.causal_minimal`  | Record a branch-scoped causal question or identifying story                     |
| `pad_annotation`                                               | `bayesgrove.pad_scaffold`    | Label a branch with PAD class and utility dimensions                            |

Each summary should stay plain and serializable. A good pattern is:

- `summary_kind` for dispatch,
- `passed` and `severity` for review urgency,
- `metrics` for compact numeric or categorical evidence,
- extra fields only when they remain stable plain data.

## Package a domain module

A domain module is just a reusable setup layer around
[`bg_init()`](https://sims1253.github.io/bayesgrove/reference/bg_init.md)
and your node registration helpers. This is the easiest way to publish a
project-specific extension package.

``` r
init_normal_workflow <- function(path) {
  handle <- bg_init(
    path = path,
    project_name = "normal_workflow",
    workflow_packs = list(
      "bayesguide.default_bayesian",
      "bayesgrove.prior_workflow",
      "bayesgrove.model_checks",
      "bayesgrove.model_selection",
      "bayesgrove.pad_scaffold"
    )
  )

  register_normal_model_nodes(handle)
  handle
}
```

That helper can live in your analysis package, a lab-internal utility
package, or even a plain project script. The important part is that the
node set and the selected workflow packs evolve together.

## Register a backend plugin

When you need a compile-and-fit backend rather than ad hoc executors,
register a backend plugin. The built-in helpers already cover common
cases:

``` r
bg_register_backend(handle, "cmdstanr", bg_cmdstanr_plugin())
bg_register_backend(handle, "brms", bg_brms_plugin())
bg_register_backend(handle, "diagnostics", bg_diagnostics_plugin())
```

A custom backend is just a named list with these methods:

- `backend_compile`
- `backend_fit`
- `backend_source_hash`
- `backend_runtime_signature`

Keep backend plugins focused on compilation and execution mechanics.
Workflow semantics should still be expressed through summaries and
workflow packs, not hardcoded into the backend layer.

## What a ruleset means right now

Today, a practical “ruleset” in bayesgrove means two things together:

1.  the `workflow_packs` you activate in
    [`bg_init()`](https://sims1253.github.io/bayesgrove/reference/bg_init.md),
    and
2.  the summary kinds your executors emit for those packs to interpret.

That path is public and stable enough to build around. By contrast,
registering entirely new workflow-pack providers from an external
package is not yet a public API. If you need custom review logic today,
prefer one of these approaches:

- emit the existing summary vocabulary and reuse the built-in packs,
- wrap a project-specific decision policy around
  [`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md),
  or
- pin to internal helpers only if you are comfortable tracking internal
  API changes.

## Recommended extension shape

For most projects, the easiest sustainable pattern is:

1.  create a domain module that initializes the right workflow packs,
2.  register a small node set for your modeling tasks,
3.  use built-in backend plugins where possible,
4.  emit plain-data summaries that match the review vocabulary you want,
    and
5.  keep project-specific heuristics outside the core runtime.

That keeps bayesgrove responsible for provenance, caching, branching,
and review state while your package owns the domain logic.
