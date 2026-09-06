# bayesgrove

bayesgrove is an R package for recording Bayesian model reviews
alongside the analysis. It runs models with cmdstanr or brms, collects
diagnostics, and can block downstream computation until you record a
review with a rationale. Branches keep alternative models and their
decisions in the same project.

``` text
Fit a model
  └─ Diagnostics raise a blocking review
       ├─ Downstream computation waits
       └─ You review the evidence and record a decision
            └─ Resolve the review, then continue or branch to a revised model
```

Results are cached. Reports collect the graph, diagnostics, and
decisions. Review rules are opt-in: enable a workflow pack to choose
which checks apply.

## Install

``` r

# install.packages("pak")
pak::pkg_install("sims1253/bayesgrove")
```

Model fitting also requires [cmdstanr](https://mc-stan.org/cmdstanr/)
with CmdStan, or [brms](https://paulbuerkner.com/brms/).

## Start here

[Follow the getting-started
tutorial](https://sims1253.github.io/bayesgrove/articles/getting-started.html):
fit a model, inspect a failed predictive check, record a review, and try
a revised model. The example Stan programs ship with the package.

For an interactive session, `bg_repl(handle)` shows pending reviews and
suggested actions for an existing project.

| To learn about… | Read |
|----|----|
| Nodes, reviews, decisions, and execution holds | [Concepts](https://sims1253.github.io/bayesgrove/articles/concepts.html) |
| Divergences and reparameterization | [Eight schools](https://sims1253.github.io/bayesgrove/articles/eight-schools.html) |
| Comparing models and accepting or rejecting branches | [Roaches case study](https://sims1253.github.io/bayesgrove/articles/case-study-roaches.html) |
| Review packs and custom executors | [Extensions](https://sims1253.github.io/bayesgrove/articles/extensions.html) |

Browse [all
guides](https://sims1253.github.io/bayesgrove/articles/index.html) for
simulation studies, teaching, and sharing project bundles, or use the
[function
reference](https://sims1253.github.io/bayesgrove/reference/index.html).
Run
[`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_api_boundary.md)
to check which functions are stable or experimental.

The package draws on *Bayesian Workflow* (Gelman et al., 2020) and
simulation-based calibration (Talts et al., 2018). Use
`citation("bayesgrove")` for citations.
