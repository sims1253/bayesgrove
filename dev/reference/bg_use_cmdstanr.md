# Register cmdstanr node kinds on a project.

Registers the built-in cmdstanr executor node kinds: `stan_data`,
`cmdstanr_fit`, `prior_fit`, `loo`, `compare`, `ppc`, `loo_pit`, and
`sbc`. Each executor is a built-in resolved by reference (no source text
persisted). The fit executors compute HMC diagnostics themselves and
emit `hmc_diagnostics` summaries with severity rules; no user-written
diagnostic code is required.

## Usage

``` r
bg_use_cmdstanr(project)
```

## Arguments

- project:

  A `bg_handle`.

## Value

Invisibly, the project handle.

## Details

The `ppc` kind's posterior-predictive p-values are conservative
tripwires: an extreme value flags gross misfit, but an unremarkable
value is weak evidence that the model is adequate. Treat the graphical
check via
[`bg_plot()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_plot.md)
as the primary posterior-predictive diagnostic and the p-values as its
alarm layer.
