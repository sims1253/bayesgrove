# Plot a node's artifact or diagnostics

Selects a plot based on the node's kind:

- `ppc`:
  [`bayesplot::ppc_dens_overlay()`](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
  on the stored plot-ready data.

- `loo_pit`: a base-graphics PIT ECDF plot against the Uniform CDF.

- `sbc`: a base-graphics rank histogram.

- `cmdstanr_fit` / `brms_fit`:
  [`bayesplot::mcmc_trace()`](https://mc-stan.org/bayesplot/reference/MCMC-traces.html)
  over the draws.

## Usage

``` r
bg_plot(project, node_id, ...)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The node to plot.

- ...:

  Passed to the underlying bayesplot function (ignored for the
  base-graphics kinds).

## Value

For `ppc` and fit kinds, the bayesplot ggplot object. For `loo_pit` and
`sbc`, `NULL` invisibly (the plot is drawn as a side effect).

## Details

The bayesplot-backed kinds need the `bayesplot` package (a soft
dependency); the function aborts with an informative message when it is
missing or the node kind has no plot.

For posterior-predictive checking, this graphical view is the primary
diagnostic: the p-values in a `ppc` node's summary are conservative
tripwires that flag gross misfit, and an unremarkable p-value is weak
evidence of adequacy. Read the overlay, not just the numbers.
