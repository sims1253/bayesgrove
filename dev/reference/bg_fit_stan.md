# Fit a Stan model in one call

Creates a data node (via
[bg_set_node_data](https://sims1253.github.io/bayesgrove/dev/reference/bg_set_node_data.md)),
a `cmdstanr_fit` node consuming it, dispatches
[bg_run](https://sims1253.github.io/bayesgrove/dev/reference/bg_run.md),
and returns the fit node id invisibly while printing the run handle.
Pure sugar over the existing verbs — no new semantics. Extra arguments
in `...` become the fit node's params (e.g. `chains`, `seed`,
`iter_warmup`, `iter_sampling`).

## Usage

``` r
bg_fit_stan(handle, stan_file, data, label = NULL, ...)
```

## Arguments

- handle:

  A `bg_handle`.

- stan_file:

  Path to the Stan model file.

- data:

  A named list (or environment) of Stan data.

- label:

  Optional label for the fit node.

- ...:

  Passed to the fit node as params (e.g. `chains = 4`).

## Value

The fit node id, invisibly. The run handle is printed.

## Details

Call
[`bg_use_cmdstanr()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_use_cmdstanr.md)
first so the `cmdstanr_fit` kind is registered.
