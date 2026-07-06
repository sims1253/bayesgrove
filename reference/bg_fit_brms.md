# Fit a brms model in one call

Creates a data node (via
[bg_set_node_data](https://sims1253.github.io/bayesgrove/reference/bg_set_node_data.md)),
a `brms_fit` node consuming it, dispatches
[bg_run](https://sims1253.github.io/bayesgrove/reference/bg_run.md), and
returns the fit node id invisibly while printing the run handle. Pure
sugar over the existing verbs. Extra arguments in `...` become the fit
node's params (e.g. `chains`, `seed`, `iter`).

## Usage

``` r
bg_fit_brms(handle, formula, data, label = NULL, ...)
```

## Arguments

- handle:

  A `bg_handle`.

- formula:

  A `brmsformula` or formula object.

- data:

  A data frame of observations.

- label:

  Optional label for the fit node.

- ...:

  Passed to the fit node as params (e.g. `chains = 4`).

## Value

The fit node id, invisibly. The run handle is printed.

## Details

Call
[`bg_use_brms()`](https://sims1253.github.io/bayesgrove/reference/bg_use_brms.md)
first so the `brms_fit` kind is registered.
