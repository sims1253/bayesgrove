# Register brms node kinds on a project.

Registers `brms_fit` and `brms_prior_fit` built-in executors. The `loo`,
`compare`, `ppc`, `loo_pit`, and `sbc` kinds are shared with
[`bg_use_cmdstanr()`](https://sims1253.github.io/bayesgrove/reference/bg_use_cmdstanr.md)
and are registered here too so a brms-only project can use them without
calling both setup functions.

## Usage

``` r
bg_use_brms(project)
```

## Arguments

- project:

  A `bg_handle`.

## Value

Invisibly, the project handle.
