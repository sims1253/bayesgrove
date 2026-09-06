# Compute HMC diagnostic severity from sampler metrics.

Thresholds follow Vehtari et al. (2021) for R-hat/ESS. Defaults are
overridable via node params (`rhat_error`, `rhat_warn`, `ess_warn`,
`ebfmi_warn`, `divergence_rate_error`).

## Usage

``` r
bg_hmc_severity(metrics, thresholds = list())
```

## Arguments

- metrics:

  Named list with: divergences, max_treedepth_hits, max_rhat,
  min_bulk_ess, min_tail_ess, e_bfmi, num_transitions. `num_transitions`
  is the total post-warmup transitions across all chains (the
  denominator for the per-transition divergence rate), since divergences
  are summed across chains. May optionally carry `n_chains` to scale the
  default ESS warning threshold.

- thresholds:

  Optional list overriding default thresholds.

## Value

One of "ok", "warning", "error".

## Details

The ESS warning threshold scales with the chain count: Vehtari et al.
(2021) recommend ~100 effective samples per chain. When `metrics`
carries an `n_chains` entry, `ess_warn` defaults to `100 * n_chains`;
otherwise it stays 400 (the historical default). An explicit
`thresholds$ess_warn` always wins over both.
