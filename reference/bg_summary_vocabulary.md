# Built-in summary-kind vocabulary

Returns a named list of summary-kind descriptors. Each descriptor has:
`kind`, `title`, `expected_metrics` (named list of type strings,
informative not enforced), `emitted_by` (free text), `consumed_by_packs`
(character).

## Usage

``` r
bg_summary_vocabulary()
```

## Value

Named list of summary-kind descriptors.

## Details

### Evidence coverage

Every summary kind a built-in workflow pack matches on is emitted by at
least one shipped executor (or is documented as "bring your own"). The
table below maps the kinds packs most commonly ask for to the built-in
node kind that produces them:

|  |  |  |
|----|----|----|
| summary kind | produced by built-in kind | notes |
| `hmc_diagnostics` | `cmdstanr_fit` / `brms_fit` | HMC sampler diagnostics |
| `prior_predictive_check` | `prior_fit` / `brms_prior_fit` | prior-predictive draws |
| `posterior_predictive_check` | `ppc` | observed y + capped yrep draws |
| `loo_diagnostics` | `loo` | PSIS-LOO Pareto-k |
| `loo_pit_calibration` | `loo_pit` | randomized PIT + PIET severity |
| `sbc_result` | `sbc` | ESS-bounded ranks + valid bins |
| `comparison_results` / `stacking_weights` | `compare` | model comparison |
| `prior_spec` | user executor | bring your own (record prior) |
| `projpred_selection` | user executor | bring your own (`projpred`) |
| `causal_*` / `pad_*` | user executors | bring your own (causal/PAD) |

A test (`test-summary-vocabulary-coverage.R`) asserts every literal
`summary_kinds = "<kind>"` referenced by a pack source is present here,
so a pack can never demand evidence no shipped executor can produce
without the vocabulary (and its test) being updated in lockstep.
