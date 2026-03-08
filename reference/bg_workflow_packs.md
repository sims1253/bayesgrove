# List active workflow packs

List active workflow packs

## Usage

``` r
bg_workflow_packs(project)
```

## Arguments

- project:

  A `bg_handle`.

## Value

A list of active workflow-pack descriptors. The built-in default pack id
is `bayesguide.default_bayesian`. Additional built-in pack ids are
`bayesgrove.prior_workflow`, `bayesgrove.model_checks`,
`bayesgrove.model_selection`, `bayesgrove.causal_minimal`, and
`bayesgrove.pad_scaffold`.

## Details

The built-in default pack is an opinionated Bayesian workflow layer. It
derives computation-review obligations from fresh warning/error
summaries, adds branch-scoped fit criticism for model-diagnostic
evidence, requires project-scoped comparison decisions when multiple fit
candidates are clean, and asks each candidate branch to be explicitly
accepted or rejected after a current comparison exists. The optional
phase-10 packs extend that vocabulary with prior rationale and prior
predictive review, posterior predictive and SBC review, model-selection
evidence including stacking weights, minimal causal framing, and PAD
annotations with utility dimensions.
