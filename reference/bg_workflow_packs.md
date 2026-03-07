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
is `bayesguide.default_bayesian`.

## Details

The built-in default pack is an opinionated Bayesian workflow layer. It
derives computation-review obligations from fresh warning/error
summaries, adds branch-scoped fit criticism for model-diagnostic
evidence, requires project-scoped comparison decisions when multiple fit
candidates are clean, and asks each candidate branch to be explicitly
accepted or rejected after a current comparison exists.
