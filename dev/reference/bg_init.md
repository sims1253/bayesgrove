# Initialize a bayesgrove Project

Creates the directory structure and initial state for a new bayesgrove
project.

## Usage

``` r
bg_init(
  path = ".",
  project_name = NULL,
  config = list(),
  workflow_packs = NULL
)
```

## Arguments

- path:

  Directory path where the project should be initialized.

- project_name:

  Optional. Name of the project. Defaults to the basename of the path.

- config:

  Optional. Configuration list.

- workflow_packs:

  Optional list of active workflow packs. These are normalized and fixed
  at project initialization. The project starts empty by default.
  Built-in packs such as `bayesgrove.default_bayesian` provide
  computation review, branch-scoped fit criticism, candidate-comparison
  guidance, and explicit branch acceptance or rejection decisions.
  Optional built-in packs add prior rationale and prior predictive
  review (`bayesgrove.prior_workflow`), posterior predictive and SBC
  review (`bayesgrove.model_checks`), model-selection review with
  stacking weights (`bayesgrove.model_selection`), minimal causal
  framing (`bayesgrove.causal_minimal`), and PAD annotations
  (`bayesgrove.pad_scaffold`).

## Value

A `bg_handle` representing the open project.
