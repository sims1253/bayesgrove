# Create an execution plan

Create an execution plan

## Usage

``` r
bg_plan(
  project,
  targets = NULL,
  external_holds = list(),
  include_inactive = FALSE
)
```

## Arguments

- project:

  A `bg_handle`.

- targets:

  Optional character vector of target node IDs.

- external_holds:

  Optional named list mapping node ids to external hold reasons. Held
  nodes remain distinct from structural blockers.

- include_inactive:

  Whether to keep retired or disabled nodes in the planning graph.
  Defaults to `FALSE`.

## Value

A `bg_run_plan` list.
