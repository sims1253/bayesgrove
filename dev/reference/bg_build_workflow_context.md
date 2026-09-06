# Build the workflow context for a given scope

Build the workflow context for a given scope

## Usage

``` r
bg_build_workflow_context(project, scope = "project")
```

## Arguments

- project:

  A `bg_handle`.

- scope:

  Scope string. Defaults to `project`.

## Value

A workflow context list partitioned into structural, execution, and
evidence.
