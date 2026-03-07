# Build a workflow context for protocol evaluation

Build a workflow context for protocol evaluation

## Usage

``` r
bg_workflow_context(project, scope = c("project", "branch"), branch_id = NULL)
```

## Arguments

- project:

  A `bg_handle`.

- scope:

  One of `project` or `branch`.

- branch_id:

  Optional branch id, required for branch-scoped queries.

## Value

A `bg_workflow_context` plain-data list.
