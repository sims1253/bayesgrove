# Retire an entire branch

Retirement removes the branch from future planning and workflow guidance
while preserving its provenance and cached artifacts.

## Usage

``` r
bg_retire_branch(project, branch_id, reason = NULL)
```

## Arguments

- project:

  A `bg_handle`.

- branch_id:

  The branch id to retire.

- reason:

  Optional rationale stored in branch and node metadata.

## Value

Invisibly returns the retired branch node ids.
