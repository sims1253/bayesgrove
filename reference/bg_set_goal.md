# Set the active inferential goal for a branch

Set the active inferential goal for a branch

## Usage

``` r
bg_set_goal(
  project,
  branch_id,
  kind,
  label = NULL,
  rationale,
  metadata = list(),
  goal_id = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- branch_id:

  A branch scope id such as `branch:...`.

- kind:

  Goal kind string.

- label:

  Optional user-facing label.

- rationale:

  Required rationale string.

- metadata:

  Optional metadata.

- goal_id:

  Optional goal id.

## Value

The recorded decision.
