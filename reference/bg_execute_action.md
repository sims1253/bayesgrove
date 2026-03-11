# Execute a workflow action from the protocol surface

Resolves an action by `action_id` from the current
[`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
surface and executes it inside bayesgrove so GUI clients do not need to
own workflow-command semantics.

## Usage

``` r
bg_execute_action(project, action_id, overrides = list())
```

## Arguments

- project:

  A `bg_handle`.

- action_id:

  Action identifier from
  [`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md).

- overrides:

  Optional named list of user-supplied inputs.

## Value

A plain-data execution result.
