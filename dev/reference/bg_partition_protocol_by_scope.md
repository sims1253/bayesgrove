# Partition protocol results by scope

Helper for UI layers to group obligations and actions by scope. Returns
a nested structure where each scope has its own obligations and actions,
making branch-aware rendering straightforward.

## Usage

``` r
bg_partition_protocol_by_scope(result, project = NULL)
```

## Arguments

- result:

  A `bg_next_actions` result object.

- project:

  Optional `bg_handle` to include display labels.

## Value

A named list where each element corresponds to a scope, containing:

- `scope`: The scope string

- `scope_label`: Human-readable label (if project provided)

- `obligations`: List of obligations for this scope

- `actions`: List of actions for this scope
