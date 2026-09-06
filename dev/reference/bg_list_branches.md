# List all branches with their metadata

Returns a data-frame-like list of all registered branches with their
identifying information and optional display labels.

## Usage

``` r
bg_list_branches(project)
```

## Arguments

- project:

  A `bg_handle`.

## Value

A named list of branch records, each containing `branch_id`, `label`,
`root_node_id`, `source_node_id`, `created_at`, and `has_goal`.
