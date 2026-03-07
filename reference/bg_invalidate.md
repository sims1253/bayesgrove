# Invalidate a node's result

Invalidation is an explicit escape hatch for intentional recomputation.
It does not alter the structural graph state.

## Usage

``` r
bg_invalidate(project, node_id, recursive = TRUE)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The ID of the node to invalidate.

- recursive:

  Whether to recursively invalidate downstream nodes (default: TRUE).
