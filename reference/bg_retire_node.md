# Retire a node and downstream lineage

Retirement removes a node path from future planning and workflow
guidance while preserving its structural provenance and cached
artifacts.

## Usage

``` r
bg_retire_node(project, node_id, recursive = TRUE, reason = NULL)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The ID of the node to retire.

- recursive:

  Whether to recursively retire downstream nodes (default: TRUE).

- reason:

  Optional rationale stored in node metadata.

## Value

Invisibly returns the retired node ids.
