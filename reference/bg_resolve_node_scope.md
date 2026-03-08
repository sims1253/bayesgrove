# Resolve the workflow scope for a node

Resolve the workflow scope for a node

## Usage

``` r
bg_resolve_node_scope(
  project,
  node_id,
  graph = NULL,
  branches = NULL,
  scope_resolution = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  Node id.

- graph:

  Optional pre-read graph.

- branches:

  Optional pre-read branch registry entries.

- scope_resolution:

  Optional scope-resolution cache created by
  `bg_node_scope_resolution()`.

## Value

A scope string, either `project` or a branch id.
