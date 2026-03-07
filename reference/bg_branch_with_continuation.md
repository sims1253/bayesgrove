# Branch a node with downstream continuation

Creates a branch from a node and optionally clones immediate downstream
nodes to establish a continuation path. This is a narrowly scoped helper
for guided workflows where a branched fit should flow into downstream
diagnostic or comparison nodes.

## Usage

``` r
bg_branch_with_continuation(
  project,
  node_id,
  label = NULL,
  copy_params = TRUE,
  continuation_kinds = NULL,
  continuation_depth = 1L
)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The ID of the node to branch.

- label:

  Optional label for the new branched node.

- copy_params:

  Whether to copy the parameters of the branched node (default: TRUE).

- continuation_kinds:

  Character vector of node kinds to clone downstream. If NULL (default),
  clones all immediate children. If empty, behaves like
  [`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
  with no continuation.

- continuation_depth:

  How many levels of downstream nodes to clone. Only 1 (immediate
  children) is supported in v1.

## Value

A list containing the `branch` record and `continuation_nodes` mapping
source node IDs to their cloned counterparts.
