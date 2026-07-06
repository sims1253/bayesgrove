# Branch a node with downstream continuation (deprecated)

Deprecated: use
[`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
with the `continue` argument instead — `continue = TRUE` clones all
immediate children, a character vector clones only children of those
kinds. This wrapper delegates to
[`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
and returns the historical `list(branch, continuation_nodes)` shape; it
warns once per session and will be removed in a future release.

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
  children) is supported.

## Value

A list containing the `branch` record and `continuation_nodes` mapping
source node IDs to their cloned counterparts.

## See also

[`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
