# Branch a node in the bayesgrove project graph

Clones an existing node and its upstream dependencies (edges) to create
a new branch. With `continue`, immediate downstream nodes are cloned
too, so a branched fit flows into fresh diagnostic or comparison nodes
instead of the originals.

## Usage

``` r
bg_branch(
  project,
  node_id,
  label = NULL,
  copy_params = TRUE,
  continue = character()
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

- continue:

  Downstream continuation: `TRUE` clones all immediate children of
  `node_id` onto the branch (their inputs remapped to the branch root),
  a character vector clones only children of those node kinds (e.g.
  `c("check", "ppc")`), and the default
  [`character()`](https://rdrr.io/r/base/character.html) (or `FALSE`)
  clones nothing. Only immediate children are cloned.

## Value

A `bg_branch_record` list. It always carries a `$continuation_nodes`
entry: a named list (keyed by source node id) of
`list(source_id, clone_id, kind, label)` records for each cloned child,
empty when `continue` requested none.
