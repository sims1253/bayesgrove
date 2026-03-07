# Branch a node in the BayesGrove project graph

Clones an existing node and its upstream dependencies (edges) to create
a new branch.

## Usage

``` r
bg_branch(project, node_id, label = NULL, copy_params = TRUE)
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

## Value

A `bg_branch_record`.
