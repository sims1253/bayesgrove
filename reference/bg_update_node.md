# Update a node in the bayesgrove project graph

`params` and `metadata` default to MERGING into the node's existing
values via [utils::modifyList](https://rdrr.io/r/utils/modifyList.html),
so `bg_update_node(handle, id, params = list(stan_file = f))` updates
just `stan_file` and preserves `chains`, `seed`, etc. Pass
`replace = TRUE` for intentional wholesale replacement (the historical
behavior of
[`dagriculture::dagri_update_node`](https://sims1253.github.io/dagriculture/reference/dagri_update_node.html)).

## Usage

``` r
bg_update_node(
  project,
  node_id,
  label = NULL,
  params = NULL,
  metadata = NULL,
  replace = FALSE
)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The node ID to update.

- label:

  Optional new label.

- params:

  Optional new parameters (merged by default).

- metadata:

  Optional new metadata (merged by default).

- replace:

  Logical scalar, default `FALSE`. When `TRUE`, `params` and `metadata`
  REPLACE the existing lists wholesale instead of merging.

## Value

The node ID.
