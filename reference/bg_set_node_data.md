# Attach a data object to a node via the content-addressed store

Serializes `data` through the project's CAS store and records a
`data_ref = "cas:sha256:..."` param on the node, so the `stan_data`
executor can fetch it at run time without an embedded code channel.
Existing params are preserved (merged, not replaced). The data object
must be serializable via
[`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html).

## Usage

``` r
bg_set_node_data(project, node_id, data)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  The node to attach data to (typically a `stan_data` node).

- data:

  An R object to attach.

## Value

The node ID, invisibly.
