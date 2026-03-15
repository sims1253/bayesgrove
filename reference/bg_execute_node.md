# Execute a single node: resolve inputs, run executor, store artifact, update job

Execute a single node: resolve inputs, run executor, store artifact,
update job

## Usage

``` r
bg_execute_node(handle, node_id, fingerprint, input_bindings, graph, job_id)
```

## Arguments

- handle:

  A `bg_handle`.

- node_id:

  Node to execute.

- fingerprint:

  Execution fingerprint.

- input_bindings:

  Input binding list from the run plan.

- graph:

  The graph (used to look up the node).

- job_id:

  Job ID to update.

## Value

A list with `ok` (logical) and either `ref` (artifact ref) or `error`.
