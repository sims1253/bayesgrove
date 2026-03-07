# Persist executor summaries

Persist executor summaries

## Usage

``` r
bg_write_summaries(
  project,
  node_id,
  artifact_ref,
  execution_fingerprint,
  summaries
)
```

## Arguments

- project:

  A `bg_handle`.

- node_id:

  Node id for the execution.

- artifact_ref:

  Persisted artifact ref.

- execution_fingerprint:

  Predicted execution fingerprint.

- summaries:

  List of emitted summary payloads.

## Value

Named list of persisted summary entries.
