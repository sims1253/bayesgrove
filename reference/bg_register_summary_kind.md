# Register a custom summary kind

Adds a summary-kind descriptor to the project's runtime registry so that
executors emitting it are recognized by
[`bg_write_summaries()`](https://sims1253.github.io/bayesgrove/reference/bg_write_summaries.md)
validation.

## Usage

``` r
bg_register_summary_kind(
  project,
  kind,
  title = NULL,
  expected_metrics = list(),
  emitted_by = NULL,
  consumed_by_packs = character()
)
```

## Arguments

- project:

  A `bg_handle`.

- kind:

  Summary-kind name (a non-empty string).

- title:

  Optional human-readable title.

- expected_metrics:

  Optional named list of metric type strings.

- emitted_by:

  Optional free-text description of the emitter.

- consumed_by_packs:

  Optional character vector of consuming pack ids.

## Value

Invisibly, the registered descriptor.
