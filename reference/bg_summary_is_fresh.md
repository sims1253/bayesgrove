# Determine whether a summary is fresh

Determine whether a summary is fresh

## Usage

``` r
bg_summary_is_fresh(
  project,
  summary,
  predicted_fingerprints = NULL,
  artifact_index = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- summary:

  A persisted summary entry.

- predicted_fingerprints:

  Optional named fingerprint map.

- artifact_index:

  Optional normalized artifact index.

## Value

Logical scalar.
