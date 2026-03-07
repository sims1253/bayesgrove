# Read persisted summaries

Read persisted summaries

## Usage

``` r
bg_read_summaries(
  project,
  scope = NULL,
  include_stale = TRUE,
  include_inactive = TRUE,
  predicted_fingerprints = NULL,
  artifact_index = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- scope:

  Optional scope filter.

- include_stale:

  Whether to keep stale entries.

- include_inactive:

  Whether to keep summaries from retired or disabled nodes. Defaults to
  `TRUE`.

- predicted_fingerprints:

  Optional named fingerprint map.

- artifact_index:

  Optional artifact index.

## Value

Named list of summary entries annotated with `is_fresh` and `is_stale`.
