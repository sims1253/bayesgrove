# Bundle a BayesGrove project for reproducible handoff

Creates a portable `.tar.gz` archive of the project, including the
structural graph, decision log, job history, configuration, and
optionally cached artifacts or raw data files.

## Usage

``` r
bg_bundle(
  project,
  path = NULL,
  include_data = c("omit", "freeze_source", "copy", "recipe_only"),
  include_fits = FALSE
)
```

## Arguments

- project:

  A `bg_handle`.

- path:

  File path where the bundle should be saved (default: a temp file).

- include_data:

  How to handle external data sources: 'omit', 'freeze_source', or
  'copy'. The legacy alias 'recipe_only' is accepted and treated as
  'omit'.

- include_fits:

  Logical, whether to package the large model fit artifacts.

## Value

The file path to the generated bundle.
