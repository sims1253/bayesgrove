# Generate a reproducible markdown report of the workflow

Generate a reproducible markdown report of the workflow

## Usage

``` r
bg_export_report(
  project,
  path = NULL,
  format = c("html", "md"),
  out_file = NULL
)
```

## Arguments

- project:

  A `bg_handle`.

- path:

  Optional output path. Defaults to `workflow_report.md` or
  `workflow_report.html` in the project root depending on `format`.

- format:

  Output format: markdown (`"md"`) or html (`"html"`).

- out_file:

  Deprecated alias for `path`.

## Value

The path to the written report file.
