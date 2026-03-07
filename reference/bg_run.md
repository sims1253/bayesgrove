# Run a project workflow

Run a project workflow

## Usage

``` r
bg_run(
  project,
  targets = NULL,
  mode = c("sync", "async"),
  backend = c("auto", "callr", "mirai")
)
```

## Arguments

- project:

  A `bg_handle`.

- targets:

  Optional character vector of target node IDs.

- mode:

  Execution mode: 'sync' or 'async'.

- backend:

  For async mode, the backend to use ('auto', 'callr', 'mirai').

## Value

A `bg_run_handle` list.
