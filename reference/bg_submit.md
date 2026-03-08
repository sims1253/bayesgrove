# Submit a project workflow for asynchronous execution

Submit a project workflow for asynchronous execution

## Usage

``` r
bg_submit(
  project,
  targets = NULL,
  mode = c("async"),
  backend = c("auto", "mirai")
)
```

## Arguments

- project:

  A `bg_handle`.

- targets:

  Optional character vector of target node IDs.

- mode:

  Execution mode: 'async'.

- backend:

  The async backend to use (`"mirai"`). `"auto"` resolves to `"mirai"`
  and errors if the package is not installed.

## Value

A `bg_run_handle` list.
