# Run a project workflow

Executes the workflow, driving every node that is ready and unheld to
completion. Execution proceeds in topological **waves**: each wave is
the set of nodes whose upstream inputs are already available, so the
nodes within a wave are mutually independent. With `parallel = "auto"`
(the default) and active
[mirai::daemons](https://mirai.r-lib.org/reference/daemons.html) set, a
wave is dispatched concurrently; otherwise it runs sequentially in
topological order. Either way the protocol holds and the pause flag are
evaluated at wave boundaries, so a hold discovered from a fresh summary
in one wave takes effect before the next wave is dispatched.

## Usage

``` r
bg_run(project, targets = NULL, parallel = c("auto", "never", "always"))
```

## Arguments

- project:

  A `bg_handle`.

- targets:

  Optional character vector of target node IDs.

- parallel:

  One of `"auto"`, `"never"`, `"always"`. `"auto"` (default) dispatches
  waves in parallel when mirai daemons are active, otherwise falls back
  to sequential execution. `"never"` always runs sequentially.
  `"always"` requires mirai and active daemons, erroring if unavailable.

## Value

A `bg_run_handle` list.
