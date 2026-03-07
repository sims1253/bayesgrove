# Pause the workflow execution

Pausing the workflow stops auto-advancing and marks the workflow state
so that no new jobs are dispatched. It does not cancel currently running
jobs.

## Usage

``` r
bg_pause(project)
```

## Arguments

- project:

  A `bg_handle`.
