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

## Details

In a synchronous runtime this is a near-no-op: it only gates the next
[bg_run](https://sims1253.github.io/bayesgrove/reference/bg_run.md)
call. Under parallel execution (Milestone 3) pause takes effect at WAVE
BOUNDARIES — the in-flight wave finishes, then the run stops before the
next wave dispatches, and
[bg_resume](https://sims1253.github.io/bayesgrove/reference/bg_resume.md)
clears the flag. Reclassified `experimental` while the wave-boundary
semantics settle.
