# Reconcile background daemon jobs

This checks the status of background jobs coordinated via `mirai` and
updates the job logs if they have finished or failed unexpectedly.

## Usage

``` r
bg_reconcile_daemon_jobs(project)
```

## Arguments

- project:

  A `bg_handle`.
