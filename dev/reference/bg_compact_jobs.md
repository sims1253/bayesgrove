# Compact the append-only jobs log

Rewrites `jobs.jsonl` atomically with only the latest snapshot for each
job. This preserves the last-record-wins view returned by
[`bg_jobs()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_jobs.md)
while bounding cold-open parsing costs for long-lived projects. Sequence
numbers are reassigned in their existing order so later appends remain
monotonic.

## Usage

``` r
bg_compact_jobs(project)
```

## Arguments

- project:

  A writable `bg_handle`.

## Value

Invisibly, a list with `before`, `after`, and `removed` line counts.
