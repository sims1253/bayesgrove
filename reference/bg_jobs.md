# Read current state of all jobs

Reads the jobs log, consulting a per-handle cache that is invalidated by
file mtime+size. Within a session, repeated calls after the first parse
return the cached snapshot, so a run of N nodes does O(N) rather than
O(N^2) full log parses.

## Usage

``` r
bg_jobs(project, status = NULL, detailed = FALSE)
```

## Arguments

- project:

  A `bg_handle`.

- status:

  Optional character vector of statuses to filter by.

- detailed:

  Logical, whether to return detailed job information (default FALSE).

## Value

A named list of job records.
