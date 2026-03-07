# Read current state of all jobs

Read current state of all jobs

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
