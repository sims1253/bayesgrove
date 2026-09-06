# Restore user-supplied executors from persisted source.

Opening a project never runs project-supplied code. This function is the
explicit, opt-in path to restore executors stored as source text in the
project's runtime manifest.

## Usage

``` r
bg_restore_executors(project, trust = FALSE)
```

## Arguments

- project:

  A `bg_handle`.

- trust:

  Logical. If `FALSE` (default), print sources and abort without
  evaluating. If `TRUE`, evaluate the sources.

## Value

Invisibly, the `project` handle with executors populated.

## Details

With `trust = FALSE` (the default) the stored source for each kind is
printed and the function aborts, so the user can review what would be
evaluated before proceeding. With `trust = TRUE` each source string is
evaluated in a child of
[`globalenv()`](https://rdrr.io/r/base/environment.html) (not in the
bayesgrove namespace) and a warning notes that closures over the
original environment are not restored.
