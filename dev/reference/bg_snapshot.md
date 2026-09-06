# Retrieve a complete snapshot of the project state

This aggregates the project configuration, the structural graph, the
active runtime registries, the pending decision gates, the active jobs,
and the overall workflow status into a single nested list.

## Usage

``` r
bg_snapshot(project)
```

## Arguments

- project:

  A `bg_handle`.

## Value

A list representing the `bg_project_snapshot` schema.
