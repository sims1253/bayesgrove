# Return the descriptive extension registry for a project

Exposes Bayesgrove-owned runtime extension descriptors as a read-only,
protocol-facing registry for GUI consumers.

## Usage

``` r
bg_extension_registry(project)
```

## Arguments

- project:

  A `bg_handle`.

## Value

A plain-data extension registry.
