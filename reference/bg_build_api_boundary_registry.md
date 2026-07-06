# Build the API boundary registry

Returns a data frame containing all exported `bg_*` functions with their
classification.

## Usage

``` r
bg_build_api_boundary_registry()
```

## Value

A data.frame with columns: `fn` (function name) and `classification`
(`stable`, `experimental`, `internal`, or `deprecated`).
