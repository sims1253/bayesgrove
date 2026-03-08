# Build the API boundary registry

Returns a data frame containing all exported `bg_*` functions with their
classification, notes, and remote accessibility flags.

## Usage

``` r
bg_build_api_boundary_registry()
```

## Value

A data.frame with columns: `fn` (function name), `classification`
(`stable`, `experimental`, or `internal_exported`), `note`
(description), and `remote_accessible` (logical).
