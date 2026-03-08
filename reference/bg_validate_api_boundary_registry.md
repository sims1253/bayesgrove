# Validate API boundary registry integrity

Internal helper to validate that the registry is well-formed. Used by
tests to catch drift.

## Usage

``` r
bg_validate_api_boundary_registry()
```

## Value

`TRUE` if valid, otherwise raises an error.
