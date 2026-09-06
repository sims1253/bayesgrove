# Get a display label for a scope

Returns a human-readable label for a scope string. For project scope,
returns "Project". For branch scopes, returns the branch label or a
truncated branch id.

## Usage

``` r
bg_scope_label(project, scope)
```

## Arguments

- project:

  A `bg_handle`.

- scope:

  A scope string like "project" or "branch:...".

## Value

A character string suitable for display.
