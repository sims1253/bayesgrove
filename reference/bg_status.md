# Get workflow status

Get workflow status

## Usage

``` r
bg_status(project, auto_advance = TRUE)
```

## Arguments

- project:

  A `bg_handle`.

- auto_advance:

  Whether to automatically submit newly eligible nodes if the last run
  was async (default TRUE).

## Value

A `bg_status` list summarizing the project.
