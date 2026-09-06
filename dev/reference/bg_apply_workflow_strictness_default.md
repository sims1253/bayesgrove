# Apply a project-level `workflow_strictness` default to pack refs.

When a project config sets `workflow_strictness` (e.g. `"advisory"`),
each pack ref that does not declare its own `config$strictness` inherits
it, so packs without an explicit per-ref setting still downgrade
blocking obligations. Explicit per-ref strictness always wins.

## Usage

``` r
bg_apply_workflow_strictness_default(pack_refs, default_strictness)
```
