# List built-in workflow templates

List built-in workflow templates

## Usage

``` r
bg_list_templates(template_ref = NULL)
```

## Arguments

- template_ref:

  Optional template ref. When provided, returns the matching descriptor
  or `NULL`.

## Value

Either a list of built-in template descriptors keyed by `template_ref`,
or a single descriptor when `template_ref` is supplied. Built-in refs
are `diagnostic_check`, `sbc_check`, `branch_comparison`,
`branch_and_modify_fit`, and `review_decision`.

## Details

Built-in templates are intentionally small and explicit. Some create
nodes directly, while others wrap narrow workflow macros such as
`bg_branch(continue = )` or
[`bg_record_decision()`](https://sims1253.github.io/bayesgrove/reference/bg_record_decision.md).
The guided REPL uses the same registry to execute template-backed
actions.
