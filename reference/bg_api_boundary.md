# Query the bayesgrove API boundary

Returns the API classification registry for all exported `bg_*`
functions. Each function is classified as `stable`, `experimental`,
`internal`, or `deprecated`.

## Usage

``` r
bg_api_boundary(fn = NULL)
```

## Arguments

- fn:

  Optional function name to filter by. If `NULL`, returns all entries.

## Value

A data.frame with columns:

- fn:

  Function name (character)

- classification:

  One of `stable`, `experimental`, `internal`, or `deprecated`

## Details

### Classification Meanings

- `stable`: Core public API expected to remain backward-compatible.
  Includes project lifecycle, graph editing, branching, decisions,
  execution control, status/results, and established workflow entry
  points.

- `experimental`: Newer or provisional APIs that may evolve. Includes
  workflow protocol/context/template surfaces, REPL, and practitioner
  sugar helpers.

- `internal`: Technically exported for worker/process reasons but not
  intended as ergonomic user-facing API. Includes artifact helpers, job
  primitives, and backend registration.

- `deprecated`: Kept for backward compatibility for one release; warns
  on use and names its replacement. Currently
  [`bg_branch_with_continuation()`](https://sims1253.github.io/bayesgrove/reference/bg_branch_with_continuation.md)
  (use `bg_branch(continue = )`).

## Examples

``` r
# Get all API boundary entries
bg_api_boundary()
#>                                fn classification
#> 21                    bg_add_gate         stable
#> 7                     bg_add_node         stable
#> 22                 bg_answer_gate         stable
#> 64                bg_api_boundary         stable
#> 14                      bg_branch         stable
#> 16              bg_branch_lineage         stable
#> 15    bg_branch_with_continuation     deprecated
#> 54      bg_build_workflow_context   experimental
#> 51                      bg_bundle         stable
#> 4                        bg_close         stable
#> 13                bg_commit_graph       internal
#> 35                bg_compact_jobs   experimental
#> 49         bg_compute_fingerprint         stable
#> 8                      bg_connect         stable
#> 27              bg_execute_action   experimental
#> 52               bg_export_report         stable
#> 26          bg_extension_registry   experimental
#> 37                    bg_fit_brms   experimental
#> 36                    bg_fit_stan   experimental
#> 47                    bg_get_goal         stable
#> 38               bg_graph_mermaid         stable
#> 1                       bg_handle         stable
#> 63                bg_hmc_severity         stable
#> 2                         bg_init         stable
#> 20                  bg_invalidate         stable
#> 34                        bg_jobs         stable
#> 17               bg_list_branches         stable
#> 57              bg_list_templates   experimental
#> 55                bg_next_actions         stable
#> 3                         bg_open         stable
#> 56 bg_partition_protocol_by_scope   experimental
#> 30                       bg_pause   experimental
#> 23               bg_pending_gates         stable
#> 28                        bg_plan         stable
#> 39                        bg_plot   experimental
#> 40        bg_read_branch_registry         stable
#> 25              bg_read_decisions         stable
#> 41          bg_read_goal_registry         stable
#> 12                  bg_read_graph         stable
#> 42              bg_read_summaries         stable
#> 24             bg_record_decision         stable
#> 59          bg_register_node_kind       internal
#> 45       bg_register_summary_kind   experimental
#> 11                 bg_remove_node         stable
#> 58                        bg_repl   experimental
#> 60           bg_restore_executors   experimental
#> 33                      bg_result         stable
#> 31                      bg_resume   experimental
#> 19               bg_retire_branch         stable
#> 18                 bg_retire_node         stable
#> 29                         bg_run         stable
#> 48                 bg_scope_label         stable
#> 46                    bg_set_goal         stable
#> 10               bg_set_node_data   experimental
#> 50                    bg_snapshot         stable
#> 32                      bg_status         stable
#> 44          bg_summary_vocabulary   experimental
#> 9                  bg_update_node         stable
#> 62                    bg_use_brms         stable
#> 61                bg_use_cmdstanr         stable
#> 5         bg_use_default_workflow         stable
#> 6           bg_use_workflow_packs         stable
#> 53              bg_workflow_packs   experimental
#> 43             bg_write_summaries         stable

# Filter to a specific function
bg_api_boundary("bg_run")
#>        fn classification
#> 29 bg_run         stable

# Filter to experimental functions
subset(bg_api_boundary(), classification == "experimental")
#>                                fn classification
#> 54      bg_build_workflow_context   experimental
#> 35                bg_compact_jobs   experimental
#> 27              bg_execute_action   experimental
#> 26          bg_extension_registry   experimental
#> 37                    bg_fit_brms   experimental
#> 36                    bg_fit_stan   experimental
#> 57              bg_list_templates   experimental
#> 56 bg_partition_protocol_by_scope   experimental
#> 30                       bg_pause   experimental
#> 39                        bg_plot   experimental
#> 45       bg_register_summary_kind   experimental
#> 58                        bg_repl   experimental
#> 60           bg_restore_executors   experimental
#> 31                      bg_resume   experimental
#> 10               bg_set_node_data   experimental
#> 44          bg_summary_vocabulary   experimental
#> 53              bg_workflow_packs   experimental
```
