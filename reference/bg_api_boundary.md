# Query the bayesgrove API boundary

Returns the API classification registry for all exported `bg_*`
functions. Each function is classified as `stable`, `experimental`, or
`internal_exported`, and carries a `remote_accessible` flag indicating
whether it is part of the remote protocol contract.

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

  One of `stable`, `experimental`, or `internal_exported`

- note:

  Short description of the function's role

- remote_accessible:

  Logical indicating if exposed via remote protocol

## Details

### Classification Meanings

- `stable`: Core public API expected to remain backward-compatible.
  Includes project lifecycle, graph editing, branching, decisions,
  execution control, status/results, and established workflow entry
  points.

- `experimental`: Newer or provisional APIs that may evolve. Includes
  workflow protocol/context/template surfaces, REPL, and newer branch
  continuation helpers.

- `internal_exported`: Technically exported for worker/process reasons
  but not intended as ergonomic user-facing API. Includes artifact
  helpers, job primitives, backend registration, and daemon
  reconciliation.

### Remote Accessibility

The `remote_accessible` column indicates whether a function is part of
the IPC contract exposed via remote protocol (e.g., WebSocket).
Functions marked `TRUE` form a deliberate public contract for external
clients. The remote-accessible set is intentionally smaller than the
full in-process R API and backs the experimental
[`bg_serve()`](https://sims1253.github.io/bayesgrove/reference/bg_serve.md)
protocol boundary.

## Examples

``` r
# Get all API boundary entries
bg_api_boundary()
#>                                fn    classification
#> 18                    bg_add_gate            stable
#> 5                     bg_add_node            stable
#> 19                 bg_answer_gate            stable
#> 67                bg_api_boundary            stable
#> 11                      bg_branch            stable
#> 13              bg_branch_lineage            stable
#> 12    bg_branch_with_continuation      experimental
#> 48                 bg_brms_plugin            stable
#> 52      bg_build_workflow_context      experimental
#> 45                      bg_bundle            stable
#> 28                      bg_cancel            stable
#> 57              bg_check_artifact internal_exported
#> 4                        bg_close            stable
#> 47             bg_cmdstanr_plugin            stable
#> 10                bg_commit_graph internal_exported
#> 43         bg_compute_fingerprint            stable
#> 6                      bg_connect            stable
#> 61                  bg_create_job internal_exported
#> 49          bg_diagnostics_plugin            stable
#> 23              bg_execute_action      experimental
#> 46               bg_export_report            stable
#> 22          bg_extension_registry      experimental
#> 59              bg_fetch_artifact internal_exported
#> 40                    bg_get_goal            stable
#> 1                       bg_handle            stable
#> 2                         bg_init            stable
#> 17                  bg_invalidate            stable
#> 33                        bg_jobs            stable
#> 14               bg_list_branches            stable
#> 55              bg_list_templates      experimental
#> 60                     bg_log_job internal_exported
#> 53                bg_next_actions      experimental
#> 3                         bg_open            stable
#> 54 bg_partition_protocol_by_scope      experimental
#> 29                       bg_pause            stable
#> 20               bg_pending_gates            stable
#> 24                        bg_plan            stable
#> 34        bg_read_branch_registry            stable
#> 35          bg_read_goal_registry            stable
#> 9                   bg_read_graph            stable
#> 36              bg_read_summaries            stable
#> 65       bg_reconcile_daemon_jobs internal_exported
#> 21             bg_record_decision            stable
#> 63            bg_register_backend internal_exported
#> 64          bg_register_node_kind internal_exported
#> 8                  bg_remove_node            stable
#> 56                        bg_repl      experimental
#> 41          bg_resolve_node_scope            stable
#> 32                      bg_result            stable
#> 30                      bg_resume            stable
#> 16               bg_retire_branch            stable
#> 15                 bg_retire_node            stable
#> 25                         bg_run            stable
#> 42                 bg_scope_label            stable
#> 68                       bg_serve      experimental
#> 39                    bg_set_goal            stable
#> 44                    bg_snapshot            stable
#> 31                      bg_status            stable
#> 58              bg_store_artifact internal_exported
#> 26                      bg_submit            stable
#> 38            bg_summary_is_fresh            stable
#> 62                  bg_update_job internal_exported
#> 7                  bg_update_node            stable
#> 27                        bg_wait            stable
#> 66              bg_worker_process internal_exported
#> 51            bg_workflow_context      experimental
#> 50              bg_workflow_packs      experimental
#> 37             bg_write_summaries            stable
#>                                           note remote_accessible
#> 18                         Add a decision gate             FALSE
#> 5                      Add a node to the graph              TRUE
#> 19                      Answer a decision gate              TRUE
#> 67           Query API classification registry             FALSE
#> 11                 Create a branch from a node             FALSE
#> 13                 Get ancestor branch lineage              TRUE
#> 12 Branch with downstream continuation (newer)             FALSE
#> 48                         brms backend plugin             FALSE
#> 52        Build workflow context (lower-level)             FALSE
#> 45                  Bundle project for handoff             FALSE
#> 28                         Cancel an async run              TRUE
#> 57        Check artifact cache (worker-facing)             FALSE
#> 4                       Close a project handle             FALSE
#> 47                     cmdstanr backend plugin             FALSE
#> 10           Internal graph persistence helper             FALSE
#> 43                   Compute cache fingerprint             FALSE
#> 6                            Connect two nodes              TRUE
#> 61           Create job record (worker-facing)             FALSE
#> 49                          Diagnostics plugin             FALSE
#> 23 Execute a protocol action inside bayesgrove              TRUE
#> 46                      Export workflow report             FALSE
#> 22   Return the descriptive extension registry              TRUE
#> 59   Fetch artifact from cache (worker-facing)             FALSE
#> 40                             Get branch goal             FALSE
#> 1                 S7 class for project handles             FALSE
#> 2                     Initialize a new project             FALSE
#> 17                   Invalidate cached results             FALSE
#> 33                            List job records             FALSE
#> 14                           List all branches              TRUE
#> 55                     List built-in templates             FALSE
#> 60               Log job entry (worker-facing)             FALSE
#> 53               Compute next workflow actions              TRUE
#> 3                     Open an existing project             FALSE
#> 54         Partition protocol results by scope             FALSE
#> 29                    Pause workflow execution             FALSE
#> 20                          List pending gates             FALSE
#> 24                    Create an execution plan             FALSE
#> 34                        Read branch registry             FALSE
#> 35                          Read goal registry             FALSE
#> 9                       Read the project graph             FALSE
#> 36                    Read persisted summaries             FALSE
#> 65            Reconcile background daemon jobs             FALSE
#> 21                 Record an explicit decision              TRUE
#> 63             Register backend implementation             FALSE
#> 64                 Register node kind executor             FALSE
#> 8                 Remove a node from the graph              TRUE
#> 56                            Interactive REPL             FALSE
#> 41                 Resolve node workflow scope             FALSE
#> 32                      Retrieve a node result             FALSE
#> 30                   Resume workflow execution             FALSE
#> 16               Retire a branch from planning             FALSE
#> 15                 Retire a node from planning             FALSE
#> 25                  Run workflow synchronously             FALSE
#> 42                 Get display label for scope             FALSE
#> 68   Serve the local experimental IPC protocol             FALSE
#> 39                 Set branch inferential goal             FALSE
#> 44                        Get project snapshot              TRUE
#> 31                         Get workflow status              TRUE
#> 58     Store artifact in cache (worker-facing)             FALSE
#> 26              Submit workflow asynchronously              TRUE
#> 38                     Check summary freshness             FALSE
#> 62           Update job record (worker-facing)             FALSE
#> 7                       Update node properties              TRUE
#> 27                         Wait for async jobs             FALSE
#> 66  Worker process entry point (cross-process)             FALSE
#> 51         Build workflow context for protocol             FALSE
#> 50                  List active workflow packs             FALSE
#> 37                    Write executor summaries             FALSE

# Filter to a specific function
bg_api_boundary("bg_run")
#>        fn classification                       note remote_accessible
#> 25 bg_run         stable Run workflow synchronously             FALSE

# Filter to experimental functions
subset(bg_api_boundary(), classification == "experimental")
#>                                fn classification
#> 12    bg_branch_with_continuation   experimental
#> 52      bg_build_workflow_context   experimental
#> 23              bg_execute_action   experimental
#> 22          bg_extension_registry   experimental
#> 55              bg_list_templates   experimental
#> 53                bg_next_actions   experimental
#> 54 bg_partition_protocol_by_scope   experimental
#> 56                        bg_repl   experimental
#> 68                       bg_serve   experimental
#> 51            bg_workflow_context   experimental
#> 50              bg_workflow_packs   experimental
#>                                           note remote_accessible
#> 12 Branch with downstream continuation (newer)             FALSE
#> 52        Build workflow context (lower-level)             FALSE
#> 23 Execute a protocol action inside bayesgrove              TRUE
#> 22   Return the descriptive extension registry              TRUE
#> 55                     List built-in templates             FALSE
#> 53               Compute next workflow actions              TRUE
#> 54         Partition protocol results by scope             FALSE
#> 56                            Interactive REPL             FALSE
#> 68   Serve the local experimental IPC protocol             FALSE
#> 51         Build workflow context for protocol             FALSE
#> 50                  List active workflow packs             FALSE
```
