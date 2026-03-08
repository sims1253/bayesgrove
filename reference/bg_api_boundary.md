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
clients. Currently all functions have `remote_accessible = FALSE` since
no `bg_serve()` remote protocol layer exists yet.

## Examples

``` r
# Get all API boundary entries
bg_api_boundary()
#>                                fn    classification
#> 18                    bg_add_gate            stable
#> 5                     bg_add_node            stable
#> 19                 bg_answer_gate            stable
#> 65                bg_api_boundary            stable
#> 11                      bg_branch            stable
#> 13              bg_branch_lineage            stable
#> 12    bg_branch_with_continuation      experimental
#> 46                 bg_brms_plugin            stable
#> 50      bg_build_workflow_context      experimental
#> 43                      bg_bundle            stable
#> 26                      bg_cancel            stable
#> 55              bg_check_artifact internal_exported
#> 4                        bg_close            stable
#> 45             bg_cmdstanr_plugin            stable
#> 10                bg_commit_graph internal_exported
#> 41         bg_compute_fingerprint            stable
#> 6                      bg_connect            stable
#> 59                  bg_create_job internal_exported
#> 47          bg_diagnostics_plugin            stable
#> 44               bg_export_report            stable
#> 57              bg_fetch_artifact internal_exported
#> 38                    bg_get_goal            stable
#> 1                       bg_handle            stable
#> 2                         bg_init            stable
#> 17                  bg_invalidate            stable
#> 31                        bg_jobs            stable
#> 14               bg_list_branches            stable
#> 53              bg_list_templates      experimental
#> 58                     bg_log_job internal_exported
#> 51                bg_next_actions      experimental
#> 3                         bg_open            stable
#> 52 bg_partition_protocol_by_scope      experimental
#> 27                       bg_pause            stable
#> 20               bg_pending_gates            stable
#> 22                        bg_plan            stable
#> 32        bg_read_branch_registry            stable
#> 33          bg_read_goal_registry            stable
#> 9                   bg_read_graph            stable
#> 34              bg_read_summaries            stable
#> 63       bg_reconcile_daemon_jobs internal_exported
#> 21             bg_record_decision            stable
#> 61            bg_register_backend internal_exported
#> 62          bg_register_node_kind internal_exported
#> 8                  bg_remove_node            stable
#> 54                        bg_repl      experimental
#> 39          bg_resolve_node_scope            stable
#> 30                      bg_result            stable
#> 28                      bg_resume            stable
#> 16               bg_retire_branch            stable
#> 15                 bg_retire_node            stable
#> 23                         bg_run            stable
#> 40                 bg_scope_label            stable
#> 37                    bg_set_goal            stable
#> 42                    bg_snapshot            stable
#> 29                      bg_status            stable
#> 56              bg_store_artifact internal_exported
#> 24                      bg_submit            stable
#> 36            bg_summary_is_fresh            stable
#> 60                  bg_update_job internal_exported
#> 7                  bg_update_node            stable
#> 25                        bg_wait            stable
#> 64              bg_worker_process internal_exported
#> 49            bg_workflow_context      experimental
#> 48              bg_workflow_packs      experimental
#> 35             bg_write_summaries            stable
#>                                           note remote_accessible
#> 18                         Add a decision gate             FALSE
#> 5                      Add a node to the graph             FALSE
#> 19                      Answer a decision gate             FALSE
#> 65           Query API classification registry             FALSE
#> 11                 Create a branch from a node             FALSE
#> 13                 Get ancestor branch lineage             FALSE
#> 12 Branch with downstream continuation (newer)             FALSE
#> 46                         brms backend plugin             FALSE
#> 50        Build workflow context (lower-level)             FALSE
#> 43                  Bundle project for handoff             FALSE
#> 26                         Cancel an async run             FALSE
#> 55        Check artifact cache (worker-facing)             FALSE
#> 4                       Close a project handle             FALSE
#> 45                     cmdstanr backend plugin             FALSE
#> 10           Internal graph persistence helper             FALSE
#> 41                   Compute cache fingerprint             FALSE
#> 6                            Connect two nodes             FALSE
#> 59           Create job record (worker-facing)             FALSE
#> 47                          Diagnostics plugin             FALSE
#> 44                      Export workflow report             FALSE
#> 57   Fetch artifact from cache (worker-facing)             FALSE
#> 38                             Get branch goal             FALSE
#> 1                 S7 class for project handles             FALSE
#> 2                     Initialize a new project             FALSE
#> 17                   Invalidate cached results             FALSE
#> 31                            List job records             FALSE
#> 14                           List all branches             FALSE
#> 53                     List built-in templates             FALSE
#> 58               Log job entry (worker-facing)             FALSE
#> 51               Compute next workflow actions             FALSE
#> 3                     Open an existing project             FALSE
#> 52         Partition protocol results by scope             FALSE
#> 27                    Pause workflow execution             FALSE
#> 20                          List pending gates             FALSE
#> 22                    Create an execution plan             FALSE
#> 32                        Read branch registry             FALSE
#> 33                          Read goal registry             FALSE
#> 9                       Read the project graph             FALSE
#> 34                    Read persisted summaries             FALSE
#> 63            Reconcile background daemon jobs             FALSE
#> 21                 Record an explicit decision             FALSE
#> 61             Register backend implementation             FALSE
#> 62                 Register node kind executor             FALSE
#> 8                 Remove a node from the graph             FALSE
#> 54                            Interactive REPL             FALSE
#> 39                 Resolve node workflow scope             FALSE
#> 30                      Retrieve a node result             FALSE
#> 28                   Resume workflow execution             FALSE
#> 16               Retire a branch from planning             FALSE
#> 15                 Retire a node from planning             FALSE
#> 23                  Run workflow synchronously             FALSE
#> 40                 Get display label for scope             FALSE
#> 37                 Set branch inferential goal             FALSE
#> 42                        Get project snapshot             FALSE
#> 29                         Get workflow status             FALSE
#> 56     Store artifact in cache (worker-facing)             FALSE
#> 24              Submit workflow asynchronously             FALSE
#> 36                     Check summary freshness             FALSE
#> 60           Update job record (worker-facing)             FALSE
#> 7                       Update node properties             FALSE
#> 25                         Wait for async jobs             FALSE
#> 64  Worker process entry point (cross-process)             FALSE
#> 49         Build workflow context for protocol             FALSE
#> 48                  List active workflow packs             FALSE
#> 35                    Write executor summaries             FALSE

# Filter to a specific function
bg_api_boundary("bg_run")
#>        fn classification                       note remote_accessible
#> 23 bg_run         stable Run workflow synchronously             FALSE

# Filter to experimental functions
subset(bg_api_boundary(), classification == "experimental")
#>                                fn classification
#> 12    bg_branch_with_continuation   experimental
#> 50      bg_build_workflow_context   experimental
#> 53              bg_list_templates   experimental
#> 51                bg_next_actions   experimental
#> 52 bg_partition_protocol_by_scope   experimental
#> 54                        bg_repl   experimental
#> 49            bg_workflow_context   experimental
#> 48              bg_workflow_packs   experimental
#>                                           note remote_accessible
#> 12 Branch with downstream continuation (newer)             FALSE
#> 50        Build workflow context (lower-level)             FALSE
#> 53                     List built-in templates             FALSE
#> 51               Compute next workflow actions             FALSE
#> 52         Partition protocol results by scope             FALSE
#> 54                            Interactive REPL             FALSE
#> 49         Build workflow context for protocol             FALSE
#> 48                  List active workflow packs             FALSE
```
