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
#> 20                    bg_add_gate            stable
#> 7                     bg_add_node            stable
#> 21                 bg_answer_gate            stable
#> 69                bg_api_boundary            stable
#> 13                      bg_branch            stable
#> 15              bg_branch_lineage            stable
#> 14    bg_branch_with_continuation      experimental
#> 50                 bg_brms_plugin            stable
#> 54      bg_build_workflow_context      experimental
#> 47                      bg_bundle            stable
#> 30                      bg_cancel            stable
#> 59              bg_check_artifact internal_exported
#> 4                        bg_close            stable
#> 49             bg_cmdstanr_plugin            stable
#> 12                bg_commit_graph internal_exported
#> 45         bg_compute_fingerprint            stable
#> 8                      bg_connect            stable
#> 63                  bg_create_job internal_exported
#> 51          bg_diagnostics_plugin            stable
#> 25              bg_execute_action      experimental
#> 28                bg_execute_node            stable
#> 48               bg_export_report            stable
#> 24          bg_extension_registry      experimental
#> 61              bg_fetch_artifact internal_exported
#> 42                    bg_get_goal            stable
#> 1                       bg_handle            stable
#> 2                         bg_init            stable
#> 19                  bg_invalidate            stable
#> 35                        bg_jobs            stable
#> 16               bg_list_branches            stable
#> 57              bg_list_templates      experimental
#> 62                     bg_log_job internal_exported
#> 55                bg_next_actions      experimental
#> 3                         bg_open            stable
#> 56 bg_partition_protocol_by_scope      experimental
#> 31                       bg_pause            stable
#> 22               bg_pending_gates            stable
#> 26                        bg_plan            stable
#> 36        bg_read_branch_registry            stable
#> 37          bg_read_goal_registry            stable
#> 11                  bg_read_graph            stable
#> 38              bg_read_summaries            stable
#> 67       bg_reconcile_daemon_jobs internal_exported
#> 23             bg_record_decision            stable
#> 65            bg_register_backend internal_exported
#> 66          bg_register_node_kind internal_exported
#> 10                 bg_remove_node            stable
#> 58                        bg_repl      experimental
#> 43          bg_resolve_node_scope            stable
#> 34                      bg_result            stable
#> 32                      bg_resume            stable
#> 18               bg_retire_branch            stable
#> 17                 bg_retire_node            stable
#> 27                         bg_run            stable
#> 44                 bg_scope_label            stable
#> 70                       bg_serve      experimental
#> 41                    bg_set_goal            stable
#> 46                    bg_snapshot            stable
#> 33                      bg_status            stable
#> 60              bg_store_artifact internal_exported
#> 40            bg_summary_is_fresh            stable
#> 64                  bg_update_job internal_exported
#> 9                  bg_update_node            stable
#> 5         bg_use_default_workflow      experimental
#> 6           bg_use_workflow_packs      experimental
#> 29                        bg_wait            stable
#> 68                  bg_worker_log internal_exported
#> 53            bg_workflow_context      experimental
#> 52              bg_workflow_packs      experimental
#> 39             bg_write_summaries            stable
#>                                                              note
#> 20                                            Add a decision gate
#> 7                                         Add a node to the graph
#> 21                                         Answer a decision gate
#> 69                              Query API classification registry
#> 13                                    Create a branch from a node
#> 15                                    Get ancestor branch lineage
#> 14                    Branch with downstream continuation (newer)
#> 50                                            brms backend plugin
#> 54                           Build workflow context (lower-level)
#> 47                                     Bundle project for handoff
#> 30                                            Cancel an async run
#> 59                           Check artifact cache (worker-facing)
#> 4                                          Close a project handle
#> 49                                        cmdstanr backend plugin
#> 12                              Internal graph persistence helper
#> 45                                      Compute cache fingerprint
#> 8                                               Connect two nodes
#> 63                              Create job record (worker-facing)
#> 51                                             Diagnostics plugin
#> 25                    Execute a protocol action inside bayesgrove
#> 28                                          Execute a single node
#> 48                                         Export workflow report
#> 24                      Return the descriptive extension registry
#> 61                      Fetch artifact from cache (worker-facing)
#> 42                                                Get branch goal
#> 1                                    S7 class for project handles
#> 2                                        Initialize a new project
#> 19                                      Invalidate cached results
#> 35                                               List job records
#> 16                                              List all branches
#> 57                                        List built-in templates
#> 62                                  Log job entry (worker-facing)
#> 55                                  Compute next workflow actions
#> 3                                        Open an existing project
#> 56                            Partition protocol results by scope
#> 31                                       Pause workflow execution
#> 22                                             List pending gates
#> 26                                       Create an execution plan
#> 36                                           Read branch registry
#> 37                                             Read goal registry
#> 11                                         Read the project graph
#> 38                                       Read persisted summaries
#> 67                               Reconcile background daemon jobs
#> 23                                    Record an explicit decision
#> 65                                Register backend implementation
#> 66                                    Register node kind executor
#> 10                                   Remove a node from the graph
#> 58                                               Interactive REPL
#> 43                                    Resolve node workflow scope
#> 34                                         Retrieve a node result
#> 32                                      Resume workflow execution
#> 18                                  Retire a branch from planning
#> 17                                    Retire a node from planning
#> 27                                     Run workflow synchronously
#> 44                                    Get display label for scope
#> 70                      Serve the local experimental IPC protocol
#> 41                                    Set branch inferential goal
#> 46                                           Get project snapshot
#> 33                                            Get workflow status
#> 60                        Store artifact in cache (worker-facing)
#> 40                                        Check summary freshness
#> 64                              Update job record (worker-facing)
#> 9                                          Update node properties
#> 5     Activate the built-in starter workflow packs and node kinds
#> 6  Persist additional workflow-pack activations in project config
#> 29                                            Wait for async jobs
#> 68                                 Write worker debug log entries
#> 53                            Build workflow context for protocol
#> 52                                     List active workflow packs
#> 39                                       Write executor summaries
#>    remote_accessible
#> 20             FALSE
#> 7               TRUE
#> 21              TRUE
#> 69             FALSE
#> 13             FALSE
#> 15              TRUE
#> 14             FALSE
#> 50             FALSE
#> 54             FALSE
#> 47             FALSE
#> 30              TRUE
#> 59             FALSE
#> 4              FALSE
#> 49             FALSE
#> 12             FALSE
#> 45             FALSE
#> 8               TRUE
#> 63             FALSE
#> 51             FALSE
#> 25              TRUE
#> 28             FALSE
#> 48             FALSE
#> 24              TRUE
#> 61             FALSE
#> 42             FALSE
#> 1              FALSE
#> 2              FALSE
#> 19             FALSE
#> 35             FALSE
#> 16              TRUE
#> 57             FALSE
#> 62             FALSE
#> 55              TRUE
#> 3              FALSE
#> 56             FALSE
#> 31             FALSE
#> 22             FALSE
#> 26             FALSE
#> 36             FALSE
#> 37             FALSE
#> 11             FALSE
#> 38             FALSE
#> 67             FALSE
#> 23              TRUE
#> 65             FALSE
#> 66             FALSE
#> 10              TRUE
#> 58             FALSE
#> 43             FALSE
#> 34             FALSE
#> 32             FALSE
#> 18             FALSE
#> 17             FALSE
#> 27              TRUE
#> 44             FALSE
#> 70             FALSE
#> 41             FALSE
#> 46              TRUE
#> 33              TRUE
#> 60             FALSE
#> 40             FALSE
#> 64             FALSE
#> 9               TRUE
#> 5               TRUE
#> 6               TRUE
#> 29             FALSE
#> 68             FALSE
#> 53             FALSE
#> 52             FALSE
#> 39             FALSE

# Filter to a specific function
bg_api_boundary("bg_run")
#>        fn classification                       note remote_accessible
#> 27 bg_run         stable Run workflow synchronously              TRUE

# Filter to experimental functions
subset(bg_api_boundary(), classification == "experimental")
#>                                fn classification
#> 14    bg_branch_with_continuation   experimental
#> 54      bg_build_workflow_context   experimental
#> 25              bg_execute_action   experimental
#> 24          bg_extension_registry   experimental
#> 57              bg_list_templates   experimental
#> 55                bg_next_actions   experimental
#> 56 bg_partition_protocol_by_scope   experimental
#> 58                        bg_repl   experimental
#> 70                       bg_serve   experimental
#> 5         bg_use_default_workflow   experimental
#> 6           bg_use_workflow_packs   experimental
#> 53            bg_workflow_context   experimental
#> 52              bg_workflow_packs   experimental
#>                                                              note
#> 14                    Branch with downstream continuation (newer)
#> 54                           Build workflow context (lower-level)
#> 25                    Execute a protocol action inside bayesgrove
#> 24                      Return the descriptive extension registry
#> 57                                        List built-in templates
#> 55                                  Compute next workflow actions
#> 56                            Partition protocol results by scope
#> 58                                               Interactive REPL
#> 70                      Serve the local experimental IPC protocol
#> 5     Activate the built-in starter workflow packs and node kinds
#> 6  Persist additional workflow-pack activations in project config
#> 53                            Build workflow context for protocol
#> 52                                     List active workflow packs
#>    remote_accessible
#> 14             FALSE
#> 54             FALSE
#> 25              TRUE
#> 24              TRUE
#> 57             FALSE
#> 55              TRUE
#> 56             FALSE
#> 58             FALSE
#> 70             FALSE
#> 5               TRUE
#> 6               TRUE
#> 53             FALSE
#> 52             FALSE
```
