# Package index

## Project Lifecycle

- [`bg_handle()`](https://sims1253.github.io/bayesgrove/reference/bg_handle.md)
  : BayesGrove Project Handle
- [`bg_init()`](https://sims1253.github.io/bayesgrove/reference/bg_init.md)
  : Initialize a BayesGrove Project
- [`bg_open()`](https://sims1253.github.io/bayesgrove/reference/bg_open.md)
  : Open a BayesGrove Project
- [`bg_close()`](https://sims1253.github.io/bayesgrove/reference/bg_close.md)
  : Close a BayesGrove Project

## Graph Construction

- [`bg_add_node()`](https://sims1253.github.io/bayesgrove/reference/bg_add_node.md)
  : Add a node to the BayesGrove project graph
- [`bg_connect()`](https://sims1253.github.io/bayesgrove/reference/bg_connect.md)
  : Connect two nodes in the BayesGrove project graph
- [`bg_update_node()`](https://sims1253.github.io/bayesgrove/reference/bg_update_node.md)
  : Update a node in the BayesGrove project graph
- [`bg_remove_node()`](https://sims1253.github.io/bayesgrove/reference/bg_remove_node.md)
  : Remove a node from the BayesGrove project graph

## Execution & Orchestration

- [`bg_repl()`](https://sims1253.github.io/bayesgrove/reference/bg_repl.md)
  : Interactive REPL for BayesGrove
- [`bg_plan()`](https://sims1253.github.io/bayesgrove/reference/bg_plan.md)
  : Create an execution plan
- [`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md)
  : Run a project workflow
- [`bg_submit()`](https://sims1253.github.io/bayesgrove/reference/bg_submit.md)
  : Submit a project workflow for asynchronous execution
- [`bg_wait()`](https://sims1253.github.io/bayesgrove/reference/bg_wait.md)
  : Wait for asynchronous jobs to complete
- [`bg_cancel()`](https://sims1253.github.io/bayesgrove/reference/bg_cancel.md)
  : Cancel an asynchronous run
- [`bg_jobs()`](https://sims1253.github.io/bayesgrove/reference/bg_jobs.md)
  : Read current state of all jobs
- [`bg_reconcile_daemon_jobs()`](https://sims1253.github.io/bayesgrove/reference/bg_reconcile_daemon_jobs.md)
  : Reconcile background daemon jobs
- [`bg_compute_fingerprint()`](https://sims1253.github.io/bayesgrove/reference/bg_compute_fingerprint.md)
  : Compute the cache fingerprint for a node
- [`bg_status()`](https://sims1253.github.io/bayesgrove/reference/bg_status.md)
  : Get workflow status
- [`bg_result()`](https://sims1253.github.io/bayesgrove/reference/bg_result.md)
  : Retrieve a result artifact from the workflow
- [`bg_pause()`](https://sims1253.github.io/bayesgrove/reference/bg_pause.md)
  : Pause the workflow execution
- [`bg_resume()`](https://sims1253.github.io/bayesgrove/reference/bg_resume.md)
  : Resume the workflow execution
- [`bg_snapshot()`](https://sims1253.github.io/bayesgrove/reference/bg_snapshot.md)
  : Retrieve a complete snapshot of the project state
- [`bg_bundle()`](https://sims1253.github.io/bayesgrove/reference/bg_bundle.md)
  : Bundle a BayesGrove project for reproducible handoff
- [`bg_export_report()`](https://sims1253.github.io/bayesgrove/reference/bg_export_report.md)
  : Generate a reproducible markdown report of the workflow

## Workflow Protocol

- [`bg_list_templates()`](https://sims1253.github.io/bayesgrove/reference/bg_list_templates.md)
  : List built-in workflow templates
- [`bg_workflow_packs()`](https://sims1253.github.io/bayesgrove/reference/bg_workflow_packs.md)
  : List active workflow packs
- [`bg_workflow_context()`](https://sims1253.github.io/bayesgrove/reference/bg_workflow_context.md)
  : Build a workflow context for protocol evaluation
- [`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
  : Compute deterministic next workflow actions
- [`bg_partition_protocol_by_scope()`](https://sims1253.github.io/bayesgrove/reference/bg_partition_protocol_by_scope.md)
  : Partition protocol results by scope
- [`bg_build_workflow_context()`](https://sims1253.github.io/bayesgrove/reference/bg_build_workflow_context.md)
  : Build the workflow context for a given scope

## Workflow State

- [`bg_read_branch_registry()`](https://sims1253.github.io/bayesgrove/reference/bg_read_branch_registry.md)
  : Read the persisted branch registry
- [`bg_read_goal_registry()`](https://sims1253.github.io/bayesgrove/reference/bg_read_goal_registry.md)
  : Read the persisted goal registry
- [`bg_list_branches()`](https://sims1253.github.io/bayesgrove/reference/bg_list_branches.md)
  : List all branches with their metadata
- [`bg_branch_lineage()`](https://sims1253.github.io/bayesgrove/reference/bg_branch_lineage.md)
  : Get the ancestor branch lineage for a branch
- [`bg_resolve_node_scope()`](https://sims1253.github.io/bayesgrove/reference/bg_resolve_node_scope.md)
  : Resolve the workflow scope for a node
- [`bg_scope_label()`](https://sims1253.github.io/bayesgrove/reference/bg_scope_label.md)
  : Get a display label for a scope
- [`bg_set_goal()`](https://sims1253.github.io/bayesgrove/reference/bg_set_goal.md)
  : Set the active inferential goal for a branch
- [`bg_get_goal()`](https://sims1253.github.io/bayesgrove/reference/bg_get_goal.md)
  : Get the active inferential goal for a scope
- [`bg_read_summaries()`](https://sims1253.github.io/bayesgrove/reference/bg_read_summaries.md)
  : Read persisted summaries
- [`bg_write_summaries()`](https://sims1253.github.io/bayesgrove/reference/bg_write_summaries.md)
  : Persist executor summaries
- [`bg_summary_is_fresh()`](https://sims1253.github.io/bayesgrove/reference/bg_summary_is_fresh.md)
  : Determine whether a summary is fresh

## Branching & Invalidation

- [`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
  : Branch a node in the BayesGrove project graph
- [`bg_branch_with_continuation()`](https://sims1253.github.io/bayesgrove/reference/bg_branch_with_continuation.md)
  : Branch a node with downstream continuation
- [`bg_retire_node()`](https://sims1253.github.io/bayesgrove/reference/bg_retire_node.md)
  : Retire a node and downstream lineage
- [`bg_retire_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_retire_branch.md)
  : Retire an entire branch
- [`bg_invalidate()`](https://sims1253.github.io/bayesgrove/reference/bg_invalidate.md)
  : Invalidate a node's result

## Decisions & Gates

- [`bg_add_gate()`](https://sims1253.github.io/bayesgrove/reference/bg_add_gate.md)
  : Add a decision gate to an edge
- [`bg_answer_gate()`](https://sims1253.github.io/bayesgrove/reference/bg_answer_gate.md)
  : Answer a decision gate
- [`bg_pending_gates()`](https://sims1253.github.io/bayesgrove/reference/bg_pending_gates.md)
  : List pending decision gates
- [`bg_record_decision()`](https://sims1253.github.io/bayesgrove/reference/bg_record_decision.md)
  : Record an explicit decision

## Extensions

- [`bg_register_backend()`](https://sims1253.github.io/bayesgrove/reference/bg_register_backend.md)
  : Register a backend plugin
- [`bg_register_node_kind()`](https://sims1253.github.io/bayesgrove/reference/bg_register_node_kind.md)
  : Register a node kind in the BayesGrove runtime registry
- [`bg_cmdstanr_plugin()`](https://sims1253.github.io/bayesgrove/reference/bg_cmdstanr_plugin.md)
  : Baseline cmdstanr backend plugin
- [`bg_brms_plugin()`](https://sims1253.github.io/bayesgrove/reference/bg_brms_plugin.md)
  : brms backend plugin
- [`bg_diagnostics_plugin()`](https://sims1253.github.io/bayesgrove/reference/bg_diagnostics_plugin.md)
  : Baseline diagnostics plugin
