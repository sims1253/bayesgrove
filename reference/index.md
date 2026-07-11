# Package index

## Concepts & API Boundary

- [`bayesgrove-concepts`](https://sims1253.github.io/bayesgrove/reference/bayesgrove-concepts.md)
  : Concepts: what asks, what answers, what blocks
- [`bg_api_boundary()`](https://sims1253.github.io/bayesgrove/reference/bg_api_boundary.md)
  : Query the bayesgrove API boundary

## Project Lifecycle

- [`bg_handle()`](https://sims1253.github.io/bayesgrove/reference/bg_handle.md)
  : bayesgrove Project Handle
- [`bg_init()`](https://sims1253.github.io/bayesgrove/reference/bg_init.md)
  : Initialize a bayesgrove Project
- [`bg_open()`](https://sims1253.github.io/bayesgrove/reference/bg_open.md)
  : Open a bayesgrove Project
- [`bg_close()`](https://sims1253.github.io/bayesgrove/reference/bg_close.md)
  : Close a bayesgrove Project

## Graph Construction

- [`bg_add_node()`](https://sims1253.github.io/bayesgrove/reference/bg_add_node.md)
  : Add a node to the bayesgrove project graph
- [`bg_connect()`](https://sims1253.github.io/bayesgrove/reference/bg_connect.md)
  : Connect two nodes in the bayesgrove project graph
- [`bg_update_node()`](https://sims1253.github.io/bayesgrove/reference/bg_update_node.md)
  : Update a node in the bayesgrove project graph
- [`bg_remove_node()`](https://sims1253.github.io/bayesgrove/reference/bg_remove_node.md)
  : Remove a node from the bayesgrove project graph
- [`bg_set_node_data()`](https://sims1253.github.io/bayesgrove/reference/bg_set_node_data.md)
  : Attach a data object to a node via the content-addressed store
- [`bg_read_graph()`](https://sims1253.github.io/bayesgrove/reference/bg_read_graph.md)
  : Read project graph
- [`bg_commit_graph()`](https://sims1253.github.io/bayesgrove/reference/bg_commit_graph.md)
  : Write project graph safely

## Execution & Orchestration

- [`bg_repl()`](https://sims1253.github.io/bayesgrove/reference/bg_repl.md)
  : Interactive REPL for bayesgrove
- [`bg_plan()`](https://sims1253.github.io/bayesgrove/reference/bg_plan.md)
  : Create an execution plan
- [`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md)
  : Run a project workflow
- [`bg_jobs()`](https://sims1253.github.io/bayesgrove/reference/bg_jobs.md)
  : Read current state of all jobs
- [`bg_compact_jobs()`](https://sims1253.github.io/bayesgrove/reference/bg_compact_jobs.md)
  : Compact the append-only jobs log
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
  : Bundle a bayesgrove project for reproducible handoff
- [`bg_export_report()`](https://sims1253.github.io/bayesgrove/reference/bg_export_report.md)
  : Generate a reproducible markdown report of the workflow

## Practitioner Shortcuts

- [`bg_fit_stan()`](https://sims1253.github.io/bayesgrove/reference/bg_fit_stan.md)
  : Fit a Stan model in one call
- [`bg_fit_brms()`](https://sims1253.github.io/bayesgrove/reference/bg_fit_brms.md)
  : Fit a brms model in one call

## Visualization

- [`bg_graph_mermaid()`](https://sims1253.github.io/bayesgrove/reference/bg_graph_mermaid.md)
  : Mermaid flowchart of the active project graph
- [`bg_plot()`](https://sims1253.github.io/bayesgrove/reference/bg_plot.md)
  : Plot a node's artifact or diagnostics

## Workflow Protocol

- [`bg_list_templates()`](https://sims1253.github.io/bayesgrove/reference/bg_list_templates.md)
  : List built-in workflow templates
- [`bg_use_default_workflow()`](https://sims1253.github.io/bayesgrove/reference/bg_use_default_workflow.md)
  : Activate the built-in starter workflow
- [`bg_use_workflow_packs()`](https://sims1253.github.io/bayesgrove/reference/bg_use_workflow_packs.md)
  : Activate workflow packs for a project
- [`bg_workflow_packs()`](https://sims1253.github.io/bayesgrove/reference/bg_workflow_packs.md)
  : List active workflow packs
- [`bg_next_actions()`](https://sims1253.github.io/bayesgrove/reference/bg_next_actions.md)
  : Compute deterministic next workflow actions
- [`bg_execute_action()`](https://sims1253.github.io/bayesgrove/reference/bg_execute_action.md)
  : Execute a workflow action from the protocol surface
- [`bg_extension_registry()`](https://sims1253.github.io/bayesgrove/reference/bg_extension_registry.md)
  : Return the descriptive extension registry for a project
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
- [`bg_summary_vocabulary()`](https://sims1253.github.io/bayesgrove/reference/bg_summary_vocabulary.md)
  : Built-in summary-kind vocabulary
- [`bg_register_summary_kind()`](https://sims1253.github.io/bayesgrove/reference/bg_register_summary_kind.md)
  : Register a custom summary kind

## Branching & Invalidation

- [`bg_branch()`](https://sims1253.github.io/bayesgrove/reference/bg_branch.md)
  : Branch a node in the bayesgrove project graph
- [`bg_branch_with_continuation()`](https://sims1253.github.io/bayesgrove/reference/bg_branch_with_continuation.md)
  : Branch a node with downstream continuation (deprecated)
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
- [`bg_read_decisions()`](https://sims1253.github.io/bayesgrove/reference/bg_read_decisions.md)
  : Read the project decision log

## Extensions

- [`bg_register_node_kind()`](https://sims1253.github.io/bayesgrove/reference/bg_register_node_kind.md)
  : Register a node kind in the bayesgrove runtime registry
- [`bg_restore_executors()`](https://sims1253.github.io/bayesgrove/reference/bg_restore_executors.md)
  : Restore user-supplied executors from persisted source.
- [`bg_use_cmdstanr()`](https://sims1253.github.io/bayesgrove/reference/bg_use_cmdstanr.md)
  : Register cmdstanr node kinds on a project.
- [`bg_use_brms()`](https://sims1253.github.io/bayesgrove/reference/bg_use_brms.md)
  : Register brms node kinds on a project.
- [`bg_hmc_severity()`](https://sims1253.github.io/bayesgrove/reference/bg_hmc_severity.md)
  : Compute HMC diagnostic severity from sampler metrics.
