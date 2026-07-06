# API Contracts: `bayesgrove`

**Status:** Draft
**Date:** 2026-03-03

bayesgrove's own public API and canonical type specifications. The
`dagriculture` side of the boundary (graph value model, structural API,
errors, return shapes) lives in
[`../../dagriculture/design/api-contracts.md`](../../dagriculture/design/api-contracts.md);
this document covers only the bayesgrove layer.

## Canonical Public Types

### `bg_handle`

Fields:

- `project_id`: scalar string
- `path`: scalar string
- `readonly`: scalar logical
- `closed`: scalar logical
- `loaded_graph_version`: scalar integer
- `lock_token`: `NULL` or scalar string
- `registries`: named list
- `metadata`: named list

Rules:

- `bg_handle` is reference-semantic, not value-semantic
- callers should not assume copy-on-modify behavior for the live handle
- staged mutations operate on copied plain-data payloads; the backing
  environment swaps to the new payload only after a successful commit
- the live handle is updated only after a successful persisted commit
- a successful persisted commit means: version/lock checks passed and durable
  state was written via an atomic replace path
- failed compare-and-swap or write attempts leave the handle at its last
  committed state

### `bg_registry_binding`

Fields:

- `name`: scalar string
- `registry_id`: scalar string
- `expected_version`: scalar integer
- `compatibility`: `exact`, `compatible`, or `upgraded`
- `metadata`: named list

### `bg_project_snapshot`

Fields:

- `project_id`: scalar string
- `name`: scalar string
- `path`: scalar string
- `graph`: `dagri_graph`
- `registry_bindings`: named map of `bg_registry_binding`
- `decisions`: named map of `bg_decision_record`
- `gate_specs`: named map of `bg_pending_gate`
- `artifacts`: named list
- `jobs`: named map of `bg_job`
- `config`: named list
- `status`: `bg_status`

Rules:

- all named maps are hydrated from persisted JSON objects keyed by id

### `bg_pending_gate`

Fields:

- `id`: scalar string
- `edge_id`: scalar string
- `prompt`: scalar string
- `options`: character vector
- `refs`: list
- `created_at`: timestamp string
- `metadata`: named list

Rules:

- `edge_id` is canonical
- upstream and downstream node ids are resolved dynamically from the graph when
  needed
- UI helpers may expose denormalized edge endpoints, but they are not part of
  the persisted contract

### `bg_pending_gate_view`

Fields:

- `id`: scalar string
- `edge_id`: scalar string
- `from_node_id`: scalar string
- `to_node_id`: scalar string
- `from_label`: `NULL` or scalar string
- `to_label`: `NULL` or scalar string
- `prompt`: scalar string
- `options`: character vector
- `refs`: list
- `created_at`: timestamp string
- `metadata`: named list

Rules:

- this is a query-time denormalized view for analyst-facing tools
- `from_*` and `to_*` are derived from the current graph snapshot plus the
  underlying `bg_pending_gate`
- persisted gate specs remain keyed only by `id` and `edge_id`

### `bg_decision_record`

Fields:

- `decision_id`: scalar string
- `scope`: `project`, `node:<id>`, `edge:<id>`, or `gate:<id>`
- `kind`: `gate_answer` or `note`
- `prompt`: scalar string
- `choice`: scalar string
- `rationale`: `NULL` or scalar string
- `refs`: list
- `evidence`: character vector
- `status`: `active` or `superseded`
- `created_at`: timestamp string
- `supersedes`: `NULL` or scalar string
- `metadata`: named list

### `bg_job`

Fields:

- `job_id`: scalar string
- `run_id`: scalar string
- `node_id`: scalar string
- `status`: `queued`, `running`, `succeeded`, `failed`, `cancelled`, or `orphaned`
- `submitted_at`: timestamp string
- `started_at`: `NULL` or timestamp string
- `finished_at`: `NULL` or timestamp string
- `progress`: named list with `stage`, `percent` (`NULL` or 0-100 number), and
  optional `details`
- `result_ref`: `NULL` or scalar string
- `error`: `NULL` or named list
- `backend`: scalar string
- `metadata`: named list

### `bg_run_plan`

Fields:

- `graph_plan`: `dagri_plan`
- `targets`: character vector
- `eligible`: character vector
- `blocked`: named list mapping node id to reason
- `cache_hits`: character vector
- `missing_results`: character vector
- `to_execute`: character vector
- `input_bindings`: named list mapping node id to ordered input bindings
- `mode`: `sync` or `async`
- `metadata`: named list

Rules:

- `eligible` is the structurally eligible set from `dagri_plan`
- `cache_hits` and `missing_results` are `bayesgrove` overlays on top of that
  structural set
- `cache_hits` are determined by computing predicted intent-based fingerprints
  for eligible nodes and looking those fingerprints up in the artifact index
- a node may be structurally eligible and still omitted from `to_execute` when
  it is covered by `cache_hits`

### `bg_run_handle`

Fields:

- `run_id`: scalar string
- `status`: `queued`, `running`, `succeeded`, `failed`, `cancelled`, `blocked`, or `partial`
- `mode`: `sync` or `async`
- `targets`: character vector
- `job_ids`: character vector
- `submitted_at`: timestamp string
- `started_at`: `NULL` or timestamp string
- `finished_at`: `NULL` or timestamp string
- `summary`: named list
- `error`: `NULL` or named list
- `metadata`: named list

### `bg_status`

Fields:

- `workflow_state`: `idle`, `running`, `paused`, `blocked`, or `degraded`
- `active_jobs`: scalar integer
- `pending_gates`: scalar integer
- `runnable_nodes`: scalar integer
- `blocked_nodes`: scalar integer
- `last_run_id`: `NULL` or scalar string
- `health`: `ok`, `warning`, or `error`
- `messages`: character vector

### `bg_result_meta`

Fields:

- `node_id`: scalar string
- `fingerprint`: scalar string
- `artifact_ref`: scalar string
- `created_at`: timestamp string
- `kind`: scalar string
- `size_bytes`: `NULL` or scalar number
- `metadata`: named list

Rules:

- `fingerprint` is the intent-based cache key computed from normalized node
  intent plus upstream fingerprints, not a hash of the materialized artifact
  bytes

## `bayesgrove` Public API

### Project Lifecycle

```r
bg_init(path, project_name = NULL, config = list())
bg_open(path, readonly = FALSE)
bg_close(project)

bg_snapshot(project)
bg_config(project)
bg_set_config(project, config)
```

### Workflow Construction

```r
bg_add_node(project, kind, label, params = list(), inputs = NULL, metadata = list())
bg_connect(project, from, to, edge_type = "data", metadata = list())

bg_update_node(project, node_id, label = NULL, params = NULL, metadata = NULL)
bg_remove_node(project, node_id)

bg_branch(project, node_id, label = NULL, copy_params = TRUE)
bg_invalidate(project, node_id, recursive = TRUE)
```

`bg_invalidate(project, node_id, recursive = TRUE)` persistence effect:

- invalidation does not alter structural state in `dagriculture`
- it updates the `bg_artifact_index` by marking the currently active indexed
  result for the target node (and optionally downstream nodes when
  `recursive = TRUE`) as `status = "superseded"`
- once superseded, the next `bg_plan()` must treat that fingerprint as a
  `missing_results` candidate unless a newer active result already exists
- this is the explicit escape hatch for intentional recomputation even when the
  structural graph and deterministic fingerprints would otherwise allow reuse
- with intent-based caching, `recursive = TRUE` is the only consistency-
  preserving mode for normal workflow use because downstream fingerprints do not
  change when only an upstream artifact is manually superseded
- `recursive = FALSE` is therefore an explicit unsafe override; implementations
  may reject it in alpha, and if they allow it they should warn loudly that
  downstream cached results may remain mathematically stale until descendants
  are also invalidated or recomputed

### Gates And Decisions

```r
bg_add_gate(project, from, to, prompt, options, refs = NULL, metadata = list())
bg_answer_gate(project, id, choice, rationale = NULL, refs = NULL, evidence = NULL)
bg_reopen_gate(project, id, reason = NULL)
bg_pending_gates(project)

bg_record_decision(project, scope, prompt, choice, rationale = NULL, refs = NULL, evidence = NULL)
bg_decision(project, decision_id)
bg_decisions(project, scope = NULL, status = NULL)
```

`bg_pending_gates(project)` must return analyst-facing denormalized views with
resolved current edge endpoints and labels, not raw edge ids alone.

### Execution

```r
bg_plan(project, targets = NULL, mode = c("sync", "async"))

bg_run(project, targets = NULL, mode = c("sync", "async"))
bg_submit(project, targets = NULL, mode = c("async"))
bg_wait(project, run = NULL, jobs = NULL, timeout = NULL)
bg_cancel(project, run = NULL, jobs = NULL)

bg_status(project)
bg_jobs(project, status = NULL, detailed = FALSE)
bg_progress(project, jobs = NULL)
```

### Results And Handoff

```r
bg_result(project, node_id, version = c("latest", "stable"))
bg_result_meta(project, node_id)
bg_collect(project, node_ids)

bg_artifact(project, artifact_ref)
bg_artifacts(project, node_id = NULL)

bg_export_report(project, path = NULL, format = c("html", "md"))
bg_bundle(project, path = NULL, include_data = c("omit", "freeze_source", "copy"), include_fits = FALSE)
```

`bg_bundle(include_data = ...)` semantics:

- `omit`: do not include source data bytes in the bundle
- `freeze_source`: preserve source references, but pin them to a resolved
  immutable source binding or digest so the bundle captures exactly which input
  was used without copying the bytes
- `copy`: copy source data bytes into the bundle

### Maintenance

```r
bg_recover(project)
bg_health(project)
bg_gc(project, days = NULL)
```

`bg_gc()` responsibilities may include:

- removing unreachable artifacts per retention policy
- compacting append-only JSONL logs by dropping superseded entries
- rewriting derived indexes after safe compaction

## Behavioral Return Rules

- `bg_init()` and `bg_open()` return `bg_handle`.
- `bg_snapshot()` returns `bg_project_snapshot`.
- Mutating graph-construction functions return ids or typed records, not a new
  handle.
- the core alpha contract therefore favors imperative construction when callers
  need to capture generated ids
- alpha may also ship pipe-friendly convenience wrappers for interactive use,
  but those wrappers sit above and must not redefine the canonical low-level
  return contracts
- `bg_add_gate()` and `bg_reopen_gate()` return `bg_pending_gate`.
- `bg_answer_gate()` must resolve the structural gate in `dagriculture`, persist the
  semantic answer, and return `bg_decision_record`.
- `bg_pending_gates()` returns a named map of `bg_pending_gate_view` and must
  perform the graph join needed for analyst-facing endpoint context.
- `bg_record_decision()` returns `bg_decision_record`.
- `bg_plan()` returns `bg_run_plan`.
- `bg_run()`, `bg_submit()`, `bg_wait()`, and `bg_cancel()` return
  `bg_run_handle`.

## Typed Error Model

### `bayesgrove` Errors

- `bg_error_invalid_handle`
- `bg_error_project_invalid`
- `bg_error_project_conflict`
- `bg_error_registry_missing`
- `bg_error_backend_missing`
- `bg_error_unknown_scope`
- `bg_error_gate_invalid`
- `bg_error_no_result`
- `bg_error_job_invalid`
- `bg_error_execution_failed`
- `bg_error_export_failed`
- `bg_error_recovery_failed`

Notes:

- `bg_error_project_conflict` should include enough `details` to retry or
  inspect the failed write attempt, such as the expected persisted version and
  the staged mutation payload or proposed graph snapshot.

### Error Payload Contract

Every public typed error should expose:

- `class`
- `message`
- `code`
- `details`

When relevant, `details` should include ids or paths that localize the failure.

## Canonical Public Return Shapes

### `bg_pending_gate`

```r
list(
  id = "gate_prior_review",
  edge_id = "edge_data_fit",
  prompt = "Proceed with the baseline weakly informative prior?",
  options = c("yes", "no"),
  refs = list(),
  created_at = "2026-03-03T14:43:00Z",
  metadata = list()
)
```

### `bg_run_plan`

```r
list(
  graph_plan = list(
    targets = c("node_fit", "node_diag"),
    topo_order = c("node_data", "node_fit", "node_diag"),
    eligible = c("node_fit"),
    blocked = list(),
    external_blocked = list(),
    terminal = c("node_diag"),
    pending_gates = character()
  ),
  targets = c("node_fit", "node_diag"),
  eligible = c("node_fit"),
  blocked = list(),
  cache_hits = character(),
  missing_results = c("node_fit"),
  to_execute = c("node_fit"),
  input_bindings = list(
    node_fit = list(
      list(
        edge_id = "edge_data_fit",
        from_node_id = "node_data",
        edge_type = "data",
        artifact_ref = "cas:sha256:1111222233334444"
      )
    )
  ),
  mode = "async",
  metadata = list()
)
```

### `bg_run_handle`

```r
list(
  run_id = "run_01JNB5R4J5ATW0VVBEM6D3J8TW",
  status = "running",
  mode = "async",
  targets = c("node_fit", "node_diag"),
  job_ids = c("job_01JNB5T49W3QDS0X1EQFQYKBMQ"),
  submitted_at = "2026-03-03T14:45:00Z",
  started_at = "2026-03-03T14:45:01Z",
  finished_at = NULL,
  summary = list(total_jobs = 1L, completed_jobs = 0L),
  error = NULL,
  metadata = list()
)
```
