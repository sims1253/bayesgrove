# bayesgrove contracts

The [function reference](https://sims1253.github.io/bayesgrove/reference/index.html)
documents public signatures and return values. `bg_api_boundary()` classifies
exports as stable, experimental, internal, or deprecated. The notes below
describe the implementation boundaries that those functions share.

## Graph values and project handles

[dagriculture owns graph structure and planning](dagriculture-boundary.md).
bayesgrove's editing functions modify a graph value and persist it through
`bg_commit_graph()`.

A `bg_handle` has reference semantics: its S7 properties use shared
environment-backed state. Assigning a handle to another variable does not
create an independent project session. See [the handle definition](../R/handle.R).

A writable project holds a directory lock until closed. Read-only handles
can inspect the project without acquiring the writer lock. A graph commit:

1. Rejects closed or read-only handles and non-increasing graph versions.
2. Acquires the graph file lock and compares the persisted version with the
   handle's loaded version.
3. Writes a temporary JSON file in the destination directory and renames it.
4. Updates the handle's loaded version after the write succeeds.

These checks apply to a graph commit; they do not make separate graph,
registry, and log writes a single crash-safe transaction.
See [project I/O](../R/project-io.R), [persistence helpers](../R/persist-io.R),
and [project tests](../tests/testthat/test-project.R).

## Cache identity and stored results

A fingerprint identifies a computation using its kind, parameters, upstream
fingerprints, executor, source, environment, and format version. An artifact
hash identifies the serialized result bytes. These are different keys:
the artifact index connects a computation to its result.

`bg_invalidate()` marks indexed results as superseded; recursive invalidation
also covers descendants. It does not delete the graph nodes.
See [fingerprinting](../R/fingerprint.R), [invalidation](../R/branch.R),
and [result storage](../R/run.R).

The index shares one artifact per fingerprint across nodes, and a
fingerprint names a computation rather than the inputs it realized. So each
succeeded job's final record carries `provenance`: its own `artifact_ref`
(equal to the job's `result_ref`), `fingerprint`, the `inputs` passed to the
executor in order, what it `consumed`, the `effective` settings read back
from the result, the `environment` of the process that ran it, and a `pad`
snapshot whose P/A/D components carry SHA-256 digests over canonical JSON.
The main process writes it on both the sequential and the parallel path.
File contents (a `stan_file` source closure and file-path inputs) are hashed
in the process that runs the executor, before it runs, and paths are
normalized there. File-backed data is supported only as a regular file
hashed before the run at the identical path: every consumed D input whose
value is a string must be one, every reported data file must name a
consumed D input at that path, and these files and the Stan source must not
change during execution. Any violation fails closed: the record keeps a
`capture_error` and no `pad`, so it cannot be bound. Inputs that were not
consumed are never checked. If capture fails outright, the succeeded job
carries a `capture_error` instead of `provenance`, distinguishing it from
executions recorded before provenance existed.
`bg_node_pad()` resolves a node to the most recent succeeded job that
produced its current artifact at its fingerprint. `bg_run(rerun = )`
re-executes cached nodes to record their own provenance.
See [execution provenance](../R/provenance.R) and
[architecture/rules-and-evidence.md](rules-and-evidence.md).

## Reviews and decisions

A gate specification refers to a graph edge. `bg_pending_gates()` joins it
with the current graph to return endpoint IDs and labels.
`bg_answer_gate()` requires a valid choice and rationale and records the
answer with its edge context.

Workflow packs derive obligations from summaries and project context.
Blocking obligations create execution holds. Decisions record choices,
rationales, scope, and evidence references; summaries carry fingerprints so
packs can distinguish current evidence from stale evidence. A summary is
fresh only while its fingerprint is the node's predicted fingerprint and its
`artifact_ref` is the artifact that fingerprint resolves to. Summaries
written by an execution carry its `job_id`; `bg_write_summaries()` rejects
caller-supplied ones.

See [decisions and gates](../R/decisions.R),
[summary freshness](../R/summary-freshness.R), and
[the workflow protocol](../R/workflow-protocol.R). The machine-readable
protocol schemas and fixtures live in [inst/protocol](../inst/protocol/).

## Executor trust

Opening a project does not run persisted executor source.
Activate built-in backends explicitly in a new project; their saved references
restore on reopen. Restoring saved user executor source requires
`bg_restore_executors(trust = TRUE)`. Node parameters are data.
See [project loading](../R/project.R) and
[the extension guide](../vignettes/extensions.Rmd).
