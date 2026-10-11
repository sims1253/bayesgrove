# Research-search contract

The experimental interface has four operations:

```text
bg_research_init    establish the question, goal, and policy
bg_research_state   read candidates, evidence, comparisons, decisions, and history
bg_research_propose validate an action and report applicable findings
bg_research_apply   revalidate a current proposal and atomically record it
```

Glade and agents call the same interface as R users. No web server, client-owned
copy of the research state, or agent-specific research operations are required.
The Glade restart review and its Positron prototype informed this choice: the
prototype already uses the researcher's R session and rejects stale submissions.
No open PRs were present in either repository when this implementation began.

## Scope and compatibility

This is an experimental search primitive alongside the existing graph-based
workflow. It does not reinterpret old branches as candidates, alter `bg_run()`,
or migrate existing decision records. Callers can attach evidence obtained with
the existing executors or with other tools. They must specify what P, A, and D
actually produced that evidence. Bayesgrove cannot verify those assertions by
reading an arbitrary Stan program or a caller-supplied result. A candidate can
instead be bound to an execution in the graph, and evidence can be derived from
that execution's recorded summaries (see Bindings and derived evidence below).

A candidate is data, not executable source. Store source contents, data versions,
and approximation settings explicitly when needed to reconstruct the candidate.
A path alone does not preserve a file. Large fit artifacts can remain in the
existing artifact store; a reference in a research result is a reference, not an
additional copy. Bundles preserve the research document, but existing rules for
excluding fits and external files still apply.

## Actions

Every proposal requires a declared actor (`id`, `type`) and a non-empty rationale.
The `action` is a named list. Unknown fields are errors. Every action may also
supply `basis`, a named list of `evidence_ids`, `comparison_ids`, or `decision_ids`
that motivated it. These must refer to existing records and remain in history.

| kind | Required fields besides kind | Meaning |
| --- | --- | --- |
| `create` | `label`, `components` | Create a root candidate. With optional `node_id`, bind it to the node's execution; `components` then defaults to that execution's P/A/D snapshot without digests. |
| `revise` | `candidate_id`, `label`, `components` | Replace the supplied P/A/D components in a descendant. Other components are inherited. With optional `node_id`, bind the revision to the node's execution; `components` may then be omitted. Without `node_id` the revision is unbound. |
| `evidence` | `candidate_id`, `label`, `check`, `utility`, and `result` or `node_id` | Attach a result to an exact candidate. With `result`, the evidence is asserted. With `node_id` instead, it is derived from a run for a bound candidate, and `check` may be omitted when exactly one summary kind matches. `utility` may be omitted when the vocabulary maps the check to exactly one utility. A check without a `utilities` mapping does not constrain `utility`. |
| `compare` | `candidate_ids`, `evidence_ids`, `criteria`, `result` | Record a comparison covering at least two candidates, including closed ones. |
| `review` | `candidate_id`, `evidence_ids` | Record review of specific evidence. |
| `accept`, `reject` | `candidate_id`, `evidence_ids` | Record a judgment for the investigation's goal. Rejection does not close a lineage. |
| `note` | `candidate_id` | Record reasoning without changing a candidate. |
| `close`, `reopen` | `candidate_id` | Close or reopen a lineage with the proposal's rationale. |
| `reframe` | `question`, `goal` | Record a corrected or changed question and goal. Requires a declared human actor. |
| `configure` | `mode`, `rules` | Replace the whole policy. `rules` is a ruleset, a reference string, or a rule list. Requires a declared human actor. |

Each successful operation returns the full state. The last history entry carries
the generated `id`; for create, revise, evidence, compare, and decisions, this is
also the new record's ID. A root requires P. A and D can be absent or explicitly
cleared in a revision. Components are replaced whole, not recursively merged.
A revision that changes nothing is rejected.

`binding`, `provenance`, `pad`, digests, fingerprints, artifact references, job
ids, and summary ids are engine-owned. An action that supplies any of them, at
the top level or inside a P/A/D component, is rejected.

Plain lists, strings, finite numbers, logical values, and NULL are supported.
Named fields must be unique. Arrays are lists on reading. Executable functions,
environments, classed objects, matrices, arrays, other attributes except names,
NA, and infinite values are rejected instead of
being silently converted by JSON. Convert results into an explicit record first.

Closed ancestors prevent evidence collection, revision, and acceptance beneath
them. Historical notes, reviews, rejections, and comparisons remain possible.
Reopening a descendant cannot bypass a closed ancestor. Review evidence must
belong to the candidate named by the decision; reviews do not transfer across
revisions. New evidence is not covered by earlier reviews. Reframing keeps previous questions
and goals in history. Each research record carries the `goal_version` it was
created under, referring to the initialization or reframe history entry. Human
reviews from a previous goal do not satisfy requirements under the new goal.

## Bindings and derived evidence

The full contract is section 2 of
[Rules, rulesets, and run-derived evidence](rules-and-evidence.md). A
`node_id` resolves to the execution that produced the node's current artifact,
which must have recorded a P/A/D snapshot (see `bg_node_pad()`). A bound
candidate stores `binding = list(node_id, job_id, artifact_ref, pad,
research_version)`, where `research_version` is the version its history entry
records.

| action | parent | `node_id` | result |
| --- | --- | --- | --- |
| `create` | none | given | Bound. `components` defaults to the snapshot without digests. |
| `create` | none | absent | Unbound. |
| `revise` | any | absent | Unbound. Bindings are never inherited. |
| `revise` | unbound | given | Bound. `changed` comes from the `components` diff and may be empty. |
| `revise` | bound | given | Bound. `changed` is the union of components whose snapshot digests differ and components whose description changed. An empty union is rejected, so an unchanged clone resolving to the same execution is not a revision. |

`components` remains the caller's description, and rule paths read it. The
binding is authoritative for provenance.

Derived evidence requires a bound candidate. The evidence node is resolved,
and its execution is eligible by content identity. A check whose execution
consumed the bound artifact as its subject is used first, even if it returned
the subject unchanged. Otherwise a node whose artifact is the bound artifact
contributes the bound execution's own summaries, together with those of every
succeeded execution with the same output and fingerprint. Anything else is
rejected, naming the P/A/D components in which the other execution differs
when that is known. Summaries are matched by the job id the engine stamped on
them, and the most recent summary of the selected kind is used. `sbc` and
`compare` nodes are not supported yet. The record stores
`result = list(summary_kind, severity, passed, metrics)` and
`provenance = list(kind = "derived", node_id, job_id, artifact_ref,
subject_artifact_ref, summary_ids)`. Evidence without `provenance` is asserted.
`require_evidence` and `require_utility_evidence` with
`provenance = "derived"` accept only derived evidence.

Propose derives bindings and evidence and reports them in the proposal's
`derivation`. Apply derives them again under the research lock. A different
derivation, for example after the node was re-run, makes the proposal stale,
and the transition stores exactly the derivation apply checked. Re-running
nodes never rewrites recorded bindings or evidence.

## Policy

Record preserves actions without generating policy findings. Guide returns
findings but allows actions. Enforce returns the same findings and prevents an
operation when any applicable blocking rule remains unmet. Advisory findings
never hold an action. Structural invariants, such as closed-lineage and
stale-proposal checks, apply in all modes.

The rule contract, including rule shape, predicates, evaluation outcomes,
rulesets, vocabularies, and pinning, is specified in
[Rules, rulesets, and run-derived evidence](rules-and-evidence.md), section 1.
In short, a rule has `id`, `on`, optional `when` conditions, one `require`
condition, `severity`, and `message`. Optional `explanation`, `suggestion`,
`interpretation`, and `references` provide guidance without a UI. References
are source strings; the engine does not fetch or verify them. A predicate that
is missing, has a different version than the pinned one, raises an error, or
returns anything other than `TRUE` or `FALSE` produces a finding in `when` as
well as in `require`, so a failure never switches a rule off.

Legacy rules (`id`, `actions`, `type`, `message`, plus `path`/`values` or
`check`) remain valid, are stored as supplied, and are always blocking:

| type | Additional fields | Requirement |
| --- | --- | --- |
| `require_values` | `path`, `values` | The candidate field contains every specified value. |
| `forbid_values` | `path`, `values` | The candidate field contains none of the specified values. |
| `require_evidence` | `check` | This candidate has evidence with that check name. |
| `require_review` | `check` | All recorded evidence for that check has human review. Missing evidence also leaves the requirement unmet. |

A path is a sequence such as `c("P", "covariates")`, relative to the
candidate's `components`. Rules for `create` and `revise` inspect the
prospective candidate, which has no ID. Evidence rules refer to exact
candidate IDs, so parent evidence cannot satisfy a requirement for a new
revision. Comparison rules decide applicability for each selected candidate.
Evidence presence does not mean a check passed, and review does not certify
scientific validity. There is no default model ranking, scalar reward, or
automatic acceptance threshold.

`bg_use_rules()` installs a ruleset, a reference such as `"lab.review@1"`, or a
rule list through `configure`, and `bg_rules()` returns the active policy.
Installing a ruleset pins its resolved rules, merged vocabulary, custom
predicate versions, and source digests in the state. Ordinary actions are
evaluated against that stored policy, never against the live registry. A
`configure` proposal carries the resolved `policy_digest`; apply resolves
again, and a different digest makes the proposal stale.

Mode and rule changes are historical operations. They do not resolve concerns
or rewrite earlier decisions. `allowed` and `findings` in a submitted proposal
are never trusted: apply recomputes them against the current state and policy.

Actors are caller-declared provenance. Human-review requirements and human-only policy changes or reframing are
protocol checks, not authentication. A host exposing this interface to untrusted
agents must bind identity and authorize policy changes itself. Direct R access
can also call legacy graph operations; this API is not a sandbox.

## Persistence and handoff

The state lives at `.bayesgrove/workflow/research.json` with
`schema_version` 2. Readers accept versions 1 and 2; any successful apply
writes version 2, which older bayesgrove versions reject. A write requires the
project writer lock, a current proposal version, and an atomic replacement under
a document lock. A failure leaves the prior research state intact. Replaying a
proposal after a successful write is stale. After an ambiguous response, inspect
the history before proposing anything again.

`bg_snapshot()` includes research state. Project bundles include it, and exported
reports contain candidate ancestry, bindings, derived evidence, operation
history, and the exact research records. Older projects return NULL research state and need no migration.

The vocabulary draws on the PAD taxonomy and goal-dependent utilities in
[Bürkner, Scholz, and Radev](https://arxiv.org/abs/2209.02439). The action and
storage contracts here are software design decisions, not claims that the paper
prescribes this implementation.
