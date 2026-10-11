# Rules, rulesets, and run-derived evidence

Status: accepted design for the 0.8 series. Extends the
[research-search contract](research-search.md).

bayesgrove codifies the Bayesian workflow (Gelman et al., 2020; the 2026
*Bayesian Workflow* book) using the model vocabulary of Bürkner, Scholz, and
Radev (2023). This document fixes three decisions so they can be built in
separate pull requests:

1. **Rule API.** Rules and rulesets form a public, data-first contract. The
   official Bayesian workflow is one ruleset written against that contract, not
   a privileged code path. Anyone can replace or extend it.
2. **Run-derived evidence.** A research candidate can be bound to an execution
   node. Evidence attached from a run carries P/A/D derived by the engine from
   the node, its inputs, and its artifacts, rather than asserted by the caller.
3. **The official ruleset.** `bayesgrove.workflow` declares the taxonomy's
   utilities and goal kinds as vocabulary, and goal-dependent requirements as
   rules, with references to the book and the paper.

Workflow packs keep their current behavior. They drive guided review of the
execution graph through R provider functions. The rule API governs research
operations. Once runs produce derived evidence, research rules also see real
diagnostics. Converting pack obligations into rules is future work and is not
part of this series.

Presence of evidence and human review still do not certify that a check
passed or that a model is adequate. Rules require records. They don't judge
science.

## 1. Rule API

### Rules

A rule is a JSON-compatible named list:

| field | required | meaning |
| --- | --- | --- |
| `id` | yes | Unique within its ruleset. `[a-z0-9_-]+`. `vocabulary` is reserved. |
| `on` | yes | Research actions: `create`, `revise`, `evidence`, `compare`, `review`, `accept`, `reject`, `note`, `close`, `reopen`. |
| `when` | no | List of conditions. The rule applies only if every condition is true. |
| `require` | yes | One requirement. |
| `severity` | no | `blocking` (default) or `advisory`. |
| `message` | yes | One-sentence statement of the unmet requirement. |
| `explanation`, `suggestion`, `interpretation` | no | Guidance strings. |
| `references` | no | Source strings, for example a book section. Never fetched. |

Conditions and requirements have the same shape, `list(type = <predicate id>,
...arguments)`, and use one predicate registry.

### Evaluation

A predicate is evaluated for one **subject** and has three possible outcomes:
`TRUE`, `FALSE`, or **failure**. Failure means the predicate is missing, its
version doesn't match the pinned one, it raised an error, or it returned
anything other than a length-one, non-missing logical.

For each rule and subject:

1. Every `when` condition is evaluated. Any failure produces a finding at the
   rule's severity, with the failure reason. Otherwise, if any condition is
   `FALSE`, the rule does not apply to this subject.
2. Otherwise `require` is evaluated. `FALSE` or failure produces a finding.

A failure can therefore never switch a rule off.

**Subjects.** The candidate under evaluation depends on the action:

| action | candidate |
| --- | --- |
| `create`, `revise` | The prospective candidate: merged proposed `components`, the prospective `binding`, `parent_id`, and **no `id`**. Evidence predicates therefore never see the parent's evidence. |
| `evidence`, `review`, `accept`, `reject`, `note`, `close`, `reopen` | The existing candidate record, before the action. |
| `compare` | Each selected candidate in turn. Applicability is decided per candidate, and the rule yields one finding if any applicable candidate fails. |

The subject that predicates receive is `list(state, action, candidate,
actor)`, where `state` is the research state before the action.

**Paths.** `path` arguments are relative to `candidate$components`, as they
are today. `c("P", "covariates")` reads `components$P$covariates`.

Built-in predicates:

| type | arguments | `TRUE` when |
| --- | --- | --- |
| `goal_kind` | `values` | `state$goal$kind` is one of `values`. |
| `actor_type` | `values` | The proposing actor's type is one of `values`. |
| `candidate_values` | `path`, `values` | The components field contains every value. |
| `require_values` | `path`, `values` | Same as `candidate_values` (existing name). |
| `forbid_values` | `path`, `values` | The components field contains none of the values. |
| `require_evidence` | `check`, optional `provenance` (`any` (default), `derived`) | The exact candidate has evidence with that check and provenance. |
| `require_utility_evidence` | `utility`, optional `provenance` | The exact candidate has evidence for that utility. |
| `require_review` | `check` | All evidence for that check has a human review under the current `goal_version`. Missing evidence is `FALSE`. |

Evidence and review semantics are unchanged from the research-search contract.

**Severity.** Guide reports every finding. Enforce holds an action only for
blocking findings. Legacy rules are blocking, so current behavior is
preserved. The research-search contract's "any unmet rule holds" sentence
becomes "any unmet blocking rule holds."

### Three rule validators

- **Legacy rules** (`id`, `actions`, `type`, `message`, plus `path`/`values` or
  `check`) keep today's validation, including any non-empty `id` string. They
  are evaluated as `on = actions` and `require = list(type, ...)`. Stored
  legacy rules are never rewritten.
- **Raw rules** follow the grammar above. They are validated by
  `bg_rule()`/`bg_ruleset()` and by `configure`.
- **Resolved rules** are produced by ruleset resolution. They have a qualified
  `id` (`<ruleset id>/<rule id>`) and a `source`.

Reading state validates every stored rule's structure (fields, severity,
actions, condition and requirement shapes). Only predicate availability is
deferred to evaluation.

### Custom predicates

```r
bg_register_rule_predicate(id, fn, arguments = character(), version = "1",
                           replace = FALSE, optional = character())
```

- `id` must contain a dot (`mylab.prereg_match`). Undotted ids are reserved
  for built-ins.
- `fn(subject, args)` is the predicate. See Evaluation for its outcomes.
- The registry is session-local, like executors. Re-registering an id
  requires `replace = TRUE`.
- No R code is persisted or restored. Persisted rules refer to a predicate by
  id, and the pinned policy records each custom predicate's `version` (see
  Pinning). A predicate registered with a different version fails at
  evaluation.
- Every name in `arguments` is required and must be non-`NULL`; `optional`
  lists arguments a condition may omit. `NULL` arguments are always invalid.
- `configure` and `bg_ruleset_check()` reject unknown predicate ids, unknown
  arguments, and missing required arguments. Rule constructors check
  arguments when the predicate is registered. State reads never consult the
  registry. At evaluation, a missing or `NULL` required argument is a
  failure, so it can never satisfy a rule.

**Portability.** Pinning makes the declarative rules identical everywhere.
A custom predicate behaves identically only where a compatible, trusted
implementation is registered. A host exposing this interface to agents must
control predicate registration as well as actor identity. Direct R access
can still re-register predicates, so this is a protocol, not a sandbox.

### Rulesets

A ruleset is a JSON-compatible named list:

| field | required | meaning |
| --- | --- | --- |
| `id` | yes | Dotted namespace, for example `bayesgrove.workflow`, `lab.prereg`. |
| `version` | yes | `MAJOR.MINOR.PATCH` without leading zeros, optionally `-prerelease`. |
| `title`, `description`, `references` | no | Documentation. |
| `extends` | no | References, resolved in order: `"<id>@<major>"` or `"<id>@<exact version>"`. |
| `disable` | no | Qualified rule ids to drop from the inherited rules. |
| `override` | no | Named by qualified rule id. A partial rule. |
| `vocabulary` | no | `utilities`, `goal_kinds`, `checks` (see below). |
| `rules` | no | The ruleset's own raw rules. |

**Version selection.** `id@MAJOR` selects the highest registered non-prerelease
version with that major. An exact version selects only that version.
Registering the same `id@version` again is a no-op if the content digest is
identical, and an error otherwise.

**Resolution** is recursive: `resolve(R)`

1. Resolves each `extends` entry, in order, and concatenates the results.
2. Coalesces duplicate qualified ids. Identical definitions from the same
   source collapse to one (diamond inheritance). Any other duplicate is an
   error.
3. Applies `R.disable`, then `R.override`. Targets must exist in the inherited
   rules. An override replaces each supplied field whole. Optional fields can
   be removed with `NULL`. `id` cannot be overridden, and required fields
   cannot be removed. Overridden rules keep `source` (the author) and gain
   `overridden_by` (a list of `<id>@<version>`).
4. Appends `R.rules`, qualified as `<R.id>/<rule id>`, with
   `source = "<R.id>@<R.version>"`.

Cycles and unknown parents are errors. Because `disable` and `override` are
applied inside each ruleset's own resolution, a parent's choices are settled
before a child sees the result.

**Vocabulary.** `utilities` and `goal_kinds` are lists of
`list(id, title, description, references)`. `checks` is a list of
`list(id, title, utilities)`. A check is usually a summary kind, and
`utilities` lists the utilities that check informs. The `utilities` mapping is
optional: a check without one does not constrain the evidence's `utility`.

- Vocabularies merge along the resolution order: later parents override
  earlier ones, and the child overrides all. An entry with the same id
  replaces the earlier one whole.
- An absent category is unconstrained. A present category must be non-empty.
- **Goal kinds** (when declared) are validated as hard errors in every mode.
  They apply to the goal given to `bg_research_init()`, the proposed goal of a
  `reframe`, and the current goal when `configure` installs a vocabulary. To
  switch between rulesets with disjoint goal kinds, configure a policy without
  that category, reframe, then configure again.
- **Evidence** is checked by the implicit rule `<ruleset id>/vocabulary`. It
  is advisory in Guide, blocking in Enforce, and skipped in Record. It covers:
  - an undeclared `utility` or `check`;
  - a declared check whose `utilities` mapping does not include the
    evidence's `utility`.
- If evidence omits `utility` and its check maps to exactly one utility, that
  utility is filled in.
- Records made before a vocabulary was installed are never revalidated.

**Registry.** Built-in rulesets ship as `inst/rulesets/<id>.json` and load
through the same validation as user rulesets. `bg_register_ruleset(ruleset)`
adds one to the session, and `bg_rulesets()` lists the available rulesets.

### Pinning and policy replacement

The research state holds one policy:

```text
mode
rules        resolved rules, or the legacy/raw rule list as supplied
ruleset      NULL, or list(id, version, title, digest, sources, policy_digest)
vocabulary   NULL, or the merged vocabulary
predicates   NULL, or list of list(id, version) for custom predicates in use
```

- `sources` lists every `id@version` with its content digest. Digests are
  SHA-256 over canonical JSON (named lists sorted recursively, `auto_unbox`,
  `digits = I(17)`), computed by one central helper.
- `policy_digest` covers the resolved `rules`, `vocabulary`, and `predicates`.
- `configure` replaces the whole policy, so absent fields become `NULL`, and
  does not change `goal_version`.
- The initialization and configure history entries store the requested input
  **and** the resolved policy. History alone can therefore reconstruct every
  policy that was in force.
- A proposal that installs a policy (`configure`) carries its resolved
  `policy_digest`. Apply resolves again, and a different digest is a stale
  proposal.
- Ordinary actions are evaluated against the stored policy and pinned
  predicate versions. They never consult the live ruleset registry.

### Functions

| function | purpose |
| --- | --- |
| `bg_rule(...)` | Validate and return one raw rule (plain list). |
| `bg_ruleset(id, version, rules, extends, disable, override, vocabulary, ...)` | Validate and return a ruleset (plain list). |
| `bg_ruleset_check(ruleset)` | Resolve and report `ok`, `errors`, `warnings`, and the resolved rule count. Warnings cover rules naming checks or utilities outside the vocabulary and rules that can never apply. This is the conformance kit for authors. |
| `bg_register_ruleset(ruleset)`, `bg_rulesets()` | Session registry plus built-ins. |
| `bg_register_rule_predicate(...)` | Custom predicate (above). |
| `bg_use_rules(project, rules, mode, actor, rationale)` | Proposes and applies `configure`. `rules` is a ruleset, a reference string, or a rule list. Human actor only, as `configure` requires today. |
| `bg_rules(project)` | The active policy: `mode`, `ruleset`, `rules`, `vocabulary`, `predicates`. This is what an agent reads before acting. |

`bg_research_init(rules = )` and the `configure` action's `rules` field accept
the same three forms. All new exports are experimental in `bg_api_boundary()`.

### State schema and compatibility

The research state's `schema_version` becomes `2`. Version 2 allows the new
rule shapes and the optional policy, `binding`, and `provenance` fields.

- New readers accept versions 1 and 2. Version 1 needs no content migration.
- Any successful apply writes version 2.
- Older bayesgrove versions reject version 2 as unsupported. Compatibility is
  one-way, and NEWS states this.

## 2. Run-derived evidence

Provenance is keyed on **executions and content-addressed artifacts**, not on
the current graph. The artifact index shares one artifact reference per
fingerprint across nodes. A fingerprint names a computation, not the realized
inputs it consumed (non-recursive invalidation can change upstream data under
an unchanged fingerprint). So the engine records, at execution time, what
each execution actually consumed and ran. Verification then compares
artifact content addresses.

### Engine-owned fields

`binding`, `provenance`, `pad`, digests, fingerprints, artifact references,
job ids, and summary ids are engine-owned. A public action that contains any
of them is rejected. The engine derives them:

- during `propose`, to report them and evaluate rules;
- again during `apply`, under the research lock.

If the apply-time derivation differs from the proposal's, the proposal is
stale. The transition stores exactly the derivation that apply checked.

### Execution records

Each successful execution (a job in `jobs.jsonl`, one per executed node)
records `provenance` in its final job record:

```text
provenance = list(
  artifact_ref,        # this execution's own output
  fingerprint,
  inputs,              # list(list(node_id, artifact_ref)), in the exact order
                       # passed to the executor
  consumed = list(D = <input node ids>, subject = <input node id or NULL>,
                  data_files = <list(path, sha256) for file-path data>),
  effective = list(P, A),  # executor-reported settings actually used (below)
  environment,             # manifest of the process that executed the node
  pad                      # P/A/D snapshot with digests (below)
)
```

Only a **succeeded** job carries provenance. Its `provenance$artifact_ref`
equals the job's `result_ref`.

- `inputs` is built from the same input bindings `bg_execute_node()` uses to
  assemble the executor's `inputs`. Execution, D, and subject resolution
  therefore share one ordering. Fingerprint edge ordering is not used.
- Summaries written by the execution store its `job_id`.
- Job compaction keeps each job's final record, so provenance survives
  compaction.
- Parallel execution records provenance in the main process, which is the
  single writer, like index binding. Workers return the executor's
  `metadata` (currently dropped in `bg_wave_worker()`) and the worker's
  environment manifest. Effective settings and environment describe the
  process that ran the fit.
- Summaries get `job_id` only on this engine path. The exported
  `bg_write_summaries()` rejects caller-supplied `job_id`s, so callers cannot
  forge execution-owned summaries.
- Executions from before this change have no `provenance` and cannot support
  bindings or derived evidence. `bg_run(project, rerun = <node ids>)` (new)
  executes the listed nodes even on a cache hit and binds their own output.
  This is the recovery path, including for a clone that reuses another node's
  cached artifact, which invalidation cannot reach. Semantics:
  - The listed nodes form a pending-rerun set for that call.
  - A pending node's cached output does not count as available, so its
    descendants are not ready until it has executed and been finalized.
  - A node leaves the set after successful finalization, so plan refreshes
    don't run it again.
  - Gates and holds apply as usual. A held node stays unexecuted and is
    reported.
  - Descendants are not re-executed implicitly, because their fingerprints are
    unchanged. List them too to refresh them. Until then, their recorded
    consumption truthfully names the earlier artifact.
- **Summary freshness** additionally requires a summary's `artifact_ref` to
  equal the node's active artifact. Without this, a same-fingerprint rerun
  would leave the earlier run's diagnostics marked fresh, also for other nodes
  sharing the index entry. Cached freshness flags are recomputed before
  wave-boundary hold evaluation. Stale summaries stay readable as history, and
  research evidence keeps referring to them by id.

**Executor reports.** An executor may return
`metadata$provenance = list(consumed, effective)`. Built-in executors do:

- **Fits** report `consumed$D`, the single data input they selected (using the
  same helper that selects the data). If that data is a file path, they also
  report `consumed$data_files`, the SHA-256 of the file contents at execution
  time. They also report `effective`, read back from the fitted object on the
  process that ran it:
  - cmdstanr `effective$A`: from `fit$metadata()`, the CmdStan version,
    algorithm (`fixed_param` mode is `algorithm == "fixed_param"`),
    `iter_warmup`, `iter_sampling`, `adapt_delta`, `max_treedepth`, and seed;
    chains from `fit$num_chains()`.
  - brms `effective$P`: `stats::family(fit)` and the applied priors from
    `brms::prior_summary(fit)`.
  - brms `effective$A`: `fit$backend`, `fit$algorithm`, `fit$version`, and
    the sampler settings. With the cmdstanr backend these come from
    `attr(fit$fit, "metadata")$metadata`; with rstan, from
    `fit$fit@sim`/`@stan_args`. A setting that isn't available or doesn't apply
    is recorded as such, not guessed.
- **brms executors** pass `backend` and `algorithm` explicitly, from node
  params or the session options resolved at execution time, so the recorded
  value is the one used.
- **Check executors** (`loo`, `ppc`, `loo_pit`) report `consumed$subject`, the
  input id of the fit they evaluated, chosen by the same code that selects it.
- An executor that reports nothing gets `consumed$D` = all inputs,
  `consumed$subject` = NULL, and `effective` = empty.

### P/A/D declarations and snapshots

A node kind can declare a classification of its parameters:

```r
bg_register_node_kind(..., pad = list(
  P = c("formula", "family", "priors"),
  A = c("chains", "iter_warmup", "iter_sampling", "adapt_delta",
        "max_treedepth", "seed", "backend", "algorithm"),
  locator = c("data_input"),
  other = c("thresholds")
))
```

The declaration is persisted and restored with the kind in the runtime
manifest. Classification is total: when `pad` is declared, every parameter on
an executed node must appear in exactly one of `P`, `A`, `D`, `locator`, or
`other`.

- `locator` is for parameters that select inputs, such as `data_input`. They
  are recorded but excluded from digests.
- `other` is for parameters that don't change the fitted model.

An unclassified parameter means the execution records no `pad`, so it cannot
be bound.

The execution's `pad` snapshot:

- **P**:
  - the kind and the P parameters;
  - `effective$P`;
  - for a `stan_file`, the hash of its source closure **contents** (the path
    is only a locator, so bundling and relocation keep the same identity).
- **A**: the executor reference or hash, the A parameters, `effective$A`, and
  the execution `environment`.
- **D**: the D parameters, plus one data identity per consumed input, in
  consumed order. That identity is the content hash from `data_files` when
  the input was a file path, and otherwise the consumed `artifact_ref`, the
  content address of the realized data. Producer node ids and locator
  parameters are recorded alongside for provenance but are **not** part of the
  digest, so identical data from different producer nodes has the same D.

Each component carries a `digest`: SHA-256 over canonical JSON of the
component's identity fields.

Built-in declarations:

| kind | P | A | D params | other |
| --- | --- | --- | --- | --- |
| `cmdstanr_fit` | `stan_file` | `chains`, `iter_warmup`, `iter_sampling`, `adapt_delta`, `max_treedepth`, `seed` | none (locator `data_input`) | `thresholds` |
| `prior_fit` | `stan_file` | `iter_warmup`, `iter_sampling`, `seed` | none (locator `data_input`) | none |
| `brms_fit` | `formula`, `family`, `priors` | `chains`, `iter_warmup`, `iter_sampling`, `adapt_delta`, `max_treedepth`, `seed`, `backend`, `algorithm` | none (locator `data_input`) | `thresholds` |
| `brms_prior_fit` | `formula` | `chains`, `iter_warmup`, `iter_sampling`, `seed`, `backend`, `algorithm` | none (locator `data_input`) | none |

Parameters an executor ignores are deliberately unclassified, so a node
carrying them records no `pad`. Settings an executor hard-codes, such as
`chains = 1` for `prior_fit`, appear in `effective`.

`bg_node_pad(project, node_id)` (exported, experimental) returns the
`provenance` of the execution that produced the node's current artifact (see
Resolution), or `NULL` if there is none.

### Resolution: from a node to an execution

To resolve a node:

1. Compute its predicted fingerprint with `bg_predicted_fingerprints()`.
2. Take the artifact the runner would use for that fingerprint and node
   (`bg_check_artifact()` semantics).
3. Find a producing execution: a succeeded job whose
   `provenance$artifact_ref` equals that artifact and whose fingerprint
   matches. The artifact must exist in the CAS with a valid digest.

Several executions can produce byte-identical output at the same fingerprint,
for example deterministic reruns. Resolution therefore picks a
**representative**: the most recent matching job in log order. It does not
claim to have found the unique historical execution. Identity is carried by
the output's content address. The representative may belong to another node
with the same fingerprint (cache reuse). That is truthful, because the
artifact the node resolves to *is* that output. A node with no fresh artifact, or whose artifact has no
recorded provenance, cannot be resolved. The error says to run it, or to
invalidate and re-run it.

### Binding a candidate

`create` and `revise` accept an optional `node_id`. The node is resolved
(above), and the execution must have a `pad`. The prospective candidate
stores:

```text
binding = list(node_id, job_id, artifact_ref, pad, research_version)
```

Transitions:

| action | parent | `node_id` | result |
| --- | --- | --- | --- |
| `create` | none | given | Bound. `components` defaults to the snapshot `pad` without digests. |
| `create` | none | absent | Unbound, as today. |
| `revise` | any | absent | **Unbound.** Bindings are never inherited, because a revision may describe a different model. |
| `revise` | unbound | given | Bound. `changed` comes from the `components` diff, as today, and may be empty. Adding a binding is itself the change. |
| `revise` | bound | given | Bound. `changed` is the union of components with differing snapshot digests and components whose description changed. An empty union is rejected, so an unchanged `bg_branch()` clone resolving to the same execution is not a revision. |

`components` remains the caller's description, and rule paths read it. The
binding is authoritative for provenance.

### Derived evidence

An `evidence` action uses exactly one of two forms:

- **Asserted:** `result` is given. This is today's behavior.
- **Derived:** `node_id` is given, and `result` must be absent.

For derived evidence:

1. The candidate is bound.
2. The node is resolved to its producing execution (above).
3. Eligibility is decided by content identity. Graph reachability and the
   current graph are not consulted.
   The two paths are tried in this order:
   - **Check** (first): if the representative execution reports
     `consumed$subject` and that input's consumed `artifact_ref` equals the
     binding's `artifact_ref`, the evidence comes from that execution. This
     holds even if the check returned its subject unchanged.
   - **Self** (otherwise): if the node's resolved artifact equals the
     binding's `artifact_ref`, the evidence is the bound fit's own
     diagnostics. The eligible jobs are the bound `job_id` and every other
     succeeded job with the same output and fingerprint, so a byte-identical
     rerun does not lock out a fit's own diagnostics.
   - Otherwise the action fails.
4. Summaries are matched by the engine-stamped `job_id` of the eligible jobs.
   An omitted `check` must resolve to exactly one summary kind across them.
   For the selected kind, the most recent matching summary in log order is
   used.
   `check` may be omitted when exactly one kind matches. `label` is required.
   `utility` follows the vocabulary rules.

The record stores:

```text
result     = list(summary_kind, severity, passed, metrics)
provenance = list(kind = "derived", node_id, job_id, artifact_ref,
                  subject_artifact_ref, summary_ids)
```

Evidence without `provenance` is asserted. The `provenance = "derived"`
argument of `require_evidence` and `require_utility_evidence` requires the
derived form.

A check that evaluated the bound fit's exact output is evidence about that
fit even if the graph has since changed. Re-running nodes never rewrites
evidence.

`sbc` (which runs its own model on simulated data) and `compare` (which
covers several models) report no subject and are not supported in this
series. Their provenance needs its own design.

**Availability limits.** These can make *new* derivation impossible, but they
never make existing dated evidence false:

- Bundles relocate Stan paths that are part of fingerprints. They also copy
  only indexed artifacts, so historical executions may not resolve from a
  restored project.
- There is no artifact garbage collection today. A future collector needs a
  retention policy for artifacts referenced by bindings and jobs.

## 3. The official ruleset

`inst/rulesets/bayesgrove.workflow.json`, id `bayesgrove.workflow`, version
`1.0.0`:

- **Vocabulary.**
  - The ten utilities from Bürkner, Scholz, and Radev (2023): causal
    consistency, parameter recoverability, predictive performance, fairness,
    structural faithfulness, parsimony, interpretability, convergence,
    estimation speed, robustness.
  - Goal kinds `observable_prediction` and `latent_inference`, matching the
    existing packs.
  - Checks that map the built-in summary kinds to the utilities they inform.
- **Rules.** The goal-dependent requirements of the paper's utility decision
  trees, expressed only with built-in predicates. Every rule carries
  `references` to the paper and to *Bayesian Workflow* (2026) chapters.
  Blocking rules cover only established practice: convergence evidence derived
  from the run before acceptance, and human review of computation diagnostics.
  Goal-specific expectations start as advisory.

A test loads the file through `bg_ruleset_check()` and asserts that it uses
no internal API. The official ruleset is held to the same standard as a
third-party ruleset.

## Pull requests

1. Rule API: evaluation semantics, predicates, rulesets, resolution, pinning,
   schema 2, and `bg_use_rules()`, `bg_rules()`, `bg_ruleset_check()`.
2. Run-derived evidence: execution provenance in job records, executor
   reports, `pad` declarations, `bg_node_pad()`, bindings, derived evidence.
3. Official ruleset `bayesgrove.workflow` with vocabulary, rules, and
   references.
4. New evidence for uncovered utilities: estimation speed from sampler timing,
   and robustness from prior sensitivity (priorsense, suggested).
5. Book-mapped guide: replay a book case study through the research interface
   with the official ruleset.

Each pull request updates the research-search vignette and contract where
behavior changes and adds NEWS entries.
