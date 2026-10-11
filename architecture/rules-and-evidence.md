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
                           replace = FALSE)
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
- `configure` and `bg_ruleset_check()` reject unknown predicate ids and
  unknown arguments.

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
| `version` | yes | `MAJOR.MINOR.PATCH`, optionally `-prerelease`. |
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
`utilities` lists the utilities that check informs.

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
  SHA-256 over canonical JSON (sorted keys, `auto_unbox`, `digits = I(17)`),
  the same serialization as fingerprints.
- `policy_digest` covers the resolved `rules`, `vocabulary`, and `predicates`.
- `configure` replaces the whole policy, so absent fields become `NULL`, and
  does not change `goal_version`.
- The initialization and configure history entries store the requested input
  **and** the resolved policy. History alone can therefore reconstruct every
  policy that was in force.
- A proposal carries the resolved `policy_digest`. Apply resolves again, and a
  different digest is a stale proposal.

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

### Engine-owned fields

`binding`, `provenance`, `pad`, digests, fingerprints, artifact references,
and summary ids are engine-owned. A public action that contains any of them
is rejected. The engine derives them:

- during `propose`, to report them and evaluate rules;
- again during `apply`, under the research lock.

If the apply-time derivation differs from the proposal's, the proposal is
stale. The caller reads the state and proposes again. The transition stores
exactly the derivation that apply checked.

### P/A/D declarations

A node kind can declare its P/A/D classification:

```r
bg_register_node_kind(..., pad = list(
  P = c("formula", "family", "priors"),
  A = c("chains", "iter_warmup", "iter_sampling", "adapt_delta",
        "max_treedepth", "seed"),
  D = c("data_input"),
  other = c("thresholds"),
  fixed = list(),                       # settings the executor hard-codes
  defaults = list(chains = 4, iter_warmup = 1000, iter_sampling = 1000,
                  adapt_delta = 0.8, max_treedepth = 10)
))
```

The declaration is persisted with the kind in the runtime manifest.

**Classification is total.** Every parameter present on a node must appear in
exactly one of `P`, `A`, `D`, or `other`. `other` is reserved for parameters
that don't change the fitted model, such as diagnostic thresholds. Binding a
node that has an unclassified parameter fails.

`bg_node_pad(project, node_id)` (exported, experimental) derives each
component. A component digest is SHA-256 over the canonical JSON of the
component without its digest field:

- **P**:
  - the kind;
  - the P parameters, with `defaults` applied;
  - for a `stan_file`, the hash of the source closure **contents** (the path
    is only a locator and is excluded from the digest, so bundling and
    relocation keep the same identity).
- **A**:
  - the executor reference, or a hash of the user executor;
  - the A parameters, with `defaults` applied;
  - `fixed`;
  - the environment manifest (R and backend versions), which `bg_plan()`
    already uses in fingerprints.
- **D**:
  - the D parameters;
  - for each upstream input in input order, its name and the content address
    (CAS reference) of the artifact currently bound to that upstream node.

  D therefore identifies the realized data consumed, not the computation that
  produced it, and environment changes don't alter D. Binding requires every
  upstream input to have a current, node-bound artifact, located through the
  same lookup as Derived evidence below. A missing one fails with a message to
  run the upstream node.

Built-in declarations:

| kind | P | A | D | other | fixed |
| --- | --- | --- | --- | --- | --- |
| `cmdstanr_fit` | `stan_file` | `chains`, `iter_warmup`, `iter_sampling`, `adapt_delta`, `max_treedepth`, `seed` | `data_input` | `thresholds` | none |
| `prior_fit` | `stan_file` | `iter_warmup`, `iter_sampling`, `seed` | `data_input` | none | `chains = 1`, `fixed_param = TRUE` |
| `brms_fit` | `formula`, `family`, `priors` | `chains`, `iter_warmup`, `iter_sampling`, `adapt_delta`, `max_treedepth`, `seed` | `data_input` | `thresholds` | none |
| `brms_prior_fit` | `formula` | `chains`, `iter_warmup`, `iter_sampling`, `seed` | `data_input` | none | `sample_prior = "only"` |

Each row's `defaults` list the executor's own fallbacks. A parameter the
executor ignores is not classified. If such a parameter is present on a node,
binding fails, so a binding never claims an influence that doesn't exist.

### Binding a candidate

`create` and `revise` accept an optional `node_id`. The node must exist and
its kind must declare `pad`. The prospective candidate stores:

```text
binding = list(node_id, pad, research_version, graph_version)
```

`pad` is the derived P/A/D with digests. The two versions record when the
binding was derived.

Transitions:

| action | parent | `node_id` | result |
| --- | --- | --- | --- |
| `create` | none | given | Bound. `components` defaults to the derived `pad` without digests. |
| `create` | none | absent | Unbound, as today. |
| `revise` | any | absent | **Unbound.** Bindings are never inherited, because a revision may describe a different model. |
| `revise` | unbound | given | Bound. `changed` comes from the `components` diff, as today, and may be empty. Adding a binding is itself the change. |
| `revise` | bound | given | Bound. `changed` is the union of components with differing digests and components whose description changed. An empty union is rejected, so an unchanged `bg_branch()` clone does not count as a revision. |

`components` remains the caller's description, and rule paths read it. The
binding is authoritative for provenance.

### Evidence subjects

A check node's evidence belongs to a fit only through the input that its
executor actually consumes. A check kind declares this with
`evidence_subject`:

| kind | `evidence_subject` |
| --- | --- |
| `cmdstanr_fit`, `prior_fit`, `brms_fit`, `brms_prior_fit` | `self` |
| `loo` | its first input, in dagriculture input order |
| `ppc` | the input named by `fit_input`, else the first input |
| `loo_pit` | the input named by `fit_input`, else the first input |

`sbc` (which runs its own model on simulated data) and `compare` (which
covers several models) are not supported in this series. Their provenance
needs its own design.

### Derived evidence

An `evidence` action uses exactly one of two forms:

- **Asserted:** `result` is given. This is today's behavior.
- **Derived:** `node_id` is given, and `result` must be absent.

For derived evidence, the engine checks:

1. The candidate is bound.
2. The node's kind declares `evidence_subject`, and the subject resolves to the
   bound node. Graph reachability alone is not sufficient.
3. The bound node's current P/A/D digests equal the binding's. Otherwise the
   action fails, names the changed component, and suggests a revision.
4. The evidence node has a current, own execution:
   - its predicted fingerprint (from `bg_predicted_fingerprints()`, which
     resolves upstream fingerprints topologically) has an **active binding for
     this node id** in the artifact index (cache reuse from another node
     doesn't count);
   - the bound artifact's CAS content exists and matches its digest;
   - for the selected `check` (a summary kind), the summaries have this node
     id, the predicted fingerprint, and an `artifact_ref` equal to the bound
     artifact.

   If several summaries match, the most recently created one is used.
5. `check` may be omitted when exactly one summary kind matches. `label` is
   required. `utility` follows the vocabulary rules.

The record stores:

```text
result     = list(summary_kind, severity, passed, metrics) from the summary
provenance = list(kind = "derived", node_id, subject_node_id,
                  fingerprint, artifact_ref, summary_ids)
```

Evidence without `provenance` is asserted. The `provenance = "derived"`
argument of `require_evidence` and `require_utility_evidence` requires the
derived form.

Derived evidence is a dated record. Re-running a node does not rewrite it.
The binding check in step 3 prevents a new run from being attributed to a
candidate whose model has changed.

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
2. Run-derived evidence: `pad` and `evidence_subject` on node kinds,
   `bg_node_pad()`, bindings, derived evidence, built-in declarations.
3. Official ruleset `bayesgrove.workflow` with vocabulary, rules, and
   references.
4. New evidence for uncovered utilities: estimation speed from sampler timing,
   and robustness from prior sensitivity (priorsense, suggested).
5. Book-mapped guide: replay a book case study through the research interface
   with the official ruleset.

Each pull request updates the research-search vignette and contract where
behavior changes and adds NEWS entries.
