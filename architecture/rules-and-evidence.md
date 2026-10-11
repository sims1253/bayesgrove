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
   node. Evidence attached from a run carries P/A/D read from the node and its
   fingerprint, so it is derived rather than asserted.
3. **The official ruleset.** `bayesgrove.workflow` declares the taxonomy's
   utilities and goal kinds as vocabulary, and the goal-dependent requirements
   as rules, with references to the book and the paper.

Workflow packs keep their current behavior. They drive guided review of the
execution graph through R provider functions. The rule API governs research
operations. Once runs produce derived evidence, research rules also see real
diagnostics. Converting pack obligations into rules is future work and is not
part of this series.

## 1. Rule API

### Rules

A rule is a JSON-compatible named list:

| field | required | meaning |
| --- | --- | --- |
| `id` | yes | Unique within its ruleset. `[a-z0-9_-]+`. |
| `on` | yes | Research actions the rule applies to: `create`, `revise`, `evidence`, `compare`, `review`, `accept`, `reject`, `note`, `close`, `reopen`. |
| `when` | no | List of conditions. The rule applies only if every condition holds. |
| `require` | yes | One requirement. The rule produces a finding when it is not satisfied. |
| `severity` | no | `blocking` (default) or `advisory`. |
| `message` | yes | One-sentence statement of the unmet requirement. |
| `explanation`, `suggestion`, `interpretation` | no | Guidance strings. |
| `references` | no | Source strings, for example a book section. Never fetched. |

Conditions and requirements share one shape, `list(type = <predicate id>,
...arguments)`, and one predicate registry. A predicate answers `TRUE` or
`FALSE` for one subject: the state, the action, the candidate under
evaluation, and the actor. As today, `compare` evaluates every selected
candidate, and a rule fails if any candidate fails.

Built-in predicates:

| type | arguments | holds when |
| --- | --- | --- |
| `goal_kind` | `values` | `goal$kind` is one of `values`. |
| `actor_type` | `values` | The proposing actor's type is one of `values`. |
| `candidate_values` | `path`, `values` | The candidate field contains every value. |
| `require_values` | `path`, `values` | Same as `candidate_values` (existing name). |
| `forbid_values` | `path`, `values` | The candidate field contains none of the values. |
| `require_evidence` | `check`, optional `provenance` (`any`, `derived`) | The exact candidate has evidence with that check, derived from a run if requested. |
| `require_utility_evidence` | `utility`, optional `provenance` | The exact candidate has evidence for that utility. |
| `require_review` | `check` | All evidence for that check has a human review under the current goal. Missing evidence fails. |

Evidence and review semantics are unchanged from the research-search contract.
Exact candidate ids apply, reviews don't transfer, and reviews from an earlier
goal version don't count.

**Legacy shape.** The existing flat shape (`id`, `actions`, `type`, `message`,
plus `path`/`values` or `check`) remains valid input and valid stored state.
It is read as `on = actions` and `require = list(type, path, values | check)`.
Existing projects need no migration.

**Severity.** Guide reports every finding. Enforce holds an action only for
`blocking` findings. Legacy rules are blocking, so current behavior is
preserved.

### Custom predicates

```r
bg_register_rule_predicate(id, fn, arguments = character())
```

- `id` must contain a dot (`mylab.prereg_match`). Undotted ids are reserved
  for built-ins.
- `fn(subject, args)` returns a single `TRUE` or `FALSE`. `subject` is
  `list(state, action, candidate, actor)`.
- The registry is session-local, like executors. Persisted rules refer to a
  predicate by id only, and no R code is persisted or restored.
- **Validation.** `configure` and `bg_ruleset_check()` reject unknown
  predicate ids and unknown arguments. Reading state checks structure only, so
  a project opens on a machine without the predicate.
- **Fail closed.** A predicate that is missing at evaluation time, errors,
  or returns something other than a single logical makes its rule unmet, and
  the finding states the reason. In Enforce, a blocking rule then holds the
  action. Guide reports the problem.

### Rulesets

A ruleset is a JSON-compatible named list:

| field | required | meaning |
| --- | --- | --- |
| `id` | yes | Dotted namespace, for example `bayesgrove.workflow`, `lab.prereg`. |
| `version` | yes | Semantic version string. |
| `title`, `description`, `references` | no | Documentation. |
| `extends` | no | `"<id>@<major>"` references, resolved in order. |
| `disable` | no | Fully qualified rule ids (`<ruleset id>/<rule id>`) to drop. |
| `override` | no | Named by qualified rule id. A partial rule whose fields (except `id`) replace those of the inherited rule. |
| `vocabulary` | no | `utilities`, `goal_kinds`, `checks` (see below). |
| `rules` | no | The ruleset's own rules. |

**Resolution.** Resolution flattens the `extends` chain depth-first, applies
`disable`, then `override`, then appends the ruleset's own rules. Every
resolved rule gets a qualified `id` (`<ruleset id>/<rule id>`) and a `source`
(`<ruleset id>@<version>`). Duplicate qualified ids, cycles, unknown parents,
and dangling `disable`/`override` targets are errors. Vocabularies merge
along the chain, and a child entry with the same id replaces its parent's.

**Vocabulary.** `utilities` and `goal_kinds` are lists of
`list(id, title, description, references)`. `checks` is a list of
`list(id, title, utilities)`, which maps a check (usually a summary kind) to
the utilities it informs. When the active ruleset declares a vocabulary:

- an `evidence` action whose `utility` or `check` is not declared,
- an `init`/`reframe` whose `goal$kind` is not declared,

each produce a finding from the implicit rule `<ruleset id>/vocabulary`. It is
advisory in Guide and blocking in Enforce, and Record ignores it. If an
evidence action omits `utility` and its `check` maps to exactly one utility,
that utility is filled in.

**Registry.** Built-in rulesets ship as `inst/rulesets/<id>.json` and load
through the same validation as user rulesets. `bg_register_ruleset(ruleset)`
adds one to the session. `bg_rulesets()` lists the available rulesets.

**Pinning.** `configure` resolves the ruleset when it is applied and stores
the result in the research state:

```text
ruleset = list(id, version, title, sources = list(list(id, version, digest)))
rules   = <resolved, qualified rules>
vocabulary = <merged vocabulary>
```

The research document is self-contained. Bundles and other machines evaluate
the same rules without the registry. Upgrading a ruleset is a new `configure`
by a human, so it is recorded in history.

### Functions

| function | purpose |
| --- | --- |
| `bg_rule(...)` | Validate and return one rule (plain list). |
| `bg_ruleset(id, version, rules, extends, disable, override, vocabulary, ...)` | Validate and return a ruleset (plain list). |
| `bg_ruleset_check(ruleset)` | Resolve and report `ok`, `errors`, `warnings`, resolved rule count. Warnings cover rules naming checks or utilities outside the vocabulary and rules that can never apply. This is the conformance kit for authors. |
| `bg_register_ruleset(ruleset)`, `bg_rulesets()` | Session registry plus built-ins. |
| `bg_register_rule_predicate(id, fn, arguments)` | Custom predicate. |
| `bg_use_rules(project, rules, mode, actor, rationale)` | Proposes and applies `configure`. `rules` is a ruleset, `"<id>@<major>"`, or a rule list. Human actor only, as `configure` requires today. |
| `bg_rules(project)` | Active `mode`, `ruleset` metadata, resolved `rules`, and `vocabulary`. This is what an agent reads before acting. |

`bg_research_init(rules = )` and the `configure` action's `rules` field accept
the same three forms. All new exports are experimental in `bg_api_boundary()`.

### State schema

The research state's `schema_version` becomes `2`. Version 2 allows the new
rule shape and the optional `ruleset`, `vocabulary`, candidate `binding`, and
evidence `provenance` fields. The reader accepts versions 1 and 2. Any
successful apply writes version 2. Version 1 is a subset of version 2, so
nothing is rewritten except the version number.

## 2. Run-derived evidence

### P/A/D of a node

A node kind may declare which parameters belong to P, A, and D:

```r
bg_register_node_kind(..., pad = list(P = c("formula", "family", "priors"),
                                      A = c("chains", "iter_warmup", "iter_sampling",
                                            "adapt_delta", "max_treedepth", "seed"),
                                      D = c("data")))
```

The declaration is persisted in the runtime manifest with the kind.

`bg_node_pad(project, node_id)` (exported, experimental) derives:

- **P**: the kind, the P parameters, and the Stan source closure hash when a
  `stan_file` is present.
- **A**: the executor reference or hash, plus the A parameters.
- **D**: the D parameters, plus every upstream node's fingerprint keyed by
  input name.

Each component also carries a `digest`. Parameters not named in `pad` belong
to no component. They configure checks rather than the model.

Built-in declarations:

| kind | P | A | D |
| --- | --- | --- | --- |
| `cmdstanr_fit`, `prior_fit` | `stan_file` (+ source hash) | `chains`, `iter_warmup`, `iter_sampling`, `adapt_delta`, `max_treedepth`, `seed` | upstream inputs |
| `brms_fit`, `brms_prior_fit` | `formula`, `family`, `priors` | same sampler parameters | `data` + upstream inputs |

Check kinds (`loo`, `ppc`, `loo_pit`, `sbc`, `compare`) declare no `pad`. Their
evidence belongs to the fit they consume.

### Binding a candidate

`create` and `revise` accept an optional `node_id`. When it is present:

- The node must exist and declare `pad`.
- The candidate stores `binding = list(node_id, pad, bound_at_version)`, with
  the derived P/A/D at that moment.
- `components` becomes optional. When omitted, it defaults to the derived
  `pad` without digests. When supplied, it remains the caller's description,
  and the binding stays authoritative for provenance.
- For a bound `revise`, the "must change P, A, or D" check compares binding
  digests. `changed` lists the components whose digests differ.

### Derived evidence

An `evidence` action may name a `node_id` instead of supplying `result`:

- The candidate must be bound.
- The evidence node must be the bound node or a descendant of it.
- The bound node's current P/A/D digests must equal the candidate's binding.
  Otherwise the action fails with a message naming the changed component and
  suggesting a revision.
- The evidence node must have a current artifact. Its summaries become the
  `result`.
- `check` selects a summary kind and can be omitted when the node emitted one.
  `label` stays required. `utility` follows the vocabulary rule above.
- The record stores `provenance = list(kind = "derived", node_id, fingerprint,
  artifact, summary_kinds)`. Evidence without `provenance` is asserted.

`require_evidence(provenance = "derived")` and
`require_utility_evidence(provenance = "derived")` can then require that
diagnostics come from the actual run.

Derived evidence is a dated record. Re-running a node with a different
fingerprint does not rewrite it. The binding check prevents a new run from
being attributed to a candidate whose model has changed.

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
  - Checks that map built-in summary kinds to utilities.
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

1. Rule API: rules, predicates, rulesets, resolution, pinning, schema 2,
   `bg_use_rules()`/`bg_rules()`/`bg_ruleset_check()`.
2. Run-derived evidence: `pad` on node kinds, `bg_node_pad()`, bindings,
   derived evidence, built-in declarations.
3. Official ruleset `bayesgrove.workflow` with vocabulary, rules, and
   references.
4. New evidence for uncovered utilities: estimation speed from sampler timing,
   and robustness from prior sensitivity (priorsense, suggested).
5. Book-mapped guide: replay a book case study through the research interface
   with the official ruleset.

Each pull request updates the research-search vignette and contract where
behavior changes and adds NEWS entries.
