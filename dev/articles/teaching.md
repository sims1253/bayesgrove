# Teaching with bayesgrove

Use bayesgrove’s decision log to review students’ modeling choices and
their reasons. Blocking obligations require students to record a review
before downstream computation can proceed.

## The assignment shape

Students submit two files:

1.  The exported report — `bg_export_report(handle, format = "html")` —
    the human-readable narrative with the graph diagram and decision
    provenance.
2.  The project bundle — `bg_bundle(handle)` — the machine-readable
    trail you verify against the report.

Open the bundle to compare its decision and summary logs with the
report:

``` r

utils::untar("student-submission.tar.gz", exdir = "check")
handle <- bg_open("check/project")

bg_status(handle) # blocked? then obligations were left unresolved
bg_next_actions(handle) # prints outstanding obligations as a checklist
bg_export_report(handle, format = "md") # regenerate and diff against the submitted report
```

## Grading checklist: obligations to rubric items

The workflow packs raise obligations; each maps to something gradeable.

| Obligation (pack) | Evidence behind it | Rubric item |
|:---|:---|:---|
| `review_computation_validity` | `hmc_diagnostics` summary with warning/error severity | Every sampler warning has a recorded `computation_review` decision whose rationale names the diagnostic (divergences, R-hat, ESS, E-BFMI). |
| Prior rationale (prior pack) | Prior/prior-predictive nodes | Every prior choice has a decision with a substantive rationale, not “default”. |
| Prior/posterior predictive review | `prior_predictive_check`, `posterior_predictive_check` summaries | PPC results interpreted, not just produced; extreme p-values acknowledged. |
| Calibration review (SBC pack) | `sbc_result`, `loo_pit_calibration` summaries | Calibration checked where the assignment requires it; miscalibration discussed. |
| `review_fit_criticism` | Branch-scoped diagnostic problems | Model revisions happen on branches with the criticism recorded, not by silently overwriting params. |
| `compare_candidate_branches` / `accept_or_reject_branch` | `comparison_results`, `stacking_weights` summaries | The final model was *chosen*: a comparison exists and a branch-disposition decision cites it. |

Two log-level checks catch the common shortcuts:

- **No orphaned warnings.** Every summary in `workflow/summaries.jsonl`
  with severity `warning`/`error` should be referenced (via its node or
  evidence field) by some decision in `decisions/decisions.jsonl`.
- **Rationales are text, not tokens.** Sort decisions by rationale
  length; one-word rationales cluster at the bottom and are easy to
  spot-check.

## Course setup tips

- Have students start from
  [`bg_init()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_init.md) +
  [`bg_use_cmdstanr()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_use_cmdstanr.md) +
  [`bg_use_default_workflow()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_use_default_workflow.md)
  so the baseline review pack is active from the first fit. Stricter
  packs (prior rationale, SBC review) can be introduced per assignment
  via
  [`bg_use_workflow_packs()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_use_workflow_packs.md).
- [`bg_add_gate()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_add_gate.md)
  lets *you* author checkpoints into a template project — e.g. a gate
  before the hierarchical model that asks “justify the pooling
  structure” — distinct from the evidence-driven obligations (see
  `?bayesgrove-concepts` for the gates-vs-obligations table).
- The REPL
  ([`bg_repl()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_repl.md))
  shows the obligation checklist interactively and `explain <n>` prints
  an obligation’s *why* together with its literature references — the
  teaching hook for “why does the software care about divergences?”.
- Plots are one call per node: `bg_plot(handle, node_id)` renders PPC
  density overlays, LOO-PIT ECDFs, SBC rank histograms, and trace plots
  for flagged fits.

## What this does not automate

The log records decisions, timestamps, and evidence references. Grading
a rationale — whether “heavier tails to absorb outliers” is justified
for the data at hand — remains the instructor’s job.
