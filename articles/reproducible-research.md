# Reproducible research with bayesgrove

This article is for researchers who want to *report* a bayesgrove-backed
analysis: what to archive, what a reviewer can verify from the archive,
and how to phrase it in a methods section. The companion narrative is
[`vignette("concepts")`](https://sims1253.github.io/bayesgrove/articles/concepts.md);
the mechanics of a full analysis are in
[`vignette("eight-schools")`](https://sims1253.github.io/bayesgrove/articles/eight-schools.md).

## What the bundle contains

[`bg_bundle()`](https://sims1253.github.io/bayesgrove/reference/bg_bundle.md)
produces a single portable `.tar.gz` of the project:

``` r

library(bayesgrove)

handle <- bg_open("analysis/")
bundle <- bg_bundle(handle, path = "analysis-bundle.tar.gz")
```

Inside the archive, under `.bayesgrove/`, an independent reader finds:

| File | Contents |
|:---|:---|
| `graph/graph.json` | The full analysis graph: nodes, edges, params, gates. |
| `decisions/decisions.jsonl` | Every recorded decision: prompt, choice, rationale, scope, evidence, timestamps. |
| `workflow/summaries.jsonl` | Typed diagnostic evidence (HMC diagnostics, PPC p-values, LOO Pareto-k counts, LOO-PIT calibration, SBC ranks) with severities and execution fingerprints. |
| `runs/jobs.jsonl` | The execution history: which node ran when, with what outcome. |
| `cache/` | Content-addressed artifacts (large fit objects are excluded unless `include_fits = TRUE`). |
| `bundle_manifest.json` | A reproducibility manifest: R version, platform, package versions, CmdStan version, creation time. |

Two properties make this an audit trail rather than a snapshot:

1.  **Append-only logs.** Decisions and summaries are JSONL appends with
    monotonic sequence numbers; nothing is edited in place. The history
    of a revised prior is the sequence of decisions, not the final
    state.
2.  **Fingerprints.** Every summary carries the execution fingerprint of
    the node that produced it — a deterministic hash of the node’s
    params, its upstream fingerprints, the Stan program *contents*, and
    the environment. A reader can verify that the diagnostic evidence
    cited for a decision came from the exact configuration that was
    eventually reported.

## Restoring a bundle on another machine

Untar the bundle and open it:

``` r

utils::untar("analysis-bundle.tar.gz", exdir = "restored")
handle <- bg_open("restored/analysis")
```

[`bg_open()`](https://sims1253.github.io/bayesgrove/reference/bg_open.md)
compares the bundle’s reproducibility manifest against the current
session and warns when the R version, platform, or CmdStan version
differ. Cached artifacts stay usable either way — fingerprints, not the
manifest, decide what reruns — but the warning tells you up front that
bit-level reproduction may require matching the recorded environment.

Executors are **never** restored implicitly: opening a project runs no
project-supplied code. Built-in executors re-register with
[`bg_use_cmdstanr()`](https://sims1253.github.io/bayesgrove/reference/bg_use_cmdstanr.md)
/
[`bg_use_brms()`](https://sims1253.github.io/bayesgrove/reference/bg_use_brms.md);
user executors restore only through the explicit
`bg_restore_executors(trust = TRUE)` gate after you have reviewed their
persisted source.

## The machine-readable protocol trace

Both logs are plain JSONL and parse with any JSON reader:

``` r

decisions <- lapply(
  readLines("restored/analysis/.bayesgrove/decisions/decisions.jsonl"),
  jsonlite::fromJSON
)
summaries <- lapply(
  readLines("restored/analysis/.bayesgrove/workflow/summaries.jsonl"),
  jsonlite::fromJSON
)
```

This supports checks that a notebook-based workflow cannot: e.g. “every
`hmc_diagnostics` summary with severity `warning` or `error` is
referenced by at least one `computation_review` decision”. That is the
property bayesgrove *enforces* during the analysis (blocking obligations
hold downstream nodes), and the trace lets a reviewer confirm it after
the fact.

For a human-readable rendering,
[`bg_export_report()`](https://sims1253.github.io/bayesgrove/reference/bg_export_report.md)
produces a Markdown or HTML report with the graph (as a Mermaid
diagram), the full decision provenance, and the artifact index.

## SBC and simulation studies at scale

Calibration studies are hundreds of independent fits — exactly the shape
the wave scheduler parallelizes. Start `mirai` daemons and run; nodes
whose inputs are ready dispatch concurrently, and results are
byte-identical to a sequential run because artifacts are
content-addressed:

``` r

mirai::daemons(8)
bg_run(handle) # sbc / simulation nodes fan out across the daemons
mirai::daemons(0)
```

The built-in `sbc` node kind runs the Talts et al. (2018) rank-statistic
loop and emits an `sbc_result` summary (rank histogram plus a
chi-squared uniformity check) that the SBC review pack turns into an
obligation like any other piece of evidence. See
[`?bg_run`](https://sims1253.github.io/bayesgrove/reference/bg_run.md)
for the wave semantics and
[`vignette("simulation-study")`](https://sims1253.github.io/bayesgrove/articles/simulation-study.md)
for a full design.

## Reporting template

A “data and code availability” statement for a bayesgrove-backed
analysis:

> The analysis was managed with bayesgrove (Scholz, YEAR), which records
> the model graph, all diagnostic evidence, and all analyst decisions in
> machine-readable form. The complete project bundle — including the
> decision log with rationales, typed diagnostic summaries with
> execution fingerprints, and a reproducibility manifest (R X.Y.Z,
> CmdStan X.Y.Z) — is archived at DOI. Model fits can be regenerated
> from the bundle with
> [`bg_open()`](https://sims1253.github.io/bayesgrove/reference/bg_open.md)
> followed by
> [`bg_run()`](https://sims1253.github.io/bayesgrove/reference/bg_run.md);
> environment mismatches with the recorded manifest are surfaced on
> open.

Cite the package and the workflow paper it operationalizes with
`citation("bayesgrove")`.
