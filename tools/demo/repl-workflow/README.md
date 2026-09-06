# REPL VHS demos

This directory contains REPL recordings for individual workflow steps and a
full walkthrough.

Available tapes:

- `repl-showcase.tape`: the shortest clip; review a warning, repair the model,
  and rerun it so the candidates are ready for comparison
- `repl-remediation.tape`: starts with a branch that has a diagnostic warning
  and uses templates to repair it
- `repl-comparison.tape`: starts with candidates ready for comparison and
  demonstrates `branch_comparison`
- `repl-disposition.tape`: starts after model comparison and records the final
  branch disposition
- `repl-workflow.tape`: the full walkthrough

Each recording uses the same launcher. The shorter recordings start from a
checkpoint.

If you want a single short asset, start with `repl-showcase.mp4`.

The demos use the REPL and the `bayesgrove.default_bayesian` workflow pack.
Using one pack keeps the recordings short and deterministic. For prior-workflow,
model-check, model-selection, causal, and PAD examples, see
`vignette("extensions", package = "bayesgrove")`.

The workflow starts from a checkpoint. You inspect the guidance, branch to
resolve diagnostic problems, use templates for comparisons and reviews, compare
candidates that pass the checks, and accept a branch.

Run the full launcher manually from the repository root:

```bash
R --quiet
source("tools/demo/repl-workflow/launch-demo.R")
```

Once the REPL opens, use these commands:

- `dashboard` to refresh the full status, obligations, holds, and decisions view
- `next` to preview the top recommended action before execution
- `lineage` to inspect the current branch and its ancestors
- `export md workflow_report.md` to write a markdown report from the same session

Run a focused checkpoint manually:

```bash
R --quiet
options(bg_demo_autostart = FALSE)
source("tools/demo/repl-workflow/launch-demo.R")
demo_repl_workflow(checkpoint = "comparison_ready", start_repl = TRUE)
```

Render any tape from the repository root:

```bash
vhs tools/demo/repl-workflow/repl-remediation.tape
vhs tools/demo/repl-workflow/repl-showcase.tape
vhs tools/demo/repl-workflow/repl-comparison.tape
vhs tools/demo/repl-workflow/repl-disposition.tape
vhs tools/demo/repl-workflow/repl-workflow.tape
```

Render all checked-in demos and matching GIF previews:

```bash
bash tools/demo/repl-workflow/render-all-demos.sh
```

You can also pass tape basenames to render only a subset:

```bash
bash tools/demo/repl-workflow/render-all-demos.sh repl-comparison repl-disposition
```

The batch script recompacts every MP4 after `vhs` renders it, using a
smaller default export profile (`960px`, `12fps`, `CRF 30`) before deriving the
GIF previews. You can tune that profile per run:

```bash
DEMO_WIDTH=840 DEMO_FPS=10 DEMO_CRF=32 bash tools/demo/repl-workflow/render-all-demos.sh
```
