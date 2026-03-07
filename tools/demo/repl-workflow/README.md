# REPL VHS Demos

The repo now keeps several focused REPL demos instead of relying on one large
walkthrough for every use case.

Available tapes:

- `repl-remediation.tape`: starts at a warning-driven branch and shows the
  template-backed remediation loop
- `repl-comparison.tape`: starts at the comparison-ready checkpoint and focuses
  on `branch_comparison`
- `repl-disposition.tape`: starts after model comparison and focuses on the
  final branch disposition action
- `repl-workflow.tape`: the original full end-to-end walkthrough

Each focused tape uses the same launcher and checkpoints into a meaningful
state instead of replaying the full setup story every time.

This matches the package's current strengths and the design notes in
[`design/system-design.md`](/home/m0hawk/Documents/bayesguide/design/system-design.md)
and
[`design/workflow-protocol.md`](/home/m0hawk/Documents/bayesguide/design/workflow-protocol.md):
resume from a checkpoint, inspect protocol guidance, branch to resolve
diagnostics, execute template-backed comparison/review actions, compare clean
candidates, and explicitly accept the surviving branch.

Run the full launcher manually from the repository root:

```bash
R --quiet
source("tools/demo/repl-workflow/launch-demo.R")
```

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
vhs tools/demo/repl-workflow/repl-comparison.tape
vhs tools/demo/repl-workflow/repl-disposition.tape
vhs tools/demo/repl-workflow/repl-workflow.tape
```

Render every checked-in demo plus matching GIF previews in one shot:

```bash
bash tools/demo/repl-workflow/render-all-demos.sh
```

You can also pass tape basenames to render only a subset:

```bash
bash tools/demo/repl-workflow/render-all-demos.sh repl-comparison repl-disposition
```

The batch script now recompacts every MP4 after `vhs` renders it, using a
smaller default export profile (`960px`, `12fps`, `CRF 30`) before deriving the
GIF previews. You can tune that profile per run:

```bash
DEMO_WIDTH=840 DEMO_FPS=10 DEMO_CRF=32 bash tools/demo/repl-workflow/render-all-demos.sh
```
