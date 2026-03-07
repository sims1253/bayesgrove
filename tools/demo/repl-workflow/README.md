# REPL VHS Demo

This demo records a meaningful REPL checkpoint instead of a blank project.

Story:

- a simple Bayesian workflow has already run through data preparation,
  compilation, and a clean baseline fit
- a centered branch was then run and completed with warning diagnostics, so the
  workflow protocol surfaces blocking computation-review and fit-criticism
  obligations for that branch
- the REPL guides the user to branch and modify the problematic fit into a
  non-centered revision and retire the stale warning branch
- once two clean fits remain, the demo continues into comparison creation,
  explicit `model_comparison`, and explicit branch acceptance

This matches the package's current strengths and the design notes in
[`design/system-design.md`](/home/m0hawk/Documents/bayesguide/design/system-design.md)
and
[`design/workflow-protocol.md`](/home/m0hawk/Documents/bayesguide/design/workflow-protocol.md):
resume from a checkpoint, inspect protocol guidance, branch to resolve
diagnostics, compare clean candidates, and explicitly accept the surviving
branch.

Run the launcher manually from the repository root:

```bash
R --quiet
source("tools/demo/repl-workflow/launch-demo.R")
```

Render the tape from the repository root:

```bash
vhs tools/demo/repl-workflow/repl-workflow.tape
```

When `vhs` runs successfully, it writes an MP4 at
[`tools/demo/repl-workflow/repl-workflow.mp4`](/home/m0hawk/Documents/bayesguide/tools/demo/repl-workflow/repl-workflow.mp4)
and uses slower command typing so short REPL commands are easier to follow.
