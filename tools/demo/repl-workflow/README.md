# REPL VHS Demo

This demo records a meaningful REPL checkpoint instead of a blank project.

Story:

- a simple Bayesian workflow has already run through data preparation,
  compilation, and an initial centered fit
- the fit completed with warning diagnostics, so the workflow protocol holds
  downstream PPC work and surfaces a blocking review obligation
- the REPL guides the user to branch and modify the fit into a non-centered
  parametrization
- after rerunning the branch, the obligation clears and the workflow can
  continue from the healthier branch

This matches the package's current strengths and the design notes in
[`design/system-design.md`](/home/m0hawk/Documents/bayesguide/design/system-design.md)
and
[`design/workflow-protocol.md`](/home/m0hawk/Documents/bayesguide/design/workflow-protocol.md):
resume from a checkpoint, inspect protocol guidance, branch to resolve
diagnostics, and continue the DAG from the revised branch.

Run the launcher manually from the repository root:

```bash
R --quiet
source("tools/demo/repl-workflow/launch-demo.R")
```

Render the tape from the repository root:

```bash
vhs tools/demo/repl-workflow/repl-workflow.tape
```

The tape now renders an MP4 at
[`tools/demo/repl-workflow/repl-workflow.mp4`](/home/m0hawk/Documents/bayesguide/tools/demo/repl-workflow/repl-workflow.mp4)
and uses slower command typing so short REPL commands are easier to follow.
