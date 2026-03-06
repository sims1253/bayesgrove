# REPL VHS Demo

This demo records a meaningful REPL checkpoint instead of a blank project.

Story:

- a confounded simulation branch has already produced cached training/test data,
  a descriptive imbalance check, and two competing outcome models
- a decision gate blocks the final comparison until the user records an
  explicit rationale
- after the gate is answered, the comparison runs
- the user then branches the adjusted fit, and the REPL shows that the branch
  can reuse cached work until its specification changes

This matches the package's current strengths and the design notes in
[`design/system-design.md`](/home/m0hawk/Documents/bayesguide/design/system-design.md)
and
[`design/workflow-protocol.md`](/home/m0hawk/Documents/bayesguide/design/workflow-protocol.md):
resume from a checkpoint, make an explicit workflow decision, continue the DAG,
and branch for further iteration.

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
