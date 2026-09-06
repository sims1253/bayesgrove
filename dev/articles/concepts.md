# Concepts

## The review loop

A node runs an executor, which returns a result and may emit diagnostic
summaries. Workflow packs read those summaries and request reviews.

``` text
Diagnostic summary
  └─ Workflow pack raises a blocking obligation
       ├─ Hold: downstream nodes cannot run
       └─ Suggested action: review, revise, or collect more evidence
            └─ Obligation resolved: the hold lifts
```

A recorded decision can resolve a review. Other obligations need new
evidence, such as a model comparison. Advisory obligations do not hold
execution.

| Term | Meaning |
|----|----|
| Node | A computation and its dependencies in the graph. |
| Executor | The function called with `(node, inputs)` to run a node. |
| Artifact | A saved result, stored by its content hash. |
| Summary | Diagnostic evidence returned by an executor. |
| Workflow pack | Rules that use project context and summaries to request reviews or other work. |
| Obligation | Work the analysis needs, such as reviewing divergences. |
| Action | A suggested step, such as recording a decision or adding a check. |
| Decision | A recorded choice and its rationale. |
| Hold | A restriction that prevents a node from running. |
| Gate | A checkpoint you place on an edge and answer with a decision. |
| Fingerprint | A hash of a node’s configuration, inputs, executor, source, and environment. |
| Wave | Nodes ready to run together because their inputs are available. |

Use a gate when you know in advance that a step needs sign-off. Workflow
packs raise obligations as the analysis develops. Both can block
execution.

## Scopes

Decisions and obligations carry a scope:

- `project`: applies to the whole project.
- `branch:<id>`: applies to a branch.

A decision recorded on a branch does not satisfy a project-scope
obligation, and vice versa. Use `bg_next_actions(handle)` to see what is
outstanding at the relevant scope.

## Evidence freshness

A summary is *fresh* when its fingerprint matches the node’s current
configuration; it is *stale* when the node’s params, upstream inputs, or
environment changed since the summary was written. Packs consume fresh
summaries by default; a stale summary produces no obligation until the
node is rerun.

See the [getting-started
tutorial](https://sims1253.github.io/bayesgrove/dev/articles/getting-started.md)
for a complete review and `?bayesgrove-concepts` for the console
reference.
