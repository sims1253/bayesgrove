# Keep research search distinct from execution dependencies

A model lineage records changes to P, A, and D; an execution graph records which
computations need which inputs. We store research candidates and their evidence
separately so adding a check cannot change candidate ownership, and comparisons
can cross or revisit closed lineages. Existing execution APIs remain available;
the experimental research interface constrains its own operations, not arbitrary
R execution or the existing workflow-pack policies.

Each research operation atomically saves one state document containing both the
change and its history. This avoids partial updates across a candidate registry
and a decision log at the cost of rewriting the document on each operation.
Expected-version proposals prevent stale clients from applying a decision twice.
A database or event replay layer is deferred until measured workloads require it.
