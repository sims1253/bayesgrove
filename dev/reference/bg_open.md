# Open a bayesgrove Project

Opens an existing project. Opening a project never runs project-supplied
code: persisted node-kind executors are restored structurally only (the
kind name, contracts, and stored executor source kept as inert
metadata). To restore executable executors from the persisted source,
call
[`bg_restore_executors()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_restore_executors.md)
with `trust = TRUE` after opening, or re-register them with
[`bg_register_node_kind()`](https://sims1253.github.io/bayesgrove/dev/reference/bg_register_node_kind.md).

## Usage

``` r
bg_open(path = ".", readonly = FALSE, force = FALSE)
```

## Arguments

- path:

  Path to the project directory.

- readonly:

  Whether to open the project in read-only mode.

- force:

  Logical. If `TRUE` and the project is locked, steal the lock (use when
  the prior holder is known to be stale). Ignored when
  `readonly = TRUE`.

## Value

A `bg_handle`.

## Details

A non-readonly open acquires a single-writer lock
(`.bayesgrove/project.lock`) so two writers cannot mutate a project
concurrently. If the lock is already held, the holder's pid/host is
reported; pass `force = TRUE` to steal a stale lock. Readonly opens skip
locking and can run alongside a writer.
