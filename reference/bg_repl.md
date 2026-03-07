# Interactive REPL for BayesGrove

Interactive REPL for BayesGrove

## Usage

``` r
bg_repl(project, initial_scope = NULL)
```

## Arguments

- project:

  A `bg_handle`.

- initial_scope:

  Optional initial scope (e.g., "project" or "branch:xxx").

## Value

Invisibly returns `NULL` when the session exits.

## Details

`bg_repl()` provides an interactive loop for navigating workflow state,
reviewing protocol guidance, and applying common workflow actions
without manually calling the lower-level APIs.

The REPL is scope-aware. In project scope it can surface obligations and
actions across the whole workflow; in branch scope it focuses on the
selected branch and its branch-local graph view.

Supported commands include:

- `help`: show the command list

- `status`: print workflow state and health

- `guide`: show active obligations and suggested actions

- `actions`: list actionable protocol suggestions with payload details

- `do <n>`: execute the nth suggested action

- `scope` / `scope <scope>`: inspect or change the current scope

- `branches` and `use <n>`: inspect branches and switch by index

- `goal` / `goal set`: inspect or set a branch-scoped inferential goal

- `decisions`: show recent decisions for the current scope

- `nodes`: print the scoped execution graph

- `result <node>`: inspect a cached result and fresh summaries

- `branch <node>`: create a new branch from a node and switch to it

- `set <node> <key>=<value>`: update a node label or parameter

- `invalidate <node>`: invalidate a node and downstream cache lineage

- `retire <node>`: retire a node and its downstream lineage

- `retire-branch <branch>`: retire an entire branch from future planning

- `gates` / `answer`: inspect and answer pending structural gates

- `run` / `submit`: execute or enqueue ready work in the current scope

- `jobs` and `cancel <run_id>`: inspect or cancel background work

- `exit`, `quit`, `q`: leave the REPL

The function must be called from an interactive R session.
