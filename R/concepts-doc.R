# Help-topic version of the concepts vignette's core table. Keep the two in
# sync: vignettes/concepts.Rmd is the narrative, this topic is the quick
# console reference (`?bayesgrove-concepts`).

#' Concepts: what asks, what answers, what blocks
#'
#' bayesgrove's question surfaces — gates, obligations, and decisions —
#' overlap by design. This topic is the one-table disambiguation; the
#' `vignette("concepts")` article carries the full narrative and diagram.
#'
#' | Concept        | Who produces it     | What it does |
#' |:---------------|:--------------------|:-------------|
#' | **Node**       | The user            | Pairs code with a graph slot. |
#' | **Summary**    | An executor (built-in or user-registered) | Typed evidence (HMC, ppc, loo_pit, sbc, ...). |
#' | **Obligation** | A workflow pack     | A demand raised from a summary (severity: advisory or blocking). |
#' | **Action**     | A workflow pack     | A copy-pasteable call resolving the obligation. |
#' | **Decision**   | The user            | The recorded answer (choice + rationale). |
#' | **Hold**       | The engine          | Stops downstream execution until a blocking decision lands. |
#' | **Gate**       | The user            | A pre-planned checkpoint on an edge. |
#'
#' Obligations and gates both block, but they are not the same: gates are
#' *pre-planned* checkpoints the user authors on an edge ("review before
#' fitting the hierarchical model"); obligations are *evidence-driven* and
#' arise dynamically from summaries.
#'
#' @section Scopes:
#' Decisions and obligations carry a scope: `project` (whole project) or
#' `branch:<id>` (one branch). A decision recorded on a branch does not
#' satisfy a project-scope obligation, and vice versa. Use
#' [bg_next_actions()] to see what is outstanding at the relevant scope.
#'
#' @section Evidence freshness:
#' A summary is *fresh* when its fingerprint matches the node's current
#' configuration, *stale* when the node's params, upstream inputs, or
#' environment changed since it was written. Packs consume fresh summaries by
#' default; a stale summary produces no obligation until the node is rerun.
#'
#' @seealso `vignette("concepts")`, [bg_next_actions()], [bg_record_decision()],
#'   [bg_add_gate()], [bg_run()]
#' @name bayesgrove-concepts
NULL
