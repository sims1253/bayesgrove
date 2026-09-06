# Console reference for the review terms in vignettes/concepts.Rmd.

#' Concepts: what asks, what answers, what blocks
#'
#' Workflow packs request reviews based on diagnostic summaries and project
#' context. Blocking obligations hold downstream computation until resolved.
#' See `vignette("concepts")` for the review flow and glossary.
#'
#' | Concept        | Who produces it     | What it does |
#' |:---------------|:--------------------|:-------------|
#' | **Node**       | The user            | A computation and its dependencies. |
#' | **Summary**    | An executor (built-in or user-registered) | Typed evidence (HMC, ppc, loo_pit, sbc, ...). |
#' | **Obligation** | A workflow pack     | Work the analysis needs, such as reviewing divergences. |
#' | **Action**     | A workflow pack     | A suggested step, such as recording a decision or adding a check. |
#' | **Decision**   | The user            | The recorded answer (choice + rationale). |
#' | **Hold**       | The engine          | Prevents a node from running. |
#' | **Gate**       | The user            | A pre-planned checkpoint on an edge. |
#'
#' Use a gate when you know in advance that a step needs sign-off.
#' Workflow packs raise obligations as the analysis develops. A decision can
#' resolve a review; other obligations need new evidence. Advisory obligations
#' do not hold execution.
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
