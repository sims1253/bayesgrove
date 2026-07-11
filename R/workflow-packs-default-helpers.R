# Default Workflow Pack - Context Accessors
# -----------------------------------------
# Shared context, summary, decision, and comparison accessors used across
# the default Bayesian workflow pack.

#' @keywords internal
bg_default_bayesian_all_nodes <- function(context) {
  context$metadata$cross_scope_nodes %||%
    context$structural$nodes %||%
    list()
}

#' @keywords internal
bg_default_bayesian_all_edges <- function(context) {
  context$metadata$cross_scope_edges %||%
    context$structural$edges %||%
    list()
}

#' @keywords internal
bg_default_bayesian_all_summaries <- function(context) {
  context$metadata$cross_scope_summaries %||%
    context$evidence$summaries %||%
    list()
}

#' @keywords internal
bg_default_bayesian_all_decisions <- function(context) {
  context$metadata$cross_scope_decisions %||%
    context$evidence$decisions %||%
    list()
}

#' @keywords internal
bg_default_bayesian_fresh_summaries <- function(summaries) {
  Filter(
    function(summary) {
      isTRUE(summary$is_fresh) && !isTRUE(summary$is_stale)
    },
    summaries %||% list()
  )
}

#' @keywords internal
bg_default_bayesian_diagnostic_summary <- function(summary) {
  grepl(
    "diagnostics",
    summary$summary_kind %||% "",
    ignore.case = TRUE
  )
}

#' @keywords internal
bg_default_bayesian_fit_criticism_summary <- function(summary, node = NULL) {
  summary_kind <- summary$summary_kind %||% ""
  node_kind <- node$kind %||% ""

  if (identical(node_kind, "loo")) {
    return(FALSE)
  }

  bg_pack_is_fit_node(node) ||
    grepl("diagnostics|fit|criticism", summary_kind, ignore.case = TRUE) ||
    grepl("diagnostic|check", node_kind, ignore.case = TRUE)
}

#' @keywords internal
bg_default_bayesian_decision_summary_ids <- function(decision) {
  # Decisions read back from JSONL carry list-typed metadata fields
  # (simplifyVector = FALSE), so coerce before sorting.
  sort(unique(as.character(c(
    decision$metadata$summary_ids %||% character(),
    decision$metadata$summary_id %||% character()
  ))))
}

#' @keywords internal
bg_default_bayesian_scope_decisions <- function(decisions, scope, kind = NULL) {
  Filter(
    function(decision) {
      identical(decision$scope %||% NULL, scope) &&
        (is.null(kind) || identical(decision$kind %||% NULL, kind))
    },
    decisions %||% list()
  )
}

#' @keywords internal
bg_default_bayesian_current_decision_summary_ids <- function(
  decisions,
  scope,
  kind,
  fresh_summary_ids
) {
  current <- lapply(
    bg_default_bayesian_scope_decisions(decisions, scope = scope, kind = kind),
    function(decision) {
      decision_summary_ids <- bg_default_bayesian_decision_summary_ids(decision)
      if (
        length(decision_summary_ids) == 0 ||
          !all(decision_summary_ids %in% fresh_summary_ids)
      ) {
        return(character())
      }

      decision_summary_ids
    }
  )

  sort(unique(unlist(current, use.names = FALSE)))
}

#' @keywords internal
bg_default_bayesian_comparison_state <- function(context) {
  candidate_basis <- bg_default_bayesian_candidate_basis(
    bg_default_bayesian_clean_fit_candidates(context)
  )
  if (is.null(candidate_basis)) {
    return(NULL)
  }

  comparison_evidence <- bg_default_bayesian_comparison_evidence(
    context,
    candidate_basis
  )
  comparison_context <- bg_default_bayesian_comparison_context(
    candidate_basis,
    comparison_evidence
  )

  list(
    candidate_basis = candidate_basis,
    comparison_evidence = comparison_evidence,
    comparison_context = comparison_context
  )
}

#' @keywords internal
bg_default_bayesian_hold_node_ids <- function(comparison_evidence) {
  sort(unique(as.character(
    comparison_evidence$node_ids %||%
      comparison_evidence$node_id %||%
      character()
  )))
}
