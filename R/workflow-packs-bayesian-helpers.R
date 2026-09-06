# Bayesian Semantics Workflow Pack - Helper Functions
# ---------------------------------------------------
# Context helpers, summary accessors, decision helpers, and review/goal
# utilities shared across the Bayesian workflow packs.

# --- Context Helpers ---

bg_pack_sort_ids <- function(x) {
  sort(unique(as.character(x %||% character())))
}

bg_pack_fit_nodes <- function(context) {
  Filter(
    bg_pack_is_fit_node,
    context$structural$nodes %||% list()
  )
}

bg_pack_fit_node_ids <- function(context) {
  bg_pack_sort_ids(names(bg_pack_fit_nodes(context)))
}

bg_pack_node_label <- function(context, node_id) {
  node <- (context$structural$nodes %||% list())[[node_id]] %||% list()
  node$label %||% node_id
}

bg_pack_source_node_id <- function(context, node_ids = NULL) {
  node_ids <- if (is.null(node_ids)) {
    bg_pack_sort_ids(bg_pack_fit_node_ids(context))
  } else {
    bg_pack_sort_ids(node_ids)
  }
  if (length(node_ids) == 0) {
    return(NULL)
  }

  branch_root <- context$scope_context$branch_root %||% NULL
  if (!is.null(branch_root) && branch_root %in% node_ids) {
    return(branch_root)
  }

  node_ids[[1]]
}

# --- Summary Accessors ---

bg_pack_fresh_summaries <- function(
  context,
  summary_kinds = NULL,
  node_ids = NULL,
  include_cross_scope = FALSE
) {
  summaries <- if (isTRUE(include_cross_scope)) {
    bg_default_bayesian_all_summaries(context)
  } else {
    context$evidence$summaries %||% list()
  }

  node_ids <- bg_pack_sort_ids(node_ids)

  matches <- Filter(
    function(summary) {
      isTRUE(summary$is_fresh) &&
        !isTRUE(summary$is_stale) &&
        (is.null(summary_kinds) ||
          (summary$summary_kind %||% "") %in% summary_kinds) &&
        (length(node_ids) == 0 || (summary$node_id %||% "") %in% node_ids)
    },
    summaries
  )

  if (length(matches) == 0) {
    return(list())
  }

  matches[order(vapply(matches, `[[`, character(1), "summary_id"))]
}

bg_pack_pending_review_summaries <- function(
  context,
  decision_kind,
  summary_kinds,
  node_ids = NULL,
  include_cross_scope = FALSE
) {
  summaries <- bg_pack_fresh_summaries(
    context,
    summary_kinds = summary_kinds,
    node_ids = node_ids,
    include_cross_scope = include_cross_scope
  )
  if (length(summaries) == 0) {
    return(list())
  }

  fresh_summary_ids <- bg_pack_sort_ids(vapply(
    summaries,
    `[[`,
    character(1),
    "summary_id"
  ))
  reviewed_ids <- bg_default_bayesian_current_decision_summary_ids(
    decisions = context$evidence$decisions %||% list(),
    scope = context$scope,
    kind = decision_kind,
    fresh_summary_ids = fresh_summary_ids
  )

  Filter(
    function(summary) !(summary$summary_id %||% "") %in% reviewed_ids,
    summaries
  )
}

# --- Decision Helpers ---

bg_pack_scope_decisions <- function(context, kind = NULL) {
  bg_default_bayesian_scope_decisions(
    context$evidence$decisions %||% list(),
    scope = context$scope,
    kind = kind
  )
}

bg_pack_has_scope_decision <- function(context, kind, node_ids = NULL) {
  decisions <- bg_pack_scope_decisions(context, kind = kind)
  if (length(decisions) == 0) {
    return(FALSE)
  }

  node_ids <- bg_pack_sort_ids(node_ids)
  if (length(node_ids) == 0) {
    return(TRUE)
  }

  any(vapply(
    decisions,
    function(decision) {
      decision_node_ids <- bg_pack_sort_ids(c(
        decision$metadata$node_ids %||% character(),
        decision$evidence %||% character()
      ))
      length(intersect(node_ids, decision_node_ids)) > 0
    },
    logical(1)
  ))
}

# --- Review and Goal Utilities ---

bg_pack_review_severity <- function(summaries, default = "blocking") {
  if (length(summaries) == 0) {
    return(default)
  }

  severities <- unique(vapply(
    summaries,
    function(summary) {
      summary$severity %||% if (isFALSE(summary$passed)) "warning" else "ok"
    },
    character(1)
  ))

  if (any(severities %in% c("warning", "error"))) {
    return("blocking")
  }

  "advisory"
}

bg_pack_goal_kind <- function(context) {
  context$inferential_goal$kind %||% NULL
}

bg_pack_goal_kind_allowed <- function(context, allowed_goal_kinds = NULL) {
  if (is.null(allowed_goal_kinds)) {
    return(TRUE)
  }

  goal_kind <- bg_pack_goal_kind(context)
  !is.null(goal_kind) && goal_kind %in% allowed_goal_kinds
}

bg_pack_primary_utilities <- function(context) {
  goal_kind <- bg_pack_goal_kind(context)
  if (is.null(goal_kind) || length(goal_kind) != 1) {
    return(character())
  }

  switch(
    goal_kind,
    observable_prediction = c(
      "predictive_performance",
      "estimation_speed",
      "interpretability",
      "robustness"
    ),
    latent_inference = c(
      "causal_consistency",
      "convergence",
      "parameter_recoverability",
      "estimation_speed",
      "interpretability",
      "robustness"
    ),
    character()
  )
}
