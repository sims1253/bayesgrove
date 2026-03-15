# Causal Dagitty Workflow Pack - Formula & Contract Helpers
# ----------------------------------------------------------
# Formula parsing, dagitty contract extraction, and violation detection
# helpers for the causal dagitty workflow pack.

bg_phase10_formula_nodes <- function(context) {
  Filter(
    function(node) !is.null((node$params %||% list())$formula),
    context$structural$nodes %||% list()
  )
}

bg_phase10_formula_object <- function(formula) {
  if (inherits(formula, "formula")) {
    return(formula)
  }

  if (is.character(formula) && length(formula) > 0) {
    return(tryCatch(
      stats::as.formula(paste(formula, collapse = " "), env = baseenv()),
      error = function(...) NULL
    ))
  }

  NULL
}

bg_phase10_formula_response <- function(formula) {
  formula <- bg_phase10_formula_object(formula)
  if (is.null(formula) || length(formula) < 3) {
    return(NULL)
  }

  paste(deparse(formula[[2]]), collapse = " ")
}

bg_phase10_formula_terms <- function(formula) {
  formula <- bg_phase10_formula_object(formula)
  if (is.null(formula) || length(formula) < 3) {
    return(character())
  }

  labels <- attr(stats::terms(formula), "term.labels") %||% character()
  if (length(labels) == 0) {
    return(character())
  }

  terms <- unique(unlist(
    lapply(labels, function(label) {
      all.vars(stats::as.formula(paste("~", label), env = baseenv()))
    }),
    use.names = FALSE
  ))

  bg_phase10_sort_ids(terms)
}

bg_phase10_causal_terms <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- x[nzchar(x)]

  bg_phase10_sort_ids(x)
}

bg_phase10_causal_allowed_formulas <- function(x, response = NULL) {
  if (is.null(x)) {
    return(list())
  }

  raw_specs <- if (inherits(x, "formula")) {
    list(x)
  } else if (is.character(x)) {
    as.list(x)
  } else if (is.list(x) && !is.data.frame(x)) {
    x
  } else {
    list(x)
  }

  formulas <- lapply(raw_specs, function(spec) {
    if (inherits(spec, "formula")) {
      return(spec)
    }

    if (
      is.character(spec) && length(spec) == 1 && grepl("~", spec, fixed = TRUE)
    ) {
      return(bg_phase10_formula_object(spec))
    }

    terms <- bg_phase10_causal_terms(spec)
    if (length(terms) == 0 || is.null(response)) {
      return(NULL)
    }

    stats::as.formula(
      paste(response, "~", paste(terms, collapse = " + ")),
      env = baseenv()
    )
  })

  Filter(Negate(is.null), formulas)
}

bg_phase10_causal_contract_from_summary <- function(summary, response = NULL) {
  metrics <- summary$metrics %||% list()

  required_terms <- bg_phase10_causal_terms(metrics$required_terms)
  forbidden_terms <- bg_phase10_causal_terms(metrics$forbidden_terms)
  optional_terms <- bg_phase10_causal_terms(metrics$optional_terms)
  ranked_candidate_terms <- bg_phase10_causal_terms(
    metrics$ranked_candidate_terms %||% metrics$preferred_order
  )
  allowed_formulas <- bg_phase10_causal_allowed_formulas(
    metrics$allowed_formulas,
    response = response
  )
  allowed_formula_terms <- lapply(allowed_formulas, bg_phase10_formula_terms)

  allowed_terms <- bg_phase10_sort_ids(c(
    required_terms,
    optional_terms,
    ranked_candidate_terms,
    unlist(allowed_formula_terms, use.names = FALSE)
  ))

  list(
    summary = summary,
    summary_id = summary$summary_id %||% NULL,
    required_terms = required_terms,
    forbidden_terms = forbidden_terms,
    optional_terms = optional_terms,
    ranked_candidate_terms = ranked_candidate_terms,
    allowed_terms = allowed_terms,
    allowed_formulas = allowed_formulas,
    allowed_formula_terms = allowed_formula_terms
  )
}

bg_phase10_current_causal_contract <- function(context) {
  summaries <- bg_phase10_fresh_summaries(
    context,
    summary_kinds = "causal_selection_contract"
  )
  if (length(summaries) == 0) {
    return(NULL)
  }

  reviewed_ids <- bg_default_bayesian_current_decision_summary_ids(
    decisions = context$evidence$decisions %||% list(),
    scope = context$scope,
    kind = "causal_selection_contract_review",
    fresh_summary_ids = vapply(summaries, `[[`, character(1), "summary_id")
  )
  reviewed <- Filter(
    function(summary) (summary$summary_id %||% "") %in% reviewed_ids,
    summaries
  )
  if (length(reviewed) == 0) {
    return(NULL)
  }

  formula_nodes <- bg_phase10_formula_nodes(context)
  response <- NULL
  if (length(formula_nodes) > 0) {
    response <- bg_phase10_formula_response(
      (formula_nodes[[1]]$params %||% list())$formula
    )
  }

  latest_idx <- which.max(vapply(
    reviewed,
    function(s) {
      s$created_at %||% s$updated_at %||% ""
    },
    character(1)
  ))
  if (length(latest_idx) == 0) {
    latest_idx <- length(reviewed)
  }

  bg_phase10_causal_contract_from_summary(
    reviewed[[latest_idx]],
    response = response
  )
}

bg_phase10_formula_contract_violation <- function(node_id, node, contract) {
  formula <- (node$params %||% list())$formula
  rhs_terms <- bg_phase10_formula_terms(formula)

  missing_required <- setdiff(contract$required_terms, rhs_terms)
  forbidden_present <- intersect(contract$forbidden_terms, rhs_terms)

  extra_terms <- character()
  if (length(contract$allowed_terms) > 0) {
    extra_terms <- setdiff(rhs_terms, contract$allowed_terms)
  }

  not_in_allowed_formulas <- FALSE
  if (length(contract$allowed_formula_terms) > 0) {
    not_in_allowed_formulas <- !any(vapply(
      contract$allowed_formula_terms,
      function(allowed_terms) setequal(rhs_terms, allowed_terms),
      logical(1)
    ))
  }

  list(
    node_id = node_id,
    node = node,
    formula = formula,
    rhs_terms = rhs_terms,
    missing_required = bg_phase10_sort_ids(missing_required),
    forbidden_present = bg_phase10_sort_ids(forbidden_present),
    extra_terms = bg_phase10_sort_ids(extra_terms),
    not_in_allowed_formulas = not_in_allowed_formulas,
    is_consistent = length(missing_required) == 0 &&
      length(forbidden_present) == 0 &&
      length(extra_terms) == 0 &&
      !isTRUE(not_in_allowed_formulas)
  )
}

bg_phase10_contract_suggested_formula <- function(node, contract) {
  current_formula <- (node$params %||% list())$formula
  response <- bg_phase10_formula_response(current_formula)
  if (is.null(response)) {
    return(NULL)
  }

  if (length(contract$allowed_formulas) > 0) {
    return(paste(deparse(contract$allowed_formulas[[1]]), collapse = " "))
  }

  rhs_terms <- contract$required_terms
  if (length(rhs_terms) == 0) {
    rhs_terms <- contract$ranked_candidate_terms
  }

  rhs <- if (length(rhs_terms) == 0) {
    "1"
  } else {
    paste(rhs_terms, collapse = " + ")
  }

  paste(response, "~", rhs)
}
