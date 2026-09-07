# Rules are data and never restore or evaluate persisted R code.
bg_research_policy <- function(mode, rules) {
  bg_research_string(mode, "mode")
  if (!mode %in% c("record", "guide", "enforce")) {
    cli::cli_abort("Mode must be record, guide, or enforce.")
  }
  rules <- bg_research_data(rules)
  if (!is.list(rules)) {
    cli::cli_abort("Rules must be a list.")
  }
  ids <- character()
  for (rule in rules) {
    if (!is.list(rule)) {
      cli::cli_abort("Each rule must be a named list.")
    }
    required <- c("id", "actions", "type", "message")
    common <- c(
      required,
      "explanation",
      "references",
      "suggestion",
      "interpretation"
    )
    bg_research_string(rule$type, "rule.type")
    specific <- switch(
      rule$type,
      require_values = c("path", "values"),
      forbid_values = c("path", "values"),
      require_evidence = "check",
      require_review = "check",
      cli::cli_abort("Unknown rule type {.val {rule$type}}.")
    )
    bg_research_fields(rule, c(required, specific), c(common, specific), "rule")
    for (key in c(
      "id",
      "message",
      intersect(
        c("explanation", "suggestion", "interpretation", "check"),
        names(rule)
      )
    )) {
      bg_research_string(rule[[key]], paste0("rule.", key))
    }
    if (rule$id %in% ids) {
      cli::cli_abort("Rule ids must be unique.")
    }
    ids <- c(ids, rule$id)
    actions <- unlist(rule$actions, use.names = FALSE)
    if (
      !is.character(actions) ||
        length(actions) == 0L ||
        !all(
          actions %in%
            c(
              "create",
              "revise",
              "evidence",
              "compare",
              "review",
              "accept",
              "reject",
              "note",
              "close",
              "reopen"
            )
        )
    ) {
      cli::cli_abort("Rule actions must name supported research operations.")
    }
    if (rule$type %in% c("require_values", "forbid_values")) {
      path <- unlist(rule$path, use.names = FALSE)
      if (
        !is.character(path) ||
          length(path) < 1L ||
          !all(nzchar(path)) ||
          !path[[1]] %in% c("P", "A", "D")
      ) {
        cli::cli_abort("Rule path must start with P, A, or D.")
      }
      if (
        length(rule$values) == 0L ||
          is.list(rule$values) && any(vapply(rule$values, is.list, logical(1)))
      ) {
        cli::cli_abort("Rule values must contain one or more scalar values.")
      }
    }
    if (
      !is.null(rule$references) &&
        !is.character(unlist(rule$references, use.names = FALSE))
    ) {
      cli::cli_abort("Rule references must be strings.")
    }
  }
  list(mode = mode, rules = unname(rules))
}

bg_research_findings <- function(state, action, candidate) {
  findings <- list()
  for (rule in state$rules) {
    if (!action$kind %in% unlist(rule$actions, use.names = FALSE)) {
      next
    }
    candidates <- if (action$kind == "compare") {
      state$candidates[unlist(action$candidate_ids, use.names = FALSE)]
    } else {
      list(candidate)
    }
    failed <- any(vapply(
      candidates,
      function(subject) {
        bg_research_rule_fails(state, subject, rule)
      },
      logical(1)
    ))
    if (failed) findings[[rule$id]] <- rule
  }
  findings
}

bg_research_rule_fails <- function(state, candidate, rule) {
  if (rule$type %in% c("require_values", "forbid_values")) {
    value <- candidate$components
    for (field in unlist(rule$path, use.names = FALSE)) {
      value <- if (is.list(value)) value[[field]] else NULL
    }
    value <- unlist(value, use.names = FALSE)
    expected <- unlist(rule$values, use.names = FALSE)
    return(
      if (rule$type == "require_values") {
        !all(expected %in% value)
      } else {
        any(expected %in% value)
      }
    )
  }
  evidence <- Filter(
    function(e) {
      identical(e$candidate_id, candidate$id) && identical(e$check, rule$check)
    },
    state$evidence
  )
  if (length(evidence) == 0L) {
    return(TRUE)
  }
  if (rule$type == "require_evidence") {
    return(FALSE)
  }
  reviewed <- unlist(
    lapply(
      Filter(
        function(d) {
          identical(d$candidate_id, candidate$id) &&
            identical(d$kind, "review") &&
            identical(d$actor$type, "human")
        },
        state$decisions
      ),
      `[[`,
      "evidence_ids"
    ),
    use.names = FALSE
  )
  !all(names(evidence) %in% reviewed)
}
