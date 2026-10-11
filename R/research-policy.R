# Rules are data and never restore or evaluate persisted R code.

bg_research_modes <- function() c("record", "guide", "enforce")

bg_research_mode <- function(mode) {
  bg_research_string(mode, "mode")
  if (!mode %in% bg_research_modes()) {
    cli::cli_abort("Mode must be record, guide, or enforce.")
  }
  mode
}

# Resolves requested rules into the policy a state stores: mode, rules,
# ruleset, vocabulary, and predicates. `rules` is a ruleset, a reference
# string, or a rule list. `goal` is checked against declared goal kinds.
bg_research_policy <- function(mode, rules, goal = NULL) {
  bg_research_mode(mode)
  rules <- bg_research_data(rules)
  if (is.character(rules) || (is.list(rules) && !is.null(names(rules)))) {
    policy <- bg_ruleset_policy(rules)
  } else {
    policy <- list(rules = bg_rule_list(rules), ruleset = NULL)
  }
  predicates <- bg_rule_predicate_pins(policy$rules)
  policy <- list(
    mode = mode,
    rules = policy$rules,
    ruleset = policy$ruleset,
    vocabulary = policy$vocabulary,
    predicates = predicates
  )
  if (!is.null(policy$ruleset)) {
    policy$ruleset$policy_digest <- bg_research_policy_digest(policy)
  }
  if (!is.null(goal)) {
    bg_research_goal_kind(goal, policy$vocabulary)
  }
  policy
}

bg_research_policy_digest <- function(policy) {
  bg_canonical_digest(list(
    rules = policy$rules %||% list(),
    vocabulary = policy$vocabulary,
    predicates = policy$predicates
  ))
}

# Validates the structure of a stored policy. Predicate availability is only
# checked at evaluation.
bg_research_policy_stored <- function(state) {
  fields <- c("mode", "rules", "ruleset", "vocabulary", "predicates")
  if (anyDuplicated(names(state)[names(state) %in% fields])) {
    cli::cli_abort("Policy fields must be unique.")
  }
  # Data validation rejects duplicate keys, which JSON readers keep and `$`
  # would silently resolve to the first. Stored rules are not rewritten.
  for (field in fields) {
    bg_research_data(state[[field]])
  }
  bg_research_mode(state$mode)
  legacy <- identical(state$schema_version, 1L)
  if (
    legacy &&
      !all(vapply(
        state[intersect(
          c("ruleset", "vocabulary", "predicates"),
          names(state)
        )],
        is.null,
        logical(1)
      ))
  ) {
    cli::cli_abort("Schema version 1 has no ruleset policy.")
  }
  shapes <- if (legacy) {
    "legacy"
  } else if (is.null(state$ruleset)) {
    c("legacy", "raw")
  } else {
    "resolved"
  }
  if (is.null(state$rules)) {
    cli::cli_abort("Rules must be a list.")
  }
  bg_rule_list(state$rules, shapes)
  if (!is.null(state$ruleset)) {
    ruleset <- state$ruleset
    bg_research_fields(
      ruleset,
      c("id", "version", "digest", "sources", "policy_digest"),
      c("id", "version", "title", "digest", "sources", "policy_digest"),
      "ruleset"
    )
    bg_ruleset_parse_ref(bg_ruleset_ref(ruleset))
    for (key in intersect(
      c("title", "digest", "policy_digest"),
      names(ruleset)
    )) {
      if (key != "title" || !is.null(ruleset$title)) {
        bg_research_string(ruleset[[key]], paste0("ruleset.", key))
      }
    }
    if (!is.list(ruleset$sources) || !length(ruleset$sources)) {
      cli::cli_abort("Ruleset sources must be a non-empty list.")
    }
    for (source in ruleset$sources) {
      bg_research_fields(
        source,
        c("id", "version", "digest"),
        c("id", "version", "digest"),
        "ruleset.sources"
      )
      bg_ruleset_parse_ref(bg_ruleset_ref(source))
      bg_research_string(source$digest, "ruleset.sources.digest")
    }
  }
  if (!is.null(state$vocabulary)) {
    if (is.null(state$ruleset)) {
      cli::cli_abort("A vocabulary requires a ruleset.")
    }
    bg_vocabulary_check(bg_vocabulary_normalize(state$vocabulary))
  }
  if (!is.null(state$predicates)) {
    ids <- vapply(
      state$predicates,
      function(pin) {
        bg_research_fields(pin, c("id", "version"), c("id", "version"), "pin")
        bg_research_string(pin$version, "predicates.version")
        bg_research_string(pin$id, "predicates.id")
        pin$id
      },
      character(1)
    )
    if (anyDuplicated(ids) || !all(grepl(bg_rule_dotted_pattern, ids))) {
      cli::cli_abort("Pinned predicates must have unique dotted ids.")
    }
  }
  invisible(state)
}

bg_research_policy_view <- function(state) {
  list(
    mode = state$mode,
    ruleset = state$ruleset,
    rules = state$rules,
    vocabulary = state$vocabulary,
    predicates = state$predicates
  )
}

# Goal kinds, when declared, are hard errors in every mode.
bg_research_goal_kind <- function(goal, vocabulary) {
  kinds <- bg_vocabulary_ids(vocabulary, "goal_kinds")
  kind <- goal$kind
  if (
    !is.null(kinds) &&
      (!is.character(kind) || length(kind) != 1L || !kind %in% kinds)
  ) {
    cli::cli_abort(
      "The goal kind must be one of {.val {kinds}}, as declared by the ruleset vocabulary."
    )
  }
  invisible(goal)
}

# Fills a missing utility when the check maps to exactly one utility.
bg_research_fill_utility <- function(action, vocabulary) {
  if (!is.null(action$utility)) {
    return(action)
  }
  check <- Filter(
    function(entry) identical(entry$id, action$check),
    vocabulary$checks
  )
  utilities <- if (length(check)) unlist(check[[1]]$utilities)
  if (length(utilities) == 1L) {
    action$utility <- utilities
    action <- action[order(names(action), method = "radix")]
  }
  action
}

# Evaluates every rule for its subjects. Record mode does not call this.
bg_research_findings <- function(state, action, candidate, actor) {
  findings <- list()
  pins <- state$predicates
  for (rule in state$rules) {
    effective <- bg_rule_effective(rule)
    if (!action$kind %in% effective$on) {
      next
    }
    candidates <- if (action$kind == "compare") {
      state$candidates[unlist(action$candidate_ids, use.names = FALSE)]
    } else {
      list(candidate)
    }
    failures <- character()
    failing <- character()
    hit <- FALSE
    for (subject_candidate in candidates) {
      subject <- list(
        state = state,
        action = action,
        candidate = subject_candidate,
        actor = actor
      )
      outcome <- bg_rule_outcome(effective, subject, pins)
      if (outcome$finding) {
        hit <- TRUE
        failures <- c(failures, outcome$failures)
        failing <- c(failing, subject_candidate$id)
      }
    }
    if (hit) {
      finding <- rule
      finding$severity <- effective$severity
      if (length(failures)) {
        finding$failures <- as.list(unique(failures))
      }
      if (action$kind == "compare") {
        finding$candidate_ids <- as.list(failing)
      }
      findings[[rule$id]] <- finding
    }
  }
  vocabulary <- bg_research_vocabulary_finding(state, action)
  if (!is.null(vocabulary)) {
    findings[[vocabulary$id]] <- vocabulary
  }
  findings
}

# A failed `when` condition produces a finding; it never switches a rule off.
bg_rule_outcome <- function(effective, subject, pins) {
  failures <- character()
  applies <- TRUE
  for (condition in effective$when) {
    result <- bg_rule_predicate(condition, subject, pins)
    if (is.character(result)) {
      failures <- c(failures, result)
    } else if (!result) {
      applies <- FALSE
    }
  }
  if (length(failures)) {
    return(list(finding = TRUE, failures = failures))
  }
  if (!applies) {
    return(list(finding = FALSE))
  }
  result <- bg_rule_predicate(effective$require, subject, pins)
  if (is.character(result)) {
    return(list(finding = TRUE, failures = result))
  }
  list(finding = !result, failures = character())
}

# The implicit rule <ruleset id>/vocabulary: advisory in Guide, blocking in
# Enforce. Record mode never evaluates findings.
bg_research_vocabulary_finding <- function(state, action) {
  vocabulary <- state$vocabulary
  if (action$kind != "evidence" || is.null(vocabulary)) {
    return(NULL)
  }
  utilities <- bg_vocabulary_ids(vocabulary, "utilities")
  checks <- bg_vocabulary_ids(vocabulary, "checks")
  issues <- character()
  if (!is.null(utilities) && !action$utility %in% utilities) {
    issues <- c(
      issues,
      sprintf("Utility '%s' is not declared.", action$utility)
    )
  }
  if (!is.null(checks)) {
    if (!action$check %in% checks) {
      issues <- c(issues, sprintf("Check '%s' is not declared.", action$check))
    } else {
      mapped <- unlist(
        vocabulary$checks[[match(action$check, checks)]]$utilities
      )
      if (!is.null(mapped) && !action$utility %in% mapped) {
        issues <- c(
          issues,
          sprintf(
            "Check '%s' informs %s, not '%s'.",
            action$check,
            paste0("'", mapped, "'", collapse = ", "),
            action$utility
          )
        )
      }
    }
  }
  if (length(issues) == 0L) {
    return(NULL)
  }
  list(
    id = paste0(state$ruleset$id, "/vocabulary"),
    on = list("evidence"),
    severity = if (state$mode == "enforce") "blocking" else "advisory",
    message = "Evidence uses terms outside the ruleset vocabulary.",
    explanation = paste(issues, collapse = " "),
    source = bg_ruleset_ref(state$ruleset)
  )
}
