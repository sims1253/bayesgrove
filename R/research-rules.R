# Rule API: rules, predicates, rulesets, resolution, and the session registry.
# Rules, rulesets, and vocabularies are plain data. Predicates are referenced
# by id; custom predicate functions live only in the session registry and are
# never persisted or restored.

.bg_rules_env <- new.env(parent = emptyenv())
.bg_rules_env$predicates <- list()
.bg_rules_env$rulesets <- list()

bg_rule_actions <- function() {
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
}

bg_rule_id_pattern <- "^[a-z0-9_-]+$"
bg_rule_dotted_pattern <- "^[a-z0-9_-]+(\\.[a-z0-9_-]+)+$"
# Canonical semver: no leading zeros, so each version has one spelling.
bg_rule_number_pattern <- "(0|[1-9][0-9]*)"
bg_rule_version_pattern <- paste0(
  "^",
  bg_rule_number_pattern,
  "\\.",
  bg_rule_number_pattern,
  "\\.",
  bg_rule_number_pattern,
  "(-[0-9A-Za-z.-]+)?$"
)

bg_rule_required_fields <- function() c("id", "on", "require", "message")

bg_rule_optional_fields <- function() {
  c(
    "when",
    "severity",
    "explanation",
    "suggestion",
    "interpretation",
    "references"
  )
}

# --- Built-in predicates ---

bg_rule_builtin_predicates <- function() {
  values <- list(required = c("path", "values"), fn = bg_predicate_values)
  list(
    goal_kind = list(required = "values", fn = bg_predicate_goal_kind),
    actor_type = list(required = "values", fn = bg_predicate_actor_type),
    candidate_values = values,
    require_values = values,
    forbid_values = list(
      required = c("path", "values"),
      fn = function(subject, args) !any(bg_predicate_found(subject, args))
    ),
    require_evidence = list(
      required = "check",
      optional = "provenance",
      fn = function(subject, args) {
        length(bg_predicate_evidence(subject, "check", args)) > 0L
      }
    ),
    require_utility_evidence = list(
      required = "utility",
      optional = "provenance",
      fn = function(subject, args) {
        length(bg_predicate_evidence(subject, "utility", args)) > 0L
      }
    ),
    require_review = list(required = "check", fn = bg_predicate_review)
  )
}

bg_predicate_goal_kind <- function(subject, args) {
  kind <- subject$state$goal$kind
  is.character(kind) &&
    length(kind) == 1L &&
    kind %in% unlist(args$values, use.names = FALSE)
}

bg_predicate_actor_type <- function(subject, args) {
  type <- subject$actor$type
  is.character(type) &&
    length(type) == 1L &&
    type %in% unlist(args$values, use.names = FALSE)
}

# Paths are relative to candidate$components: c("P", "covariates").
bg_predicate_found <- function(subject, args) {
  value <- subject$candidate$components
  for (field in unlist(args$path, use.names = FALSE)) {
    value <- if (is.list(value)) value[[field]] else NULL
  }
  unlist(args$values, use.names = FALSE) %in% unlist(value, use.names = FALSE)
}

bg_predicate_values <- function(subject, args) {
  all(bg_predicate_found(subject, args))
}

# Evidence belongs to an exact candidate. A prospective candidate has no id,
# so it never sees its parent's evidence.
bg_predicate_evidence <- function(subject, field, args) {
  id <- subject$candidate$id
  if (is.null(id)) {
    return(list())
  }
  derived <- identical(args$provenance, "derived")
  Filter(
    function(e) {
      identical(e$candidate_id, id) &&
        identical(e[[field]], args[[field]]) &&
        (!derived || identical(e$provenance$kind, "derived"))
    },
    subject$state$evidence
  )
}

bg_predicate_review <- function(subject, args) {
  evidence <- bg_predicate_evidence(subject, "check", args)
  if (length(evidence) == 0L) {
    return(FALSE)
  }
  state <- subject$state
  reviewed <- unlist(
    lapply(
      Filter(
        function(d) {
          identical(d$candidate_id, subject$candidate$id) &&
            identical(d$kind, "review") &&
            identical(d$goal_version, state$goal_version) &&
            identical(d$actor$type, "human")
        },
        state$decisions
      ),
      `[[`,
      "evidence_ids"
    ),
    use.names = FALSE
  )
  all(names(evidence) %in% reviewed)
}

# --- Validation ---

bg_rule_strings <- function(x, field, allow_empty = FALSE) {
  values <- unlist(x, use.names = FALSE)
  if (
    (is.list(x) && length(values) != length(x)) ||
      (!allow_empty && length(values) == 0L) ||
      (length(values) && !is.character(values)) ||
      anyNA(values) ||
      !all(nzchar(trimws(values)))
  ) {
    cli::cli_abort("{.val {field}} must contain non-empty strings.")
  }
  as.list(values)
}

bg_rule_scalars <- function(values) {
  if (
    length(values) == 0L ||
      (is.list(values) &&
        !all(vapply(
          values,
          function(value) {
            is.atomic(value) &&
              !is.null(value) &&
              length(value) == 1L &&
              !is.na(value)
          },
          logical(1)
        )))
  ) {
    cli::cli_abort("Rule values must contain one or more scalar values.")
  }
  as.list(values)
}

bg_rule_path <- function(path) {
  path <- unlist(path, use.names = FALSE)
  if (
    !is.character(path) ||
      length(path) < 1L ||
      !all(nzchar(path)) ||
      !path[[1]] %in% c("P", "A", "D")
  ) {
    cli::cli_abort("Rule path must start with P, A, or D.")
  }
  as.list(path)
}

# A condition or requirement: list(type = <predicate id>, ...arguments).
bg_rule_condition <- function(condition, label) {
  if (!is.list(condition) || is.null(names(condition))) {
    cli::cli_abort(
      "{.val {label}} must be a named list with a predicate {.field type}."
    )
  }
  bg_research_string(condition$type, paste0(label, ".type"))
  spec <- bg_rule_builtin_predicates()[[condition$type]]
  if (is.null(spec)) {
    if (!grepl(bg_rule_dotted_pattern, condition$type)) {
      cli::cli_abort(
        "Unknown rule predicate {.val {condition$type}}. Custom predicate ids contain a dot."
      )
    }
    # Argument names depend on the registration; a NULL value never has a
    # meaning, so it is rejected regardless of registration.
    if (any(vapply(condition, is.null, logical(1)))) {
      cli::cli_abort(
        "{.val {label}} passes a NULL argument to predicate {.val {condition$type}}."
      )
    }
    return(condition)
  }
  allowed <- c("type", spec$required, spec$optional)
  bg_research_fields(condition, c("type", spec$required), allowed, label)
  # Every supplied argument is validated, including a present NULL: a missing
  # value must never weaken a requirement or switch a rule off.
  for (key in setdiff(names(condition), "type")) {
    value <- condition[[key]]
    field <- paste0(label, ".", key)
    condition[key] <- list(switch(
      key,
      check = ,
      utility = {
        bg_research_string(value, field)
        value
      },
      provenance = {
        bg_research_string(value, field)
        if (!value %in% c("any", "derived")) {
          cli::cli_abort("Evidence provenance must be any or derived.")
        }
        value
      },
      path = bg_rule_path(value),
      values = if (condition$type %in% c("goal_kind", "actor_type")) {
        bg_rule_strings(value, field)
      } else {
        bg_rule_scalars(value)
      }
    ))
  }
  condition
}

# Validates the fields shared by raw and resolved rules; returns them in
# canonical form, with arrays as lists.
bg_rule_body <- function(rule) {
  # Absent, null, and empty optional fields mean the same thing; drop them so
  # JSON-authored and R-built rules share a digest.
  empty <- vapply(rule, function(value) length(value) == 0L, logical(1))
  optional <- c(bg_rule_optional_fields(), "overridden_by")
  rule <- rule[!(names(rule) %in% optional & empty)]
  on <- unlist(rule$on, use.names = FALSE)
  if (
    !is.character(on) ||
      length(on) == 0L ||
      anyDuplicated(on) ||
      !all(on %in% bg_rule_actions())
  ) {
    cli::cli_abort("Rule {.field on} must name supported research operations.")
  }
  rule$on <- as.list(on)
  if (!is.null(rule$when)) {
    if (!is.list(rule$when) || !is.null(names(rule$when))) {
      cli::cli_abort("Rule {.field when} must be a list of conditions.")
    }
    rule$when <- lapply(rule$when, bg_rule_condition, label = "rule.when")
  }
  rule$require <- bg_rule_condition(rule$require, "rule.require")
  if (
    !is.null(rule$severity) &&
      !identical(rule$severity, "blocking") &&
      !identical(rule$severity, "advisory")
  ) {
    cli::cli_abort("Rule severity must be blocking or advisory.")
  }
  for (key in intersect(
    c("message", "explanation", "suggestion", "interpretation"),
    names(rule)
  )) {
    bg_research_string(rule[[key]], paste0("rule.", key))
  }
  if (!is.null(rule$references)) {
    rule$references <- bg_rule_strings(rule$references, "rule.references")
  }
  rule
}

bg_rule_check_raw <- function(rule) {
  bg_research_fields(
    rule,
    bg_rule_required_fields(),
    c(bg_rule_required_fields(), bg_rule_optional_fields()),
    "rule"
  )
  bg_research_string(rule$id, "rule.id")
  if (!grepl(bg_rule_id_pattern, rule$id) || identical(rule$id, "vocabulary")) {
    cli::cli_abort(
      "Rule id {.val {rule$id}} must match [a-z0-9_-]+ and must not be the reserved id {.val vocabulary}."
    )
  }
  bg_research_data(bg_rule_body(rule))
}

bg_rule_check_resolved <- function(rule) {
  bg_research_fields(
    rule,
    c(bg_rule_required_fields(), "source"),
    c(
      bg_rule_required_fields(),
      bg_rule_optional_fields(),
      "source",
      "overridden_by"
    ),
    "rule"
  )
  bg_research_string(rule$id, "rule.id")
  bg_research_string(rule$source, "rule.source")
  parts <- strsplit(rule$id, "/", fixed = TRUE)[[1]]
  source <- bg_ruleset_parse_ref(rule$source)
  if (
    length(parts) != 2L ||
      !identical(parts[[1]], source$id) ||
      !grepl(bg_rule_id_pattern, parts[[2]]) ||
      identical(parts[[2]], "vocabulary") ||
      is.null(source$version)
  ) {
    cli::cli_abort(
      "Resolved rule {.val {rule$id}} needs a qualified id and an exact source."
    )
  }
  if (!is.null(rule$overridden_by)) {
    rule$overridden_by <- bg_rule_strings(
      rule$overridden_by,
      "rule.overridden_by"
    )
    for (ref in unlist(rule$overridden_by)) {
      bg_ruleset_parse_ref(ref)
    }
  }
  bg_research_data(bg_rule_body(rule))
}

# Legacy rules keep their original validation and are never rewritten.
bg_rule_check_legacy <- function(rule) {
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
  actions <- unlist(rule$actions, use.names = FALSE)
  if (
    !is.character(actions) ||
      length(actions) == 0L ||
      !all(actions %in% bg_rule_actions())
  ) {
    cli::cli_abort("Rule actions must name supported research operations.")
  }
  if (rule$type %in% c("require_values", "forbid_values")) {
    bg_rule_path(rule$path)
    bg_rule_scalars(rule$values)
  }
  if (
    !is.null(rule$references) &&
      !is.character(unlist(rule$references, use.names = FALSE))
  ) {
    cli::cli_abort("Rule references must be strings.")
  }
  rule
}

bg_rule_shape <- function(rule) {
  if (any(c("type", "actions") %in% names(rule))) {
    "legacy"
  } else if ("source" %in% names(rule)) {
    "resolved"
  } else {
    "raw"
  }
}

# Validates a rule list. `shapes` limits which validators may apply.
bg_rule_list <- function(rules, shapes = c("legacy", "raw")) {
  if (!is.list(rules) || (length(rules) && !is.null(names(rules)))) {
    cli::cli_abort("Rules must be a list.")
  }
  ids <- character()
  rules <- lapply(rules, function(rule) {
    if (!is.list(rule) || (length(rule) && is.null(names(rule)))) {
      cli::cli_abort("Each rule must be a named list.")
    }
    shape <- bg_rule_shape(rule)
    if (!shape %in% shapes) {
      cli::cli_abort(
        "Rule {.val {rule$id %||% ''}} has the wrong shape here: {shape} rules are not accepted."
      )
    }
    rule <- switch(
      shape,
      legacy = bg_rule_check_legacy(rule),
      raw = bg_rule_check_raw(rule),
      resolved = bg_rule_check_resolved(rule)
    )
    if (rule$id %in% ids) {
      cli::cli_abort("Rule ids must be unique.")
    }
    ids <<- c(ids, rule$id)
    rule
  })
  unname(rules)
}

# Legacy rules evaluate as on = actions, require = list(type, ...).
bg_rule_effective <- function(rule) {
  if (identical(bg_rule_shape(rule), "legacy")) {
    return(list(
      on = unlist(rule$actions, use.names = FALSE),
      when = list(),
      require = rule[intersect(
        c("type", "path", "values", "check"),
        names(rule)
      )],
      severity = "blocking"
    ))
  }
  list(
    on = unlist(rule$on, use.names = FALSE),
    when = rule$when %||% list(),
    require = rule$require,
    severity = if (bg_rule_blocking(rule)) "blocking" else "advisory"
  )
}

bg_rule_conditions <- function(rule) {
  effective <- bg_rule_effective(rule)
  c(effective$when, list(effective$require))
}

#' Define a research rule
#'
#' Validates one rule and returns it as a plain list. A rule names the
#' research actions it governs, optional `when` conditions that decide whether
#' it applies, and one requirement. Conditions and requirements have the form
#' `list(type = <predicate id>, ...arguments)`.
#'
#' Built-in predicates are `goal_kind` (`values`), `actor_type` (`values`),
#' `candidate_values` and `require_values` (`path`, `values`), `forbid_values`
#' (`path`, `values`), `require_evidence` (`check`, optional `provenance`),
#' `require_utility_evidence` (`utility`, optional `provenance`), and
#' `require_review` (`check`). Paths are relative to the candidate's
#' `components`, for example `c("P", "covariates")`. Custom predicates use a
#' dotted id; see [bg_register_rule_predicate()].
#'
#' A predicate that is missing, has a different version than the pinned one,
#' raises an error, or returns anything other than `TRUE` or `FALSE` produces a
#' finding, whether in `when` or in `require`. A failure never switches a rule
#' off.
#'
#' @param id Rule id, unique within its ruleset. Lowercase letters, digits,
#'   `_`, and `-`. `vocabulary` is reserved.
#' @param on Research actions the rule governs: `create`, `revise`,
#'   `evidence`, `compare`, `review`, `accept`, `reject`, `note`, `close`, or
#'   `reopen`.
#' @param require One requirement, as a condition list.
#' @param message One sentence stating the unmet requirement.
#' @param when Optional list of conditions. The rule applies only if all of
#'   them are true.
#' @param severity `blocking` (the default when `NULL`) or `advisory`. Enforce
#'   holds an action only for blocking findings.
#' @param explanation,suggestion,interpretation Optional guidance strings.
#' @param references Optional source strings. They are never fetched.
#' @return A validated rule (named list).
#' @export
#' @examples
#' bg_rule(
#'   id = "review-ppc",
#'   on = "accept",
#'   when = list(list(type = "goal_kind", values = "observable_prediction")),
#'   require = list(type = "require_review", check = "ppc"),
#'   message = "Review the predictive check before accepting."
#' )
bg_rule <- function(
  id,
  on,
  require,
  message,
  when = NULL,
  severity = NULL,
  explanation = NULL,
  suggestion = NULL,
  interpretation = NULL,
  references = NULL
) {
  rule <- list(
    id = id,
    on = on,
    when = when,
    require = require,
    severity = severity,
    message = message,
    explanation = explanation,
    suggestion = suggestion,
    interpretation = interpretation,
    references = references
  )
  rule <- bg_rule_check_raw(bg_research_data(Filter(Negate(is.null), rule)))
  bg_rule_abort_problems(bg_rule_predicate_problems(list(rule), FALSE))
  rule
}

# --- Custom predicates ---

#' Register a custom rule predicate
#'
#' Adds a predicate to this session's registry. Persisted rules refer to the
#' predicate by `id`, and an installed policy pins its `version`. No R code is
#' persisted: a session must register a compatible implementation before rules
#' that use it can be satisfied. A predicate that is not registered, or that is
#' registered with a different version than the pinned one, fails at
#' evaluation and produces a finding.
#'
#' A host that exposes the research interface to agents must control predicate
#' registration as well as actor identity. This is a protocol, not a sandbox.
#'
#' @param id Dotted predicate id, for example `"mylab.prereg_match"`. Undotted
#'   ids are reserved for built-in predicates.
#' @param fn A function `fn(subject, args)` returning `TRUE` or `FALSE`.
#'   `subject` is `list(state, action, candidate, actor)`, where `state` is the
#'   research state before the action. `args` holds the condition's arguments.
#' @param arguments Argument names every condition must supply, with a
#'   non-`NULL` value.
#' @param optional Argument names a condition may supply.
#' @param version Implementation version recorded when a policy is installed.
#' @param replace Replace an existing registration with the same `id`.
#' @return The registration, invisibly: `list(id, version, arguments,
#'   optional)`.
#' @export
#' @examples
#' bg_register_rule_predicate(
#'   "mylab.has_label",
#'   function(subject, args) identical(subject$action$label, args$label),
#'   arguments = "label"
#' )
bg_register_rule_predicate <- function(
  id,
  fn,
  arguments = character(),
  version = "1",
  replace = FALSE,
  optional = character()
) {
  bg_research_string(id, "id")
  if (!grepl(bg_rule_dotted_pattern, id)) {
    cli::cli_abort(
      "Custom predicate ids must be dotted, for example {.val mylab.prereg_match}. Undotted ids are reserved for built-ins."
    )
  }
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function of {.code (subject, args)}.")
  }
  arguments <- unlist(bg_rule_strings(arguments, "arguments", TRUE)) %||%
    character()
  optional <- unlist(bg_rule_strings(optional, "optional", TRUE)) %||%
    character()
  names <- c(arguments, optional)
  if ("type" %in% names || anyDuplicated(names)) {
    cli::cli_abort("Predicate arguments must be unique and must not be type.")
  }
  bg_research_string(version, "version")
  if (!is.logical(replace) || length(replace) != 1L || is.na(replace)) {
    cli::cli_abort("{.arg replace} must be TRUE or FALSE.")
  }
  if (!replace && !is.null(.bg_rules_env$predicates[[id]])) {
    cli::cli_abort(
      "Predicate {.val {id}} is already registered. Use {.code replace = TRUE}."
    )
  }
  registration <- list(
    id = id,
    version = version,
    arguments = arguments,
    optional = optional
  )
  .bg_rules_env$predicates[[id]] <- c(registration, list(fn = fn))
  invisible(registration)
}

# Argument problems for one custom condition against its registration: every
# declared argument is required and non-NULL; others must be optional.
bg_rule_argument_problems <- function(condition, entry) {
  supplied <- setdiff(names(condition), "type")
  given <- supplied[!vapply(condition[supplied], is.null, logical(1))]
  unknown <- setdiff(supplied, c(entry$arguments, entry$optional))
  missing <- setdiff(entry$arguments, given)
  c(
    if (length(unknown)) {
      sprintf(
        "passes unknown argument(s) %s to predicate '%s'",
        paste0("'", unknown, "'", collapse = ", "),
        condition$type
      )
    },
    if (length(missing)) {
      sprintf(
        "omits required argument(s) %s of predicate '%s'",
        paste0("'", missing, "'", collapse = ", "),
        condition$type
      )
    }
  )
}

# Problems with custom predicates in a rule list. Installing a policy
# (`unknown = TRUE`) also rejects unregistered predicates. State reads never
# call this: compatibility with the registry is checked at evaluation.
bg_rule_predicate_problems <- function(rules, unknown = TRUE) {
  problems <- character()
  for (rule in rules) {
    for (condition in bg_rule_conditions(rule)) {
      if (!is.null(bg_rule_builtin_predicates()[[condition$type]])) {
        next
      }
      entry <- .bg_rules_env$predicates[[condition$type]]
      if (is.null(entry)) {
        if (unknown) {
          problems <- c(
            problems,
            sprintf(
              "Rule '%s' uses unknown predicate '%s'.",
              rule$id,
              condition$type
            )
          )
        }
        next
      }
      problems <- c(
        problems,
        sprintf(
          "Rule '%s' %s.",
          rule$id,
          bg_rule_argument_problems(condition, entry)
        )
      )
    }
  }
  unique(problems)
}

bg_rule_abort_problems <- function(problems) {
  if (length(problems)) {
    cli::cli_abort(c(
      "Rules use custom predicates incorrectly.",
      stats::setNames(problems, rep("x", length(problems)))
    ))
  }
}

# Pins each custom predicate in use as list(id, version); NULL if none.
bg_rule_predicate_pins <- function(rules) {
  problems <- bg_rule_predicate_problems(rules)
  if (length(problems)) {
    cli::cli_abort(c("Rules refer to unavailable predicates.", problems))
  }
  ids <- as.character(unique(unlist(lapply(rules, function(rule) {
    lapply(bg_rule_conditions(rule), `[[`, "type")
  }))))
  ids <- sort(
    setdiff(ids, names(bg_rule_builtin_predicates())),
    method = "radix"
  )
  if (length(ids) == 0L) {
    return(NULL)
  }
  lapply(ids, function(id) {
    list(id = id, version = .bg_rules_env$predicates[[id]]$version)
  })
}

# Evaluates one condition: TRUE, FALSE, or a failure reason (string).
bg_rule_predicate <- function(condition, subject, pins) {
  type <- condition$type
  fn <- bg_rule_builtin_predicates()[[type]]$fn
  if (is.null(fn)) {
    entry <- .bg_rules_env$predicates[[type]]
    pinned <- Filter(function(p) identical(p$id, type), pins %||% list())
    if (is.null(entry)) {
      return(sprintf("Predicate '%s' is not registered in this session.", type))
    }
    if (length(pinned) == 0L) {
      return(sprintf("Predicate '%s' is not pinned by the policy.", type))
    }
    if (!identical(entry$version, pinned[[1]]$version)) {
      return(sprintf(
        "Predicate '%s' is registered as version '%s', but the policy pins version '%s'.",
        type,
        entry$version,
        pinned[[1]]$version
      ))
    }
    problems <- bg_rule_argument_problems(condition, entry)
    if (length(problems)) {
      return(sprintf("Condition %s.", paste(problems, collapse = "; ")))
    }
    fn <- entry$fn
  }
  args <- condition[setdiff(names(condition), "type")]
  value <- tryCatch(fn(subject, args), error = function(e) {
    sprintf(
      "Predicate '%s' raised an error: %s",
      type,
      cli::ansi_strip(conditionMessage(e))
    )
  })
  if (is.character(value) && length(value) == 1L) {
    return(value)
  }
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    return(sprintf(
      "Predicate '%s' did not return TRUE or FALSE.",
      type
    ))
  }
  isTRUE(as.vector(value))
}

# --- Vocabulary ---

bg_vocabulary_normalize <- function(vocabulary) {
  categories <- c("utilities", "goal_kinds", "checks")
  bg_research_fields(vocabulary, character(), categories, "vocabulary")
  for (category in names(vocabulary)) {
    entries <- vocabulary[[category]]
    if (!is.list(entries) || !is.null(names(entries)) || !length(entries)) {
      cli::cli_abort(
        "Vocabulary {.field {category}} must be a non-empty list of entries."
      )
    }
    fields <- if (category == "checks") {
      c("id", "title", "utilities")
    } else {
      c("id", "title", "description", "references")
    }
    label <- paste0("vocabulary.", category)
    entries <- lapply(entries, function(entry) {
      bg_research_fields(entry, "id", fields, label)
      entry <- entry[names(entry) == "id" | lengths(entry) > 0L]
      for (key in intersect(c("id", "title", "description"), names(entry))) {
        bg_research_string(entry[[key]], paste0(label, ".", key))
      }
      for (key in intersect(c("references", "utilities"), names(entry))) {
        entry[[key]] <- bg_rule_strings(entry[[key]], paste0(label, ".", key))
      }
      entry
    })
    ids <- vapply(entries, `[[`, character(1), "id")
    if (anyDuplicated(ids)) {
      cli::cli_abort("Vocabulary {.field {category}} ids must be unique.")
    }
    vocabulary[[category]] <- entries
  }
  vocabulary
}

# Later entries replace earlier ones with the same id, whole.
bg_vocabulary_merge <- function(base, extra) {
  if (is.null(extra)) {
    return(base)
  }
  base <- base %||% list()
  for (category in names(extra)) {
    entries <- base[[category]] %||% list()
    ids <- vapply(entries, `[[`, character(1), "id")
    for (entry in extra[[category]]) {
      at <- match(entry$id, ids)
      if (is.na(at)) {
        entries[[length(entries) + 1L]] <- entry
        ids <- c(ids, entry$id)
      } else {
        entries[[at]] <- entry
      }
    }
    base[[category]] <- entries
  }
  base[order(names(base), method = "radix")]
}

bg_vocabulary_ids <- function(vocabulary, category) {
  entries <- vocabulary[[category]]
  if (is.null(entries)) {
    return(NULL)
  }
  vapply(entries, `[[`, character(1), "id")
}

bg_vocabulary_check <- function(vocabulary) {
  utilities <- bg_vocabulary_ids(vocabulary, "utilities")
  if (is.null(utilities)) {
    return(invisible(vocabulary))
  }
  for (check in vocabulary$checks) {
    unknown <- setdiff(unlist(check$utilities), utilities)
    if (length(unknown)) {
      cli::cli_abort(
        "Vocabulary check {.val {check$id}} maps to undeclared utilities {.val {unknown}}."
      )
    }
  }
  invisible(vocabulary)
}

# --- Rulesets ---

bg_ruleset_parse_ref <- function(ref) {
  bg_research_string(ref, "ruleset reference")
  parts <- regmatches(ref, regexec("^([^@]+)@(.+)$", ref))[[1]]
  if (
    length(parts) != 3L ||
      !grepl(bg_rule_dotted_pattern, parts[[2]]) ||
      !(grepl(paste0("^", bg_rule_number_pattern, "$"), parts[[3]]) ||
        grepl(bg_rule_version_pattern, parts[[3]]))
  ) {
    cli::cli_abort(
      "Ruleset reference {.val {ref}} must be {.code <id>@<major>} or {.code <id>@<version>}."
    )
  }
  exact <- grepl(bg_rule_version_pattern, parts[[3]])
  list(
    id = parts[[2]],
    major = if (exact) NULL else as.integer(parts[[3]]),
    version = if (exact) parts[[3]] else NULL
  )
}

bg_ruleset_ref <- function(ruleset) paste0(ruleset$id, "@", ruleset$version)

bg_ruleset_qualified_id <- function(id) {
  bg_research_string(id, "qualified rule id")
  parts <- strsplit(id, "/", fixed = TRUE)[[1]]
  if (
    length(parts) != 2L ||
      !grepl(bg_rule_dotted_pattern, parts[[1]]) ||
      !grepl(bg_rule_id_pattern, parts[[2]])
  ) {
    cli::cli_abort(
      "{.val {id}} must be a qualified rule id {.code <ruleset id>/<rule id>}."
    )
  }
  id
}

# Validates a ruleset and returns its canonical form, which digests use.
bg_ruleset_normalize <- function(ruleset) {
  ruleset <- bg_research_data(ruleset)
  if (!is.list(ruleset) || is.null(names(ruleset))) {
    cli::cli_abort("A ruleset must be a named list.")
  }
  bg_research_fields(
    ruleset,
    c("id", "version"),
    c(
      "id",
      "version",
      "title",
      "description",
      "references",
      "extends",
      "disable",
      "override",
      "vocabulary",
      "rules"
    ),
    "ruleset"
  )
  bg_research_string(ruleset$id, "ruleset.id")
  if (!grepl(bg_rule_dotted_pattern, ruleset$id)) {
    cli::cli_abort(
      "Ruleset id {.val {ruleset$id}} must be a dotted namespace such as {.val lab.prereg}."
    )
  }
  bg_research_string(ruleset$version, "ruleset.version")
  if (!grepl(bg_rule_version_pattern, ruleset$version)) {
    cli::cli_abort(
      "Ruleset version {.val {ruleset$version}} must be MAJOR.MINOR.PATCH with an optional -prerelease."
    )
  }
  for (key in intersect(c("title", "description"), names(ruleset))) {
    bg_research_string(ruleset[[key]], paste0("ruleset.", key))
  }
  for (key in c("references", "extends", "disable")) {
    if (!is.null(ruleset[[key]])) {
      ruleset[[key]] <- bg_rule_strings(
        ruleset[[key]],
        paste0("ruleset.", key),
        TRUE
      )
    }
  }
  for (ref in unlist(ruleset$extends)) {
    bg_ruleset_parse_ref(ref)
  }
  for (id in unlist(ruleset$disable)) {
    bg_ruleset_qualified_id(id)
  }
  if (length(ruleset$override)) {
    if (!is.list(ruleset$override) || is.null(names(ruleset$override))) {
      cli::cli_abort("Ruleset overrides must be named by qualified rule id.")
    }
    ruleset$override <- Map(
      bg_ruleset_override_patch,
      names(ruleset$override),
      ruleset$override
    )
  }
  if (!is.null(ruleset$vocabulary)) {
    ruleset$vocabulary <- bg_vocabulary_normalize(ruleset$vocabulary)
  }
  if (!is.null(ruleset$rules)) {
    ruleset$rules <- bg_rule_list(ruleset$rules, "raw")
    bg_rule_abort_problems(bg_rule_predicate_problems(ruleset$rules, FALSE))
  }
  # Empty optional fields carry no meaning; drop them so digests agree.
  keep <- vapply(
    names(ruleset),
    function(key) key %in% c("id", "version") || length(ruleset[[key]]) > 0L,
    logical(1)
  )
  ruleset[keep]
}

# An override replaces each supplied field whole; NULL removes optional ones.
bg_ruleset_override_patch <- function(target, patch) {
  bg_ruleset_qualified_id(target)
  fields <- setdiff(
    c(bg_rule_required_fields(), bg_rule_optional_fields()),
    "id"
  )
  if (!is.list(patch) || is.null(names(patch)) || !length(patch)) {
    cli::cli_abort(
      "Override {.val {target}} must be a non-empty partial rule."
    )
  }
  bg_research_fields(patch, character(), fields, "override")
  removed <- names(patch)[vapply(patch, is.null, logical(1))]
  if (length(intersect(removed, bg_rule_required_fields()))) {
    cli::cli_abort(
      "Override {.val {target}} cannot remove required rule fields."
    )
  }
  set <- Filter(Negate(is.null), patch)
  probe <- list(
    on = "accept",
    require = list(type = "goal_kind", values = "probe"),
    message = "probe"
  )
  probe[names(set)] <- set
  probe <- bg_rule_body(probe)
  probe$id <- target
  bg_rule_abort_problems(bg_rule_predicate_problems(list(probe), FALSE))
  patch[names(set)] <- probe[names(set)]
  patch
}

#' Define a ruleset
#'
#' Validates a ruleset and returns it as a plain list. A ruleset collects raw
#' rules under a dotted namespace and version, and can extend, disable, and
#' override rules from other registered rulesets.
#'
#' Resolution is recursive. Each `extends` reference is resolved in order and
#' the results are concatenated. Identical rules inherited along several paths
#' collapse to one; any other duplicate is an error. `disable` and then
#' `override` are applied to the inherited rules, and the ruleset's own `rules`
#' are appended with qualified ids `<id>/<rule id>`. Overridden rules keep their
#' `source` and gain `overridden_by`.
#'
#' `id@MAJOR` selects the highest registered non-prerelease version with that
#' major. An exact version selects only that version.
#'
#' @param id Dotted namespace, for example `"lab.prereg"`.
#' @param version `MAJOR.MINOR.PATCH`, optionally followed by `-prerelease`.
#' @param rules List of raw rules, see [bg_rule()].
#' @param extends References to other rulesets, `"<id>@<major>"` or
#'   `"<id>@<version>"`, resolved in order.
#' @param disable Qualified ids of inherited rules to drop.
#' @param override Named list of partial rules, named by qualified rule id.
#'   Each supplied field replaces the inherited one whole. `NULL` removes an
#'   optional field.
#' @param vocabulary Optional named list with `utilities`, `goal_kinds`, and
#'   `checks`. Utilities and goal kinds are lists of `list(id, title,
#'   description, references)`. Checks are lists of `list(id, title,
#'   utilities)`. A declared category must be non-empty; an absent category is
#'   unconstrained.
#' @param title,description Optional documentation strings.
#' @param references Optional source strings. They are never fetched.
#' @return A validated ruleset (named list).
#' @seealso [bg_register_ruleset()], [bg_ruleset_check()], [bg_use_rules()].
#' @export
#' @examples
#' bg_ruleset(
#'   "lab.review",
#'   "1.0.0",
#'   rules = list(bg_rule(
#'     id = "review-ppc",
#'     on = "accept",
#'     require = list(type = "require_review", check = "ppc"),
#'     message = "Review the predictive check before accepting."
#'   ))
#' )
bg_ruleset <- function(
  id,
  version,
  rules = list(),
  extends = character(),
  disable = character(),
  override = list(),
  vocabulary = NULL,
  title = NULL,
  description = NULL,
  references = NULL
) {
  bg_ruleset_normalize(Filter(
    Negate(is.null),
    list(
      id = id,
      version = version,
      title = title,
      description = description,
      references = references,
      extends = extends,
      disable = disable,
      override = override,
      vocabulary = vocabulary,
      rules = rules
    )
  ))
}

bg_ruleset_builtin <- function() {
  if (is.null(.bg_rules_env$builtin)) {
    dir <- bg_protocol_asset_path("rulesets")
    files <- if (nzchar(dir)) {
      sort(list.files(dir, "\\.json$", full.names = TRUE), method = "radix")
    } else {
      character()
    }
    builtin <- list()
    for (file in files) {
      ruleset <- tryCatch(
        bg_ruleset_normalize(jsonlite::read_json(file, simplifyVector = FALSE)),
        error = function(e) {
          cli::cli_abort("Invalid built-in ruleset {.path {file}}.", parent = e)
        }
      )
      if (!identical(paste0(ruleset$id, ".json"), basename(file))) {
        cli::cli_abort(
          "Built-in ruleset {.path {file}} must be named <id>.json."
        )
      }
      builtin[[bg_ruleset_ref(ruleset)]] <- ruleset
    }
    .bg_rules_env$builtin <- builtin
  }
  .bg_rules_env$builtin
}

# The registered ruleset with the same id and an equal version, or NULL.
# Canonical versions make this a key match; comparing numerically as well
# keeps resolution independent of registration order.
bg_ruleset_registered <- function(ruleset) {
  split <- function(version) {
    list(
      core = numeric_version(sub("-.*$", "", version)),
      pre = sub("^[^-]*", "", version)
    )
  }
  target <- split(ruleset$version)
  matches <- Filter(
    function(r) {
      other <- split(r$version)
      identical(r$id, ruleset$id) &&
        other$core == target$core &&
        identical(other$pre, target$pre)
    },
    bg_ruleset_registry()
  )
  if (length(matches)) matches[[1]]
}

bg_ruleset_registry <- function() {
  c(bg_ruleset_builtin(), .bg_rules_env$rulesets)
}

# Clears the session registries; built-ins reload on next use. For tests.
bg_rule_registry_reset <- function() {
  .bg_rules_env$predicates <- list()
  .bg_rules_env$rulesets <- list()
  .bg_rules_env$builtin <- NULL
  invisible(NULL)
}

#' Register a ruleset for this session
#'
#' Validates a ruleset and adds it to the session registry, alongside the
#' built-in rulesets shipped in `inst/rulesets/`. Registering the same
#' `id@version` again is a no-op if its content is identical and an error
#' otherwise: publish a new version instead.
#'
#' @param ruleset A ruleset, see [bg_ruleset()]. A list read from JSON is
#'   validated the same way.
#' @return The validated ruleset, invisibly.
#' @export
#' @examples
#' bg_register_ruleset(bg_ruleset("lab.empty", "1.0.0"))
#' bg_rulesets()
bg_register_ruleset <- function(ruleset) {
  ruleset <- bg_ruleset_normalize(ruleset)
  key <- bg_ruleset_ref(ruleset)
  existing <- bg_ruleset_registered(ruleset)
  if (!is.null(existing)) {
    if (
      identical(bg_canonical_digest(existing), bg_canonical_digest(ruleset))
    ) {
      return(invisible(ruleset))
    }
    cli::cli_abort(
      "Ruleset {.val {key}} is already registered with different content. Register a new version."
    )
  }
  .bg_rules_env$rulesets[[key]] <- ruleset
  invisible(ruleset)
}

#' List available rulesets
#'
#' @return A data frame with one row per registered ruleset version and columns
#'   `id`, `version`, `title`, `origin` (`builtin` or `session`), and `digest`
#'   (SHA-256 over the ruleset's canonical JSON).
#' @export
#' @examples
#' bg_rulesets()
bg_rulesets <- function() {
  builtin <- bg_ruleset_builtin()
  registry <- bg_ruleset_registry()
  rows <- data.frame(
    id = vapply(registry, `[[`, character(1), "id"),
    version = vapply(registry, `[[`, character(1), "version"),
    title = vapply(registry, function(r) r$title %||% NA_character_, ""),
    origin = ifelse(names(registry) %in% names(builtin), "builtin", "session"),
    digest = vapply(registry, bg_canonical_digest, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  rows[order(rows$id, rows$version, method = "radix"), , drop = FALSE]
}

bg_ruleset_lookup <- function(ref) {
  parsed <- bg_ruleset_parse_ref(ref)
  registry <- bg_ruleset_registry()
  if (!is.null(parsed$version)) {
    ruleset <- registry[[ref]]
  } else {
    matches <- Filter(
      function(r) {
        identical(r$id, parsed$id) &&
          !grepl("-", r$version, fixed = TRUE) &&
          identical(as.integer(sub("\\..*$", "", r$version)), parsed$major)
      },
      registry
    )
    ruleset <- if (length(matches)) {
      versions <- numeric_version(vapply(matches, `[[`, "", "version"))
      matches[[which(versions == max(versions))[[1]]]]
    }
  }
  if (is.null(ruleset)) {
    cli::cli_abort("Unknown ruleset reference {.val {ref}}.")
  }
  ruleset
}

bg_ruleset_coalesce <- function(rules) {
  out <- list()
  ids <- character()
  json <- character()
  for (rule in rules) {
    text <- bg_canonical_json(rule)
    at <- match(rule$id, ids)
    if (is.na(at)) {
      out[[length(out) + 1L]] <- rule
      ids <- c(ids, rule$id)
      json <- c(json, text)
    } else if (!identical(json[[at]], text)) {
      cli::cli_abort(
        "Rule {.val {rule$id}} is inherited with conflicting definitions."
      )
    }
  }
  out
}

# Returns list(rules, vocabulary, sources) in resolution order.
bg_ruleset_resolve <- function(ruleset, stack = character()) {
  # Exact versions, so a ruleset may extend an earlier version of itself.
  ref <- bg_ruleset_ref(ruleset)
  if (ref %in% stack) {
    cli::cli_abort(
      "Ruleset inheritance has a cycle: {.val {c(stack, ref)}}."
    )
  }
  stack <- c(stack, ref)
  rules <- list()
  vocabulary <- NULL
  sources <- list()
  for (parent_ref in unlist(ruleset$extends)) {
    parent <- bg_ruleset_resolve(bg_ruleset_lookup(parent_ref), stack)
    rules <- c(rules, parent$rules)
    vocabulary <- bg_vocabulary_merge(vocabulary, parent$vocabulary)
    sources <- c(sources, parent$sources)
  }
  rules <- bg_ruleset_coalesce(rules)
  ids <- vapply(rules, `[[`, character(1), "id")
  for (target in unlist(ruleset$disable)) {
    if (!target %in% ids) {
      cli::cli_abort(
        "Ruleset {.val {ref}} disables {.val {target}}, which it does not inherit."
      )
    }
  }
  keep <- !ids %in% unlist(ruleset$disable)
  rules <- rules[keep]
  ids <- ids[keep]
  for (target in names(ruleset$override)) {
    at <- match(target, ids)
    if (is.na(at)) {
      cli::cli_abort(
        "Ruleset {.val {ref}} overrides {.val {target}}, which it does not inherit."
      )
    }
    rule <- rules[[at]]
    patch <- ruleset$override[[target]]
    for (field in names(patch)) {
      rule[[field]] <- patch[[field]]
    }
    rule$overridden_by <- c(as.list(rule$overridden_by), list(ref))
    rules[[at]] <- rule
  }
  own <- lapply(ruleset$rules, function(rule) {
    rule$id <- paste0(ruleset$id, "/", rule$id)
    if (rule$id %in% ids) {
      cli::cli_abort(
        "Ruleset {.val {ref}} redefines inherited rule {.val {rule$id}}. Override it instead."
      )
    }
    rule$source <- ref
    rule
  })
  rules <- bg_rule_list(c(rules, own), "resolved")
  sources <- c(
    sources,
    list(list(
      id = ruleset$id,
      version = ruleset$version,
      digest = bg_canonical_digest(ruleset)
    ))
  )
  refs <- vapply(sources, function(s) paste0(s$id, "@", s$version), "")
  list(
    rules = rules,
    vocabulary = bg_vocabulary_merge(vocabulary, ruleset$vocabulary),
    sources = sources[!duplicated(refs)]
  )
}

# Resolves a ruleset object or reference into the policy fields it pins.
bg_ruleset_policy <- function(ruleset) {
  if (is.character(ruleset)) {
    ruleset <- bg_ruleset_lookup(ruleset)
  } else {
    ruleset <- bg_ruleset_normalize(ruleset)
    registered <- bg_ruleset_registered(ruleset)
    if (
      !is.null(registered) &&
        !identical(
          bg_canonical_digest(registered),
          bg_canonical_digest(ruleset)
        )
    ) {
      cli::cli_abort(
        "Ruleset {.val {bg_ruleset_ref(ruleset)}} differs from the registered version with the same id and version."
      )
    }
  }
  resolved <- bg_ruleset_resolve(ruleset)
  vocabulary <- resolved$vocabulary
  if (!is.null(vocabulary)) {
    vocabulary <- bg_research_data(bg_vocabulary_check(vocabulary))
  }
  list(
    rules = resolved$rules,
    ruleset = list(
      id = ruleset$id,
      version = ruleset$version,
      title = ruleset$title,
      digest = bg_canonical_digest(ruleset),
      sources = resolved$sources
    ),
    vocabulary = vocabulary
  )
}

#' Check a ruleset
#'
#' Resolves a ruleset and reports problems without changing any project. This
#' is the conformance kit for ruleset authors: a ruleset that passes here can
#' be installed with [bg_use_rules()] in a session that registers the same
#' predicates.
#'
#' Errors cover invalid rulesets, failed resolution, and unknown predicates or
#' predicate arguments. Warnings cover rules that name checks, utilities, or
#' goal kinds outside the resolved vocabulary and rules that can never apply
#' or never be satisfied.
#'
#' @param ruleset A ruleset (see [bg_ruleset()]) or a reference string such
#'   as `"lab.review@1"`.
#' @return A list with `ok` (no errors), `errors` and `warnings` (character
#'   vectors), and `rule_count` (resolved rules).
#' @export
#' @examples
#' bg_ruleset_check(bg_ruleset(
#'   "lab.review",
#'   "1.0.0",
#'   rules = list(bg_rule(
#'     id = "review-ppc",
#'     on = "accept",
#'     require = list(type = "require_review", check = "ppc"),
#'     message = "Review the predictive check before accepting."
#'   ))
#' ))
bg_ruleset_check <- function(ruleset) {
  policy <- tryCatch(
    bg_ruleset_policy(bg_research_data(ruleset)),
    error = function(e) cli::ansi_strip(conditionMessage(e))
  )
  if (is.character(policy)) {
    return(list(
      ok = FALSE,
      errors = policy,
      warnings = character(),
      rule_count = 0L
    ))
  }
  errors <- bg_rule_predicate_problems(policy$rules)
  list(
    ok = length(errors) == 0L,
    errors = errors,
    warnings = bg_ruleset_warnings(policy$rules, policy$vocabulary),
    rule_count = length(policy$rules)
  )
}

bg_ruleset_warnings <- function(rules, vocabulary) {
  declared <- list(
    check = bg_vocabulary_ids(vocabulary, "checks"),
    utility = bg_vocabulary_ids(vocabulary, "utilities"),
    goal_kind = bg_vocabulary_ids(vocabulary, "goal_kinds")
  )
  evidence_types <- c(
    "require_evidence",
    "require_utility_evidence",
    "require_review"
  )
  warnings <- character()
  warn <- function(...) warnings <<- c(warnings, sprintf(...))
  for (rule in rules) {
    effective <- bg_rule_effective(rule)
    prospective <- all(effective$on %in% c("create", "revise"))
    conditions <- c(effective$when, list(effective$require))
    for (i in seq_along(conditions)) {
      condition <- conditions[[i]]
      in_when <- i <= length(effective$when)
      for (field in intersect(c("check", "utility"), names(condition))) {
        if (
          condition$type %in%
            evidence_types &&
            !is.null(declared[[field]]) &&
            !condition[[field]] %in% declared[[field]]
        ) {
          warn(
            "Rule '%s' names %s '%s', which the vocabulary does not declare.",
            rule$id,
            field,
            condition[[field]]
          )
        }
      }
      if (prospective && condition$type %in% evidence_types) {
        warn(
          "Rule '%s' %s on evidence, but a prospective candidate has none.",
          rule$id,
          if (in_when) {
            "can never apply: it conditions"
          } else {
            "can never be satisfied: it relies"
          }
        )
      }
      values <- unlist(condition$values)
      known <- switch(
        condition$type,
        goal_kind = declared$goal_kind,
        actor_type = c("human", "agent")
      )
      if (!is.null(known) && !any(values %in% known)) {
        warn(
          "Rule '%s' can never %s: no %s in %s is declared.",
          rule$id,
          if (in_when) "apply" else "be satisfied",
          sub("_", " ", condition$type, fixed = TRUE),
          paste0("'", values, "'", collapse = ", ")
        )
      } else if (!is.null(known) && !all(values %in% known)) {
        warn(
          "Rule '%s' names %s %s, which the vocabulary does not declare.",
          rule$id,
          sub("_", " ", condition$type, fixed = TRUE),
          paste0("'", setdiff(values, known), "'", collapse = ", ")
        )
      }
    }
  }
  unique(warnings)
}
