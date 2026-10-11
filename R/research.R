#' Start a recorded model search
#'
#' Creates one investigation in an existing project. Candidates and evidence
#' are independent of the execution graph. This experimental interface records
#' research operations; it does not replace [bg_run()] or enforce rules on
#' arbitrary R code. See `vignette("research-search")` for the action contract.
#'
#' @param project A `bg_handle`.
#' @param question The research question.
#' @param goal The inferential goal, as a named list.
#' @param actor Named list with `id` and `type` (`human` or `agent`). Authorship
#'   is caller-declared, not authenticated.
#' @param mode `record`, `guide`, or `enforce`.
#' @param rules The research policy: a ruleset (see [bg_ruleset()]), a
#'   reference string such as `"lab.review@1"`, or a list of rules (see
#'   [bg_rule()]). Rulesets are resolved and pinned when the investigation
#'   starts. If the resolved vocabulary declares goal kinds, `goal$kind` must
#'   be one of them.
#' @return The persisted research state.
#' @export
bg_research_init <- function(
  project,
  question,
  goal,
  actor,
  mode = "record",
  rules = list()
) {
  bg_research_access(project, write = TRUE)
  bg_research_string(question, "question")
  goal <- bg_research_goal(goal)
  actor <- bg_research_actor(actor)
  requested <- bg_research_data(rules)
  policy <- bg_research_policy(mode, requested, goal)
  path <- bg_research_path(project)
  bg_with_file_lock(paste0(path, ".lock"), {
    if (file.exists(path)) {
      cli::cli_abort("This project already has an investigation.")
    }
    state <- list(
      schema_name = "bg_research",
      schema_version = bg_research_schema_version,
      project_id = project@project_id,
      version = 1L,
      goal_version = 1L,
      question = question,
      goal = goal,
      mode = policy$mode,
      rules = policy$rules,
      ruleset = policy$ruleset,
      vocabulary = policy$vocabulary,
      predicates = policy$predicates,
      candidates = list(),
      evidence = list(),
      comparisons = list(),
      decisions = list(),
      history = list(list(
        version = 1L,
        action = list(
          kind = "initialize",
          question = question,
          goal = goal,
          mode = policy$mode,
          rules = requested
        ),
        policy = policy,
        actor = actor,
        rationale = "Start the investigation.",
        created_at = bg_now_timestamp()
      ))
    )
    bg_write_json_atomic(path, state, sort_keys = FALSE)
  })
  bg_research_state(project)
}

#' Read a recorded model search
#'
#' @param project A `bg_handle`.
#' @return A named list, or `NULL` if the project has no investigation.
#'   Records use JSON-compatible data: arrays are lists on reading.
#' @export
bg_research_state <- function(project) {
  bg_research_access(project)
  path <- bg_research_path(project)
  if (!file.exists(path)) {
    return(NULL)
  }
  state <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Cannot read research state at {.path {path}}.",
        parent = e
      )
    }
  )
  if (
    !is.list(state) ||
      !identical(state$schema_name, "bg_research") ||
      !(identical(state$schema_version, 1L) ||
        identical(state$schema_version, bg_research_schema_version)) ||
      !identical(state$project_id, project@project_id) ||
      !is.numeric(state$version) ||
      length(state$version) != 1L ||
      is.na(state$version) ||
      state$version < 1 ||
      !isTRUE(tryCatch(
        {
          bg_research_string(state$question, "question")
          bg_research_policy_stored(state)
          TRUE
        },
        error = function(e) FALSE
      )) ||
      !all(vapply(
        state[c(
          "candidates",
          "evidence",
          "comparisons",
          "decisions",
          "history"
        )],
        is.list,
        logical(1)
      ))
  ) {
    cli::cli_abort("Malformed or unsupported research state at {.path {path}}.")
  }
  tryCatch(
    bg_research_validate_candidates(state),
    error = function(e) {
      cli::cli_abort(
        "Malformed candidate records at {.path {path}}.",
        parent = e
      )
    }
  )
  state
}

bg_research_validate_candidates <- function(state) {
  candidates <- bg_research_data(state$candidates)
  if (length(candidates) && is.null(names(candidates))) {
    cli::cli_abort("Candidates must be named by their IDs.")
  }
  goal_versions <- vapply(
    Filter(
      function(entry) {
        entry$action$kind %in% c("initialize", "reframe")
      },
      state$history
    ),
    function(entry) entry$version,
    numeric(1)
  )
  for (id in names(candidates)) {
    candidate <- candidates[[id]]
    required <- c("id", "components", "parent_id", "status", "goal_version")
    if (
      !is.list(candidate) ||
        !all(required %in% names(candidate)) ||
        !identical(candidate$id, id)
    ) {
      cli::cli_abort("Candidate fields or ID are invalid.")
    }
    bg_research_components(candidate$components)
    bg_research_string(candidate$status, "status")
    if (
      !candidate$status %in% c("open", "closed") ||
        !is.numeric(candidate$goal_version) ||
        length(candidate$goal_version) != 1L ||
        !candidate$goal_version %in% goal_versions
    ) {
      cli::cli_abort("Candidate status or goal reference is invalid.")
    }
    seen <- id
    parent <- candidate$parent_id
    while (!is.null(parent)) {
      bg_research_string(parent, "parent_id")
      if (!parent %in% names(candidates) || parent %in% seen) {
        cli::cli_abort("Candidate lineage has a missing parent or a cycle.")
      }
      seen <- c(seen, parent)
      parent <- candidates[[parent]]$parent_id
    }
  }
  invisible(NULL)
}

#' Propose an operation in a model search
#'
#' Validates an action without changing the project. In Guide and Enforce
#' modes, returns findings with explanations and references. A proposal binds
#' the action to the current state version. [bg_research_apply()] validates
#' it again before writing.
#'
#' @param project A `bg_handle`.
#' @param action A named list with `kind` and the fields for that operation.
#' @param actor Named list with `id` and `type` (`human` or `agent`).
#' @param rationale Why this action is proposed.
#' @return A proposal with `version`, `action`, `actor`, `rationale`, `allowed`,
#'   and `findings`. A finding is the rule with its `severity`, plus
#'   `failures` when a predicate failed and `candidate_ids` for comparisons.
#'   Enforce holds an action only for blocking findings. A `configure` proposal
#'   also carries the resolved `policy` and its `policy_digest`; apply resolves
#'   the requested rules again, and a different digest makes the proposal
#'   stale.
#' @export
bg_research_propose <- function(project, action, actor, rationale) {
  state <- bg_research_state(project)
  if (is.null(state)) {
    cli::cli_abort("Start an investigation with {.fn bg_research_init}.")
  }
  bg_research_prepare(state, action, actor, rationale)
}

#' Apply a proposed research operation
#'
#' Rejects stale proposals and rechecks the current rules. One atomic write
#' saves both the research change and its history entry. Blocked operations
#' and failed writes leave the previous state intact.
#'
#' @param project A writable `bg_handle`.
#' @param proposal A result of [bg_research_propose()].
#' @return The new research state. `history` records the generated `id`.
#' @export
bg_research_apply <- function(project, proposal) {
  bg_research_access(project, write = TRUE)
  if (!is.list(proposal)) {
    cli::cli_abort("{.arg proposal} must be a research proposal.")
  }
  path <- bg_research_path(project)
  bg_with_file_lock(paste0(path, ".lock"), {
    state <- bg_research_state(project)
    stale <- function() {
      cli::cli_abort(
        "Stale research proposal. Read the current state and propose again."
      )
    }
    if (
      is.null(state) ||
        !identical(proposal$version, state$version) ||
        !identical(proposal$project_id, state$project_id)
    ) {
      stale()
    }
    checked <- bg_research_prepare(
      state,
      proposal$action,
      proposal$actor,
      proposal$rationale
    )
    # Only policy-installing proposals resolve again; a registry change since
    # the proposal makes it stale.
    if (!identical(proposal$policy_digest, checked$policy_digest)) {
      stale()
    }
    if (!checked$allowed) {
      blocking <- Filter(
        function(f) identical(f$severity, "blocking"),
        checked$findings
      )
      reasons <- vapply(blocking, `[[`, character(1), "message")
      held <- paste(reasons, collapse = " ")
      cli::cli_abort(c(
        "Research action is held by policy.",
        "i" = "{held}"
      ))
    }
    state <- bg_research_transition(state, checked)
    bg_write_json_atomic(path, state, sort_keys = FALSE)
  })
  bg_research_state(project)
}

#' Install research rules
#'
#' Proposes and applies a `configure` action, which replaces the whole research
#' policy. A ruleset or reference is resolved and pinned: the state stores the
#' resolved rules, the merged vocabulary, the versions of custom predicates in
#' use, and content digests for every source ruleset. The history entry keeps
#' both the requested rules and the resolved policy. Earlier records and
#' `goal_version` are unchanged.
#'
#' @param project A writable `bg_handle` with an investigation.
#' @param rules A ruleset (see [bg_ruleset()]), a reference string such as
#'   `"lab.review@1"` or `"lab.review@1.2.0"`, or a list of rules.
#' @param mode `record`, `guide`, or `enforce`. `NULL` keeps the current mode.
#' @param actor Named list with `id` and `type`. Only a human may change policy.
#' @param rationale Why the policy changes.
#' @return The new research state, invisibly.
#' @seealso [bg_rules()] to read the active policy.
#' @export
bg_use_rules <- function(project, rules, mode = NULL, actor, rationale) {
  state <- bg_research_state(project)
  if (is.null(state)) {
    cli::cli_abort("Start an investigation with {.fn bg_research_init}.")
  }
  action <- list(kind = "configure", mode = mode %||% state$mode, rules = rules)
  proposal <- bg_research_prepare(state, action, actor, rationale)
  invisible(bg_research_apply(project, proposal))
}

#' Read the active research policy
#'
#' Returns what an agent should read before acting: the intervention mode, the
#' pinned ruleset (with `sources` and `policy_digest`), the rules in force, the
#' merged vocabulary, and the custom predicate versions the rules rely on.
#'
#' @param project A `bg_handle` with an investigation.
#' @return A named list with `mode`, `ruleset`, `rules`, `vocabulary`, and
#'   `predicates`. `ruleset`, `vocabulary`, and `predicates` are `NULL` when
#'   not in use.
#' @export
bg_rules <- function(project) {
  state <- bg_research_state(project)
  if (is.null(state)) {
    cli::cli_abort("Start an investigation with {.fn bg_research_init}.")
  }
  bg_research_policy_view(state)
}

bg_research_schema_version <- 2L

bg_research_path <- function(project) {
  bg_storage_path(project, "workflow", "research.json")
}

bg_research_access <- function(project, write = FALSE) {
  S7::check_is_S7(project, bg_handle)
  if (project@closed) {
    cli::cli_abort("Cannot use a closed project.")
  }
  if (write && project@readonly) {
    cli::cli_abort("Cannot change a readonly project.")
  }
  if (write) {
    holder <- bg_read_lock_holder(project@path)
    if (!identical(holder$token, project@lock_token)) {
      cli::cli_abort("This handle no longer owns the project writer lock.")
    }
  }
}

bg_research_string <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort("{.val {field}} must be a non-empty string.")
  }
}

# Research specifications are data. Reject values that JSON would silently lose.
bg_research_data <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    length(setdiff(names(attributes(x)), "names")) > 0L ||
      is.object(x) ||
      !(is.list(x) || is.character(x) || is.numeric(x) || is.logical(x)) ||
      (!is.list(x) && (anyNA(x) || (is.numeric(x) && !all(is.finite(x)))))
  ) {
    cli::cli_abort(
      "Research records must contain plain lists, strings, finite numbers, or logical values."
    )
  }
  if (
    !is.null(names(x)) &&
      (anyNA(names(x)) || !all(nzchar(names(x))) || anyDuplicated(names(x)))
  ) {
    cli::cli_abort("Research fields must have unique, non-empty names.")
  }
  if (is.list(x)) {
    return(lapply(
      if (is.null(names(x))) x else x[order(names(x), method = "radix")],
      bg_research_data
    ))
  }
  if (length(x) != 1L || !is.null(names(x))) {
    return(bg_research_data(as.list(x)))
  }
  if (is.double(x) && x == trunc(x) && abs(x) <= .Machine$integer.max) {
    return(as.integer(x))
  }
  x
}

bg_research_fields <- function(x, required, allowed, label) {
  if (
    !is.list(x) ||
      (length(x) > 0L && is.null(names(x))) ||
      length(setdiff(required, names(x))) ||
      length(setdiff(names(x), allowed))
  ) {
    cli::cli_abort(
      "Invalid {.val {label}} fields. Required: {.val {required}}. Allowed: {.val {allowed}}."
    )
  }
}

bg_research_actor <- function(actor) {
  actor <- bg_research_data(actor)
  bg_research_fields(actor, c("id", "type"), c("id", "type"), "actor")
  bg_research_string(actor$id, "actor.id")
  bg_research_string(actor$type, "actor.type")
  if (!actor$type %in% c("human", "agent")) {
    cli::cli_abort("Actor type must be human or agent.")
  }
  actor
}

bg_research_ids <- function(ids, records, field, minimum = 1L) {
  ids <- unlist(ids, use.names = FALSE)
  if (
    !is.character(ids) ||
      anyNA(ids) ||
      length(ids) < minimum ||
      anyDuplicated(ids) ||
      !all(ids %in% names(records))
  ) {
    cli::cli_abort(
      "{.val {field}} must reference at least {minimum} distinct existing record(s)."
    )
  }
  ids
}

bg_research_candidate <- function(state, id) {
  bg_research_string(id, "candidate_id")
  candidate <- state$candidates[[id]]
  if (is.null(candidate)) {
    cli::cli_abort("Unknown candidate {.val {id}}.")
  }
  candidate
}

bg_research_closed_ancestor <- function(state, id) {
  while (!is.null(id)) {
    candidate <- bg_research_candidate(state, id)
    if (identical(candidate$status, "closed")) {
      return(id)
    }
    id <- candidate$parent_id
  }
  NULL
}

bg_research_components <- function(components, previous = list()) {
  bg_research_fields(components, character(), c("P", "A", "D"), "components")
  if (length(components) == 0L) {
    cli::cli_abort("Supply at least one P, A, or D component.")
  }
  for (key in names(components)) {
    previous[key] <- components[key]
  }
  if (is.null(previous$P)) {
    cli::cli_abort("A candidate must specify P; A and D may be absent.")
  }
  bg_research_data(Filter(Negate(is.null), previous))
}

bg_research_prepare <- function(state, action, actor, rationale) {
  action <- bg_research_data(action)
  actor <- bg_research_actor(actor)
  bg_research_string(rationale, "rationale")
  if (!is.list(action)) {
    cli::cli_abort("{.arg action} must be a named list.")
  }
  bg_research_string(action$kind, "action.kind")
  fields <- switch(
    action$kind,
    create = c("label", "components"),
    revise = c("candidate_id", "label", "components"),
    evidence = c("candidate_id", "label", "check", "result"),
    compare = c("candidate_ids", "evidence_ids", "criteria", "result"),
    review = c("candidate_id", "evidence_ids"),
    accept = c("candidate_id", "evidence_ids"),
    reject = c("candidate_id", "evidence_ids"),
    note = c("candidate_id"),
    close = c("candidate_id"),
    reopen = c("candidate_id"),
    configure = c("mode", "rules"),
    reframe = c("question", "goal"),
    cli::cli_abort("Unknown research action {.val {action$kind}}.")
  )
  optional <- if (action$kind == "evidence") "utility"
  bg_research_fields(
    action,
    c("kind", fields),
    c("kind", fields, optional, "basis"),
    "action"
  )
  if (!is.null(action$basis)) {
    bg_research_fields(
      action$basis,
      character(),
      c("evidence_ids", "comparison_ids", "decision_ids"),
      "basis"
    )
    for (field in names(action$basis)) {
      records <- switch(
        field,
        evidence_ids = state$evidence,
        comparison_ids = state$comparisons,
        decision_ids = state$decisions
      )
      bg_research_ids(action$basis[[field]], records, paste0("basis.", field))
    }
  }
  candidate <- NULL
  if ("candidate_id" %in% fields) {
    candidate <- bg_research_candidate(state, action$candidate_id)
    if (action$kind %in% c("revise", "evidence", "accept")) {
      closed <- bg_research_closed_ancestor(state, action$candidate_id)
      if (!is.null(closed)) {
        cli::cli_abort(
          "Lineage is closed at {.val {closed}}. Reopen it before continuing."
        )
      }
    }
  }
  if (action$kind %in% c("create", "revise")) {
    bg_research_string(action$label, "label")
    components <- bg_research_components(
      action$components,
      candidate$components %||% list()
    )
    if (
      action$kind == "revise" &&
        identical(components, bg_research_data(candidate$components))
    ) {
      cli::cli_abort(
        "A revision must change P, A, or D. Record a note or gather evidence instead."
      )
    }
    # The prospective candidate has no id, so it never sees parent evidence.
    candidate <- list(components = components, parent_id = candidate$id)
  }
  if (action$kind == "evidence") {
    bg_research_string(action$label, "label")
    bg_research_string(action$check, "check")
    action <- bg_research_fill_utility(action, state$vocabulary)
    bg_research_string(action$utility, "utility")
    if (is.null(action$result) || length(action$result) == 0L) {
      cli::cli_abort("Evidence must include a result.")
    }
  }
  if (action$kind %in% c("review", "accept", "reject")) {
    ids <- bg_research_ids(action$evidence_ids, state$evidence, "evidence_ids")
    if (
      !all(vapply(
        state$evidence[ids],
        function(e) identical(e$candidate_id, action$candidate_id),
        logical(1)
      ))
    ) {
      cli::cli_abort(
        "Decision evidence must belong to the specified candidate."
      )
    }
  }
  if (action$kind == "compare") {
    ids <- bg_research_ids(
      action$candidate_ids,
      state$candidates,
      "candidate_ids",
      2L
    )
    evidence <- bg_research_ids(
      action$evidence_ids,
      state$evidence,
      "evidence_ids",
      2L
    )
    subjects <- vapply(
      state$evidence[evidence],
      `[[`,
      character(1),
      "candidate_id"
    )
    if (!setequal(subjects, ids)) {
      cli::cli_abort(
        "Comparison evidence must cover exactly the selected candidates."
      )
    }
    bg_research_string(action$criteria, "criteria")
    if (is.null(action$result) || length(action$result) == 0L) {
      cli::cli_abort("A comparison must include a result.")
    }
  }
  if (action$kind == "close" && identical(candidate$status, "closed")) {
    cli::cli_abort("This candidate is already closed.")
  }
  if (action$kind == "reopen") {
    if (!identical(candidate$status, "closed")) {
      cli::cli_abort("This candidate is not closed.")
    }
    if (!is.null(bg_research_closed_ancestor(state, candidate$parent_id))) {
      cli::cli_abort(
        "Reopen the closed ancestor before reopening this candidate."
      )
    }
  }
  if (action$kind == "reframe") {
    if (!identical(actor$type, "human")) {
      cli::cli_abort("Only a human actor may reframe the investigation.")
    }
    bg_research_string(action$question, "question")
    bg_research_goal_kind(bg_research_goal(action$goal), state$vocabulary)
    if (
      identical(action$question, state$question) &&
        identical(action$goal, state$goal)
    ) {
      cli::cli_abort("Reframing must change the question or goal.")
    }
  }
  if (action$kind == "configure") {
    if (!identical(actor$type, "human")) {
      cli::cli_abort("Only a human actor may change research policy.")
    }
    policy <- bg_research_policy(action$mode, action$rules, state$goal)
  }
  findings <- if (state$mode == "record") {
    list()
  } else {
    bg_research_findings(state, action, candidate, actor)
  }
  blocking <- vapply(
    findings,
    function(f) identical(f$severity, "blocking"),
    logical(1)
  )
  proposal <- list(
    project_id = state$project_id,
    version = state$version,
    action = action,
    actor = actor,
    rationale = rationale,
    mode = state$mode,
    findings = findings,
    allowed = state$mode != "enforce" || !any(blocking)
  )
  if (action$kind == "configure") {
    proposal$policy <- policy
    proposal$policy_digest <- bg_research_policy_digest(policy)
  }
  proposal
}

bg_research_transition <- function(state, proposal) {
  a <- proposal$action
  id <- bg_new_id("research")
  stamp <- list(
    id = id,
    basis = a$basis %||% list(),
    goal_version = state$goal_version,
    actor = proposal$actor,
    rationale = proposal$rationale,
    created_at = bg_now_timestamp()
  )
  if (a$kind %in% c("create", "revise")) {
    parent <- if (a$kind == "revise") {
      state$candidates[[a$candidate_id]]
    } else {
      NULL
    }
    components <- bg_research_components(
      a$components,
      parent$components %||% list()
    )
    state$candidates[[id]] <- c(
      stamp,
      list(
        label = a$label,
        parent_id = parent$id,
        components = components,
        changed = as.list(names(a$components)[
          !vapply(
            names(a$components),
            function(k) identical(components[[k]], parent$components[[k]]),
            logical(1)
          )
        ]),
        status = "open"
      )
    )
  } else if (a$kind == "evidence") {
    state$evidence[[id]] <- c(stamp, a[setdiff(names(a), c("kind", "basis"))])
  } else if (a$kind == "compare") {
    state$comparisons[[id]] <- c(
      stamp,
      a[setdiff(names(a), c("kind", "basis"))]
    )
  } else if (a$kind %in% c("review", "accept", "reject", "note")) {
    state$decisions[[id]] <- c(stamp, a[setdiff(names(a), "basis")])
  } else if (a$kind %in% c("close", "reopen")) {
    state$candidates[[a$candidate_id]]$status <- if (a$kind == "close") {
      "closed"
    } else {
      "open"
    }
  } else if (a$kind == "reframe") {
    state$question <- a$question
    state$goal <- a$goal
    state$goal_version <- state$version + 1L
  } else if (a$kind == "configure") {
    # configure replaces the whole policy; absent fields become NULL.
    state[names(proposal$policy)] <- proposal$policy
  }
  # Any successful apply writes the current schema. Version 1 needs no content
  # migration, only the policy fields.
  state$schema_version <- bg_research_schema_version
  for (field in c("ruleset", "vocabulary", "predicates")) {
    if (!field %in% names(state)) state[field] <- list(NULL)
  }
  state$version <- state$version + 1L
  state$history[[length(state$history) + 1L]] <- c(
    stamp,
    list(
      version = state$version,
      action = a,
      mode = proposal$mode %||% state$mode,
      findings = proposal$findings
    ),
    if (a$kind == "configure") list(policy = proposal$policy)
  )
  state
}

bg_research_report <- function(state) {
  if (is.null(state)) {
    return(character())
  }
  lines <- c(
    "",
    "## Research search",
    "",
    state$question,
    "",
    paste0(
      "Intervention: ",
      state$mode,
      if (!is.null(state$ruleset)) {
        paste0(". Ruleset: ", bg_ruleset_ref(state$ruleset))
      },
      ". Research version: ",
      state$version,
      "."
    ),
    "",
    "### Candidates",
    ""
  )
  for (candidate in state$candidates) {
    lines <- c(
      lines,
      sprintf(
        "- %s (%s): %s; parent: %s; changed: %s.",
        candidate$label,
        candidate$id,
        candidate$status,
        candidate$parent_id %||% "none",
        paste(unlist(candidate$changed), collapse = ", ")
      )
    )
  }
  lines <- c(lines, "", "### Research history", "")
  for (event in state$history) {
    lines <- c(
      lines,
      sprintf(
        "- Version %s: %s by %s (%s). %s",
        event$version,
        event$action$kind,
        event$actor$id,
        event$actor$type,
        event$rationale
      )
    )
  }
  # Include exact evidence, policy, and references, not only a narrative summary.
  c(
    lines,
    "",
    "### Research records",
    "",
    "```json",
    as.character(jsonlite::toJSON(
      state,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE,
      digits = I(17)
    )),
    "```",
    ""
  )
}


bg_research_goal <- function(goal) {
  goal <- bg_research_data(goal)
  bg_research_fields(goal, character(), names(goal), "goal")
  if (length(goal) == 0L) {
    cli::cli_abort("The goal must describe the inferential goal.")
  }
  goal
}
