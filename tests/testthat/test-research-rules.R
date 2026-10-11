human <- list(id = "researcher", type = "human")
agent <- list(id = "assistant", type = "agent")

local_rule_registry <- function(env = parent.frame()) {
  bg_rule_registry_reset()
  withr::defer(bg_rule_registry_reset(), envir = env)
}
rules_fixture <- function(
  rules = list(),
  mode = "guide",
  goal = list(kind = "observable_prediction"),
  env = parent.frame()
) {
  h <- bg_init(tempfile("bg-rules-"))
  withr::defer(bg_close(h), envir = env)
  bg_research_init(h, "Predict positive measurements", goal, human, mode, rules)
  h
}
rules_propose <- function(h, kind, ..., actor = human) {
  bg_research_propose(
    h,
    c(list(kind = kind), list(...)),
    actor,
    "Investigate this possibility."
  )
}
rules_apply <- function(h, kind, ..., actor = human) {
  bg_research_apply(h, rules_propose(h, kind, ..., actor = actor))
}
rules_last <- function(state) tail(state$history, 1)[[1]]$id
rules_create <- function(h, likelihood = "normal") {
  rules_last(rules_apply(
    h,
    "create",
    label = likelihood,
    components = list(P = list(likelihood = likelihood))
  ))
}
rules_evidence <- function(h, id, check = "ppc", utility = "prediction") {
  rules_last(rules_apply(
    h,
    "evidence",
    candidate_id = id,
    label = "Check",
    check = check,
    utility = utility,
    result = list(p = 0.1)
  ))
}
review_rule <- function(id = "review-ppc", ...) {
  bg_rule(
    id = id,
    on = "accept",
    require = list(type = "require_review", check = "ppc"),
    message = "Review the predictive check.",
    ...
  )
}

test_that("bg_rule validates the raw rule grammar", {
  rule <- review_rule(references = "Book, chapter 7")
  expect_equal(rule$on, list("accept"))
  expect_equal(rule$references, list("Book, chapter 7"))
  expect_null(rule$severity)
  base <- list(
    id = "r",
    on = "accept",
    require = list(type = "require_review", check = "ppc"),
    message = "m"
  )
  bad <- list(
    list(id = "Bad Id"),
    list(id = "vocabulary"),
    list(on = "configure"),
    list(on = character()),
    list(require = list(type = "unknown")),
    list(require = list(type = "require_review")),
    list(require = list(type = "require_review", check = "ppc", extra = 1)),
    list(
      require = list(
        type = "require_evidence",
        check = "x",
        provenance = "asserted"
      )
    ),
    list(require = list(type = "forbid_values", path = "X", values = "a")),
    list(require = list(type = "goal_kind", values = list())),
    list(severity = "fatal"),
    list(when = list(type = "goal_kind", values = "x")),
    list(message = " ")
  )
  for (change in bad) {
    args <- base
    args[names(change)] <- change
    expect_error(do.call(bg_rule, args))
  }
  expect_error(do.call(bg_rule, base)$id, NA)
  expect_error(
    bg_rule_list(list(c(base, list(extra = TRUE))), "raw"),
    "Invalid"
  )
  # Rule lists passed to the policy reject resolved rules.
  resolved <- c(base, list(id = "lab.x/r", source = "lab.x@1.0.0"))
  resolved <- resolved[!duplicated(names(resolved), fromLast = TRUE)]
  expect_error(bg_research_policy("guide", list(resolved)), "wrong shape")
})

test_that("legacy and raw rule lists are stored as supplied and stay blocking", {
  local_rule_registry()
  legacy <- list(
    id = "Legacy rule with spaces",
    actions = "accept",
    type = "require_review",
    check = "ppc",
    message = "Review first."
  )
  raw <- bg_rule(
    id = "advice",
    on = "accept",
    require = list(type = "require_evidence", check = "prior"),
    severity = "advisory",
    message = "Consider a prior check."
  )
  h <- rules_fixture(list(legacy, raw), mode = "enforce")
  state <- bg_research_state(h)
  expect_identical(state$schema_version, 2L)
  expect_identical(state$rules[[1]], bg_research_data(legacy))
  expect_null(state$ruleset)
  expect_null(state$vocabulary)
  expect_null(state$predicates)
  id <- rules_create(h)
  ev <- rules_evidence(h, id)
  proposal <- rules_propose(h, "accept", candidate_id = id, evidence_ids = ev)
  expect_false(proposal$allowed)
  expect_named(proposal$findings, c("Legacy rule with spaces", "advice"))
  expect_equal(proposal$findings[[1]]$severity, "blocking")
  expect_equal(proposal$findings[[2]]$severity, "advisory")
  rules_apply(h, "review", candidate_id = id, evidence_ids = ev)
  # Only the advisory finding remains, so Enforce allows the action.
  proposal <- rules_propose(h, "accept", candidate_id = id, evidence_ids = ev)
  expect_true(proposal$allowed)
  expect_named(proposal$findings, "advice")
  state <- bg_research_apply(h, proposal)
  expect_equal(tail(state$history, 1)[[1]]$findings$advice$severity, "advisory")
})

test_that("held messages name only blocking findings", {
  rules <- list(
    review_rule(),
    review_rule("soft", severity = "advisory", interpretation = "Optional.")
  )
  h <- rules_fixture(rules, mode = "enforce")
  id <- rules_create(h)
  ev <- rules_evidence(h, id)
  proposal <- rules_propose(h, "accept", candidate_id = id, evidence_ids = ev)
  expect_length(proposal$findings, 2)
  error <- tryCatch(bg_research_apply(h, proposal), error = identity)
  expect_match(conditionMessage(error), "held by policy")
  expect_equal(
    lengths(regmatches(
      conditionMessage(error),
      gregexpr("Review the predictive check", conditionMessage(error))
    )),
    1L
  )
})

test_that("predicate failures in when or require produce findings, never skips", {
  local_rule_registry()
  bg_register_rule_predicate("lab.boom", function(subject, args) stop("bad"))
  bg_register_rule_predicate("lab.na", function(subject, args) NA)
  bg_register_rule_predicate("lab.two", function(subject, args) c(TRUE, TRUE))
  bg_register_rule_predicate("lab.never", function(subject, args) FALSE)
  bg_register_rule_predicate("lab.gone", function(subject, args) TRUE)
  rule <- function(id, when) {
    bg_rule(
      id = id,
      on = "note",
      when = list(list(type = when)),
      require = list(type = "actor_type", values = "human"),
      message = paste("Rule", id)
    )
  }
  rules <- list(
    rule("boom", "lab.boom"),
    rule("na", "lab.na"),
    rule("two", "lab.two"),
    rule("never", "lab.never"),
    rule("gone", "lab.gone"),
    bg_rule(
      id = "require-fails",
      on = "note",
      require = list(type = "lab.boom"),
      message = "Requirement failed."
    )
  )
  h <- rules_fixture(rules, mode = "enforce")
  expect_equal(
    bg_rules(h)$predicates,
    lapply(
      c("lab.boom", "lab.gone", "lab.na", "lab.never", "lab.two"),
      function(id) list(id = id, version = "1")
    )
  )
  id <- rules_create(h)
  .bg_rules_env$predicates[["lab.gone"]] <- NULL
  proposal <- rules_propose(h, "note", candidate_id = id, actor = agent)
  # A FALSE `when` skips the rule; every failure is a blocking finding.
  expect_named(
    proposal$findings,
    c("boom", "na", "two", "gone", "require-fails")
  )
  expect_false(proposal$allowed)
  expect_match(proposal$findings$boom$failures[[1]], "raised an error: bad")
  expect_match(proposal$findings$na$failures[[1]], "did not return TRUE")
  expect_match(proposal$findings$two$failures[[1]], "did not return TRUE")
  expect_match(proposal$findings$gone$failures[[1]], "not registered")
  expect_match(
    proposal$findings$`require-fails`$failures[[1]],
    "raised an error"
  )
})

test_that("predicates receive the subject and pinned versions are enforced", {
  local_rule_registry()
  seen <- new.env()
  bg_register_rule_predicate(
    "lab.label",
    function(subject, args) {
      seen$subject <- subject
      identical(subject$action$label, args$label)
    },
    arguments = "label"
  )
  rule <- bg_rule(
    id = "label",
    on = "create",
    require = list(type = "lab.label", label = "Approved"),
    message = "Use the approved label."
  )
  h <- rules_fixture(list(rule), mode = "enforce")
  proposal <- rules_propose(
    h,
    "create",
    label = "Other",
    components = list(P = list(likelihood = "normal"))
  )
  expect_false(proposal$allowed)
  expect_named(seen$subject, c("state", "action", "candidate", "actor"))
  expect_null(seen$subject$candidate$id)
  expect_equal(seen$subject$candidate$components$P$likelihood, "normal")
  expect_equal(seen$subject$actor, human)
  expect_equal(seen$subject$state$version, 1L)
  ok <- rules_propose(
    h,
    "create",
    label = "Approved",
    components = list(P = list(likelihood = "normal"))
  )
  expect_true(ok$allowed)
  expect_error(
    bg_register_rule_predicate("lab.label", function(subject, args) TRUE),
    "already registered"
  )
  bg_register_rule_predicate(
    "lab.label",
    function(subject, args) TRUE,
    arguments = "label",
    version = "2",
    replace = TRUE
  )
  # The state keeps the pinned version; ordinary actions never re-pin.
  mismatch <- rules_propose(
    h,
    "create",
    label = "Approved",
    components = list(P = list(likelihood = "normal"))
  )
  expect_false(mismatch$allowed)
  expect_match(mismatch$findings$label$failures[[1]], "pins version '1'")
  rules_apply(h, "configure", mode = "enforce", rules = list(rule))
  expect_equal(bg_rules(h)$predicates[[1]]$version, "2")
  expect_true(
    rules_propose(
      h,
      "create",
      label = "Anything",
      components = list(P = list(likelihood = "normal"))
    )$allowed
  )
})

test_that("custom predicate registration and use are validated", {
  local_rule_registry()
  expect_error(
    bg_register_rule_predicate("nodot", function(subject, args) TRUE),
    "dotted"
  )
  expect_error(bg_register_rule_predicate("lab.x", "TRUE"), "function")
  expect_error(
    bg_register_rule_predicate("lab.x", identity, arguments = "type"),
    "must not be type"
  )
  bg_register_rule_predicate(
    "lab.x",
    function(subject, args) TRUE,
    arguments = "a"
  )
  h <- rules_fixture()
  unknown <- bg_rule(
    id = "u",
    on = "note",
    require = list(type = "lab.missing"),
    message = "m"
  )
  bad_args <- bg_rule(
    id = "b",
    on = "note",
    require = list(type = "lab.x", b = 1),
    message = "m"
  )
  before <- bg_research_state(h)
  expect_error(
    rules_propose(h, "configure", mode = "guide", rules = list(unknown)),
    "unavailable predicates"
  )
  expect_error(
    rules_propose(h, "configure", mode = "guide", rules = list(bad_args)),
    "unavailable predicates"
  )
  expect_identical(bg_research_state(h), before)
})

test_that("compare decides applicability per candidate", {
  rule <- bg_rule(
    id = "review-lognormal",
    on = "compare",
    when = list(list(
      type = "candidate_values",
      path = c("P", "likelihood"),
      values = "lognormal"
    )),
    require = list(type = "require_review", check = "ppc"),
    message = "Review lognormal candidates before comparing."
  )
  h <- rules_fixture(list(rule), mode = "enforce")
  normal <- rules_create(h, "normal")
  lognormal <- rules_create(h, "lognormal")
  e1 <- rules_evidence(h, normal)
  e2 <- rules_evidence(h, lognormal)
  compare <- function() {
    rules_propose(
      h,
      "compare",
      candidate_ids = c(normal, lognormal),
      evidence_ids = c(e1, e2),
      criteria = "Prediction",
      result = list(preferred = lognormal)
    )
  }
  proposal <- compare()
  expect_false(proposal$allowed)
  expect_length(proposal$findings, 1)
  expect_equal(proposal$findings[[1]]$candidate_ids, list(lognormal))
  # The normal candidate is unreviewed but the rule does not apply to it.
  rules_apply(h, "review", candidate_id = lognormal, evidence_ids = e2)
  expect_true(compare()$allowed)
  expect_length(compare()$findings, 0)
})

test_that("built-in predicates cover goal, actor, values, and evidence", {
  rules <- list(
    bg_rule(
      id = "latent-only",
      on = "note",
      when = list(list(type = "goal_kind", values = "latent_inference")),
      require = list(type = "actor_type", values = "human"),
      message = "Latent goals need human notes."
    ),
    bg_rule(
      id = "humans",
      on = "note",
      require = list(type = "actor_type", values = "human"),
      message = "Only humans take notes."
    ),
    bg_rule(
      id = "no-normal",
      on = "create",
      require = list(
        type = "forbid_values",
        path = c("P", "likelihood"),
        values = "normal"
      ),
      message = "Avoid normal."
    ),
    bg_rule(
      id = "utility",
      on = "accept",
      require = list(type = "require_utility_evidence", utility = "speed"),
      message = "Record speed evidence."
    ),
    bg_rule(
      id = "derived",
      on = "accept",
      require = list(
        type = "require_evidence",
        check = "ppc",
        provenance = "derived"
      ),
      message = "Derive the predictive check from a run."
    )
  )
  h <- rules_fixture(rules)
  expect_named(
    rules_propose(
      h,
      "create",
      label = "N",
      components = list(P = list(likelihood = "normal"))
    )$findings,
    "no-normal"
  )
  id <- rules_create(h, "lognormal")
  expect_named(
    rules_propose(h, "note", candidate_id = id, actor = agent)$findings,
    "humans"
  )
  ev <- rules_evidence(h, id, utility = "speed")
  # Asserted evidence never satisfies a derived-provenance requirement.
  expect_named(
    rules_propose(h, "accept", candidate_id = id, evidence_ids = ev)$findings,
    "derived"
  )
})

test_that("evidence requirements never see a parent's evidence on revise", {
  rule <- bg_rule(
    id = "ev",
    on = "revise",
    require = list(type = "require_evidence", check = "ppc"),
    message = "m"
  )
  h <- rules_fixture(list(rule))
  id <- rules_create(h)
  rules_evidence(h, id)
  proposal <- rules_propose(
    h,
    "revise",
    candidate_id = id,
    label = "Child",
    components = list(P = list(likelihood = "lognormal"))
  )
  expect_named(proposal$findings, "ev")
})

lab_base <- function(version = "1.0.0", message = "Review first.") {
  bg_ruleset(
    "lab.base",
    version,
    title = "Base",
    rules = list(
      bg_rule(
        id = "review",
        on = "accept",
        require = list(type = "require_review", check = "ppc"),
        message = message,
        interpretation = "Look closely."
      ),
      bg_rule(
        id = "evidence",
        on = "accept",
        require = list(type = "require_evidence", check = "prior"),
        message = "Collect prior evidence."
      )
    )
  )
}

test_that("ruleset resolution concatenates, disables, overrides, and appends", {
  local_rule_registry()
  bg_register_ruleset(lab_base())
  bg_register_ruleset(bg_ruleset(
    "lab.extra",
    "2.1.0",
    rules = list(bg_rule(
      id = "humans",
      on = "note",
      require = list(type = "actor_type", values = "human"),
      message = "Humans only."
    ))
  ))
  child <- bg_ruleset(
    "lab.child",
    "0.1.0",
    extends = c("lab.base@1", "lab.extra@2"),
    disable = "lab.base/evidence",
    override = list(
      "lab.base/review" = list(
        severity = "advisory",
        interpretation = NULL
      )
    ),
    rules = list(review_rule("own"))
  )
  policy <- bg_research_policy("guide", child)
  ids <- vapply(policy$rules, `[[`, "", "id")
  expect_equal(ids, c("lab.base/review", "lab.extra/humans", "lab.child/own"))
  review <- policy$rules[[1]]
  expect_equal(review$source, "lab.base@1.0.0")
  expect_equal(review$overridden_by, list("lab.child@0.1.0"))
  expect_equal(review$severity, "advisory")
  expect_false("interpretation" %in% names(review))
  expect_equal(policy$rules[[3]]$source, "lab.child@0.1.0")
  expect_equal(
    vapply(policy$ruleset$sources, function(s) s$id, ""),
    c("lab.base", "lab.extra", "lab.child")
  )
  expect_match(policy$ruleset$sources[[1]]$digest, "^[0-9a-f]{64}$")
  expect_equal(
    policy$ruleset$policy_digest,
    bg_research_policy_digest(policy)
  )
  bad <- list(
    list(disable = "lab.base/missing"),
    list(override = list("lab.base/missing" = list(severity = "advisory"))),
    list(override = list("lab.base/review" = list(message = NULL))),
    list(override = list("lab.base/review" = list(id = "x"))),
    list(override = list("lab.base/review" = list(severity = "fatal"))),
    list(
      disable = "lab.base/evidence",
      override = list(
        "lab.base/evidence" = list(severity = "advisory")
      )
    )
  )
  for (change in bad) {
    expect_error(bg_research_policy(
      "guide",
      do.call(
        bg_ruleset,
        c(list("lab.bad", "1.0.0", extends = "lab.base@1"), change)
      )
    ))
  }
})

test_that("diamond inheritance coalesces and conflicting duplicates fail", {
  local_rule_registry()
  bg_register_ruleset(lab_base())
  bg_register_ruleset(bg_ruleset("lab.left", "1.0.0", extends = "lab.base@1"))
  bg_register_ruleset(bg_ruleset("lab.right", "1.0.0", extends = "lab.base@1"))
  diamond <- bg_ruleset(
    "lab.top",
    "1.0.0",
    extends = c("lab.left@1", "lab.right@1")
  )
  policy <- bg_research_policy("guide", diamond)
  expect_length(policy$rules, 2)
  expect_equal(
    vapply(policy$ruleset$sources, function(s) s$id, ""),
    c("lab.base", "lab.left", "lab.right", "lab.top")
  )
  # A parent that changed an inherited rule conflicts with the original.
  bg_register_ruleset(bg_ruleset(
    "lab.changed",
    "1.0.0",
    extends = "lab.base@1",
    override = list("lab.base/review" = list(severity = "advisory"))
  ))
  expect_error(
    bg_research_policy(
      "guide",
      bg_ruleset("lab.top", "1.0.1", extends = c("lab.left@1", "lab.changed@1"))
    ),
    "conflicting"
  )
  # The same rule from two versions of a ruleset has two sources.
  bg_register_ruleset(lab_base("1.1.0"))
  expect_error(
    bg_research_policy(
      "guide",
      bg_ruleset(
        "lab.top",
        "1.0.2",
        extends = c("lab.base@1.0.0", "lab.base@1.1.0")
      )
    ),
    "conflicting"
  )
})

test_that("cycles and unknown parents are errors", {
  local_rule_registry()
  bg_register_ruleset(bg_ruleset("lab.a", "1.0.0", extends = "lab.b@1"))
  bg_register_ruleset(bg_ruleset("lab.b", "1.0.0", extends = "lab.a@1"))
  expect_error(bg_research_policy("guide", "lab.a@1"), "cycle")
  expect_error(bg_research_policy("guide", "lab.none@1"), "Unknown ruleset")
  expect_error(
    bg_research_policy(
      "guide",
      bg_ruleset("lab.c", "1.0.0", extends = "lab.none@1")
    ),
    "Unknown ruleset"
  )
  expect_error(bg_research_policy("guide", "not a reference"), "must be")
})

test_that("version selection takes the highest stable version of a major", {
  local_rule_registry()
  for (version in c("1.2.0", "1.10.0", "1.11.0-rc.1", "2.0.0-beta")) {
    bg_register_ruleset(lab_base(version, message = version))
  }
  expect_equal(bg_ruleset_lookup("lab.base@1")$version, "1.10.0")
  expect_equal(bg_ruleset_lookup("lab.base@1.2.0")$version, "1.2.0")
  expect_equal(bg_ruleset_lookup("lab.base@2.0.0-beta")$version, "2.0.0-beta")
  expect_error(bg_ruleset_lookup("lab.base@2"), "Unknown ruleset")
  expect_error(bg_ruleset_lookup("lab.base@1.3.0"), "Unknown ruleset")
  expect_error(
    bg_register_ruleset(lab_base("1.2.0", message = "changed")),
    "different content"
  )
  expect_no_error(bg_register_ruleset(lab_base("1.2.0", message = "1.2.0")))
  listed <- bg_rulesets()
  expect_equal(nrow(listed), 4)
  expect_true(all(listed$origin == "session"))
  expect_named(listed, c("id", "version", "title", "origin", "digest"))
  # An inline ruleset may not shadow a registered one with other content.
  expect_error(
    bg_research_policy("guide", lab_base("1.2.0", message = "other")),
    "differs from the registered"
  )
})

test_that("rulesets validate their own fields", {
  expect_error(bg_ruleset("nodot", "1.0.0"), "dotted")
  expect_error(bg_ruleset("lab.x", "1.0"), "MAJOR")
  expect_error(bg_ruleset("lab.x", "1.0.0", extends = "lab.y"), "must be")
  expect_error(bg_ruleset("lab.x", "1.0.0", disable = "review"), "qualified")
  expect_error(
    bg_ruleset("lab.x", "1.0.0", rules = list(review_rule(), review_rule())),
    "unique"
  )
  expect_error(
    bg_ruleset("lab.x", "1.0.0", vocabulary = list(utilities = list())),
    "non-empty"
  )
  expect_error(
    bg_ruleset("lab.x", "1.0.0", vocabulary = list(terms = list())),
    "Invalid"
  )
  expect_error(
    bg_ruleset(
      "lab.x",
      "1.0.0",
      rules = list(list(
        id = "legacy",
        actions = "accept",
        type = "require_review",
        check = "ppc",
        message = "m"
      ))
    ),
    "wrong shape"
  )
  # Empty optional fields are dropped, so R and JSON forms share a digest.
  ruleset <- bg_ruleset("lab.x", "1.0.0", rules = list(review_rule()))
  expect_named(ruleset, c("id", "rules", "version"))
  json <- jsonlite::parse_json(bg_canonical_json(ruleset))
  expect_identical(
    bg_canonical_digest(bg_ruleset_normalize(json)),
    bg_canonical_digest(ruleset)
  )
})

workflow_vocabulary <- function() {
  list(
    utilities = list(
      list(id = "prediction", title = "Predictive performance"),
      list(id = "convergence")
    ),
    goal_kinds = list(
      list(id = "observable_prediction"),
      list(id = "latent_inference")
    ),
    checks = list(
      list(id = "ppc", utilities = "prediction"),
      list(id = "rhat", utilities = "convergence"),
      list(id = "loo", utilities = c("prediction", "convergence"))
    )
  )
}

test_that("vocabularies merge along the resolution order", {
  local_rule_registry()
  bg_register_ruleset(bg_ruleset(
    "lab.vocab",
    "1.0.0",
    vocabulary = workflow_vocabulary()
  ))
  child <- bg_ruleset(
    "lab.child",
    "1.0.0",
    extends = "lab.vocab@1",
    vocabulary = list(
      utilities = list(list(id = "prediction", title = "Prediction")),
      checks = list(list(id = "sbc", utilities = "convergence"))
    )
  )
  vocabulary <- bg_research_policy("guide", child)$vocabulary
  expect_equal(
    vocabulary$utilities[[1]],
    list(id = "prediction", title = "Prediction")
  )
  expect_equal(
    vapply(vocabulary$checks, `[[`, "", "id"),
    c("ppc", "rhat", "loo", "sbc")
  )
  expect_error(
    bg_research_policy(
      "guide",
      bg_ruleset(
        "lab.bad",
        "1.0.0",
        extends = "lab.vocab@1",
        vocabulary = list(checks = list(list(id = "x", utilities = "speed")))
      )
    ),
    "undeclared utilities"
  )
})

test_that("declared goal kinds are hard errors at init, reframe, and configure", {
  local_rule_registry()
  vocab <- bg_ruleset("lab.vocab", "1.0.0", vocabulary = workflow_vocabulary())
  bg_register_ruleset(vocab)
  h <- bg_init(tempfile("bg-rules-"))
  withr::defer(bg_close(h))
  expect_error(
    bg_research_init(h, "Q", list(kind = "other"), human, "record", vocab),
    "goal kind"
  )
  expect_error(
    bg_research_init(h, "Q", list(focus = "none"), human, "record", vocab),
    "goal kind"
  )
  expect_null(bg_research_state(h))
  bg_research_init(
    h,
    "Q",
    list(kind = "latent_inference"),
    human,
    "record",
    "lab.vocab@1"
  )
  expect_error(
    rules_propose(h, "reframe", question = "Q2", goal = list(kind = "other")),
    "goal kind"
  )
  rules_apply(h, "configure", mode = "record", rules = list())
  rules_apply(h, "reframe", question = "Q2", goal = list(kind = "other"))
  expect_error(
    rules_propose(h, "configure", mode = "record", rules = "lab.vocab@1"),
    "goal kind"
  )
})

test_that("the implicit vocabulary rule follows the mode", {
  local_rule_registry()
  bg_register_ruleset(bg_ruleset(
    "lab.vocab",
    "1.0.0",
    vocabulary = workflow_vocabulary()
  ))
  h <- rules_fixture("lab.vocab@1", mode = "guide")
  id <- rules_create(h)
  evidence <- function(check, utility = NULL) {
    action <- list(
      candidate_id = id,
      label = "Check",
      check = check,
      result = list(ok = TRUE)
    )
    action$utility <- utility
    do.call(rules_propose, c(list(h, "evidence"), action))
  }
  guide <- evidence("ppc", "convergence")
  expect_true(guide$allowed)
  finding <- guide$findings[["lab.vocab/vocabulary"]]
  expect_equal(finding$severity, "advisory")
  expect_equal(finding$source, "lab.vocab@1.0.0")
  expect_match(finding$explanation, "informs 'prediction', not 'convergence'")
  expect_match(
    evidence("waic", "speed")$findings[[1]]$explanation,
    "Utility 'speed' is not declared. Check 'waic' is not declared."
  )
  # A check that maps to exactly one utility fills it in.
  filled <- evidence("rhat")
  expect_equal(filled$action$utility, "convergence")
  expect_length(filled$findings, 0)
  state <- bg_research_apply(h, filled)
  expect_equal(state$evidence[[rules_last(state)]]$utility, "convergence")
  expect_error(evidence("loo"), "utility")
  expect_error(evidence("unknown"), "utility")
  rules_apply(h, "configure", mode = "enforce", rules = "lab.vocab@1")
  enforce <- evidence("ppc", "convergence")
  expect_false(enforce$allowed)
  expect_equal(enforce$findings[[1]]$severity, "blocking")
  rules_apply(h, "configure", mode = "record", rules = "lab.vocab@1")
  expect_length(evidence("ppc", "convergence")$findings, 0)
})

test_that("configure pins and replaces the whole policy and records it", {
  local_rule_registry()
  bg_register_ruleset(bg_ruleset(
    "lab.vocab",
    "1.0.0",
    title = "Vocabulary",
    vocabulary = workflow_vocabulary(),
    rules = list(review_rule())
  ))
  h <- rules_fixture("lab.vocab@1")
  state <- bg_research_state(h)
  first <- state$history[[1]]
  expect_equal(first$action$rules, "lab.vocab@1")
  expect_equal(first$policy$ruleset$id, "lab.vocab")
  expect_equal(first$policy$rules, state$rules)
  expect_equal(state$ruleset$title, "Vocabulary")
  expect_equal(state$rules[[1]]$id, "lab.vocab/review-ppc")
  active <- bg_rules(h)
  expect_named(
    active,
    c("mode", "ruleset", "rules", "vocabulary", "predicates")
  )
  expect_equal(
    active$ruleset$policy_digest,
    bg_research_policy_digest(active)
  )
  # Ordinary actions use the stored policy, not the live registry.
  bg_rule_registry_reset()
  id <- rules_create(h)
  ev <- rules_evidence(h, id)
  expect_named(
    rules_propose(h, "accept", candidate_id = id, evidence_ids = ev)$findings,
    "lab.vocab/review-ppc"
  )
  expect_error(
    bg_use_rules(h, "lab.vocab@1", actor = human, rationale = "Again."),
    "Unknown ruleset"
  )
  expect_error(
    bg_use_rules(h, list(), actor = agent, rationale = "Drop rules."),
    "Only a human"
  )
  goal_version <- bg_research_state(h)$goal_version
  state <- bg_use_rules(h, list(), actor = human, rationale = "Drop rules.")
  expect_equal(state$mode, "guide")
  expect_equal(state$rules, list())
  expect_null(state$ruleset)
  expect_null(state$vocabulary)
  expect_identical(state$goal_version, goal_version)
  entry <- tail(state$history, 1)[[1]]
  expect_equal(entry$action$rules, list())
  expect_null(entry$policy$ruleset)
  expect_true("ruleset" %in% names(entry$policy))
})

test_that("a registry change between propose and apply makes a configure stale", {
  local_rule_registry()
  bg_register_ruleset(lab_base("1.0.0"))
  h <- rules_fixture()
  proposal <- rules_propose(
    h,
    "configure",
    mode = "guide",
    rules = "lab.base@1"
  )
  expect_equal(proposal$policy$ruleset$version, "1.0.0")
  expect_equal(
    proposal$policy_digest,
    proposal$policy$ruleset$policy_digest
  )
  bg_register_ruleset(lab_base("1.1.0", message = "Newer message."))
  before <- bg_research_state(h)
  expect_error(bg_research_apply(h, proposal), "Stale")
  expect_identical(bg_research_state(h), before)
  # A changed custom predicate version also changes the pinned policy.
  bg_register_rule_predicate("lab.p", function(subject, args) TRUE)
  rule <- bg_rule(
    id = "p",
    on = "note",
    require = list(type = "lab.p"),
    message = "m"
  )
  proposal <- rules_propose(h, "configure", mode = "guide", rules = list(rule))
  bg_register_rule_predicate(
    "lab.p",
    function(subject, args) TRUE,
    version = "2",
    replace = TRUE
  )
  expect_error(bg_research_apply(h, proposal), "Stale")
  proposal <- rules_propose(
    h,
    "configure",
    mode = "guide",
    rules = "lab.base@1"
  )
  state <- bg_research_apply(h, proposal)
  expect_equal(state$ruleset$version, "1.1.0")
  expect_identical(state$ruleset$policy_digest, proposal$policy_digest)
})

test_that("schema version 1 states are read and upgraded on apply", {
  h <- rules_fixture(list(list(
    id = "legacy",
    actions = "accept",
    type = "require_review",
    check = "ppc",
    message = "Review first."
  )))
  state <- bg_research_state(h)
  v1 <- state
  v1$schema_version <- 1L
  v1$ruleset <- NULL
  v1$vocabulary <- NULL
  v1$predicates <- NULL
  v1$history[[1]]$policy <- NULL
  bg_write_json_atomic(bg_research_path(h), v1, sort_keys = FALSE)
  expect_identical(bg_research_state(h), v1)
  expect_null(bg_rules(h)$ruleset)
  state <- rules_apply(
    h,
    "create",
    label = "N",
    components = list(P = list(likelihood = "normal"))
  )
  expect_identical(state$schema_version, 2L)
  expect_true(all(c("ruleset", "vocabulary", "predicates") %in% names(state)))
  expect_identical(state$rules, v1$rules)
  # Version 1 never stored new rule shapes.
  v1$rules <- list(review_rule())
  bg_write_json_atomic(bg_research_path(h), v1, sort_keys = FALSE)
  expect_error(bg_research_state(h), "Malformed")
  v1$schema_version <- 3L
  v1$rules <- list()
  bg_write_json_atomic(bg_research_path(h), v1, sort_keys = FALSE)
  expect_error(bg_research_state(h), "Malformed")
})

test_that("reading state validates stored rule structure", {
  local_rule_registry()
  bg_register_ruleset(lab_base())
  h <- rules_fixture("lab.base@1")
  original <- bg_research_state(h)
  path <- bg_research_path(h)
  mutations <- list(
    function(s) {
      s$rules[[1]]$severity <- "fatal"
      s
    },
    function(s) {
      s$rules[[1]]$source <- NULL
      s
    },
    function(s) {
      s$rules[[1]]$id <- "review"
      s
    },
    function(s) {
      s$rules[[1]]$require <- list(type = "unknown")
      s
    },
    function(s) {
      s$ruleset$sources <- list()
      s
    },
    function(s) {
      s$ruleset <- NULL
      s
    },
    function(s) {
      s$predicates <- list(list(id = "nodot", version = "1"))
      s
    }
  )
  for (mutate in mutations) {
    bg_write_json_atomic(path, mutate(original), sort_keys = FALSE)
    expect_error(bg_research_state(h), "Malformed")
  }
  # Unknown custom predicates are only a problem at evaluation.
  state <- original
  state$rules[[1]]$require <- list(type = "lab.unregistered")
  bg_write_json_atomic(path, state, sort_keys = FALSE)
  expect_no_error(bg_research_state(h))
})

test_that("bg_ruleset_check reports errors, warnings, and the rule count", {
  local_rule_registry()
  bg_register_ruleset(bg_ruleset(
    "lab.vocab",
    "1.0.0",
    vocabulary = workflow_vocabulary()
  ))
  good <- bg_ruleset(
    "lab.good",
    "1.0.0",
    extends = "lab.vocab@1",
    rules = list(review_rule())
  )
  report <- bg_ruleset_check(good)
  expect_true(report$ok)
  expect_equal(report$rule_count, 1L)
  expect_equal(report$errors, character())
  expect_equal(report$warnings, character())
  noisy <- bg_ruleset(
    "lab.noisy",
    "1.0.0",
    extends = "lab.vocab@1",
    rules = list(
      bg_rule(
        id = "unknown-check",
        on = "accept",
        require = list(type = "require_evidence", check = "waic"),
        message = "m"
      ),
      bg_rule(
        id = "never",
        on = "accept",
        when = list(list(type = "goal_kind", values = "causal")),
        require = list(type = "require_review", check = "ppc"),
        message = "m"
      ),
      bg_rule(
        id = "prospective",
        on = "create",
        require = list(type = "require_utility_evidence", utility = "speed"),
        message = "m"
      ),
      bg_rule(
        id = "custom",
        on = "note",
        require = list(type = "lab.unknown"),
        message = "m"
      )
    )
  )
  report <- bg_ruleset_check(noisy)
  expect_false(report$ok)
  expect_equal(report$rule_count, 4L)
  expect_match(report$errors, "unknown predicate 'lab.unknown'")
  expect_true(any(grepl("check 'waic'", report$warnings)))
  expect_true(any(grepl("/never' can never apply", report$warnings)))
  expect_true(any(grepl("utility 'speed'", report$warnings)))
  expect_true(any(grepl(
    "/prospective' can never be satisfied",
    report$warnings
  )))
  missing <- bg_ruleset_check("lab.missing@1")
  expect_false(missing$ok)
  expect_match(missing$errors, "Unknown ruleset")
  expect_equal(missing$rule_count, 0L)
  expect_true(bg_ruleset_check("lab.vocab@1")$ok)
})

test_that("built-in rulesets load from inst/rulesets and tolerate none", {
  local_rule_registry()
  dir <- withr::local_tempdir()
  local_mocked_bindings(bg_protocol_asset_path = function(...) "")
  expect_equal(nrow(bg_rulesets()), 0)
  bg_rule_registry_reset()
  jsonlite::write_json(
    lab_base(),
    file.path(dir, "lab.base.json"),
    auto_unbox = TRUE,
    null = "null"
  )
  local_mocked_bindings(bg_protocol_asset_path = function(...) dir)
  listed <- bg_rulesets()
  expect_equal(listed$id, "lab.base")
  expect_equal(listed$origin, "builtin")
  expect_equal(listed$digest, bg_canonical_digest(lab_base()))
  expect_no_error(bg_register_ruleset(lab_base()))
  expect_error(
    bg_register_ruleset(lab_base(message = "changed")),
    "different content"
  )
  bg_rule_registry_reset()
  file.rename(file.path(dir, "lab.base.json"), file.path(dir, "wrong.json"))
  expect_error(bg_rulesets(), "<id>.json")
})

test_that("rule messages remain literal data in the policy", {
  marker <- tempfile("rule-message-")
  rule <- review_rule(severity = "blocking")
  rule$message <- paste0("Review {file.create('", marker, "')} first.")
  h <- rules_fixture(list(rule), mode = "enforce")
  id <- rules_create(h)
  ev <- rules_evidence(h, id)
  error <- tryCatch(
    rules_apply(h, "accept", candidate_id = id, evidence_ids = ev),
    error = identity
  )
  expect_match(conditionMessage(error), rule$message, fixed = TRUE)
  expect_false(file.exists(marker))
})
