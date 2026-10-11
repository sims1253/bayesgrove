official_ref <- "bayesgrove.workflow@1"
official_human <- list(id = "researcher", type = "human")

official_utilities <- c(
  "causal_consistency",
  "parameter_recoverability",
  "predictive_performance",
  "fairness",
  "structural_faithfulness",
  "parsimony",
  "interpretability",
  "convergence",
  "estimation_speed",
  "robustness"
)

# The predicates any third-party ruleset can rely on without registering code.
official_builtin_predicates <- c(
  "goal_kind",
  "actor_type",
  "candidate_values",
  "require_values",
  "forbid_values",
  "require_evidence",
  "require_utility_evidence",
  "require_review"
)

official_local_registry <- function(env = parent.frame()) {
  bg_rule_registry_reset()
  withr::defer(bg_rule_registry_reset(), envir = env)
}

official_json <- function() {
  path <- system.file(
    "rulesets",
    "bayesgrove.workflow.json",
    package = "bayesgrove"
  )
  expect_true(nzchar(path))
  jsonlite::read_json(path, simplifyVector = FALSE)
}

official_ids <- function(entries) vapply(entries, `[[`, character(1), "id")

official_fixture <- function(
  goal_kind = "observable_prediction",
  mode = "guide",
  rules = official_ref,
  env = parent.frame()
) {
  h <- bg_init(tempfile("bg-official-"))
  withr::defer(bg_close(h), envir = env)
  bg_research_init(
    h,
    "Which model answers the question?",
    list(kind = goal_kind),
    official_human,
    mode,
    rules
  )
  h
}

official_propose <- function(h, kind, ...) {
  bg_research_propose(
    h,
    c(list(kind = kind), list(...)),
    official_human,
    "Investigate this possibility."
  )
}

official_apply <- function(h, kind, ...) {
  state <- bg_research_apply(h, official_propose(h, kind, ...))
  tail(state$history, 1)[[1]]$id
}

official_candidate <- function(h, label = "normal") {
  official_apply(
    h,
    "create",
    label = label,
    components = list(P = list(likelihood = label))
  )
}

# Asserted evidence; derived evidence needs a run-bound candidate.
official_evidence <- function(h, id, check, utility = NULL) {
  action <- list(
    candidate_id = id,
    label = check,
    check = check,
    result = list(source = "test")
  )
  action$utility <- utility
  do.call(official_apply, c(list(h, "evidence"), action))
}

official_finding_ids <- function(proposal) {
  sub("^bayesgrove\\.workflow/", "", names(proposal$findings))
}

official_severities <- function(proposal) {
  severities <- vapply(proposal$findings, `[[`, character(1), "severity")
  stats::setNames(severities, official_finding_ids(proposal))
}

test_that("the official ruleset loads cleanly and is listed as built-in", {
  official_local_registry()
  expect_no_warning(check <- bg_ruleset_check(official_ref))
  expect_true(check$ok)
  expect_identical(check$errors, character())
  expect_identical(check$warnings, character())
  expect_equal(check$rule_count, length(official_json()$rules))

  listed <- bg_rulesets()
  row <- listed[listed$id == "bayesgrove.workflow", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$version, "1.0.0")
  expect_equal(row$origin, "builtin")
  expect_equal(row$title, "Bayesian workflow")
  expect_equal(row$digest, bg_canonical_digest(bg_ruleset_lookup(official_ref)))
})

test_that("the official ruleset is ordinary data under the public grammar", {
  official_local_registry()
  json <- official_json()
  expect_equal(json$id, "bayesgrove.workflow")
  expect_equal(json$version, "1.0.0")
  expect_true(all(nzchar(c(json$title, json$description))))
  expect_true(any(grepl("10.1214/23-SS145", unlist(json$references))))
  expect_true(any(grepl("Bayesian Workflow", unlist(json$references))))
  # Rebuilding it through the exported constructor reproduces the built-in,
  # so it relies on nothing a third-party ruleset could not use.
  rebuilt <- do.call(bg_ruleset, json)
  expect_identical(
    bg_canonical_digest(rebuilt),
    bg_canonical_digest(bg_ruleset_lookup(official_ref))
  )
  for (rule in json$rules) {
    expect_identical(do.call(bg_rule, rule)$id, rule$id)
  }
})

test_that("the official ruleset uses only built-in predicates", {
  official_local_registry()
  expect_setequal(
    official_builtin_predicates,
    names(bg_rule_builtin_predicates())
  )
  types <- unlist(lapply(official_json()$rules, function(rule) {
    lapply(c(rule$when, list(rule$require)), `[[`, "type")
  }))
  expect_true(length(types) > 0)
  expect_true(all(types %in% official_builtin_predicates))
  expect_false(any(grepl(".", types, fixed = TRUE)))
  # With no custom predicates registered, installing it pins none.
  h <- official_fixture()
  expect_null(bg_rules(h)$predicates)
  expect_identical(bg_rule_predicate_problems(bg_rules(h)$rules), character())
})

test_that("every official rule carries guidance and references", {
  rules <- official_json()$rules
  expect_gte(length(rules), 6)
  expect_lte(length(rules), 12)
  for (rule in rules) {
    for (field in c("message", "explanation", "suggestion")) {
      expect_true(
        is.character(rule[[field]]) && nzchar(rule[[field]]),
        label = paste(rule$id, field)
      )
    }
    refs <- unlist(rule$references)
    expect_true(any(grepl("Radev (2023)", refs, fixed = TRUE)), label = rule$id)
    expect_true(
      any(grepl("Bayesian Workflow (2026)", refs, fixed = TRUE)),
      label = rule$id
    )
    expect_true(rule$severity %in% c("blocking", "advisory"), label = rule$id)
  }
})

test_that("blocking rules are limited to convergence and its human review", {
  rules <- official_json()$rules
  severity <- stats::setNames(
    vapply(rules, `[[`, character(1), "severity"),
    official_ids(rules)
  )
  expect_setequal(
    names(severity)[severity == "blocking"],
    c(
      "convergence-derived",
      "review-hmc-diagnostics",
      "review-optimizer-diagnostics"
    )
  )
  for (rule in rules[severity == "blocking"]) {
    expect_equal(unlist(rule$on), "accept")
  }
  convergence <- rules[[match("convergence-derived", names(severity))]]
  expect_equal(
    convergence$require,
    list(
      type = "require_utility_evidence",
      utility = "convergence",
      provenance = "derived"
    )
  )
  # Goal-specific expectations are advisory.
  goal_specific <- Filter(
    function(rule) {
      any(vapply(rule$when, `[[`, "", "type") == "goal_kind")
    },
    rules
  )
  expect_gte(length(goal_specific), 4)
  expect_true(all(vapply(goal_specific, `[[`, "", "severity") == "advisory"))
})

test_that("the vocabulary declares the taxonomy's utilities, goals, and checks", {
  vocabulary <- official_json()$vocabulary
  utilities <- official_ids(vocabulary$utilities)
  expect_setequal(utilities, official_utilities)
  expect_length(utilities, 10)
  # The ids match the ones the workflow packs already use.
  expect_setequal(utilities, bg_pack_taxonomy_utility_dimensions())
  for (entry in c(vocabulary$utilities, vocabulary$goal_kinds)) {
    expect_true(nzchar(entry$title), label = entry$id)
    expect_true(nzchar(entry$description), label = entry$id)
    expect_true(
      any(grepl(
        "Radev (2023), \u00a7",
        unlist(entry$references),
        fixed = TRUE
      )),
      label = entry$id
    )
  }
  expect_setequal(
    official_ids(vocabulary$goal_kinds),
    c("observable_prediction", "latent_inference")
  )

  checks <- official_ids(vocabulary$checks)
  expect_true(all(checks %in% names(bg_summary_vocabulary())))
  expect_true(all(
    c(
      "hmc_diagnostics",
      "prior_predictive_check",
      "posterior_predictive_check",
      "loo_diagnostics",
      "loo_pit_calibration",
      "sbc_result",
      "comparison_results",
      "stacking_weights"
    ) %in%
      checks
  ))
  mapped <- stats::setNames(
    lapply(vocabulary$checks, function(check) unlist(check$utilities)),
    checks
  )
  expect_true(all(unlist(mapped) %in% official_utilities))
  expect_equal(mapped$hmc_diagnostics, "convergence")
  expect_equal(mapped$loo_diagnostics, "predictive_performance")
  expect_equal(mapped$sbc_result, "parameter_recoverability")
  # Pareto-k records inform predictive assessment but measure LOO reliability.
  titles <- stats::setNames(
    vapply(vocabulary$checks, `[[`, character(1), "title"),
    checks
  )
  for (check in c("loo_diagnostics", "pareto_k_diagnostics")) {
    expect_match(titles[[check]], "reliability", label = check)
    expect_match(titles[[check]], "not predictive accuracy", label = check)
  }
})

test_that("every check and utility the rules name is declared", {
  json <- official_json()
  checks <- official_ids(json$vocabulary$checks)
  utilities <- official_ids(json$vocabulary$utilities)
  goals <- official_ids(json$vocabulary$goal_kinds)
  for (rule in json$rules) {
    for (condition in c(rule$when, list(rule$require))) {
      if (!is.null(condition$check)) {
        expect_true(condition$check %in% checks, label = rule$id)
      }
      if (!is.null(condition$utility)) {
        expect_true(condition$utility %in% utilities, label = rule$id)
      }
      if (identical(condition$type, "goal_kind")) {
        expect_true(all(unlist(condition$values) %in% goals), label = rule$id)
      }
    }
  }
})

test_that("a third-party ruleset can extend, disable, and override it", {
  official_local_registry()
  lab <- bg_ruleset(
    "lab.strict",
    "1.0.0",
    extends = official_ref,
    disable = "bayesgrove.workflow/review-optimizer-diagnostics",
    override = list(
      "bayesgrove.workflow/observable-loo" = list(severity = "blocking")
    )
  )
  expect_no_warning(check <- bg_ruleset_check(lab))
  expect_true(check$ok)
  expect_identical(check$warnings, character())
  expect_equal(check$rule_count, length(official_json()$rules) - 1L)
  bg_register_ruleset(lab)

  h <- official_fixture(mode = "enforce", rules = "lab.strict@1")
  active <- bg_rules(h)
  ids <- official_ids(active$rules)
  expect_false("bayesgrove.workflow/review-optimizer-diagnostics" %in% ids)
  loo <- active$rules[[match("bayesgrove.workflow/observable-loo", ids)]]
  expect_equal(loo$severity, "blocking")
  expect_equal(loo$source, "bayesgrove.workflow@1.0.0")
  expect_equal(loo$overridden_by, list("lab.strict@1.0.0"))
  expect_setequal(
    vapply(active$ruleset$sources, `[[`, "", "id"),
    c("bayesgrove.workflow", "lab.strict")
  )
  # The vocabulary is inherited unchanged.
  expect_setequal(
    official_ids(active$vocabulary$utilities),
    official_utilities
  )

  id <- official_candidate(h)
  ev <- official_evidence(h, id, "posterior_predictive_check")
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = ev
  )
  expect_equal(official_severities(proposal)[["observable-loo"]], "blocking")
})

test_that("guide mode reports what an accept without evidence lacks", {
  official_local_registry()
  h <- official_fixture("observable_prediction", "guide")
  id <- official_candidate(h)
  ev <- official_evidence(h, id, "hmc_diagnostics")
  state <- bg_research_state(h)
  # The vocabulary maps hmc_diagnostics to one utility, so it is filled in.
  expect_equal(state$evidence[[ev]]$utility, "convergence")

  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = ev
  )
  expect_true(proposal$allowed)
  expect_equal(
    official_severities(proposal),
    c(
      "convergence-derived" = "blocking",
      "review-hmc-diagnostics" = "blocking",
      "posterior-predictive-check" = "advisory",
      "observable-loo" = "advisory"
    )
  )
  for (finding in proposal$findings) {
    expect_true(nzchar(finding$explanation))
    expect_true(length(finding$references) > 0)
  }

  # Latent goals get the latent expectations instead.
  h <- official_fixture("latent_inference", "guide")
  id <- official_candidate(h)
  ev <- official_evidence(h, id, "hmc_diagnostics")
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = ev
  )
  expect_equal(
    official_severities(proposal),
    c(
      "convergence-derived" = "blocking",
      "review-hmc-diagnostics" = "blocking",
      "posterior-predictive-check" = "advisory",
      "latent-causal-consistency" = "advisory",
      "latent-parameter-recoverability" = "advisory"
    )
  )
})

test_that("evidence satisfies the advisory rules but asserted evidence never satisfies the derived rule", {
  official_local_registry()
  h <- official_fixture("latent_inference", "guide")
  id <- official_candidate(h)
  hmc <- official_evidence(h, id, "hmc_diagnostics")
  others <- c(
    official_evidence(h, id, "posterior_predictive_check"),
    official_evidence(h, id, "sbc_result"),
    official_evidence(h, id, "dagitty_adjustment")
  )
  official_apply(h, "review", candidate_id = id, evidence_ids = hmc)
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = c(hmc, others)
  )
  # Only the derived-convergence rule remains: asserted convergence evidence,
  # even when reviewed, is not derived from a run. The run-bound counterpart
  # is "enforce mode accepts a run-bound candidate with derived diagnostics".
  expect_equal(official_finding_ids(proposal), "convergence-derived")
  expect_equal(
    proposal$findings[[1]]$require$provenance,
    "derived"
  )
})

test_that("enforce mode holds only for blocking findings", {
  official_local_registry()
  h <- official_fixture("latent_inference", "enforce")
  id <- official_candidate(h)
  hmc <- official_evidence(h, id, "hmc_diagnostics")
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  expect_false(proposal$allowed)
  expect_error(bg_research_apply(h, proposal), "held by policy")

  # An agent review does not satisfy the human-review rule.
  bg_research_apply(
    h,
    bg_research_propose(
      h,
      list(kind = "review", candidate_id = id, evidence_ids = hmc),
      list(id = "assistant", type = "agent"),
      "Agent looked at the diagnostics."
    )
  )
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  expect_true("review-hmc-diagnostics" %in% official_finding_ids(proposal))

  official_apply(h, "review", candidate_id = id, evidence_ids = hmc)
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  # The candidate is not bound to a run, so its reviewed but asserted
  # diagnostics still leave convergence-derived unsatisfied.
  expect_false(proposal$allowed)
  severities <- official_severities(proposal)
  expect_equal(
    names(severities)[severities == "blocking"],
    "convergence-derived"
  )
  expect_setequal(
    names(severities)[severities == "advisory"],
    c(
      "posterior-predictive-check",
      "latent-causal-consistency",
      "latent-parameter-recoverability"
    )
  )

  # Without the derived-convergence rule, the advisory findings remain but
  # no longer hold the action.
  relaxed <- bg_ruleset(
    "lab.relaxed",
    "1.0.0",
    extends = official_ref,
    disable = "bayesgrove.workflow/convergence-derived"
  )
  bg_use_rules(
    h,
    relaxed,
    actor = official_human,
    rationale = "Test the advisory rules alone."
  )
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  expect_true(proposal$allowed)
  expect_true(all(official_severities(proposal) == "advisory"))
  expect_length(proposal$findings, 3)
  state <- bg_research_apply(h, proposal)
  expect_equal(tail(state$history, 1)[[1]]$action$kind, "accept")
})

test_that("enforce mode accepts a run-bound candidate with derived diagnostics", {
  official_local_registry()
  h <- make_provenance_project()
  src <- bg_add_node(h, kind = "src")
  fit <- bg_add_node(
    h,
    kind = "fit",
    params = list(model = "m1", scale = 3, data_input = src),
    inputs = src
  )
  bg_run(h)
  bg_research_init(
    h,
    "Which model answers the question?",
    list(kind = "latent_inference"),
    official_human,
    "enforce",
    official_ref
  )
  id <- official_apply(h, "create", label = "Scale 3", node_id = fit)
  hmc <- official_apply(
    h,
    "evidence",
    candidate_id = id,
    label = "Sampler diagnostics",
    node_id = fit
  )
  evidence <- bg_research_state(h)$evidence[[hmc]]
  expect_identical(evidence$check, "hmc_diagnostics")
  expect_identical(evidence$utility, "convergence")
  expect_identical(evidence$provenance$kind, "derived")

  # Derived evidence satisfies convergence-derived, but the HMC diagnostics
  # still need a human review.
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  expect_false(proposal$allowed)
  severities <- official_severities(proposal)
  expect_equal(
    names(severities)[severities == "blocking"],
    "review-hmc-diagnostics"
  )

  official_apply(h, "review", candidate_id = id, evidence_ids = hmc)
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  expect_true(proposal$allowed)
  expect_true(all(official_severities(proposal) == "advisory"))
  state <- bg_research_apply(h, proposal)
  expect_equal(tail(state$history, 1)[[1]]$action$kind, "accept")
})

test_that("every check is mapped and only diagnostics inform convergence", {
  official_local_registry()
  checks <- official_json()$vocabulary$checks
  mapped <- lapply(checks, function(check) unlist(check$utilities))
  names(mapped) <- official_ids(checks)
  expect_true(all(lengths(mapped) > 0L))
  expect_true(all(unlist(mapped) %in% official_utilities))
  informs_convergence <- vapply(
    mapped,
    function(utilities) "convergence" %in% utilities,
    logical(1)
  )
  expect_setequal(
    names(mapped)[informs_convergence],
    c("hmc_diagnostics", "optimizer_diagnostics")
  )
})

test_that("derived evidence from another check cannot stand in for convergence", {
  official_local_registry()
  h <- make_provenance_project()
  bg_register_node_kind(
    h,
    "prior_fit",
    executor = function(node, inputs) {
      list(
        result = list(mean = inputs[[1]]$value),
        summaries = list(list(
          summary_kind = "prior_spec",
          severity = "ok",
          metrics = list(prior = "normal(0, 1)")
        )),
        metadata = list(provenance = list(consumed = list(D = names(inputs))))
      )
    },
    pad = list(P = "model")
  )
  src <- bg_add_node(h, kind = "src")
  fit <- bg_add_node(
    h,
    kind = "prior_fit",
    params = list(model = "m1"),
    inputs = src
  )
  bg_run(h)
  bg_research_init(
    h,
    "Which model answers the question?",
    list(kind = "observable_prediction"),
    official_human,
    "enforce",
    official_ref
  )
  id <- official_apply(h, "create", label = "Prior only", node_id = fit)
  proposal <- official_propose(
    h,
    "evidence",
    candidate_id = id,
    label = "Prior as convergence",
    node_id = fit,
    utility = "convergence"
  )
  expect_false(proposal$allowed)
  expect_true("bayesgrove.workflow/vocabulary" %in% names(proposal$findings))
  expect_error(bg_research_apply(h, proposal), "held by policy")

  # Under a utility it informs, the record is allowed but does not unblock
  # acceptance.
  prior <- official_apply(
    h,
    "evidence",
    candidate_id = id,
    label = "Prior",
    node_id = fit,
    utility = "structural_faithfulness"
  )
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = prior
  )
  expect_false(proposal$allowed)
  expect_true("convergence-derived" %in% official_finding_ids(proposal))
})

test_that("the conditional review rules are silent without their evidence", {
  official_local_registry()
  h <- official_fixture("observable_prediction", "guide")
  id <- official_candidate(h)
  ppc <- official_evidence(h, id, "posterior_predictive_check")
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = ppc
  )
  found <- official_finding_ids(proposal)
  expect_false("review-hmc-diagnostics" %in% found)
  expect_false("review-optimizer-diagnostics" %in% found)
  # Convergence evidence is still missing.
  expect_true("convergence-derived" %in% found)

  # HMC evidence activates only the HMC review rule.
  hmc <- official_evidence(h, id, "hmc_diagnostics")
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = hmc
  )
  found <- official_finding_ids(proposal)
  expect_true("review-hmc-diagnostics" %in% found)
  expect_false("review-optimizer-diagnostics" %in% found)
})

test_that("optimizer diagnostics activate the optimizer review rule", {
  official_local_registry()
  h <- official_fixture("observable_prediction", "enforce")
  id <- official_candidate(h)
  opt <- official_evidence(h, id, "optimizer_diagnostics")
  expect_equal(
    bg_research_state(h)$evidence[[opt]]$utility,
    "convergence"
  )
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = opt
  )
  severities <- official_severities(proposal)
  expect_equal(severities[["review-optimizer-diagnostics"]], "blocking")
  expect_false("review-hmc-diagnostics" %in% names(severities))

  # A second, unreviewed record keeps the rule active after a review.
  official_apply(h, "review", candidate_id = id, evidence_ids = opt)
  later <- official_evidence(h, id, "optimizer_diagnostics")
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = opt
  )
  expect_true(
    "review-optimizer-diagnostics" %in% official_finding_ids(proposal)
  )

  official_apply(h, "review", candidate_id = id, evidence_ids = later)
  proposal <- official_propose(
    h,
    "accept",
    candidate_id = id,
    evidence_ids = opt
  )
  expect_false(
    "review-optimizer-diagnostics" %in% official_finding_ids(proposal)
  )
  # Still held by convergence-derived: the evidence is asserted.
  expect_false(proposal$allowed)
  blocking <- official_severities(proposal)
  expect_equal(
    names(blocking)[blocking == "blocking"],
    "convergence-derived"
  )
})

test_that("comparisons ask for goal-specific evidence per candidate", {
  official_local_registry()
  h <- official_fixture("observable_prediction", "guide")
  a <- official_candidate(h, "normal")
  b <- official_candidate(h, "lognormal")
  ev_a <- official_evidence(h, a, "loo_diagnostics")
  ev_b <- official_evidence(h, b, "posterior_predictive_check")
  proposal <- official_propose(
    h,
    "compare",
    candidate_ids = c(a, b),
    evidence_ids = c(ev_a, ev_b),
    criteria = "Expected log predictive density",
    result = list(preferred = a)
  )
  expect_equal(official_finding_ids(proposal), "observable-compare-loo")
  expect_equal(proposal$findings[[1]]$severity, "advisory")
  expect_equal(proposal$findings[[1]]$candidate_ids, list(b))

  h <- official_fixture("latent_inference", "guide")
  a <- official_candidate(h, "normal")
  b <- official_candidate(h, "lognormal")
  ev_a <- official_evidence(h, a, "causal_framing")
  ev_b <- official_evidence(h, b, "loo_diagnostics")
  proposal <- official_propose(
    h,
    "compare",
    candidate_ids = c(a, b),
    evidence_ids = c(ev_a, ev_b),
    criteria = "Expected log predictive density",
    result = list(preferred = b)
  )
  expect_equal(
    official_finding_ids(proposal),
    "latent-compare-causal-consistency"
  )
  expect_equal(proposal$findings[[1]]$candidate_ids, list(b))
})

test_that("goal kinds outside the vocabulary are rejected at init and reframe", {
  official_local_registry()
  h <- bg_init(tempfile("bg-official-"))
  withr::defer(bg_close(h))
  expect_error(
    bg_research_init(
      h,
      "What happens next?",
      list(kind = "exploration"),
      official_human,
      "record",
      official_ref
    ),
    "goal kind must be one of"
  )
  expect_error(
    bg_research_init(
      h,
      "What happens next?",
      list(focus = "anything"),
      official_human,
      "guide",
      official_ref
    ),
    "goal kind must be one of"
  )
  expect_null(bg_research_state(h))

  h <- official_fixture("latent_inference", "record")
  expect_error(
    official_propose(
      h,
      "reframe",
      question = "What happens next?",
      goal = list(kind = "exploration")
    ),
    "goal kind must be one of"
  )
})

test_that("the vocabulary rule flags undeclared or mismatched evidence terms", {
  official_local_registry()
  h <- official_fixture("observable_prediction", "guide")
  id <- official_candidate(h)
  proposal <- official_propose(
    h,
    "evidence",
    candidate_id = id,
    label = "Check",
    check = "posterior_predictive_check",
    utility = "prediction",
    result = list(source = "test")
  )
  expect_true(proposal$allowed)
  finding <- proposal$findings[["bayesgrove.workflow/vocabulary"]]
  expect_equal(finding$severity, "advisory")
  expect_match(finding$explanation, "Utility 'prediction' is not declared")

  proposal <- official_propose(
    h,
    "evidence",
    candidate_id = id,
    label = "Check",
    check = "hmc_diagnostics",
    utility = "predictive_performance",
    result = list(source = "test")
  )
  expect_match(
    proposal$findings[["bayesgrove.workflow/vocabulary"]]$explanation,
    "informs 'convergence'"
  )

  h <- official_fixture("observable_prediction", "enforce")
  id <- official_candidate(h)
  proposal <- official_propose(
    h,
    "evidence",
    candidate_id = id,
    label = "Check",
    check = "custom_check",
    utility = "prediction",
    result = list(source = "test")
  )
  expect_false(proposal$allowed)
  expect_equal(
    proposal$findings[["bayesgrove.workflow/vocabulary"]]$severity,
    "blocking"
  )
})

test_that("bg_use_rules installs the official ruleset on a running investigation", {
  official_local_registry()
  h <- official_fixture("latent_inference", "record", rules = list())
  state <- bg_use_rules(
    h,
    official_ref,
    mode = "guide",
    actor = official_human,
    rationale = "Adopt the official workflow rules."
  )
  active <- bg_rules(h)
  expect_equal(active$mode, "guide")
  expect_equal(active$ruleset$id, "bayesgrove.workflow")
  expect_equal(active$ruleset$version, "1.0.0")
  expect_true(all(startsWith(
    official_ids(active$rules),
    "bayesgrove.workflow/"
  )))
  expect_true(all(vapply(
    active$rules,
    function(rule) identical(rule$source, "bayesgrove.workflow@1.0.0"),
    logical(1)
  )))
})
