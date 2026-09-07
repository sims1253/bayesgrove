research_fixture <- function(mode = "record", rules = list()) {
  h <- bg_init(tempfile("bg-research-"))
  actor <- list(id = "researcher", type = "human")
  bg_research_init(
    h,
    "Predict positive measurements",
    list(kind = "observable_prediction"),
    actor,
    mode,
    rules
  )
  h
}
research_action <- function(
  h,
  kind,
  ...,
  actor = list(id = "researcher", type = "human")
) {
  bg_research_propose(
    h,
    c(list(kind = kind), list(...)),
    actor,
    "Investigate this possibility."
  )
}
research_apply <- function(
  h,
  kind,
  ...,
  actor = list(id = "researcher", type = "human")
) {
  bg_research_apply(h, research_action(h, kind, ..., actor = actor))
}
research_last_id <- function(state) tail(state$history, 1)[[1]]$id
research_candidate <- function(h) {
  research_last_id(research_apply(
    h,
    "create",
    label = "Normal",
    components = list(
      P = list(likelihood = "normal", prior = "weak"),
      A = list(method = "NUTS"),
      D = list(version = "original")
    )
  ))
}
research_evidence <- function(h, id, value = 0.01) {
  research_last_id(research_apply(
    h,
    "evidence",
    candidate_id = id,
    label = "Predictive check",
    check = "ppc",
    utility = "predictive_performance",
    result = list(p = value)
  ))
}
research_rule <- function() {
  list(
    id = "human-ppc",
    actions = "accept",
    type = "require_review",
    check = "ppc",
    message = "Review the predictive check before accepting the candidate.",
    references = "https://arxiv.org/abs/2209.02439",
    interpretation = "Inspect the features relevant to the inferential goal."
  )
}

test_that("research state survives reopen with distinct P, A, and D revisions", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  ev <- research_evidence(h, root)
  p <- research_last_id(research_apply(
    h,
    "revise",
    candidate_id = root,
    label = "Lognormal",
    components = list(P = list(likelihood = "lognormal"))
  ))
  a <- research_last_id(research_apply(
    h,
    "revise",
    candidate_id = root,
    label = "VI",
    components = list(A = list(method = "VI"))
  ))
  d <- research_last_id(research_apply(
    h,
    "revise",
    candidate_id = root,
    label = "New data",
    components = list(D = list(version = "new"))
  ))
  state <- bg_research_state(h)
  expect_equal(state$candidates[[root]]$components$P$likelihood, "normal")
  expect_equal(
    state$candidates[[p]]$components$A,
    state$candidates[[root]]$components$A
  )
  expect_equal(state$candidates[[a]]$changed, list("A"))
  expect_equal(state$candidates[[d]]$changed, list("D"))
  expect_equal(state$evidence[[ev]]$candidate_id, root)
  expect_length(state$candidates, 4)
  path <- h@path
  bg_close(h)
  h <- bg_open(path)
  expect_identical(bg_research_state(h), state)
})

test_that("closed lineages remain comparable but cannot be extended until reopened", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  child <- research_last_id(research_apply(
    h,
    "revise",
    candidate_id = root,
    label = "Alternative",
    components = list(P = list(likelihood = "lognormal"))
  ))
  e1 <- research_evidence(h, root)
  e2 <- research_evidence(h, child)
  research_apply(h, "close", candidate_id = root)
  expect_error(
    research_action(
      h,
      "evidence",
      candidate_id = child,
      label = "More",
      check = "ppc",
      utility = "prediction",
      result = 1
    ),
    "Lineage is closed"
  )
  expect_error(
    research_action(
      h,
      "revise",
      candidate_id = child,
      label = "Next",
      components = list(A = "VI")
    ),
    "Lineage is closed"
  )
  state <- research_apply(
    h,
    "compare",
    candidate_ids = c(root, child),
    evidence_ids = c(e1, e2),
    criteria = "Tail prediction",
    result = list(preferred = child)
  )
  expect_length(state$comparisons, 1)
  expect_length(state$evidence, 2)
  research_apply(h, "reopen", candidate_id = root)
  expect_no_error(research_action(
    h,
    "revise",
    candidate_id = child,
    label = "Next",
    components = list(A = "VI")
  ))
})

test_that("modes preserve evidence and findings become holds only in enforce", {
  h <- research_fixture(rules = list(research_rule()))
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  ev <- research_evidence(h, root)
  before <- bg_research_state(h)$evidence
  record <- research_action(h, "accept", candidate_id = root, evidence_ids = ev)
  expect_true(record$allowed)
  expect_length(record$findings, 0)
  research_apply(h, "configure", mode = "guide", rules = list(research_rule()))
  guide <- research_action(h, "accept", candidate_id = root, evidence_ids = ev)
  expect_true(guide$allowed)
  expect_equal(
    guide$findings[[1]]$interpretation,
    research_rule()$interpretation
  )
  research_apply(
    h,
    "configure",
    mode = "enforce",
    rules = list(research_rule())
  )
  enforce <- research_action(
    h,
    "accept",
    candidate_id = root,
    evidence_ids = ev
  )
  expect_false(enforce$allowed)
  enforce$allowed <- TRUE
  enforce$findings <- list()
  expect_error(bg_research_apply(h, enforce), "held by policy")
  expect_identical(bg_research_state(h)$evidence, before)
  expect_length(bg_research_state(h)$decisions, 0)
})

test_that("human review covers exact evidence and does not transfer to a revision", {
  h <- research_fixture("enforce", list(research_rule()))
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  ev <- research_evidence(h, root)
  research_apply(
    h,
    "review",
    candidate_id = root,
    evidence_ids = ev,
    actor = list(id = "worker", type = "agent")
  )
  expect_false(
    research_action(h, "accept", candidate_id = root, evidence_ids = ev)$allowed
  )
  research_apply(h, "review", candidate_id = root, evidence_ids = ev)
  expect_true(
    research_action(h, "accept", candidate_id = root, evidence_ids = ev)$allowed
  )
  child <- research_last_id(research_apply(
    h,
    "revise",
    candidate_id = root,
    label = "VI",
    components = list(A = "VI")
  ))
  child_ev <- research_evidence(h, child)
  expect_false(
    research_action(
      h,
      "accept",
      candidate_id = child,
      evidence_ids = child_ev
    )$allowed
  )
  ev2 <- research_evidence(h, root, 0.1)
  expect_false(
    research_action(
      h,
      "accept",
      candidate_id = root,
      evidence_ids = ev2
    )$allowed
  )
  expect_error(
    research_action(h, "review", candidate_id = child, evidence_ids = ev),
    "must belong"
  )
  expect_error(
    research_action(
      h,
      "configure",
      mode = "record",
      rules = list(),
      actor = list(id = "worker", type = "agent")
    ),
    "Only a human"
  )
})

test_that("rules constrain candidate specifications before creating a revision", {
  rules <- list(
    list(
      id = "support",
      actions = c("create", "revise"),
      type = "forbid_values",
      path = c("P", "likelihood"),
      values = "normal",
      message = "Use positive support."
    ),
    list(
      id = "confounders",
      actions = c("create", "revise"),
      type = "require_values",
      path = c("P", "covariates"),
      values = c("age", "site"),
      message = "Retain the specified adjustment variables."
    )
  )
  h <- research_fixture("enforce", rules)
  withr::defer(bg_close(h))
  blocked <- research_action(
    h,
    "create",
    label = "Normal",
    components = list(P = list(likelihood = "normal"))
  )
  expect_false(blocked$allowed)
  expect_length(blocked$findings, 2)
  expect_error(bg_research_apply(h, blocked), "held by policy")
  expect_length(bg_research_state(h)$candidates, 0)
  allowed <- research_action(
    h,
    "create",
    label = "Lognormal",
    components = list(
      P = list(likelihood = "lognormal", covariates = c("age", "site"))
    )
  )
  state <- bg_research_apply(h, allowed)
  root <- research_last_id(state)
  bad_revision <- research_action(
    h,
    "revise",
    candidate_id = root,
    label = "Missing site",
    components = list(P = list(likelihood = "lognormal", covariates = "age"))
  )
  expect_false(bad_revision$allowed)
  expect_length(bg_research_state(h)$candidates, 1)
})

test_that("proposals are stale after any research mutation and cannot be replayed", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  proposal <- research_action(h, "note", candidate_id = root)
  research_apply(h, "close", candidate_id = root)
  before <- bg_research_state(h)
  expect_error(bg_research_apply(h, proposal), "Stale")
  expect_identical(bg_research_state(h), before)
  proposal <- research_action(h, "reopen", candidate_id = root)
  bg_research_apply(h, proposal)
  expect_error(bg_research_apply(h, proposal), "Stale")
})

test_that("invalid actions and non-data specifications leave state intact", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  before <- bg_research_state(h)
  expect_error(
    research_action(
      h,
      "revise",
      candidate_id = root,
      label = "Same",
      components = list(A = list(method = "NUTS"))
    ),
    "must change"
  )
  expect_error(
    research_action(
      h,
      "create",
      label = "Bad",
      components = list(P = function() 1)
    ),
    "plain lists"
  )
  expect_error(
    research_action(h, "create", label = "Bad", components = list(P = NA)),
    "plain lists"
  )
  expect_error(
    research_action(h, "create", label = "Bad", components = list(A = "NUTS")),
    "specify P"
  )
  expect_error(
    research_action(h, "close", candidate_id = root, typo = TRUE),
    "Invalid"
  )
  expect_error(research_action(h, "unknown"), "Unknown research action")
  expect_identical(bg_research_state(h), before)
})

test_that("readonly handles, closed handles, corruption and lost writer locks are rejected", {
  h <- research_fixture()
  path <- h@path
  root <- research_candidate(h)
  proposal <- research_action(h, "note", candidate_id = root)
  readonly <- bg_open(path, readonly = TRUE)
  expect_equal(bg_research_state(readonly)$version, proposal$version)
  expect_error(bg_research_apply(readonly, proposal), "readonly")
  bg_close(readonly)
  bg_close(h)
  expect_error(bg_research_state(h), "closed")
  h <- bg_open(path)
  withr::defer(bg_close(h))
  old_token <- h@lock_token
  h@lock_token <- "wrong"
  expect_error(bg_research_apply(h, proposal), "writer lock")
  h@lock_token <- old_token
  writeLines("broken", bg_research_path(h))
  expect_error(bg_research_state(h), "Cannot read research state")
})

test_that("a failed atomic write does not record a partial decision", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  proposal <- research_action(h, "note", candidate_id = root)
  before <- bg_research_state(h)
  local_mocked_bindings(bg_write_json_atomic = function(...) {
    stop("write failed")
  })
  expect_error(bg_research_apply(h, proposal), "write failed")
  expect_identical(bg_research_state(h), before)
})

test_that("comparison references can motivate revisions and decisions", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  one <- research_candidate(h)
  two <- research_candidate(h)
  e1 <- research_evidence(h, one)
  e2 <- research_evidence(h, two)
  state <- research_apply(
    h,
    "compare",
    candidate_ids = c(one, two),
    evidence_ids = c(e1, e2),
    criteria = "Illustrative predictive evidence",
    result = list(preferred = two)
  )
  comparison <- research_last_id(state)
  state <- research_apply(
    h,
    "accept",
    candidate_id = two,
    evidence_ids = e2,
    basis = list(comparison_ids = comparison)
  )
  expect_equal(
    state$decisions[[research_last_id(state)]]$basis$comparison_ids,
    comparison
  )
  expect_error(
    research_action(
      h,
      "note",
      candidate_id = one,
      basis = list(comparison_ids = "missing")
    ),
    "existing record"
  )
  expect_error(
    research_action(
      h,
      "compare",
      candidate_ids = c(one, two),
      evidence_ids = c(e1, e1),
      criteria = "PPC",
      result = 1
    ),
    "distinct existing"
  )
})

test_that("require_evidence distinguishes an unrun check from recorded evidence", {
  rule <- list(
    id = "need-prior-check",
    actions = "accept",
    type = "require_evidence",
    check = "prior_predictive",
    message = "Collect prior predictive evidence."
  )
  h <- research_fixture("enforce", list(rule))
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  ppc <- research_evidence(h, root)
  expect_false(
    research_action(
      h,
      "accept",
      candidate_id = root,
      evidence_ids = ppc
    )$allowed
  )
  research_apply(
    h,
    "evidence",
    candidate_id = root,
    label = "Prior check",
    check = "prior_predictive",
    utility = "predictive_performance",
    result = list(concern = TRUE)
  )
  # Presence satisfies this rule even if the result raises a concern.
  expect_true(
    research_action(
      h,
      "accept",
      candidate_id = root,
      evidence_ids = ppc
    )$allowed
  )
})

test_that("field order and numeric storage types do not create false revisions", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  state <- research_apply(
    h,
    "create",
    label = "Baseline",
    components = list(P = list(a = 1, b = 2))
  )
  root <- research_last_id(state)
  expect_error(
    research_action(
      h,
      "revise",
      candidate_id = root,
      label = "Same",
      components = list(P = list(b = 2L, a = 1L))
    ),
    "must change"
  )
  state <- research_apply(
    h,
    "revise",
    candidate_id = root,
    label = "With A",
    components = list(A = "NUTS")
  )
  parent <- research_last_id(state)
  state <- research_apply(
    h,
    "revise",
    candidate_id = parent,
    label = "P only",
    components = list(A = NULL)
  )
  expect_null(state$candidates[[research_last_id(state)]]$components$A)
  expect_equal(state$candidates[[research_last_id(state)]]$changed, list("A"))
})

test_that("snapshots, reports, and bundles carry the complete research record", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  ev <- research_evidence(h, root)
  research_apply(h, "review", candidate_id = root, evidence_ids = ev)
  research_apply(h, "close", candidate_id = root)
  state <- bg_research_state(h)
  expect_identical(bg_snapshot(h)$research, state)
  report <- bg_export_report(h, format = "md")
  lines <- readLines(report)
  expect_true(any(grepl("## Research search", lines, fixed = TRUE)))
  expect_true(any(grepl(ev, lines, fixed = TRUE)))
  expect_true(any(grepl("researcher", lines, fixed = TRUE)))
  bundle <- bg_bundle(h)
  extract <- withr::local_tempdir()
  utils::untar(bundle, exdir = extract)
  restored <- bg_open(file.path(extract, basename(h@path)), readonly = TRUE)
  withr::defer(bg_close(restored))
  expect_identical(bg_research_state(restored), state)
})

test_that("invalid rules cannot replace the policy and disabling it preserves concerns", {
  h <- research_fixture("enforce", list(research_rule()))
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  ev <- research_evidence(h, root)
  before <- bg_research_state(h)
  expect_error(
    research_action(
      h,
      "configure",
      mode = "guide",
      rules = list(list(type = "unknown"))
    ),
    "Unknown rule type"
  )
  expect_error(
    research_action(
      h,
      "configure",
      mode = "record",
      rules = list(research_rule(), research_rule())
    ),
    "unique"
  )
  expect_identical(bg_research_state(h), before)
  state <- research_apply(
    h,
    "configure",
    mode = "record",
    rules = list(research_rule())
  )
  expect_identical(state$evidence, before$evidence)
  expect_identical(state$decisions, before$decisions)
  expect_equal(tail(state$history, 1)[[1]]$mode, "enforce")
  expect_true(
    research_action(h, "accept", candidate_id = root, evidence_ids = ev)$allowed
  )
})

test_that("basis is stored once and named vectors have canonical field order", {
  h <- research_fixture()
  withr::defer(bg_close(h))
  state <- research_apply(
    h,
    "create",
    label = "Named vector",
    components = list(P = c(a = 1, b = 2))
  )
  root <- research_last_id(state)
  expect_error(
    research_action(
      h,
      "revise",
      candidate_id = root,
      label = "Same",
      components = list(P = c(b = 2, a = 1))
    ),
    "must change"
  )
  ev <- research_evidence(h, root)
  state <- research_apply(
    h,
    "note",
    candidate_id = root,
    basis = list(evidence_ids = ev)
  )
  decision <- state$decisions[[research_last_id(state)]]
  expect_equal(decision$basis$evidence_ids, ev)
  expect_false("basis.1" %in% names(decision))
  expect_equal(sum(names(decision) == "basis"), 1L)
})

test_that("unsupported attributes and empty constraints cannot lose meaning", {
  h <- research_fixture("enforce")
  withr::defer(bg_close(h))
  root <- research_candidate(h)
  before <- bg_research_state(h)
  for (value in list(
    matrix(1:4, 2),
    array(1:8, c(2, 2, 2)),
    structure(1, unit = "kg")
  )) {
    expect_error(
      research_action(
        h,
        "revise",
        candidate_id = root,
        label = "Bad data",
        components = list(D = value)
      ),
      "plain lists"
    )
  }
  for (values in list(list(NULL), list(character()), list(list()))) {
    rule <- list(
      id = "required",
      actions = "accept",
      type = "require_values",
      path = c("P", "covariates"),
      values = values,
      message = "Include the specified values."
    )
    expect_error(
      research_action(h, "configure", mode = "enforce", rules = list(rule)),
      "scalar values"
    )
  }
  expect_identical(bg_research_state(h), before)
  state <- research_apply(
    h,
    "create",
    label = "P only",
    components = list(P = list(likelihood = "normal"))
  )
  p_only <- research_last_id(state)
  expect_error(
    research_action(
      h,
      "revise",
      candidate_id = p_only,
      label = "Still P",
      components = list(A = NULL)
    ),
    "must change"
  )
})
