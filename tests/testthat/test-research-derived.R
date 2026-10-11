# Candidate bindings and run-derived evidence
# (architecture/rules-and-evidence.md, section 2). Uses the provenance
# fixtures from helper-provenance.R: a `src` kind and a `fit` kind whose
# `pad` declares `model` as P and `scale` as A.

derived_actor <- list(id = "researcher", type = "human")

# A project with a source, a bound-able fit, and an investigation. Extra
# kinds: `check` evaluates its first input and reports it as its subject;
# `inspect` does the same but returns its subject unchanged; `fit2` emits two
# summary kinds; `sbc` and `compare` mimic the unsupported built-ins.
derived_project <- function(
  mode = "record",
  rules = list(),
  env = parent.frame()
) {
  handle <- make_provenance_project(withr::local_tempdir(.local_envir = env))
  withr::defer(bg_close(handle), envir = env)
  subject_report <- function(subject) {
    list(provenance = list(consumed = list(D = list(), subject = subject)))
  }
  bg_register_node_kind(handle, "check", executor = function(node, inputs) {
    subject <- names(inputs)[[1]]
    list(
      result = list(checked = inputs[[subject]]$mean),
      summaries = list(list(
        summary_kind = "posterior_predictive_check",
        severity = "ok",
        passed = TRUE,
        metrics = list(p = 0.4)
      )),
      metadata = subject_report(subject)
    )
  })
  bg_register_node_kind(handle, "inspect", executor = function(node, inputs) {
    subject <- names(inputs)[[1]]
    list(
      result = inputs[[subject]],
      summaries = list(list(
        summary_kind = "loo_diagnostics",
        severity = "warning",
        passed = FALSE,
        metrics = list(bad_k = 2)
      )),
      metadata = subject_report(subject)
    )
  })
  bg_register_node_kind(
    handle,
    "fit2",
    executor = function(node, inputs) {
      list(
        result = list(mean = inputs[[1]]$value * node$params$scale),
        summaries = list(
          list(summary_kind = "hmc_diagnostics", severity = "ok"),
          list(summary_kind = "pareto_k_diagnostics", severity = "ok")
        ),
        metadata = list(provenance = list(consumed = list(D = names(inputs))))
      )
    },
    pad = list(P = "model", A = "scale")
  )
  for (kind in c("sbc", "compare")) {
    bg_register_node_kind(handle, kind, executor = function(node, inputs) {
      list(
        result = list(kind = node$kind),
        summaries = list(list(summary_kind = "sbc_result", severity = "ok")),
        metadata = list(provenance = list(consumed = list(D = list())))
      )
    })
  }
  bg_research_init(
    handle,
    "Which scale fits?",
    list(kind = "observable_prediction"),
    derived_actor,
    mode,
    rules
  )
  src <- bg_add_node(handle, kind = "src")
  fit <- bg_add_node(
    handle,
    kind = "fit",
    params = list(model = "m1", scale = 3, data_input = src),
    inputs = src
  )
  bg_run(handle)
  list(handle = handle, src = src, fit = fit)
}

add_fit <- function(p, scale, kind = "fit") {
  params <- list(model = "m1", scale = scale)
  if (kind == "fit") {
    params$data_input <- p$src
  }
  node <- bg_add_node(p$handle, kind = kind, params = params, inputs = p$src)
  bg_run(p$handle)
  node
}

add_check <- function(p, subject, kind = "check") {
  node <- bg_add_node(p$handle, kind = kind, inputs = subject)
  bg_run(p$handle)
  node
}

propose <- function(p, kind, ..., actor = derived_actor) {
  bg_research_propose(
    p$handle,
    c(list(kind = kind), list(...)),
    actor,
    "Record a step."
  )
}

act <- function(p, kind, ..., actor = derived_actor) {
  state <- bg_research_apply(p$handle, propose(p, kind, ..., actor = actor))
  tail(state$history, 1)[[1]]$id
}

candidate_of <- function(p, id) bg_research_state(p$handle)$candidates[[id]]
evidence_of <- function(p, id) bg_research_state(p$handle)$evidence[[id]]

describe("engine-owned fields", {
  it("rejects actions that supply them, at any action kind", {
    p <- derived_project()
    root <- act(p, "create", label = "Root", components = list(P = list(m = 1)))
    before <- bg_research_state(p$handle)
    owned <- list(
      list("create", label = "x", node_id = p$fit, binding = list()),
      list("create", label = "x", components = list(P = list(m = 1)), pad = 1),
      list(
        "evidence",
        candidate_id = root,
        label = "x",
        check = "c",
        utility = "u",
        result = list(a = 1),
        provenance = list(kind = "derived")
      ),
      list("note", candidate_id = root, job_id = "job_x"),
      list("note", candidate_id = root, summary_ids = list("sum_x")),
      list(
        "revise",
        candidate_id = root,
        label = "x",
        components = list(P = list(m = 2, digest = "abc"))
      )
    )
    for (action in owned) {
      expect_error(do.call(propose, c(list(p), action)), "engine-owned")
    }
    expect_error(
      do.call(propose, c(list(p), owned[[6]])),
      "components.P.digest",
      fixed = TRUE
    )
    expect_identical(bg_research_state(p$handle), before)
  })
})

describe("reserved names below the component level", {
  it("stores nested caller data as opaque data that never counts as derived", {
    rule <- bg_rule(
      id = "derived-hmc",
      on = "accept",
      require = list(
        type = "require_evidence",
        check = "hmc_diagnostics",
        provenance = "derived"
      ),
      message = "Derive the diagnostics from the run."
    )
    p <- derived_project(mode = "enforce", rules = list(rule))
    nested <- list(
      model = "m1",
      notes = list(digest = "abc", provenance = list(kind = "derived"))
    )
    root <- act(p, "create", label = "Nested", components = list(P = nested))
    candidate <- candidate_of(p, root)
    expect_identical(candidate$components$P$notes$digest, "abc")
    expect_false("binding" %in% names(candidate))

    evidence <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Asserted with nested provenance",
      check = "hmc_diagnostics",
      utility = "convergence",
      result = list(
        provenance = list(kind = "derived", job_id = "job_x"),
        summary_ids = list("sum_x")
      )
    )
    record <- evidence_of(p, evidence)
    expect_null(record$provenance)
    expect_identical(record$result$provenance$kind, "derived")
    held <- propose(p, "accept", candidate_id = root, evidence_ids = evidence)
    expect_false(held$allowed)
    expect_named(held$findings, "derived-hmc")
  })

  it("accepts only unnamed lists of IDs in basis", {
    p <- derived_project()
    root <- act(p, "create", label = "Root", components = list(P = list(m = 1)))
    evidence <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Asserted",
      check = "hmc_diagnostics",
      utility = "convergence",
      result = list(ok = TRUE)
    )
    for (ids in list(
      list(provenance = evidence),
      c(job = evidence),
      list(list(evidence))
    )) {
      expect_error(
        propose(
          p,
          "note",
          candidate_id = root,
          basis = list(evidence_ids = ids)
        ),
        "unnamed list of record IDs"
      )
    }
    expect_error(
      propose(
        p,
        "review",
        candidate_id = root,
        evidence_ids = list(provenance = evidence)
      ),
      "unnamed list of record IDs"
    )
    expect_no_error(propose(
      p,
      "note",
      candidate_id = root,
      basis = list(evidence_ids = list(evidence))
    ))
  })
})

describe("binding a candidate", {
  it("binds a create and defaults components to the snapshot without digests", {
    p <- derived_project()
    proposal <- propose(p, "create", label = "Scale 3", node_id = p$fit)
    pad <- bg_node_pad(p$handle, p$fit)
    binding <- proposal$derivation$binding
    expect_identical(binding$node_id, p$fit)
    expect_identical(binding$job_id, pad$job_id)
    expect_identical(binding$artifact_ref, pad$artifact_ref)
    expect_identical(binding$pad, pad$pad)
    expect_identical(binding$research_version, 2L)

    state <- bg_research_apply(p$handle, proposal)
    id <- tail(state$history, 1)[[1]]$id
    candidate <- state$candidates[[id]]
    expect_identical(
      bg_canonical_json(candidate$binding),
      bg_canonical_json(binding)
    )
    expect_identical(candidate$components$P$params, list(model = "m1"))
    expect_identical(candidate$components$A$params, list(scale = 3L))
    expect_null(candidate$components$P$digest)
    expect_false("other" %in% names(candidate$components))
    expect_identical(candidate$changed, list("A", "D", "P"))
    expect_identical(tail(state$history, 1)[[1]]$action$node_id, p$fit)
  })

  it("keeps caller components on a bound create", {
    p <- derived_project()
    id <- act(
      p,
      "create",
      label = "Described",
      node_id = p$fit,
      components = list(P = list(likelihood = "normal"))
    )
    candidate <- candidate_of(p, id)
    expect_identical(
      candidate$components,
      list(P = list(likelihood = "normal"))
    )
    expect_identical(candidate$binding$node_id, p$fit)
  })

  it("creates unbound candidates as before", {
    p <- derived_project()
    id <- act(p, "create", label = "Plain", components = list(P = list(m = 1)))
    expect_false("binding" %in% names(candidate_of(p, id)))
    expect_error(propose(p, "create", label = "Empty"), "Supply")
  })

  it("never inherits a binding on revise without node_id", {
    p <- derived_project()
    root <- act(p, "create", label = "Bound", node_id = p$fit)
    child <- act(
      p,
      "revise",
      candidate_id = root,
      label = "Redescribed",
      components = list(P = list(likelihood = "student"))
    )
    candidate <- candidate_of(p, child)
    expect_false("binding" %in% names(candidate))
    expect_identical(candidate$changed, list("P"))
    expect_error(
      propose(p, "revise", candidate_id = child, label = "Nothing"),
      "Supply"
    )
  })

  it("binds an unbound parent with an empty or described change", {
    p <- derived_project()
    root <- act(
      p,
      "create",
      label = "Plain",
      components = list(P = list(m = 1))
    )
    same <- act(
      p,
      "revise",
      candidate_id = root,
      label = "Run",
      node_id = p$fit
    )
    candidate <- candidate_of(p, same)
    expect_identical(candidate$changed, list())
    expect_identical(candidate$components, list(P = list(m = 1L)))
    expect_identical(candidate$binding$node_id, p$fit)

    described <- act(
      p,
      "revise",
      candidate_id = root,
      label = "Run, described",
      node_id = p$fit,
      components = list(A = list(scale = 3))
    )
    expect_identical(candidate_of(p, described)$changed, list("A"))
  })

  it("derives changed from digests and descriptions on a bound parent", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    other <- add_fit(p, scale = 4)
    child <- act(
      p,
      "revise",
      candidate_id = root,
      label = "Scale 4",
      node_id = other
    )
    candidate <- candidate_of(p, child)
    expect_identical(candidate$changed, list("A"))
    expect_identical(candidate$binding$node_id, other)
    # The description was not supplied, so it is the parent's.
    expect_identical(candidate$components, candidate_of(p, root)$components)

    described <- act(
      p,
      "revise",
      candidate_id = root,
      label = "Same run, new description",
      node_id = p$fit,
      components = list(P = list(likelihood = "normal"))
    )
    expect_identical(candidate_of(p, described)$changed, list("P"))

    both <- act(
      p,
      "revise",
      candidate_id = root,
      label = "Scale 4, described",
      node_id = other,
      components = list(D = list(version = "v2"))
    )
    expect_identical(candidate_of(p, both)$changed, list("A", "D"))
  })

  it("rejects an unchanged bg_branch() clone as a revision", {
    p <- derived_project()
    root <- act(p, "create", label = "Original", node_id = p$fit)
    clone <- bg_branch(p$handle, p$fit)$root_node_id
    bg_run(p$handle)
    expect_identical(
      bg_node_pad(p$handle, clone)$job_id,
      bg_node_pad(p$handle, p$fit)$job_id
    )
    expect_error(
      propose(
        p,
        "revise",
        candidate_id = root,
        label = "Clone",
        node_id = clone
      ),
      "same P, A, and D"
    )
  })

  it("requires a resolvable execution with a P/A/D snapshot", {
    p <- derived_project()
    expect_error(
      propose(p, "create", label = "x", node_id = "node_missing"),
      "not found"
    )
    expect_error(
      propose(p, "create", label = "x", node_id = p$src),
      "no P/A/D snapshot"
    )
    pending <- bg_add_node(
      p$handle,
      kind = "fit",
      params = list(model = "m2", scale = 1),
      inputs = p$src
    )
    expect_error(
      propose(p, "create", label = "x", node_id = pending),
      "rerun"
    )
  })

  it("is stale at apply when the execution changes after the proposal", {
    p <- derived_project()
    proposal <- propose(p, "create", label = "Scale 3", node_id = p$fit)
    bg_run(p$handle, rerun = p$fit)
    before <- bg_research_state(p$handle)
    expect_error(bg_research_apply(p$handle, proposal), "Stale")
    expect_identical(bg_research_state(p$handle), before)
    # A fresh proposal binds the new representative execution.
    id <- act(p, "create", label = "Scale 3", node_id = p$fit)
    expect_identical(
      candidate_of(p, id)$binding$job_id,
      bg_node_pad(p$handle, p$fit)$job_id
    )
  })

  it("rejects malformed stored bindings", {
    p <- derived_project()
    id <- act(p, "create", label = "Bound", node_id = p$fit)
    state <- bg_research_state(p$handle)
    state$candidates[[id]]$binding$job_id <- NULL
    bg_write_json_atomic(bg_research_path(p$handle), state, sort_keys = FALSE)
    expect_error(bg_research_state(p$handle), "Malformed candidate")
  })
})

describe("derived evidence", {
  it("derives a fit's own diagnostics through the self path", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    binding <- candidate_of(p, root)$binding
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    evidence <- evidence_of(p, id)
    expect_identical(evidence$check, "hmc_diagnostics")
    expect_null(evidence$node_id)
    expect_identical(
      evidence$result,
      list(
        summary_kind = "hmc_diagnostics",
        severity = "ok",
        passed = TRUE,
        metrics = list(max_rhat = 1L)
      )
    )
    summary <- Filter(
      function(s) identical(s$job_id, binding$job_id),
      bg_read_summaries(p$handle)
    )[[1]]
    expect_identical(
      evidence$provenance,
      list(
        kind = "derived",
        node_id = p$fit,
        job_id = binding$job_id,
        artifact_ref = binding$artifact_ref,
        subject_artifact_ref = binding$artifact_ref,
        summary_ids = list(summary$summary_id)
      )
    )
  })

  it("stores exactly one result, the checked one", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    proposal <- propose(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    # A caller cannot smuggle a result into the applied action either.
    tampered <- proposal
    tampered$action["result"] <- list(NULL)
    expect_error(bg_research_apply(p$handle, tampered), "exactly one")

    state <- bg_research_apply(p$handle, proposal)
    record <- state$evidence[[tail(state$history, 1)[[1]]$id]]
    expect_identical(sum(names(record) == "result"), 1L)
    expect_false(any(startsWith(names(record), "result.")))
    expect_identical(
      bg_canonical_json(record$result),
      bg_canonical_json(proposal$derivation$result)
    )
  })

  it("keeps a vector of pass flags readable", {
    p <- derived_project()
    bg_register_node_kind(p$handle, "multi", executor = function(node, inputs) {
      subject <- names(inputs)[[1]]
      list(
        result = list(checked = inputs[[subject]]$mean),
        summaries = list(list(
          summary_kind = "posterior_predictive_check",
          severity = "warning",
          passed = c(TRUE, FALSE)
        )),
        metadata = list(
          provenance = list(consumed = list(D = list(), subject = subject))
        )
      )
    })
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    check <- add_check(p, p$fit, kind = "multi")
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Two pass flags",
      node_id = check,
      utility = "observable_fit"
    )
    passed <- evidence_of(p, id)$result$passed
    expect_identical(unlist(passed), c(TRUE, FALSE))
    expect_no_error(act(p, "note", candidate_id = root))
  })

  it("never writes a state it could not read back", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    before <- bg_research_state(p$handle)
    proposal <- propose(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    # Only the transitioned state, the first with evidence, is unreadable.
    local_mocked_bindings(
      bg_research_validate_evidence = function(state) {
        if (length(state$evidence)) {
          cli::cli_abort("Unreadable evidence.")
        }
        invisible(NULL)
      }
    )
    expect_error(bg_research_apply(p$handle, proposal), "Unreadable")
    after <- bg_research_state(p$handle)
    expect_identical(after$version, before$version)
    expect_length(after$evidence, 0L)
  })

  it("derives a check's diagnostics through the check path", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    check <- add_check(p, p$fit)
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Predictive check",
      node_id = check,
      utility = "predictive_performance"
    )
    evidence <- evidence_of(p, id)
    check_pad <- bg_node_pad(p$handle, check)
    expect_identical(evidence$check, "posterior_predictive_check")
    expect_identical(evidence$result$metrics, list(p = 0.4))
    expect_identical(evidence$provenance$node_id, check)
    expect_identical(evidence$provenance$job_id, check_pad$job_id)
    expect_identical(evidence$provenance$artifact_ref, check_pad$artifact_ref)
    expect_identical(
      evidence$provenance$subject_artifact_ref,
      candidate_of(p, root)$binding$artifact_ref
    )
  })

  it("prefers the check path even when the check returns its subject", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    inspect <- add_check(p, p$fit, kind = "inspect")
    binding <- candidate_of(p, root)$binding
    # Same bytes as the bound fit, so the self path would also match.
    expect_identical(
      bg_node_pad(p$handle, inspect)$artifact_ref,
      binding$artifact_ref
    )
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "LOO",
      node_id = inspect,
      utility = "predictive_performance"
    )
    evidence <- evidence_of(p, id)
    expect_identical(evidence$check, "loo_diagnostics")
    expect_identical(evidence$result$passed, FALSE)
    expect_identical(
      evidence$provenance$job_id,
      final_job_for(p$handle, inspect)$job_id
    )
  })

  it("keeps a fit's own diagnostics after a byte-identical rerun", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    binding <- candidate_of(p, root)$binding
    bg_run(p$handle, rerun = p$fit)
    rerun <- bg_node_pad(p$handle, p$fit)
    expect_false(identical(rerun$job_id, binding$job_id))
    expect_identical(rerun$artifact_ref, binding$artifact_ref)

    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    # The rerun is eligible too, and its summary is the most recent.
    expect_identical(evidence_of(p, id)$provenance$job_id, rerun$job_id)
  })

  it("is stale at apply when the execution changes after the proposal", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    proposal <- propose(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    expect_identical(proposal$derivation$provenance$kind, "derived")
    expect_identical(proposal$action$check, "hmc_diagnostics")
    bg_run(p$handle, rerun = p$fit)
    expect_error(bg_research_apply(p$handle, proposal), "Stale")
  })

  it("rejects a check whose subject is a different fit", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    other <- add_fit(p, scale = 4)
    check <- add_check(p, other)
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = root,
        label = "Wrong check",
        node_id = check,
        utility = "predictive_performance"
      ),
      "evaluated a different fit"
    )
  })

  it("names the components in which another execution differs", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    other <- add_fit(p, scale = 4)
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = root,
        label = "Other fit",
        node_id = other,
        utility = "convergence"
      ),
      "neither evaluated nor produced"
    )
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = root,
        label = "Other fit",
        node_id = other,
        utility = "convergence"
      ),
      "differs from the bound execution in A"
    )
    check <- add_check(p, other)
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = root,
        label = "Wrong check",
        node_id = check,
        utility = "predictive_performance"
      ),
      "differs from the bound execution in A"
    )
  })

  it("does not support sbc or compare nodes yet", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    for (kind in c("sbc", "compare")) {
      node <- add_check(p, p$fit, kind = kind)
      expect_error(
        propose(
          p,
          "evidence",
          candidate_id = root,
          label = kind,
          node_id = node,
          utility = "convergence"
        ),
        "not supported yet"
      )
    }
  })

  it("requires check when several summary kinds match", {
    p <- derived_project()
    fit2 <- add_fit(p, scale = 3, kind = "fit2")
    root <- act(p, "create", label = "Two kinds", node_id = fit2)
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = root,
        label = "Ambiguous",
        node_id = fit2,
        utility = "convergence"
      ),
      "Supply.*check"
    )
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Pareto k",
      node_id = fit2,
      check = "pareto_k_diagnostics",
      utility = "predictive_performance"
    )
    expect_identical(
      evidence_of(p, id)$result$summary_kind,
      "pareto_k_diagnostics"
    )
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = root,
        label = "Missing",
        node_id = fit2,
        check = "loo_diagnostics",
        utility = "predictive_performance"
      ),
      "no \"loo_diagnostics\" summary|no .*loo_diagnostics.* summary"
    )
  })

  it("validates the evidence form", {
    p <- derived_project()
    plain <- act(
      p,
      "create",
      label = "Plain",
      components = list(P = list(m = 1))
    )
    bound <- act(p, "create", label = "Bound", node_id = p$fit)
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = plain,
        label = "x",
        node_id = p$fit,
        utility = "convergence"
      ),
      "not bound"
    )
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = bound,
        label = "x",
        check = "hmc_diagnostics",
        utility = "convergence",
        result = list(ok = TRUE),
        node_id = p$fit
      ),
      "exactly one"
    )
    expect_error(
      propose(p, "evidence", candidate_id = bound, label = "x", utility = "u"),
      "exactly one"
    )
    # Presence decides the form: an explicit NULL result is still a result.
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = bound,
        label = "x",
        utility = "convergence",
        node_id = p$fit,
        result = NULL
      ),
      "exactly one"
    )
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = bound,
        node_id = p$fit,
        utility = "convergence"
      ),
      "Invalid"
    )
    # Asserted evidence keeps its required check.
    expect_error(
      propose(
        p,
        "evidence",
        candidate_id = bound,
        label = "x",
        utility = "u",
        result = list(ok = TRUE)
      ),
      "check"
    )
  })

  it("fills utility from the vocabulary for an omitted check", {
    vocabulary <- list(
      utilities = list(list(id = "convergence")),
      checks = list(list(id = "hmc_diagnostics", utilities = "convergence"))
    )
    p <- derived_project(
      mode = "enforce",
      rules = bg_ruleset("lab.derived", "1.0.0", vocabulary = vocabulary)
    )
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit
    )
    expect_identical(evidence_of(p, id)$utility, "convergence")
  })
})

describe("rules see derived evidence", {
  it("satisfies require_evidence(provenance = 'derived') only when derived", {
    rules <- list(
      bg_rule(
        id = "derived-convergence",
        on = "accept",
        require = list(
          type = "require_evidence",
          check = "hmc_diagnostics",
          provenance = "derived"
        ),
        message = "Derive convergence evidence from the run."
      ),
      bg_rule(
        id = "derived-utility",
        on = "accept",
        require = list(
          type = "require_utility_evidence",
          utility = "convergence",
          provenance = "derived"
        ),
        message = "Derive convergence evidence from the run."
      )
    )
    p <- derived_project(mode = "enforce", rules = rules)
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    asserted <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Asserted diagnostics",
      check = "hmc_diagnostics",
      utility = "convergence",
      result = list(max_rhat = 1)
    )
    held <- propose(p, "accept", candidate_id = root, evidence_ids = asserted)
    expect_false(held$allowed)
    expect_setequal(
      names(held$findings),
      c("derived-convergence", "derived-utility")
    )
    expect_error(bg_research_apply(p$handle, held), "held by policy")

    derived <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Derived diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    allowed <- propose(p, "accept", candidate_id = root, evidence_ids = derived)
    expect_true(allowed$allowed)
    expect_length(allowed$findings, 0L)
    bg_research_apply(p$handle, allowed)
  })

  it("lets create and revise rules see the prospective binding", {
    bound_only <- bg_rule(
      id = "bound-only",
      on = c("create", "revise"),
      require = list(type = "test.bound"),
      message = "Bind candidates to runs."
    )
    bg_register_rule_predicate(
      "test.bound",
      function(subject, args) !is.null(subject$candidate$binding),
      replace = TRUE
    )
    withr::defer(bg_rule_registry_reset())
    p <- derived_project(mode = "enforce", rules = list(bound_only))
    expect_false(
      propose(
        p,
        "create",
        label = "x",
        components = list(P = list(m = 1))
      )$allowed
    )
    expect_true(propose(p, "create", label = "x", node_id = p$fit)$allowed)

    root <- act(p, "create", label = "Bound", node_id = p$fit)
    expect_false(
      propose(
        p,
        "revise",
        candidate_id = root,
        label = "Unbound",
        components = list(P = list(m = 2))
      )$allowed
    )
    other <- add_fit(p, scale = 4)
    expect_true(
      propose(
        p,
        "revise",
        candidate_id = root,
        label = "Scale 4",
        node_id = other
      )$allowed
    )
  })
})

describe("stored derived evidence", {
  it("is malformed unless its structure and binding are consistent", {
    p <- derived_project()
    bound <- act(p, "create", label = "Bound", node_id = p$fit)
    plain <- act(
      p,
      "create",
      label = "Plain",
      components = list(P = list(m = 1))
    )
    derived <- act(
      p,
      "evidence",
      candidate_id = bound,
      label = "Derived",
      node_id = p$fit,
      utility = "convergence"
    )
    asserted <- act(
      p,
      "evidence",
      candidate_id = plain,
      label = "Asserted",
      check = "hmc_diagnostics",
      utility = "convergence",
      result = list(max_rhat = 1)
    )
    original <- bg_research_state(p$handle)
    provenance <- original$evidence[[derived]]$provenance
    mutations <- list(
      # A hand-edited asserted record on an unbound candidate.
      function(state) {
        state$evidence[[asserted]]$provenance <- list(kind = "derived")
        state
      },
      function(state) {
        state$evidence[[asserted]]$provenance <- provenance
        state
      },
      function(state) {
        state$evidence[[derived]]$provenance$kind <- "asserted"
        state
      },
      function(state) {
        state$evidence[[derived]]$provenance$job_id <- NULL
        state
      },
      function(state) {
        state$evidence[[derived]]$provenance$summary_ids <- list()
        state
      },
      function(state) {
        state$evidence[[derived]]$provenance$summary_ids <- list("")
        state
      },
      function(state) {
        state$evidence[[
          derived
        ]]$provenance$subject_artifact_ref <- "cas:sha256:0"
        state
      },
      function(state) {
        state$evidence[[derived]]$result <- list(max_rhat = 1)
        state
      },
      function(state) {
        state$evidence[[derived]]$result$passed <- "yes"
        state
      },
      function(state) {
        state$evidence[[derived]]$check <- "other_check"
        state
      },
      function(state) {
        state$candidates[[bound]]$binding <- NULL
        state
      }
    )
    for (mutate in mutations) {
      bg_write_json_atomic(
        bg_research_path(p$handle),
        mutate(original),
        sort_keys = FALSE
      )
      expect_error(bg_research_state(p$handle), "Malformed")
    }
    bg_write_json_atomic(
      bg_research_path(p$handle),
      original,
      sort_keys = FALSE
    )
    expect_identical(bg_research_state(p$handle), original)
  })
})

describe("reports and persistence", {
  it("reports bindings and derived evidence and survives reopen", {
    p <- derived_project()
    root <- act(p, "create", label = "Scale 3", node_id = p$fit)
    id <- act(
      p,
      "evidence",
      candidate_id = root,
      label = "Sampler diagnostics",
      node_id = p$fit,
      utility = "convergence"
    )
    state <- bg_research_state(p$handle)
    expect_identical(bg_snapshot(p$handle)$research, state)
    lines <- readLines(bg_export_report(p$handle, format = "md"))
    expect_true(any(grepl(
      paste0("Bound to node ", p$fit),
      lines,
      fixed = TRUE
    )))
    expect_true(any(grepl("### Derived evidence", lines, fixed = TRUE)))
    expect_true(any(grepl(
      paste0("Sampler diagnostics (", id, ")"),
      lines,
      fixed = TRUE
    )))

    path <- p$handle@path
    bg_close(p$handle)
    reopened <- bg_open(path)
    withr::defer(bg_close(reopened))
    expect_identical(bg_research_state(reopened), state)
  })
})

describe("cmdstanr integration", {
  it("binds a cmdstanr fit and accepts with derived convergence evidence", {
    skip_on_cran()
    skip_if_not_installed("cmdstanr")
    skip_if_not_installed("posterior")
    skip_if(
      is.null(tryCatch(cmdstanr::cmdstan_version(), error = function(e) NULL)),
      "CmdStan is not installed"
    )

    stan_file <- withr::local_tempfile(fileext = ".stan")
    writeLines(
      c(
        "data { int<lower=1> N; array[N] int<lower=0,upper=1> y; }",
        "parameters { real<lower=0,upper=1> theta; }",
        "model { theta ~ beta(1,1); y ~ bernoulli(theta); }"
      ),
      stan_file
    )
    handle <- bg_init(path = withr::local_tempdir())
    withr::defer(bg_close(handle))
    bg_use_cmdstanr(handle)
    data <- bg_add_node(handle, kind = "stan_data", label = "Bernoulli data")
    bg_set_node_data(
      handle,
      data,
      list(N = 10L, y = as.integer(c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)))
    )
    fit <- bg_add_node(
      handle,
      kind = "cmdstanr_fit",
      label = "Bernoulli fit",
      inputs = data,
      params = list(
        stan_file = stan_file,
        data_input = data,
        chains = 1,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 123
      )
    )
    expect_equal(bg_run(handle, targets = fit)$status, "succeeded")

    human <- list(id = "researcher", type = "human")
    bg_research_init(
      handle,
      "What is theta?",
      list(kind = "latent_inference"),
      human
    )
    bg_use_rules(
      handle,
      bg_ruleset(
        "lab.convergence",
        "1.0.0",
        vocabulary = list(
          utilities = list(list(id = "convergence")),
          checks = list(list(id = "hmc_diagnostics", utilities = "convergence"))
        ),
        rules = list(bg_rule(
          id = "derived-convergence",
          on = "accept",
          require = list(
            type = "require_utility_evidence",
            utility = "convergence",
            provenance = "derived"
          ),
          message = "Derive convergence evidence from the run."
        ))
      ),
      mode = "enforce",
      actor = human,
      rationale = "Require derived convergence evidence."
    )
    apply_action <- function(action) {
      state <- bg_research_apply(
        handle,
        bg_research_propose(handle, action, human, "Record a step.")
      )
      tail(state$history, 1)[[1]]$id
    }
    candidate <- apply_action(list(
      kind = "create",
      label = "Beta-Bernoulli",
      node_id = fit
    ))
    state <- bg_research_state(handle)
    expect_identical(
      state$candidates[[candidate]]$components$P$kind,
      "cmdstanr_fit"
    )
    evidence <- apply_action(list(
      kind = "evidence",
      candidate_id = candidate,
      label = "HMC diagnostics",
      node_id = fit,
      check = "hmc_diagnostics"
    ))
    record <- bg_research_state(handle)$evidence[[evidence]]
    expect_identical(record$utility, "convergence")
    expect_identical(record$provenance$kind, "derived")
    expect_identical(record$result$summary_kind, "hmc_diagnostics")

    accept <- bg_research_propose(
      handle,
      list(kind = "accept", candidate_id = candidate, evidence_ids = evidence),
      human,
      "Accept with derived convergence evidence."
    )
    expect_true(accept$allowed)
    bg_research_apply(handle, accept)
  })
})
