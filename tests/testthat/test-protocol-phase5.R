describe("Pack composition equivalence (Phase 5.5)", {
  it("stan_workflow yields identical obligations to its constituent packs", {
    tmp_bundle <- withr::local_tempdir()
    handle_bundle <- bg_init(
      path = tmp_bundle,
      workflow_packs = list("bayesgrove.stan_workflow")
    )

    tmp_parts <- withr::local_tempdir()
    handle_parts <- bg_init(
      path = tmp_parts,
      workflow_packs = list(
        "bayesgrove.default_bayesian",
        "bayesgrove.process_guidance",
        "bayesgrove.model_taxonomy",
        "bayesgrove.prior_workflow",
        "bayesgrove.model_checks",
        "bayesgrove.model_selection",
        "bayesgrove.stan_workflow"
      )
    )

    # Set up identical graphs so obligations have the same scope.
    for (h in list(handle_bundle, handle_parts)) {
      bg_register_node_kind(h, "fit")
      bg_add_node(h, kind = "fit", label = "Seed")
    }

    bundle_actions <- bg_next_actions(handle_bundle)
    parts_actions <- bg_next_actions(handle_parts)

    bundle_kinds <- sort(vapply(
      bundle_actions$obligations,
      function(o) o$kind,
      character(1)
    ))
    parts_kinds <- sort(vapply(
      parts_actions$obligations,
      function(o) o$kind,
      character(1)
    ))

    expect_setequal(unname(bundle_kinds), unname(parts_kinds))
  })
})

describe("Advisory mode (Phase 5.3)", {
  it("advisory pack produces obligations but zero external holds", {
    tmp_blocking <- withr::local_tempdir()
    handle_b <- bg_init(
      path = tmp_blocking,
      workflow_packs = list(list(
        pack_id = "bayesgrove.default_bayesian",
        config = list(strictness = "blocking")
      ))
    )

    tmp_advisory <- withr::local_tempdir()
    handle_a <- bg_init(
      path = tmp_advisory,
      workflow_packs = list(list(
        pack_id = "bayesgrove.default_bayesian",
        config = list(strictness = "advisory")
      ))
    )

    for (h in list(handle_b, handle_a)) {
      bg_register_node_kind(h, "fit")
      seed_id <- bg_add_node(h, kind = "fit", label = "Seed")
      bg_branch(h, seed_id, label = "Alternative")
    }

    actions_b <- bg_next_actions(handle_b)
    actions_a <- bg_next_actions(handle_a)

    # Advisory still surfaces obligations.
    expect_gte(length(actions_a$obligations), 1)

    # Advisory obligations have severity "advisory", not "blocking".
    advisory_severities <- unique(vapply(
      actions_a$obligations,
      function(o) o$severity,
      character(1)
    ))
    expect_false("blocking" %in% advisory_severities)

    # Advisory mode produces zero external holds.
    holds_a <- bg_workflow_external_holds(handle_a)
    expect_length(holds_a, 0)

    # Blocking mode obligations retain "blocking" severity (would produce
    # external holds if diagnostics were poor).
    blocking_severities <- unique(vapply(
      actions_b$obligations,
      function(o) o$severity,
      character(1)
    ))
    expect_true("blocking" %in% blocking_severities)
  })
})

describe("Project-level strictness default (Phase 8.1)", {
  it("packs without explicit strictness inherit the project default", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      config = list(workflow_strictness = "advisory"),
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    bg_branch(handle, seed_id, label = "Alternative")

    actions <- bg_next_actions(handle)
    # Obligations still surface...
    expect_gte(length(actions$obligations), 1)
    # ...but inherit the project advisory default, so none are blocking.
    severities <- unique(vapply(
      actions$obligations,
      function(o) o$severity,
      character(1)
    ))
    expect_false("blocking" %in% severities)
    # And no external holds are produced.
    expect_length(bg_workflow_external_holds(handle), 0)
  })

  it("an explicit per-ref strictness overrides the project default", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      config = list(workflow_strictness = "advisory"),
      workflow_packs = list(list(
        pack_id = "bayesgrove.default_bayesian",
        config = list(strictness = "blocking")
      ))
    )

    bg_register_node_kind(handle, "fit")
    seed_id <- bg_add_node(handle, kind = "fit", label = "Seed")
    bg_branch(handle, seed_id, label = "Alternative")

    actions <- bg_next_actions(handle)
    # Explicit blocking wins over the project advisory default.
    severities <- unique(vapply(
      actions$obligations,
      function(o) o$severity,
      character(1)
    ))
    expect_true("blocking" %in% severities)
  })
})

describe("Deprecation alias (Phase 5.2)", {
  it("maps bayesguide.default_bayesian to bayesgrove.default_bayesian with warning", {
    expect_warning(
      ref <- bg_normalize_workflow_pack_ref("bayesguide.default_bayesian"),
      "deprecated"
    )
    expect_equal(ref$pack_id, "bayesgrove.default_bayesian")
  })
})
