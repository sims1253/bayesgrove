describe("bg_default_bayesian_decision_summary_ids", {
  it("returns sorted unique ids from character metadata", {
    decision <- list(
      metadata = list(summary_ids = c("sum_b", "sum_a", "sum_b"))
    )
    expect_identical(
      bg_default_bayesian_decision_summary_ids(decision),
      c("sum_a", "sum_b")
    )
  })

  it("coerces list-typed metadata as read back from JSONL", {
    # bg_read_decisions() parses with simplifyVector = FALSE, so multi-element
    # id fields arrive as lists; sorting them raised "'x' must be atomic".
    decision <- list(
      metadata = list(
        summary_ids = list("sum_b", "sum_a"),
        summary_id = "sum_c"
      )
    )
    expect_identical(
      bg_default_bayesian_decision_summary_ids(decision),
      c("sum_a", "sum_b", "sum_c")
    )
  })

  it("returns empty character for missing metadata", {
    expect_identical(
      bg_default_bayesian_decision_summary_ids(list()),
      character(0)
    )
  })
})

describe("computation_review decisions with multiple summary ids", {
  it("resolves the obligation after a disk round trip", {
    tmp <- tempfile("pack-helpers-")
    dir.create(tmp, recursive = TRUE)
    withr::defer(unlink(tmp, recursive = TRUE))

    handle <- bg_init(
      path = tmp,
      workflow_packs = "bayesgrove.default_bayesian"
    )

    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(
        result = list(ok = FALSE),
        summaries = list(
          list(
            summary_kind = "hmc_diagnostics",
            passed = FALSE,
            severity = "warning",
            metrics = list(divergences = 4)
          ),
          list(
            summary_kind = "loo_diagnostics",
            passed = FALSE,
            severity = "warning",
            metrics = list(pareto_k_high = 1)
          )
        )
      )
    })

    fit_id <- bg_add_node(handle, kind = "fit", label = "Fit")
    bg_run(handle, targets = fit_id)

    guide <- bg_next_actions(handle)
    obligations <- Filter(
      function(o) identical(o$kind, "review_computation_validity"),
      guide$obligations
    )
    expect_length(obligations, 1L)
    obligation <- obligations[[1]]
    summary_ids <- unlist(obligation$basis$summary_ids)
    expect_gte(length(summary_ids), 2L)

    bg_record_decision(
      handle,
      scope = "project",
      prompt = "Review both warning summaries",
      choice = "accept",
      rationale = "Both diagnostics reviewed together.",
      kind = "computation_review",
      metadata = list(summary_ids = summary_ids)
    )

    # bg_next_actions re-reads the decision from JSONL; before the coercion
    # fix this crashed with "'x' must be atomic".
    guide_after <- bg_next_actions(handle)
    kinds <- vapply(guide_after$obligations, `[[`, character(1), "kind")
    expect_false("review_computation_validity" %in% kinds)
  })
})
