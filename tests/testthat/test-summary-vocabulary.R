describe("Summary vocabulary (Phase 3)", {
  it("returns a non-empty named registry of known summary kinds", {
    vocab <- bg_summary_vocabulary()
    expect_true(is.list(vocab))
    expect_gte(length(vocab), 10)

    for (descriptor in vocab) {
      expect_true(all(
        c(
          "kind",
          "title",
          "expected_metrics",
          "emitted_by",
          "consumed_by_packs"
        ) %in%
          names(descriptor)
      ))
    }

    expect_true("hmc_diagnostics" %in% names(vocab))
    expect_true("posterior_predictive_check" %in% names(vocab))
  })

  it("warns with a suggestion when an unknown summary_kind is written", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        result = 1,
        summaries = list(list(
          # Typo: missing the trailing 's'.
          summary_kind = "hmc_diagnostic",
          severity = "ok",
          metrics = list(divergences = 0)
        ))
      )
    })
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    expect_warning(
      bg_run(handle, targets = n1),
      "hmc_diagnostics"
    )

    summaries <- bg_read_summaries(handle)
    expect_gte(length(summaries), 1)
  })

  it("aborts when severity is invalid", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        result = 1,
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = "critical",
          metrics = list()
        ))
      )
    })
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    run_res <- bg_run(handle, targets = n1)
    expect_equal(run_res$status, "failed")
    expect_match(run_res$error$message, "invalid")
  })

  it("round-trips a valid summary", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        result = 1,
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          passed = TRUE,
          severity = "ok",
          metrics = list(divergences = 0, max_rhat = 1.0)
        ))
      )
    })
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    expect_no_warning(bg_run(handle, targets = n1))

    summaries <- bg_read_summaries(handle)
    expect_gte(length(summaries), 1)
    entry <- summaries[[1]]
    expect_equal(entry$summary_kind, "hmc_diagnostics")
    expect_equal(entry$schema_version, 2L)
    expect_false(is.null(entry$seq))
  })

  it("registers a custom summary kind that silences the unknown-kind warning", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_summary_kind(
      handle,
      kind = "my_custom_check",
      title = "Custom domain check"
    )

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        result = 1,
        summaries = list(list(
          summary_kind = "my_custom_check",
          severity = "ok",
          metrics = list(score = 0.5)
        ))
      )
    })
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    expect_no_warning(bg_run(handle, targets = n1))
  })

  it("persists custom summary kinds across reopen", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_summary_kind(
      handle,
      kind = "persisted_check",
      title = "Persisted"
    )
    bg_close(handle)

    reopened <- bg_open(path = tmp)
    known <- bg_known_summary_kinds(reopened)
    expect_true("persisted_check" %in% known)
  })
})
