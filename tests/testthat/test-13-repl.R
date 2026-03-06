describe("Interactive REPL", {
  it("aborts when run non-interactively", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # testthat runs non-interactively by default, so it should error
    expect_error(bg_repl(handle), "must be run in an interactive R session")
  })

  it("formats status rows for display", {
    rows <- bayesgrove:::bg_repl_status_rows(list(
      workflow_state = "blocked",
      runnable_nodes = 0L,
      cached_nodes = 6L,
      blocked_nodes = 1L,
      pending_gates = 1L,
      active_jobs = 0L,
      total_nodes = 7L
    ))

    expect_equal(rows[["Workflow state"]], "Blocked")
    expect_equal(rows[["Runnable now"]], "0")
    expect_equal(rows[["Cached nodes"]], "6")
    expect_equal(rows[["Total nodes"]], "7")
  })

  it("builds node rows with cache and policy-hold details", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle

    bg_run(handle, targets = fixture$fit_id, mode = "sync")
    rows <- bayesgrove:::bg_repl_node_rows(handle)
    rows_by_id <- stats::setNames(rows, vapply(rows, `[[`, character(1), "id"))

    expect_equal(rows_by_id[[fixture$source_id]]$state, "cached")
    expect_equal(rows_by_id[[fixture$fit_id]]$state, "cached")
    expect_equal(rows_by_id[[fixture$compare_id]]$state, "held")
    expect_match(rows_by_id[[fixture$compare_id]]$detail, "^Policy hold:")
  })

  it("builds gate rows with route context", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      NULL
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      NULL
    })

    source_id <- bg_add_node(handle, kind = "source", label = "Data source")
    fit_id <- bg_add_node(
      handle,
      kind = "fit",
      label = "Baseline fit",
      inputs = source_id
    )
    gate <- bg_add_gate(
      handle,
      from = source_id,
      to = fit_id,
      prompt = "Proceed to model fitting?",
      options = c("yes", "no")
    )

    rows <- bayesgrove:::bg_repl_gate_rows(bg_pending_gates(handle))

    expect_length(rows, 1L)
    expect_equal(rows[[1]]$id, gate$id)
    expect_equal(rows[[1]]$route, "Data source -> Baseline fit")
    expect_equal(rows[[1]]$options, "yes, no")
  })
})
