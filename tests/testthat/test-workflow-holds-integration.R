if (!exists("make_workflow_hold_fixture", mode = "function")) {
  sys.source(
    testthat::test_path("helper-workflow-fixtures.R"),
    envir = environment()
  )
}

describe("Workflow holds end-to-end", {
  it("refreshes workflow holds mid-run without replanning after each success", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle
    original_bg_plan <- bg_plan
    call_state <- new.env(parent = emptyenv())
    call_state$plan_calls <- 0L

    run_res <- testthat::with_mocked_bindings(
      bg_plan = function(...) {
        call_state$plan_calls <- call_state$plan_calls + 1L
        original_bg_plan(...)
      },
      bg_run(handle, targets = fixture$compare_id),
      .package = "bayesgrove"
    )

    expect_equal(
      run_res$status,
      "blocked",
      info = "A fresh warning summary should hold the downstream comparison before it executes."
    )
    expect_equal(
      run_res$summary$total_executed,
      2,
      info = "The source and fit nodes should finish before the new policy hold blocks the comparison."
    )
    expect_equal(
      call_state$plan_calls,
      1L,
      info = paste(
        "Explicit targets should reuse the full-project plan and avoid",
        "a second fingerprinting pass."
      )
    )
    expect_equal(
      run_res$metadata$held_by_policy[[fixture$compare_id]],
      "Review computation validity"
    )
  })

  it("derives review obligations, planner holds, and clears them after rerun", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle

    initial_run <- bg_run(handle, targets = fixture$fit_id)
    expect_equal(
      initial_run$summary$total_executed,
      2,
      info = "The source and fit nodes should execute before any comparison work is considered."
    )

    summaries <- bg_read_summaries(handle)
    expect_equal(
      length(summaries),
      1,
      info = "The fixture should persist exactly one warning summary from the fit."
    )
    expect_equal(
      summaries[[1]]$severity,
      "warning",
      info = "The persisted fit summary should represent the blocking issue."
    )

    context <- bg_build_workflow_context(handle, scope = "project")
    expect_true(
      fixture$fit_id %in% names(context$structural$nodes),
      info = "Workflow context should include the fitted node in project scope."
    )
    expect_true(
      fixture$compare_id %in% context$structural$ready_nodes,
      info = "Without external holds, the downstream comparison remains structurally ready."
    )
    expect_equal(
      unname(vapply(context$evidence$summaries, `[[`, character(1), "node_id")),
      fixture$fit_id,
      info = "Workflow context should expose the warning summary as lightweight evidence."
    )

    next_actions <- bg_next_actions(handle)
    expect_equal(
      length(next_actions$obligations),
      1,
      info = "A fresh warning summary should produce one blocking computation-review obligation."
    )
    obligation <- next_actions$obligations[[1]]
    expect_equal(obligation$kind, "review_computation_validity")
    expect_equal(obligation$severity, "blocking")
    expect_equal(obligation$basis$node_ids, fixture$fit_id)
    expect_equal(obligation$basis$summary_ids, summaries[[1]]$summary_id)

    expect_equal(
      sum(vapply(
        next_actions$actions,
        function(x) identical(x$kind, "record_decision"),
        logical(1)
      )),
      1,
      info = "The active computation-review obligation should surface a matching review action."
    )
    action <- Filter(
      function(x) identical(x$kind, "record_decision"),
      next_actions$actions
    )[[1]]
    expect_equal(action$kind, "record_decision")
    expect_equal(action$payload$decision_type, "computation_review")
    expect_equal(action$payload$node_ids, fixture$fit_id)
    expect_equal(action$payload$summary_ids, summaries[[1]]$summary_id)

    expect_equal(
      next_actions$metadata$external_holds[[fixture$compare_id]],
      "Review computation validity",
      info = "Downstream comparison work should be held for workflow reasons, not structurally blocked."
    )

    held_plan <- bg_plan(
      handle,
      external_holds = next_actions$metadata$external_holds
    )
    expect_false(
      fixture$compare_id %in% names(held_plan$blocked),
      info = "Held nodes must not be reported as structural blockers."
    )
    expect_equal(
      held_plan$external_blocked[[fixture$compare_id]],
      "Review computation validity",
      info = "The run plan should expose workflow holds in `external_blocked`."
    )
    expect_false(
      fixture$compare_id %in% held_plan$to_execute,
      info = "Externally held nodes must stay out of the execution set."
    )
    expect_equal(
      held_plan$held_by_policy[[fixture$compare_id]],
      "Review computation validity",
      info = "The orchestrator-facing plan should also expose policy holds under `held_by_policy`."
    )

    blocked_run <- bg_run(handle, targets = fixture$compare_id)
    expect_equal(
      blocked_run$status,
      "blocked",
      info = "The sync runner must honor workflow holds instead of executing blocked downstream work."
    )
    expect_equal(
      blocked_run$summary$total_executed,
      0,
      info = "Held nodes must not execute when a blocking workflow obligation is active."
    )

    bg_invalidate(handle, fixture$fit_id, recursive = TRUE)
    fixture$set_fit_summary_mode("ok")
    bg_update_node(handle, fixture$fit_id, params = list(revision = 2L))
    rerun <- bg_run(handle, targets = fixture$fit_id)
    expect_equal(
      rerun$summary$total_executed,
      1,
      info = "Only the invalidated fit should rerun; the unchanged source should stay cached."
    )

    refreshed_summaries <- bg_read_summaries(handle)
    expect_equal(
      sum(vapply(
        refreshed_summaries,
        function(x) identical(x$severity, "warning"),
        logical(1)
      )),
      1,
      info = "The original warning summary should remain for auditability."
    )
    expect_equal(
      sum(vapply(
        refreshed_summaries,
        function(x) identical(x$severity, "ok"),
        logical(1)
      )),
      1,
      info = "The rerun should persist a clean replacement summary."
    )
    expect_equal(
      sum(vapply(
        refreshed_summaries,
        function(x) isTRUE(x$is_stale),
        logical(1)
      )),
      1,
      info = "Exactly one stale summary should remain after replacing the problematic fit result."
    )

    cleared_actions <- bg_next_actions(handle)
    expect_equal(
      names(cleared_actions$obligations),
      character(),
      info = "Once the problematic result is replaced, no computation-review obligation should remain."
    )
    expect_equal(
      names(cleared_actions$actions),
      character(),
      info = "Clearing the blocking summary should also remove the matching review action."
    )
    expect_equal(
      names(cleared_actions$metadata$external_holds),
      character(),
      info = "Planner holds should clear when no blocking obligation remains."
    )

    cleared_plan <- bg_plan(
      handle,
      external_holds = cleared_actions$metadata$external_holds
    )
    expect_equal(length(cleared_plan$external_blocked), 0)
    expect_true(
      fixture$compare_id %in% cleared_plan$to_execute,
      info = "The downstream comparison should become executable again after the clean rerun."
    )
  })

  it("mid-run warning summaries are fresh, so freshness-strict packs fire within the same run (Phase 6)", {
    # Regression: mid-run summaries were appended without is_fresh, so pack
    # filters requiring isTRUE(summary$is_fresh) never saw them when the run
    # re-derived external holds between nodes. We capture the obligations the
    # run actually evaluates by wrapping bg_workflow_external_holds, and assert
    # the freshness-strict review_fit_criticism obligation is derived from the
    # in-memory (mid-run) state — not just from the post-run disk re-read.
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle

    branch_record <- bg_branch(handle, fixture$fit_id, label = "Branched fit")
    branched_fit_id <- branch_record$root_node_id
    bg_update_node(handle, branched_fit_id, params = list(revision = 2L))
    branched_compare_id <- bg_add_node(
      handle,
      kind = "compare",
      label = "Branched comparison",
      inputs = branched_fit_id
    )

    original_bg_workflow_external_holds <- bg_workflow_external_holds
    captured <- new.env(parent = emptyenv())
    captured$midrun_fit_criticism <- FALSE

    run_res <- testthat::with_mocked_bindings(
      bg_workflow_external_holds = function(...) {
        holds <- original_bg_workflow_external_holds(...)
        # The mid-run call sees the freshly-emitted warning in-memory. If the
        # is_fresh annotation is present, the freshness-strict fit-criticism
        # pack contributes a "Review fit criticism" hold reason. Without it,
        # only the (non-freshness-strict) computation-validity hold appears.
        captured$midrun_fit_criticism <- captured$midrun_fit_criticism ||
          any(vapply(
            holds,
            function(reason) identical(reason, "Review fit criticism"),
            logical(1)
          ))
        holds
      },
      bg_run(handle, targets = branched_compare_id),
      .package = "bayesgrove"
    )

    # The branched source and fit execute; the comparison is held (computation
    # validity always fires on a warning, so the hold itself is not the signal).
    expect_equal(run_res$status, "blocked")
    expect_equal(run_res$summary$total_executed, 2)
    expect_true(
      branched_compare_id %in% names(run_res$metadata$held_by_policy)
    )
    # The discriminating assertion: the freshness-strict fit-criticism hold
    # was derivable mid-run. Without the is_fresh annotation this is FALSE.
    expect_true(
      captured$midrun_fit_criticism,
      info = paste(
        "review_fit_criticism must fire within the same bg_run() call, which",
        "requires mid-run summaries to be annotated is_fresh."
      )
    )
  })
})
