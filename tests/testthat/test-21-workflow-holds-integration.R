if (!exists("make_workflow_hold_fixture", mode = "function")) {
  sys.source(
    testthat::test_path("helper-workflow-fixtures.R"),
    envir = environment()
  )
}

describe("Workflow holds end-to-end", {
  it("derives review obligations, planner holds, and clears them after rerun", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle

    initial_run <- bg_run(handle, targets = fixture$fit_id, mode = "sync")
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

    blocked_run <- bg_run(handle, targets = fixture$compare_id, mode = "sync")
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
    rerun <- bg_run(handle, targets = fixture$fit_id, mode = "sync")
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
      cleared_actions$obligations,
      list(),
      info = "Once the problematic result is replaced, no computation-review obligation should remain."
    )
    expect_equal(
      cleared_actions$actions,
      list(),
      info = "Clearing the blocking summary should also remove the matching review action."
    )
    expect_equal(
      cleared_actions$metadata$external_holds,
      list(),
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
})
