# bg_run(rerun = ) scheduling and artifact-aware summary freshness
# (architecture/rules-and-evidence.md, section 2). Fixtures live in
# helper-provenance.R.

describe("representative executions", {
  it("picks the most recent byte-identical execution", {
    handle <- make_provenance_project()
    a <- bg_add_node(handle, kind = "src", label = "a")
    bg_run(handle, targets = a)
    b <- bg_add_node(handle, kind = "src", label = "b")
    bg_run(handle)
    job_a <- final_job_for(handle, a)

    # A byte-identical rerun of b adds a second eligible execution; the most
    # recent one in log order is the representative.
    bg_run(handle, rerun = b)
    job_b <- final_job_for(handle, b)
    eligible <- bg_execution_jobs(
      handle,
      job_a$provenance$artifact_ref,
      job_a$provenance$fingerprint
    )
    expect_identical(
      vapply(eligible, `[[`, character(1), "job_id"),
      c(job_a$job_id, job_b$job_id)
    )
    expect_identical(bg_node_pad(handle, a)$job_id, job_b$job_id)
  })
})

describe("bg_run(rerun = )", {
  # A node kind whose output differs on every execution, so a rerun replaces
  # the artifact its fingerprint resolves to.
  make_noisy_project <- function() {
    handle <- bg_init(
      path = withr::local_tempdir(.local_envir = parent.frame())
    )
    bg_register_node_kind(handle, "noisy", executor = function(node, inputs) {
      list(
        result = list(draw = stats::runif(1), upstream = names(inputs)),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          passed = TRUE,
          metrics = list(max_rhat = 1)
        ))
      )
    })
    handle
  }

  it("re-executes a cached node without re-executing its descendants", {
    handle <- make_noisy_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    b <- bg_add_node(handle, kind = "noisy", label = "b", inputs = a)
    bg_run(handle)
    old_a <- final_job_for(handle, a)
    old_b <- final_job_for(handle, b)

    run <- bg_run(handle, rerun = a)
    expect_equal(run$status, "succeeded")
    expect_equal(run$summary$total_executed, 1L)
    expect_identical(run$metadata$rerun_pending, character())
    new_a <- final_job_for(handle, a)
    expect_false(identical(new_a$job_id, old_a$job_id))
    expect_false(identical(new_a$result_ref, old_a$result_ref))
    # b's fingerprint is unchanged, so it is not re-executed and its recorded
    # consumption still names the earlier artifact.
    expect_identical(final_job_for(handle, b)$job_id, old_b$job_id)
    expect_identical(
      old_b$provenance$inputs[[1]]$artifact_ref,
      old_a$result_ref
    )
    expect_identical(bg_node_pad(handle, a)$job_id, new_a$job_id)
  })

  it("makes descendants wait for a pending rerun", {
    handle <- make_noisy_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    bg_run(handle)
    b <- bg_add_node(handle, kind = "noisy", label = "b", inputs = a)

    run <- bg_run(handle, rerun = a)
    expect_equal(run$summary$total_executed, 2L)
    new_a <- final_job_for(handle, a)
    b_job <- final_job_for(handle, b)
    expect_identical(
      b_job$provenance$inputs[[1]]$artifact_ref,
      new_a$result_ref
    )
  })

  it("adds reruns to explicit targets", {
    handle <- make_noisy_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    other <- bg_add_node(handle, kind = "noisy", label = "other")
    bg_run(handle)
    run <- bg_run(handle, targets = other, rerun = a)
    expect_true(a %in% run$targets)
    expect_equal(run$summary$total_executed, 1L)
  })

  it("reports a gated rerun as pending without executing it", {
    handle <- make_noisy_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    b <- bg_add_node(handle, kind = "noisy", label = "b", inputs = a)
    bg_run(handle)
    old_b <- final_job_for(handle, b)
    bg_add_gate(handle, a, b, "Proceed?", "yes")

    run <- bg_run(handle, rerun = b)
    expect_equal(run$status, "blocked")
    expect_identical(run$metadata$rerun_pending, b)
    expect_identical(final_job_for(handle, b)$job_id, old_b$job_id)
  })

  it("rejects unknown and inactive nodes", {
    handle <- make_noisy_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    expect_error(bg_run(handle, rerun = "node_missing"), "unknown")
    expect_error(bg_run(handle, rerun = NA_character_), "character vector")
    bg_retire_node(handle, a)
    expect_error(bg_run(handle, rerun = a), "retired or disabled")
  })
})

describe("artifact-aware summary freshness", {
  make_shared_fingerprint_project <- function() {
    handle <- bg_init(
      path = withr::local_tempdir(.local_envir = parent.frame())
    )
    bg_register_node_kind(handle, "noisy", executor = function(node, inputs) {
      list(
        result = list(draw = stats::runif(1)),
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          passed = TRUE,
          metrics = list(max_rhat = 1)
        ))
      )
    })
    handle
  }

  summary_freshness <- function(handle) {
    summaries <- bg_read_summaries(handle)
    stats::setNames(
      vapply(summaries, function(s) isTRUE(s$is_fresh), logical(1)),
      vapply(summaries, function(s) s$job_id, character(1))
    )
  }

  it("requires the summary's artifact to be the node's active artifact", {
    handle <- make_shared_fingerprint_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    bg_run(handle)
    first_job <- final_job_for(handle, a)$job_id
    expect_true(summary_freshness(handle)[[first_job]])

    bg_run(handle, rerun = a)
    second_job <- final_job_for(handle, a)$job_id
    freshness <- summary_freshness(handle)
    # Same fingerprint, new artifact: the earlier summary is stale but stays
    # readable as history.
    expect_false(freshness[[first_job]])
    expect_true(freshness[[second_job]])
    expect_length(bg_read_summaries(handle, include_stale = FALSE), 1L)
  })

  it("stales summaries of every node sharing the fingerprint", {
    handle <- make_shared_fingerprint_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    b <- bg_add_node(handle, kind = "noisy", label = "b")
    bg_run(handle)
    b_job <- final_job_for(handle, b)$job_id

    bg_run(handle, rerun = a)
    freshness <- summary_freshness(handle)
    expect_false(freshness[[b_job]])
    expect_true(freshness[[final_job_for(handle, a)$job_id]])
  })

  it("recomputes cached freshness flags before wave-boundary holds", {
    handle <- make_shared_fingerprint_project()
    a <- bg_add_node(handle, kind = "noisy", label = "a")
    bg_run(handle)
    old_summary <- names(bg_read_summaries(handle))

    seen <- new.env(parent = emptyenv())
    testthat::with_mocked_bindings(
      bg_workflow_external_holds = function(
        project,
        plan = NULL,
        state = NULL,
        graph = NULL
      ) {
        if (!is.null(state)) {
          seen$flags <- vapply(
            state$summaries,
            function(s) isTRUE(s$is_fresh),
            logical(1)
          )
        }
        list()
      },
      bg_run(handle, rerun = a),
      .package = "bayesgrove"
    )
    expect_false(seen$flags[[old_summary]])
    expect_identical(sum(seen$flags), 1L)
  })

  it("refreshes in-memory flags against an artifact index", {
    summaries <- list(
      s1 = list(
        node_id = "n",
        execution_fingerprint = "fp",
        artifact_ref = "cas:sha256:old"
      ),
      s2 = list(
        node_id = "n",
        execution_fingerprint = "fp",
        artifact_ref = "cas:sha256:new"
      )
    )
    index <- list(
      fp = list(
        artifact_ref = "cas:sha256:new",
        bindings = list(n = list(node_id = "n", status = "active"))
      )
    )
    refreshed <- bg_refresh_summary_freshness(
      NULL,
      summaries,
      predicted_fingerprints = list(n = "fp"),
      artifact_index = index
    )
    expect_false(refreshed$s1$is_fresh)
    expect_true(refreshed$s1$is_stale)
    expect_true(refreshed$s2$is_fresh)
  })
})

describe("plan state replaced on refresh", {
  it("drops a workflow hold that a rerun cleared", {
    fixture <- make_workflow_hold_fixture()
    handle <- fixture$handle
    first <- bg_run(handle)
    expect_equal(first$status, "blocked")
    expect_true(fixture$compare_id %in% names(first$metadata$held_by_policy))

    # The fit now produces a clean result. Rerunning it replaces the warning
    # summary (same fingerprint, new artifact), which clears the hold, so the
    # comparison runs in the next wave.
    fixture$set_fit_summary_mode("ok")
    run <- bg_run(handle, rerun = fixture$fit_id)
    expect_equal(run$status, "succeeded")
    expect_equal(run$summary$total_executed, 2L)
    expect_length(run$metadata$held_by_policy, 0L)
    expect_length(bg_next_actions(handle)$metadata$external_holds, 0L)
  })

  it("keeps no blockers of nodes outside a retargeted plan", {
    handle <- bg_init(path = withr::local_tempdir())
    bg_register_node_kind(handle, "src", executor = function(node, inputs) 1)
    a <- bg_add_node(handle, kind = "src", label = "a")
    gated <- bg_add_node(handle, kind = "src", label = "gated", inputs = a)
    bg_add_gate(handle, a, gated, "Proceed?", "yes")
    other <- bg_add_node(handle, kind = "src", label = "other")

    full <- bg_plan(handle)
    expect_true(gated %in% names(full$blocked))
    graph <- bg_dagri_recompute_state(bg_active_graph(handle))
    retargeted <- bg_retarget_run_plan(handle, full, graph, targets = other)
    expect_identical(
      names(retargeted$blocked),
      names(retargeted$graph_plan$blocked)
    )
    expect_false(gated %in% names(retargeted$blocked))
  })
})

describe("partial rerun failures", {
  it("removes finalized reruns from the pending set when a sibling fails", {
    handle <- bg_init(path = withr::local_tempdir())
    flag <- file.path(withr::local_tempdir(), "fail")
    bg_register_node_kind(handle, "flaky", executor = function(node, inputs) {
      if (isTRUE(node$params$may_fail) && file.exists(node$params$flag)) {
        stop("flaky failure")
      }
      list(draw = stats::runif(1))
    })
    ok <- bg_add_node(
      handle,
      kind = "flaky",
      label = "ok",
      params = list(flag = flag, may_fail = FALSE)
    )
    bad <- bg_add_node(
      handle,
      kind = "flaky",
      label = "bad",
      params = list(flag = flag, may_fail = TRUE)
    )
    bg_run(handle)
    writeLines("x", flag)

    run <- bg_run(handle, rerun = c(ok, bad))
    expect_equal(run$status, "failed")
    expect_identical(run$metadata$rerun_pending, bad)
    expect_identical(final_job_for(handle, ok)$status, "succeeded")
    expect_identical(final_job_for(handle, bad)$status, "failed")
  })
})
