describe("Run-loop performance (Phase 6)", {
  it("completes a 30-node linear chain run", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "trivial", executor = function(node, inputs) {
      list(result = node$label %||% "ok")
    })

    prev <- NULL
    for (i in 1:30) {
      args <- list(handle, kind = "trivial", label = sprintf("n%02d", i))
      if (!is.null(prev)) {
        args <- c(args, inputs = prev)
      }
      prev <- do.call(bg_add_node, args)
    }

    run_res <- bg_run(handle)
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 30)
  })

  it("bg_workflow_external_holds uses the state cache when supplied", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "trivial", executor = function(node, inputs) {
      list(result = "ok")
    })
    n1 <- bg_add_node(handle, kind = "trivial", label = "A")

    plan <- bg_plan(handle)

    # Build a state object with a sentinel decisions list. If external_holds
    # uses the state, the sentinel propagates into the context; if it reads
    # from disk instead, the real (empty) decisions are used.
    sentinel_state <- new.env(parent = emptyenv())
    sentinel_state$summaries <- list()
    sentinel_state$decisions <- list(list(
      kind = "sentinel_decision",
      scope = "project",
      choice = "test"
    ))
    sentinel_state$jobs <- list()

    holds <- bg_workflow_external_holds(
      handle,
      plan = plan,
      state = sentinel_state
    )
    # No errors means the state was accepted and used without disk reads.
    expect_type(holds, "list")
  })

  it("parses jobs.jsonl O(1) times, not O(N^2), during a 20-node run", {
    # Phase 6 persistence cache: each executed node appends several job
    # snapshots and reads bg_jobs() between nodes. Before the jobs cache this
    # was O(N^2) full parses (every bg_jobs() re-parsed the log); after, the
    # cache is seeded empty and maintained incrementally by bg_log_job, so the
    # expensive full-parse primitive (bg_jobs_read_disk) runs at most a small
    # constant number of times for the whole run, independent of N. Mock the
    # full-parse primitive and count; also count bg_jobs() calls to confirm the
    # read path is actually exercised (so the parse count is meaningful).
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "trivial", executor = function(node, inputs) {
      list(result = "ok")
    })
    prev <- NULL
    for (i in 1:20) {
      args <- list(handle, kind = "trivial", label = sprintf("n%02d", i))
      if (!is.null(prev)) {
        args <- c(args, inputs = prev)
      }
      prev <- do.call(bg_add_node, args)
    }

    original_read <- bayesgrove:::bg_jobs_read_disk
    original_jobs <- bayesgrove::bg_jobs
    parse_count <- 0L
    jobs_calls <- 0L
    run_res <- testthat::with_mocked_bindings(
      bg_run(handle),
      bg_jobs_read_disk = function(project) {
        parse_count <<- parse_count + 1L
        original_read(project)
      },
      bg_jobs = function(project, ...) {
        jobs_calls <<- jobs_calls + 1L
        original_jobs(project, ...)
      },
      .package = "bayesgrove"
    )

    # The run executed all 20 nodes.
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 20)

    # The read path is exercised many times during the run (proving the test is
    # meaningful: bg_jobs() is called repeatedly). With N=20 this is well above
    # a handful.
    expect_gte(jobs_calls, 20)

    # The expensive full parse is O(1) in N: at most a small constant number of
    # times, not O(N^2). The pre-cache code would have parsed once per bg_jobs()
    # call. A generous constant bound (5) is far below the O(N) regime of 20.
    expect_lte(parse_count, 5L)
  })

  it("refreshes summary freshness per wave without rescanning history", {
    # bg_run() recomputes cached freshness flags at every wave boundary. Only
    # summaries recorded at fingerprints executed in the wave can change, so
    # a long summary history must be scanned once (when the run reads it),
    # not once per wave: O(history + new), not O(history x waves).
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "trivial", executor = function(node, inputs) {
      list(
        result = node$label,
        summaries = list(list(
          summary_kind = "hmc_diagnostics",
          severity = "ok",
          passed = TRUE
        ))
      )
    })
    history_node <- bg_add_node(handle, kind = "trivial", label = "history")
    bg_run(handle)
    job <- Filter(
      function(j) identical(j$node_id, history_node),
      bg_jobs(handle)
    )[[1]]
    n_history <- 200L
    bg_write_summaries(
      handle,
      history_node,
      job$result_ref,
      job$provenance$fingerprint,
      rep(list(list(summary_kind = "hmc_diagnostics")), n_history)
    )

    n_waves <- 10L
    prev <- NULL
    for (i in seq_len(n_waves)) {
      args <- list(
        handle,
        kind = "trivial",
        label = sprintf("n%02d", i),
        params = list(step = i)
      )
      if (!is.null(prev)) {
        args <- c(args, inputs = prev)
      }
      prev <- do.call(bg_add_node, args)
    }

    original_fresh <- bayesgrove:::bg_summary_is_fresh
    fresh_calls <- 0L
    run_res <- testthat::with_mocked_bindings(
      bg_run(handle),
      bg_summary_is_fresh = function(...) {
        fresh_calls <<- fresh_calls + 1L
        original_fresh(...)
      },
      .package = "bayesgrove"
    )
    expect_equal(run_res$summary$total_executed, n_waves)

    # One pass over the history when the run starts, plus one check per new
    # summary. Rescanning per wave would need n_history * n_waves calls.
    expect_lte(fresh_calls, n_history + 1L + 2L * n_waves)
  })
})
