describe("Project Status and Results", {
  it("provides correct status for an empty or unrun project", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    st <- bg_status(handle)
    expect_equal(st$workflow_state, "open")
    expect_equal(st$health, "ok")
    expect_true(is.null(st$last_run_id))
    expect_true(is.character(st$messages))

    bg_register_node_kind(handle, "data", executor = function(node, inputs) 42)
    bg_add_node(handle, "data", label = "A")

    st2 <- bg_status(handle)
    expect_equal(st2$workflow_state, "idle")
    expect_equal(st2$runnable_nodes, 1)
    expect_equal(st2$health, "ok")
  })

  it("identifies blocked workflows correctly", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) 42)
    n1 <- bg_add_node(handle, "data", label = "A")
    n2 <- bg_add_node(handle, "data", label = "B", inputs = n1)

    bg_add_gate(
      handle,
      from = n1,
      to = n2,
      prompt = "OK?",
      alternatives = c("yes", "no")
    )

    st <- bg_status(handle)
    expect_equal(st$workflow_state, "blocked")
    expect_equal(st$pending_gates, 1)
    expect_equal(st$health, "warning")
  })

  it("can execute, cache to disk, and retrieve results", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(secret = 42, label = node$label)
    })

    n1 <- bg_add_node(handle, "data", label = "TestNode")

    # Run
    bg_run(handle)

    st <- bg_status(handle)
    expect_equal(st$workflow_state, "idle")
    expect_equal(st$runnable_nodes, 0)
    expect_false(is.null(st$last_run_id))

    # Retrieve result
    res <- bg_result(handle, n1)
    expect_equal(res$secret, 42)
    expect_equal(res$label, "TestNode")

    # Check that it actually lives on disk
    cache_dir <- file.path(tmp, ".bayesgrove", "cache", "sha256")
    expect_true(dir.exists(cache_dir))
    expect_gt(length(list.files(cache_dir, recursive = TRUE)), 0)
  })

  it("bg_status builds the plan once, not twice", {
    # Regression: bg_status() previously called bg_plan() once to build a seed
    # (for fingerprint-aware summary reads) and a second time with the derived
    # external holds. The holds-aware plan now reuses the seed via
    # bg_refresh_run_plan, so bg_plan should be invoked exactly once.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) 42)
    bg_add_node(handle, "data", label = "A")

    call_count <- 0L
    original_plan <- bayesgrove::bg_plan
    testthat::with_mocked_bindings(
      st <- bg_status(handle),
      bg_plan = function(...) {
        call_count <<- call_count + 1L
        original_plan(...)
      }
    )
    expect_equal(call_count, 1L)
    # And the status result is still correct.
    expect_equal(st$workflow_state, "idle")
    expect_equal(st$runnable_nodes, 1)
  })
})
