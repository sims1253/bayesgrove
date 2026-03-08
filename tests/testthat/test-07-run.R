describe("Planning and Orchestration", {
  it("creates an execution plan with cache classification", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data")
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    # Run plan without executing
    plan <- bg_plan(handle)

    expect_equal(plan$mode, "sync")
    expect_true(n1 %in% plan$missing_results)
    expect_true(n1 %in% plan$to_execute)
    expect_false(n1 %in% plan$cache_hits)
  })

  it("executes missing nodes synchronously", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    executor_ran <- FALSE
    mock_executor <- function(node, inputs) {
      executor_ran <<- TRUE
      "mock_data"
    }

    bg_register_node_kind(handle, "data", executor = mock_executor)
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    run_res <- bg_run(handle, mode = "sync")

    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 1)
    expect_true(executor_ran)

    # Check if the result was cached
    plan2 <- bg_plan(handle)
    expect_true(n1 %in% plan2$cache_hits)
    expect_false(n1 %in% plan2$to_execute)
  })

  it("uses a single full plan for untargeted sync runs without workflow packs", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    original_bg_plan <- bg_plan
    plan_calls <- 0L

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      "mock_data"
    })
    bg_add_node(handle, kind = "data", label = "A")

    run_res <- testthat::with_mocked_bindings(
      bg_plan = function(...) {
        plan_calls <<- plan_calls + 1L
        original_bg_plan(...)
      },
      bg_run(handle, mode = "sync"),
      .package = "bayesgrove"
    )

    expect_equal(run_res$status, "succeeded")
    expect_equal(plan_calls, 1L)
  })

  it("passes resolved upstream artifacts to executors", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    mock_source <- function(node, inputs) {
      "source_data"
    }
    mock_transform <- function(node, inputs) {
      paste0("transformed_", inputs[[1]])
    }

    bg_register_node_kind(handle, "source", executor = mock_source)
    bg_register_node_kind(handle, "transform", executor = mock_transform)

    n1 <- bg_add_node(handle, kind = "source")
    n2 <- bg_add_node(handle, kind = "transform", inputs = n1)

    run_res <- bg_run(handle, mode = "sync")
    expect_equal(run_res$summary$total_executed, 2)

    # Use bg_result to verify the output of n2
    plan <- bg_plan(handle)
    expect_true(n2 %in% plan$cache_hits)

    result_n2 <- bg_result(handle, n2)
    expect_equal(result_n2, "transformed_source_data")
  })

  it("refreshes downstream bindings across multi-step sync execution", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "source", executor = function(node, inputs) {
      "source_data"
    })
    bg_register_node_kind(handle, "transform", executor = function(node, inputs) {
      paste0("transform_", inputs[[1]])
    })
    bg_register_node_kind(handle, "sink", executor = function(node, inputs) {
      paste0("sink_", inputs[[1]])
    })

    n1 <- bg_add_node(handle, kind = "source")
    n2 <- bg_add_node(handle, kind = "transform", inputs = n1)
    n3 <- bg_add_node(handle, kind = "sink", inputs = n2)

    run_res <- bg_run(handle, mode = "sync")

    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 3)
    expect_equal(bg_result(handle, n3), "sink_transform_source_data")
  })

  it("rebuilds downstream bindings for multi-input nodes during sync runs", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "left", executor = function(node, inputs) {
      "left"
    })
    bg_register_node_kind(handle, "right", executor = function(node, inputs) {
      "right"
    })
    bg_register_node_kind(handle, "join", executor = function(node, inputs) {
      paste(sort(unname(unlist(inputs))), collapse = "+")
    })

    n1 <- bg_add_node(handle, kind = "left")
    n2 <- bg_add_node(handle, kind = "right")
    n3 <- bg_add_node(handle, kind = "join", inputs = c(n1, n2))

    run_res <- bg_run(handle, mode = "sync")

    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 3)
    expect_equal(bg_result(handle, n3), "left+right")
  })
})
