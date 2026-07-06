describe("Planning and Orchestration", {
  it("creates an execution plan with cache classification", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data")
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    # Run plan without executing
    plan <- bg_plan(handle)

    expect_true(n1 %in% plan$missing_results)
    expect_true(n1 %in% plan$to_execute)
    expect_false(n1 %in% plan$cache_hits)
  })

  it("executes missing nodes synchronously", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    exec_state <- new.env(parent = emptyenv())
    exec_state$ran <- FALSE
    mock_executor <- function(node, inputs) {
      exec_state$ran <- TRUE
      "mock_data"
    }

    bg_register_node_kind(handle, "data", executor = mock_executor)
    n1 <- bg_add_node(handle, kind = "data", label = "A")

    run_res <- bg_run(handle)

    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 1)
    expect_true(exec_state$ran)

    # Check if the result was cached
    plan2 <- bg_plan(handle)
    expect_true(n1 %in% plan2$cache_hits)
    expect_false(n1 %in% plan2$to_execute)
  })

  it("uses a single full plan for untargeted sync runs without workflow packs", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    original_bg_plan <- bg_plan
    call_state <- new.env(parent = emptyenv())
    call_state$plan_calls <- 0L

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      "mock_data"
    })
    bg_add_node(handle, kind = "data", label = "A")

    run_res <- testthat::with_mocked_bindings(
      bg_plan = function(...) {
        call_state$plan_calls <- call_state$plan_calls + 1L
        original_bg_plan(...)
      },
      bg_run(handle),
      .package = "bayesgrove"
    )

    expect_equal(run_res$status, "succeeded")
    expect_equal(call_state$plan_calls, 1L)
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

    run_res <- bg_run(handle)
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
    bg_register_node_kind(
      handle,
      "transform",
      executor = function(node, inputs) {
        paste0("transform_", inputs[[1]])
      }
    )
    bg_register_node_kind(handle, "sink", executor = function(node, inputs) {
      paste0("sink_", inputs[[1]])
    })

    n1 <- bg_add_node(handle, kind = "source")
    n2 <- bg_add_node(handle, kind = "transform", inputs = n1)
    n3 <- bg_add_node(handle, kind = "sink", inputs = n2)

    run_res <- bg_run(handle)

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

    run_res <- bg_run(handle)

    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 3)
    expect_equal(bg_result(handle, n3), "left+right")
  })
})

describe("Artifact store (Phase 2.3)", {
  it("writes temp files inside the project cache root, not system temp", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(result = 42)
    })
    n1 <- bg_add_node(handle, kind = "data", label = "A")
    bg_run(handle, targets = n1)

    cache_root <- file.path(tmp, ".bayesgrove", "cache", "sha256")

    # No leftover temp files in the system temp dir from this run.
    sys_tmp <- list.files(
      tempdir(),
      pattern = "\\.rds\\.tmp$",
      full.names = TRUE
    )
    expect_equal(length(sys_tmp), 0)

    # The artifact lives under the project cache root.
    artifacts <- list.files(
      cache_root,
      pattern = "\\.rds$",
      recursive = TRUE,
      full.names = TRUE
    )
    expect_gte(length(artifacts), 1)
  })

  it("stores and fetches an artifact round-trip", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    ref <- bg_store_artifact(handle, "node_x", "sha256:abc", list(value = 7))
    expect_match(ref, "^cas:sha256:")
    expect_equal(bg_fetch_artifact(handle, ref)$value, 7)
  })

  it("cleans up the temp file on a cache hit (no leak)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    cache_root <- file.path(tmp, ".bayesgrove", "cache", "sha256")

    # First store writes the artifact.
    ref <- bg_store_artifact(handle, "node_x", "sha256:abc", list(value = 7))

    # Second store with identical content hits the cache (dest exists).
    ref2 <- bg_store_artifact(handle, "node_y", "sha256:def", list(value = 7))
    expect_equal(ref, ref2)

    # No leftover .rds.tmp files in the cache root.
    leftover <- list.files(
      cache_root,
      pattern = "\\.rds\\.tmp$",
      recursive = TRUE,
      full.names = TRUE
    )
    expect_equal(length(leftover), 0)
  })

  it("bg_compute_wave groups independent siblings into one wave", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    # Three independent base nodes (no edges between them).
    a <- bg_add_node(handle, kind = "data", label = "A")
    b <- bg_add_node(handle, kind = "data", label = "B")
    c <- bg_add_node(handle, kind = "data", label = "C")

    plan <- bg_plan(handle)
    wave <- bayesgrove:::bg_compute_wave(plan)

    # All three base nodes are ready in wave 1 (inputs satisfied trivially).
    expect_setequal(wave, c(a, b, c))
  })

  it("bg_compute_wave emits one-node waves for a dependency chain", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    a <- bg_add_node(handle, kind = "data", label = "A")
    b <- bg_add_node(handle, kind = "data", label = "B", inputs = a)
    cc <- bg_add_node(handle, kind = "data", label = "C", inputs = b)

    plan <- bg_plan(handle)
    # Only the root is ready initially; B and C await A's artifact.
    expect_setequal(bayesgrove:::bg_compute_wave(plan), a)
  })

  it("bg_run(parallel = 'never') runs a graph and produces stable artifacts", {
    # The wave restructure must produce the same artifacts/fingerprints as the
    # pre-restructure sequential loop. A two-level graph (root -> two leaves)
    # exercises a multi-node wave (the two leaves) after the root wave. The
    # executor folds its input into the result so each leaf's artifact is
    # genuinely distinct (avoids a fingerprint/content collision that would
    # mask a node-identity mix-up).
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    counter <- new.env(parent = emptyenv())
    counter$n <- 0L
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      counter$n <- counter$n + 1L
      list(
        label = node$label,
        upcase = toupper(node$label),
        order = counter$n,
        suffix = node$params$suffix %||% "?"
      )
    })
    root <- bg_add_node(handle, kind = "data", label = "root")
    # Distinct params so the two leaves get distinct fingerprints (label alone
    # is display-only and does not enter the fingerprint).
    leaf1 <- bg_add_node(
      handle,
      kind = "data",
      label = "leaf1",
      params = list(suffix = "1"),
      inputs = root
    )
    leaf2 <- bg_add_node(
      handle,
      kind = "data",
      label = "leaf2",
      params = list(suffix = "2"),
      inputs = root
    )

    run_res <- bg_run(handle, parallel = "never")
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 3)

    # Artifacts are retrievable and correctly keyed per node identity.
    expect_equal(bg_result(handle, root)$label, "root")
    expect_equal(bg_result(handle, leaf1)$upcase, "LEAF1")
    expect_equal(bg_result(handle, leaf2)$upcase, "LEAF2")
    expect_equal(bg_result(handle, leaf1)$suffix, "1")
    expect_equal(bg_result(handle, leaf2)$suffix, "2")
  })
})
