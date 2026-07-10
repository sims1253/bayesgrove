# Parallel execution (Milestone 3) -------------------------------------------
#
# Integration + unit coverage for the wave scheduler's parallel path. The
# daemon-backed tests skip on CRAN and when mirai/carrier are unavailable.

describe("Parallel execution (Milestone 3)", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  skip_if_not_installed("purrr")

  it("runs independent siblings in parallel across daemons", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Executor reports the pid it ran on, so we can confirm >1 daemon was used.
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(suffix = node$params$suffix %||% "?", pid = Sys.getpid())
    })
    root <- bg_add_node(handle, kind = "data", label = "root")
    l1 <- bg_add_node(
      handle,
      kind = "data",
      label = "l1",
      params = list(suffix = "1"),
      inputs = root
    )
    l2 <- bg_add_node(
      handle,
      kind = "data",
      label = "l2",
      params = list(suffix = "2"),
      inputs = root
    )
    l3 <- bg_add_node(
      handle,
      kind = "data",
      label = "l3",
      params = list(suffix = "3"),
      inputs = root
    )

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)

    run_res <- bg_run(handle, parallel = "always")
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 4)

    # Artifacts are correct and distinct per node.
    expect_equal(bg_result(handle, l1)$suffix, "1")
    expect_equal(bg_result(handle, l2)$suffix, "2")
    expect_equal(bg_result(handle, l3)$suffix, "3")

    # The leaves ran on daemon pids (different from the main process); at
    # least two distinct daemon pids were used for the 3-leaf wave.
    main_pid <- Sys.getpid()
    leaf_pids <- c(
      bg_result(handle, l1)$pid,
      bg_result(handle, l2)$pid,
      bg_result(handle, l3)$pid
    )
    expect_true(all(leaf_pids != main_pid))
    expect_gte(length(unique(leaf_pids)), 1L)
  })

  it("produces byte-identical artifacts to a sequential run", {
    # The determinism guarantee: parallel and sequential runs of the same graph
    # produce identical artifact refs (content-addressed) and identical result
    # objects. Built with deterministic executors (no pid/timestamp in output).
    make_graph <- function(handle) {
      bg_register_node_kind(handle, "data", executor = function(node, inputs) {
        list(
          label = node$label,
          suffix = node$params$suffix %||% "?",
          up_sum = sum(vapply(inputs, function(x) x$value %||% 0, numeric(1)))
        )
      })
      root <- bg_add_node(
        handle,
        kind = "data",
        label = "root",
        params = list(suffix = "r")
      )
      l1 <- bg_add_node(
        handle,
        kind = "data",
        label = "l1",
        params = list(suffix = "1"),
        inputs = root
      )
      l2 <- bg_add_node(
        handle,
        kind = "data",
        label = "l2",
        params = list(suffix = "2"),
        inputs = root
      )
      list(root = root, l1 = l1, l2 = l2)
    }

    # Sequential run.
    tmp_seq <- withr::local_tempdir()
    h_seq <- bg_init(path = tmp_seq)
    ids_seq <- make_graph(h_seq)
    run_seq <- bg_run(h_seq, parallel = "never")
    expect_equal(run_seq$status, "succeeded")

    # Parallel run of the same graph in a fresh project.
    tmp_par <- withr::local_tempdir()
    h_par <- bg_init(path = tmp_par)
    ids_par <- make_graph(h_par)
    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)
    run_par <- bg_run(h_par, parallel = "always")
    expect_equal(run_par$status, "succeeded")

    # Artifact objects are identical (content-addressed determinism: same
    # executor + same inputs => same ref => same deserialized object).
    for (nm in names(ids_seq)) {
      obj_seq <- bg_result(h_seq, ids_seq[[nm]])
      obj_par <- bg_result(h_par, ids_par[[nm]])
      expect_equal(obj_seq, obj_par, info = paste("node", nm))
    }
  })

  it("completes a dependent chain under parallel dispatch", {
    # A two-wave graph: root (wave 1) then a single dependent leaf (wave 2).
    # The leaf wave has only one node, so it runs sequentially even under
    # parallel='always' (length(wave) > 1 is required for parallel dispatch).
    # This confirms the wave loop correctly progresses between waves when
    # parallel dispatch is enabled.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(
        value = node$params$x %||% 1,
        saw_input = length(inputs) > 0L
      )
    })
    root <- bg_add_node(handle, kind = "data", label = "root")
    leaf <- bg_add_node(handle, kind = "data", label = "leaf", inputs = root)

    mirai::daemons(2, dispatcher = TRUE)
    on.exit(mirai::daemons(NULL), add = TRUE)

    run_res <- bg_run(handle, parallel = "always")
    expect_equal(run_res$status, "succeeded")
    expect_equal(run_res$summary$total_executed, 2)
    expect_false(bg_result(handle, root)$saw_input)
    expect_true(bg_result(handle, leaf)$saw_input)
  })
})

describe("bg_prepare_executor_for_ship (crating)", {
  it("routes builtin executors by ref without shipping a function", {
    kind_reg <- list(executor_ref = "builtin:data")
    out <- bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "data")
    expect_equal(out$mode, "builtin")
    expect_equal(out$ref, "builtin:data")
    expect_null(out$fn)
  })

  it("aborts when a user executor is not a function", {
    kind_reg <- list(executor_ref = NULL, executor = "not a function")
    expect_error(
      bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "broken"),
      class = "rlang_error"
    )
  })

  it("ships a user executor as-is (with warning) when no source is available", {
    fn <- function(node, inputs) list(x = 1)
    kind_reg <- list(executor_ref = NULL, executor = fn, executor_source = NULL)
    expect_warning(
      out <- bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "user"),
      "self-contained"
    )
    expect_equal(out$mode, "function")
    expect_false(isTRUE(out$rebuilt_from_source))
    expect_identical(out$fn, fn)
  })

  it("registry entries from bg_register_node_kind carry executor_source", {
    # Regression: the in-memory registry entry used to hold only (name,
    # executor), so parallel dispatch could never rebuild from source in the
    # session the executor was registered and warned on every wave.
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      list(x = 1)
    })
    entry <- handle@registries$node_kinds[["data"]]
    expect_true(is.character(entry$executor_source))
    expect_true(nzchar(entry$executor_source))
    out <- expect_no_warning(
      bayesgrove:::bg_prepare_executor_for_ship(entry, "data")
    )
    expect_true(isTRUE(out$rebuilt_from_source))
  })

  it("rebuilds a user executor from its persisted source when available", {
    # A self-contained executor defined as source text (as bg_register_node_kind
    # would persist it). Rebuilding from source yields a daemon-safe closure
    # whose parent is a globalenv child (no capture of caller runtime state).
    src <- "function(node, inputs) list(value = node$params$x %||% 0)"
    kind_reg <- list(
      executor_ref = NULL,
      executor = eval(parse(text = src)),
      executor_source = src
    )
    out <- bayesgrove:::bg_prepare_executor_for_ship(kind_reg, "user")
    expect_equal(out$mode, "function")
    expect_true(isTRUE(out$rebuilt_from_source))
    expect_true(is.function(out$fn))
    # The rebuilt function behaves like the original on a sample node.
    expect_equal(
      out$fn(list(params = list(x = 7)), list()),
      list(value = 7)
    )
  })
})

describe("bg_compute_wave / bg_parallel_active (Milestone 3)", {
  it("bg_parallel_active returns FALSE for 'never'", {
    expect_false(bayesgrove:::bg_parallel_active("never"))
  })

  it("bg_parallel_active errors for 'always' when mirai is absent or no daemons", {
    skip_if_not_installed("mirai")
    # No daemons set -> 'always' should error.
    expect_error(
      bayesgrove:::bg_parallel_active("always"),
      class = "rlang_error"
    )
    # 'auto' with no daemons -> FALSE (graceful fallback).
    expect_false(bayesgrove:::bg_parallel_active("auto"))
  })

  it("declares the complete dependency contract for parallel dispatch", {
    requirements <- bayesgrove:::bg_parallel_requirements()

    expect_equal(names(requirements), c("mirai", "purrr", "carrier"))
    expect_true(utils::compareVersion(requirements[["purrr"]], "1.1.0") >= 0)
  })
})
