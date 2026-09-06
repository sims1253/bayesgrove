describe("Project Lifecycle", {
  it("uses a unique temporary file for atomic JSON writes", {
    tmp <- withr::local_tempdir()
    path <- file.path(tmp, "state.json")
    stale_tmp <- paste0(path, ".tmp")
    writeLines("do not touch", stale_tmp)

    bg_write_json_atomic(path, list(value = 1))

    expect_equal(readLines(stale_tmp), "do not touch")
    expect_equal(jsonlite::read_json(path)$value, 1)
  })

  it("creates structure and returns handle on init", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp, project_name = "test_proj")

    expect_s7_class(handle, bg_handle)
    expect_equal(handle@readonly, FALSE)
    expect_equal(handle@closed, FALSE)
    expect_equal(handle@loaded_graph_version, 0L)

    # Check directory structure
    expect_true(dir.exists(file.path(tmp, ".bayesgrove")))
    expect_true(dir.exists(file.path(tmp, ".bayesgrove", "graph")))
    expect_true(dir.exists(file.path(tmp, ".bayesgrove", "decisions")))

    # Check config
    config_path <- file.path(tmp, ".bayesgrove", "config.json")
    expect_true(file.exists(config_path))
    config <- jsonlite::read_json(config_path)
    expect_equal(config$project_name, "test_proj")
    expect_length(config$workflow_packs, 0)
    expect_length(config$runtime_manifest$node_kinds, 0)

    # Check graph
    graph_path <- file.path(tmp, ".bayesgrove", "graph", "graph.json")
    expect_true(file.exists(graph_path))
  })

  it("reads existing project on open", {
    tmp <- withr::local_tempdir()
    init_handle <- bg_init(path = tmp, project_name = "open_test")

    handle <- bg_open(path = tmp, readonly = TRUE)
    expect_s7_class(handle, bg_handle)
    expect_equal(handle@readonly, TRUE)
    expect_equal(handle@closed, FALSE)
    expect_equal(handle@loaded_graph_version, 0L)
    expect_equal(handle@project_id, init_handle@project_id)
  })

  it("rehydrates persisted node kind executors only with explicit trust", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "data",
      executor = function(node, inputs) 42
    )
    bg_close(handle)

    reopened <- bg_open(path = tmp)

    # Opening must NOT restore the executor (no code execution on open).
    expect_null(reopened@registries$node_kinds[["data"]]$executor)

    # trust = FALSE prints and aborts.
    expect_error(
      bg_restore_executors(reopened, trust = FALSE),
      "explicit consent"
    )

    # trust = TRUE restores it (and warns about closures).
    expect_warning(
      bg_restore_executors(reopened, trust = TRUE),
      "closures over the original environment"
    )
    expect_equal(
      reopened@registries$node_kinds[["data"]]$executor(NULL, NULL),
      42
    )
  })

  it("hydrates graphs with dagriculture and list classes preserved", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    graph <- bg_read_graph(handle)

    expect_s3_class(graph, "dagriculture_graph")
    expect_true(inherits(graph, "list"))
  })

  it("sets closed flag on close", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_equal(handle@closed, FALSE)
    bg_close(handle)
    expect_equal(handle@closed, TRUE)
  })

  it("detects persisted graph version conflicts across handles", {
    tmp <- withr::local_tempdir()
    handle_a <- bg_init(path = tmp)

    graph_a <- bg_read_graph(handle_a)
    graph_a$version <- graph_a$version + 1L
    bg_commit_graph(handle_a, graph_a)
    bg_close(handle_a)

    # Reopen as a fresh writer. The persisted graph is now at version 1.
    # A commit at the same (stale) version must be rejected.
    handle_b <- bg_open(path = tmp)
    graph_b <- bg_read_graph(handle_b)

    expect_error(
      bg_commit_graph(handle_b, graph_b),
      "Version conflict"
    )
  })
})

describe("Single-writer project lock (Phase 2.4)", {
  it("blocks a second writer while the first is open", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_error(
      bg_open(path = tmp),
      "locked by another writer"
    )

    bg_close(handle)
  })

  it("allows a readonly open alongside a writer", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    ro <- bg_open(path = tmp, readonly = TRUE)
    expect_equal(ro@readonly, TRUE)
    expect_equal(ro@lock_token, NA_character_)

    bg_close(handle)
  })

  it("releases the lock on close so a new writer can open", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_close(handle)

    # No lock dir after close.
    expect_false(dir.exists(file.path(tmp, ".bayesgrove", "project.lock")))

    handle2 <- bg_open(path = tmp)
    expect_s7_class(handle2, bg_handle)
    bg_close(handle2)
  })

  it("steals a stale lock with force = TRUE", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Force-open should succeed even though handle holds the lock.
    forced <- bg_open(path = tmp, force = TRUE)
    expect_s7_class(forced, bg_handle)

    bg_close(forced)
  })
})

describe("Executor trust model (Phase 2.2)", {
  it("does not execute hostile executor_source on open", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    sentinel <- file.path(tmp, ".bayesgrove", "PWNED")
    if (file.exists(sentinel)) {
      unlink(sentinel)
    }

    # Plant a hostile executor_source directly in config.json, simulating a
    # project directory from an untrusted source.
    config_path <- file.path(tmp, ".bayesgrove", "config.json")
    config <- jsonlite::read_json(config_path)
    config$runtime_manifest$node_kinds$hostile <- list(
      kind = "hostile",
      executor_source = paste0(
        'file.create("',
        sentinel,
        '")'
      )
    )
    jsonlite::write_json(config, config_path, auto_unbox = TRUE)
    bg_close(handle)

    # Opening the project must NOT execute the planted source.
    reopened <- bg_open(path = tmp)
    expect_false(file.exists(sentinel))
    expect_null(reopened@registries$node_kinds[["hostile"]]$executor)
  })

  it("bg_restore_executors(trust = TRUE) evaluates persisted source", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "data",
      executor = function(node, inputs) {
        list(result = 99)
      }
    )
    bg_close(handle)

    reopened <- bg_open(path = tmp)
    expect_null(reopened@registries$node_kinds[["data"]]$executor)

    expect_warning(
      bg_restore_executors(reopened, trust = TRUE),
      "closures over the original environment"
    )
    result <- reopened@registries$node_kinds[["data"]]$executor(NULL, NULL)
    expect_equal(result$result, 99)
  })

  it("running a node with an unrestored executor gives guidance", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_register_node_kind(
      handle,
      "data",
      executor = function(node, inputs) {
        list(result = 1)
      }
    )
    n1 <- bg_add_node(handle, kind = "data", label = "A")
    bg_close(handle)

    reopened <- bg_open(path = tmp)
    run_res <- bg_run(reopened, targets = n1)

    expect_equal(run_res$status, "failed")
    expect_match(
      run_res$error$message,
      "bg_restore_executors|bg_register_node_kind"
    )
  })
})
