describe("Project Lifecycle", {
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
    handle_b <- bg_open(path = tmp)

    graph_a <- bg_read_graph(handle_a)
    graph_b <- bg_read_graph(handle_b)

    graph_a$version <- graph_a$version + 1L
    graph_b$version <- graph_b$version + 1L

    bg_commit_graph(handle_a, graph_a)

    expect_error(
      bg_commit_graph(handle_b, graph_b),
      "persisted graph version"
    )
  })
})
