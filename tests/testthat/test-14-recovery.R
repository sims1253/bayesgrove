describe("Recovery and Checkpointing", {
  it("can capture a full snapshot of the project", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp, project_name = "snapshot_test")

    bg_register_node_kind(handle, "data")
    bg_add_node(handle, "data", label = "A")

    snap <- bg_snapshot(handle)

    expect_equal(snap$name, "snapshot_test")
    expect_true("graph" %in% names(snap))
    expect_equal(length(snap$graph$nodes), 1)
    expect_true("jobs" %in% names(snap))
    expect_true("status" %in% names(snap))
  })

  it("can pause and resume workflow execution", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # Pause the workflow
    bg_pause(handle)

    # Verify paused state in config
    config_path <- file.path(tmp, ".bayesgrove", "config.json")
    config <- jsonlite::read_json(config_path)
    expect_true(config$paused)

    # Resume the workflow
    bg_resume(handle)

    config <- jsonlite::read_json(config_path)
    expect_false(config$paused)
  })
})
