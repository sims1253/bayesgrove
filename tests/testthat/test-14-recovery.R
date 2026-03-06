describe("Recovery and Checkpointing", {
  it("can capture a full snapshot of the project", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp, project_name = "snapshot_test")

    bg_register_node_kind(handle, "data")
    node_id <- bg_add_node(handle, "data", label = "A")
    bg_record_decision(
      handle,
      scope = paste0("node:", node_id),
      prompt = "Keep this node?",
      choice = "yes",
      rationale = "Baseline source node."
    )

    snap <- bg_snapshot(handle)

    expect_equal(snap$name, "snapshot_test")
    expect_true("graph" %in% names(snap))
    expect_equal(length(snap$graph$nodes), 1)
    expect_true("decisions" %in% names(snap))
    expect_length(snap$decisions, 1)
    expect_true("artifacts" %in% names(snap))
    expect_true("jobs" %in% names(snap))
    expect_true("status" %in% names(snap))
  })

  it("can pause and resume workflow execution", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_register_node_kind(handle, "data", executor = function(node, inputs) 1)
    bg_add_node(handle, "data", label = "A")

    # Pause the workflow
    pause_state <- bg_pause(handle)

    # Verify paused state in config
    config_path <- file.path(tmp, ".bayesgrove", "config.json")
    config <- jsonlite::read_json(config_path)
    expect_true(config$paused)
    expect_equal(pause_state$state, "paused")
    expect_equal(bg_status(handle)$workflow_state, "paused")
    expect_error(bg_run(handle), "Workflow is paused")

    # Resume the workflow
    resume_state <- bg_resume(handle)

    config <- jsonlite::read_json(config_path)
    expect_false(config$paused)
    expect_equal(resume_state$workflow_state, "idle")
  })
})
