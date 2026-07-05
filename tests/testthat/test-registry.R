describe("Registry Layer", {
  it("adds workflow packs without changing an empty project default", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    expect_length(bg_workflow_packs(handle), 0)

    bg_use_workflow_packs(handle, "bayesgrove.default_bayesian")

    expect_equal(
      vapply(bg_workflow_packs(handle), `[[`, character(1), "pack_id"),
      "bayesgrove.default_bayesian"
    )
  })

  it("registers a node kind structurally and in runtime", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    mock_executor <- function(node, inputs) {
      "done"
    }

    bg_register_node_kind(
      project = handle,
      kind = "custom_fit",
      executor = mock_executor
    )

    # Check structural graph
    graph <- bg_read_graph(handle)
    expect_true("custom_fit" %in% names(graph$registry$kinds))

    # Check runtime registry
    expect_true(!is.null(handle@registries$node_kinds[["custom_fit"]]))
    expect_equal(
      handle@registries$node_kinds[["custom_fit"]]$executor(NULL, NULL),
      "done"
    )

    config <- jsonlite::read_json(file.path(tmp, ".bayesgrove", "config.json"))
    expect_true("custom_fit" %in% names(config$runtime_manifest$node_kinds))
    expect_match(
      config$runtime_manifest$node_kinds$custom_fit$executor_source,
      "function"
    )
  })

  it("registers the built-in starter workflow", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bg_use_default_workflow(handle)

    graph <- bg_read_graph(handle)
    expect_true(all(
      c("source", "fit", "ppc", "compare") %in% names(graph$registry$kinds)
    ))
    expect_equal(
      vapply(bg_workflow_packs(handle), `[[`, character(1), "pack_id"),
      "bayesgrove.default_bayesian"
    )
    bg_close(handle)

    reopened <- bg_open(tmp)
    reopened_graph <- bg_read_graph(reopened)
    expect_true(all(
      c("source", "fit", "ppc", "compare") %in%
        names(reopened_graph$registry$kinds)
    ))
  })

  it("returns a descriptive read-only extension registry", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "custom_fit",
      executor = function(node, inputs) {
        list(result = TRUE)
      }
    )

    registry <- bg_extension_registry(handle)

    expect_true("custom_fit" %in% names(registry$node_kinds))
    expect_true(
      "bayesgrove.default_bayesian" %in% names(registry$workflow_packs)
    )
    expect_true("review_decision" %in% names(registry$templates))
    expect_false(registry$policy$gui_extension_api)
    expect_equal(registry$policy$registry_mode, "descriptive")
    expect_true(isTRUE(registry$node_kinds$custom_fit$read_only))
  })
})
