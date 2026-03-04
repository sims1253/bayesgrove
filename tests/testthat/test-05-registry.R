describe("Registry Layer", {
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
  })

  it("registers a backend plugin with validation", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    bad_backend <- list(backend_compile = function() {})

    expect_error(
      bg_register_backend(handle, "bad", bad_backend),
      "missing required methods"
    )

    good_backend <- list(
      backend_compile = function() {},
      backend_fit = function() {},
      backend_source_hash = function() {},
      backend_runtime_signature = function() {}
    )

    bg_register_backend(handle, "good", good_backend)
    expect_true("good" %in% names(handle@registries$backends))
  })
})
