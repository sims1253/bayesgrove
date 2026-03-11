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

  it("returns a descriptive read-only extension registry", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesguide.default_bayesian")
    )

    bg_register_node_kind(
      handle,
      "custom_fit",
      executor = function(node, inputs) {
        list(result = TRUE)
      }
    )
    bg_register_backend(
      handle,
      "good",
      list(
        backend_compile = function() {},
        backend_fit = function() {},
        backend_source_hash = function() {},
        backend_runtime_signature = function(args) list(runtime = "local")
      )
    )

    registry <- bg_extension_registry(handle)

    expect_true("custom_fit" %in% names(registry$node_kinds))
    expect_true("good" %in% names(registry$backends))
    expect_true(
      "bayesguide.default_bayesian" %in% names(registry$workflow_packs)
    )
    expect_true("review_decision" %in% names(registry$templates))
    expect_false(registry$policy$gui_extension_api)
    expect_equal(registry$policy$registry_mode, "descriptive")
    expect_true(isTRUE(registry$node_kinds$custom_fit$read_only))
  })
})
