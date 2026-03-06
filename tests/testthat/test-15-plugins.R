describe("Diagnostics and Analysis Plugins", {
  it("can compile and fit using the brms plugin", {
    # Skip if brms is not actually installed locally during the test
    skip_if_not_installed("brms")

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    # We register the actual plugin logic rather than a dummy list
    bg_register_backend(handle, "brms", bg_brms_plugin())

    bg_register_node_kind(handle, "data", executor = function(node, inputs) {
      data.frame(x = 1:10, y = rnorm(10))
    })

    bg_register_node_kind(handle, "compile", executor = function(node, inputs) {
      be <- handle@registries$backends[[node$params$backend]]
      be$backend_compile(node$params)
    })

    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      be <- handle@registries$backends[[node$params$backend]]
      be$backend_fit(inputs[[2]], inputs[[1]], node$params)
    })

    n_data <- bg_add_node(handle, "data")
    n_comp <- bg_add_node(
      handle,
      "compile",
      params = list(backend = "brms", formula = "y ~ x")
    )
    n_fit <- bg_add_node(
      handle,
      "fit",
      inputs = c(n_data, n_comp),
      params = list(backend = "brms", chains = 4)
    )

    res <- bg_run(handle, mode = "sync")
    expect_equal(res$summary$total_executed, 3)

    fit_res <- bg_result(handle, n_fit)
    expect_equal(fit_res$backend, "brms")
    expect_equal(fit_res$formula, "y ~ x")
  })

  it("can run diagnostics and comparison using the diagnostic plugin", {
    skip_if_not_installed("posterior")
    skip_if_not_installed("loo")

    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)

    diag_plugin <- bg_diagnostics_plugin()

    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      list(chains = 4, data = "mock")
    })

    bg_register_node_kind(
      handle,
      "diagnostic",
      executor = function(node, inputs) {
        diag_plugin$backend_diagnose(inputs[[1]], node$params)
      }
    )

    bg_register_node_kind(handle, "compare", executor = function(node, inputs) {
      diag_plugin$backend_compare(inputs, node$params)
    })

    n_fit1 <- bg_add_node(handle, "fit")
    n_fit2 <- bg_add_node(handle, "fit")

    n_diag <- bg_add_node(handle, "diagnostic", inputs = n_fit1)
    n_comp <- bg_add_node(handle, "compare", inputs = c(n_fit1, n_fit2))

    res <- bg_run(handle, mode = "sync")
    expect_equal(res$summary$total_executed, 3)

    diag_res <- bg_result(handle, n_diag)
    expect_equal(diag_res$status, "ok")

    comp_res <- bg_result(handle, n_comp)
    expect_equal(comp_res$method, "loo")
  })
})
