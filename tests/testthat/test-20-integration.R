describe("End-to-End Workflow Integration", {
  it("runs a full workflow with branching, decisions, and cache hits", {
    # 1. Setup
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp, project_name = "e2e_test")

    # 2. Register mock backends and kinds
    bg_register_node_kind(
      handle,
      "data_source",
      executor = function(node, inputs) {
        data.frame(x = 1:100, y = rnorm(100))
      }
    )

    bg_register_node_kind(
      handle,
      "transform",
      executor = function(node, inputs) {
        df <- inputs[[1]]
        df$scaled_x <- scale(df$x)
        df
      }
    )

    mock_cmdstanr <- list(
      backend_compile = function(params) {
        list(compiled = TRUE, id = params$model_id)
      },
      backend_fit = function(compiled, data, params) {
        list(fit = TRUE, chains = params$chains, compiled = compiled)
      },
      backend_source_hash = function(params) params$model_id,
      backend_runtime_signature = function(params) {
        list(fingerprint_fields = list(v = 1))
      }
    )

    bg_register_backend(handle, "cmdstanr", mock_cmdstanr)
    bg_register_node_kind(handle, "compile", executor = function(node, inputs) {
      be <- handle@registries$backends[[node$params$backend]]
      be$backend_compile(node$params)
    })
    bg_register_node_kind(handle, "fit", executor = function(node, inputs) {
      be <- handle@registries$backends[[node$params$backend]]
      compiled <- inputs[[2]] # Assume data is 1, compile is 2 based on topo order
      data <- inputs[[1]]
      be$backend_fit(compiled, data, node$params)
    })

    # 3. Build Graph
    n_data <- bg_add_node(handle, "data_source", label = "Raw")
    n_prep <- bg_add_node(
      handle,
      "transform",
      label = "Prepped",
      inputs = n_data
    )

    n_comp_base <- bg_add_node(
      handle,
      "compile",
      params = list(backend = "cmdstanr", model_id = "base")
    )
    n_fit_base <- bg_add_node(
      handle,
      "fit",
      inputs = c(n_prep, n_comp_base),
      params = list(backend = "cmdstanr", chains = 4)
    )

    # 4. Add a gate
    gate <- bg_add_gate(
      handle,
      from = n_prep,
      to = n_fit_base,
      prompt = "Does the data look ready?",
      options = c("yes", "no")
    )

    # Verify we are blocked
    st1 <- bg_status(handle)
    expect_equal(st1$workflow_state, "blocked")
    expect_equal(st1$pending_gates, 1)

    # Answer the gate
    bg_answer_gate(handle, gate$id, "yes", rationale = "Scaling looks correct.")

    # 5. First Run (Synchronous)
    run1 <- bg_run(handle, mode = "sync")
    expect_equal(run1$summary$total_executed, 4)

    # 6. Verify Cache
    st2 <- bg_status(handle)
    expect_equal(st2$workflow_state, "idle")
    expect_equal(length(bg_plan(handle)$cache_hits), 4)

    res_base <- bg_result(handle, n_fit_base)
    expect_true(res_base$fit)
    expect_equal(res_base$chains, 4)

    # 7. Create a branch and modify it
    # We want to try 8 chains instead of 4, keeping the same compiled model
    n_fit_branch <- bg_branch(handle, n_fit_base, label = "8_chains")
    bg_update_node(
      handle,
      n_fit_branch$root_node_id,
      params = list(backend = "cmdstanr", chains = 8)
    )

    # 8. Incremental Run
    # The new fit node should run, but it should reuse data, prep, and compile!
    run2 <- bg_run(handle, mode = "sync")
    expect_equal(run2$summary$total_executed, 1) # ONLY the new fit node ran!

    res_branch <- bg_result(handle, n_fit_branch$root_node_id)
    expect_equal(res_branch$chains, 8)

    # Both fits share the same compiled object identity
    expect_equal(res_branch$compiled$id, "base")
  })
})
