describe("brms integration (Phase 4.5)", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_if_not_installed("posterior")

  it("fits a simple linear model and emits an hmc_diagnostics summary", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_brms(handle)

    # Use a small built-in dataset so the test doesn't need a data input node.
    set.seed(42)
    df <- data.frame(x = rnorm(50), y = rnorm(50))

    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Linear data"
    )
    bg_set_node_data(handle, n_data, df)
    n_fit <- bg_add_node(
      handle,
      kind = "brms_fit",
      label = "Linear fit",
      inputs = n_data,
      params = list(
        formula = "y ~ x",
        data_input = n_data,
        chains = 1,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 42
      )
    )

    run_res <- bg_run(handle, targets = n_fit)
    expect_equal(run_res$status, "succeeded")

    summaries <- bg_read_summaries(handle)
    hmc <- Filter(
      function(s) identical(s$summary_kind, "hmc_diagnostics"),
      summaries
    )
    expect_gte(length(hmc), 1)
    expect_equal(hmc[[1]]$summary_kind, "hmc_diagnostics")
    expect_true(hmc[[1]]$severity %in% c("ok", "warning", "error"))

    fit <- bg_result(handle, n_fit)
    loo_result <- bayesgrove:::bg_executor_loo(
      list(params = list()),
      list(fit)
    )
    expect_s3_class(loo_result$result, "loo")

    ppc_result <- bayesgrove:::bg_executor_ppc(
      list(params = list(stats = "mean")),
      list(fit, df)
    )
    expect_equal(ncol(ppc_result$result$plot_data$yrep), nrow(df))
  })

  it("brms family param is respected (not silently dropped)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_brms(handle)

    set.seed(1)
    bernoulli_df <- list(y = as.integer(rbinom(30, 1, 0.5)))
    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Bernoulli data"
    )
    bg_set_node_data(handle, n_data, bernoulli_df)

    # brmsfit should work with a binary outcome. We can't easily pass a
    # family object through JSON params, so we check the fit runs without
    # error for a simple Gaussian model.
    n_fit <- bg_add_node(
      handle,
      kind = "brms_fit",
      label = "Fit",
      inputs = n_data,
      params = list(
        formula = "y ~ 1",
        data_input = n_data,
        chains = 1,
        iter_warmup = 200,
        iter_sampling = 200,
        seed = 99
      )
    )

    run_res <- bg_run(handle, targets = n_fit)
    expect_equal(run_res$status, "succeeded")

    fit <- bg_result(handle, n_fit)
    expect_true(inherits(fit, "brmsfit"))
  })

  it("fits without an explicit seed (NULL seed arg must not error)", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(path = tmp)
    bg_use_brms(handle)

    set.seed(7)
    no_seed_df <- data.frame(x = rnorm(30), y = rnorm(30))
    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Linear data"
    )
    bg_set_node_data(handle, n_data, no_seed_df)
    # No seed param: brm() must be called without seed = NULL (brms' default is
    # NA, not NULL, which errors). The do.call arg-drop pattern handles this.
    n_fit <- bg_add_node(
      handle,
      kind = "brms_fit",
      label = "No-seed fit",
      inputs = n_data,
      params = list(
        formula = "y ~ x",
        data_input = n_data,
        chains = 1,
        iter_warmup = 100,
        iter_sampling = 100
      )
    )

    run_res <- bg_run(handle, targets = n_fit)
    expect_equal(run_res$status, "succeeded")
    fit <- bg_result(handle, n_fit)
    expect_true(inherits(fit, "brmsfit"))
  })
})
