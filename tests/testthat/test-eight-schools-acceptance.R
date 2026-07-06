describe("Eight-schools acceptance (Phase 4.6)", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if_not_installed("posterior")

  stan_centered <- withr::local_tempfile(fileext = ".stan")
  writeLines(
    c(
      "data { int<lower=1> J; vector[J] y; vector<lower=0>[J] sigma; }",
      "parameters { real mu; real<lower=0> tau; vector[J] theta; }",
      "model { mu ~ normal(0, 5); tau ~ cauchy(0, 5);",
      "        theta ~ normal(mu, tau); y ~ normal(theta, sigma); }"
    ),
    stan_centered
  )

  schools_data <- list(
    J = 8L,
    y = c(28, 8, -3, 7, -1, 1, 18, 12),
    sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
  )

  it("centered fit produces divergences and a blocking review obligation", {
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )
    bg_use_cmdstanr(handle)

    n_data <- bg_add_node(
      handle,
      kind = "stan_data",
      label = "Schools data"
    )
    bg_set_node_data(handle, n_data, schools_data)
    n_fit <- bg_add_node(
      handle,
      kind = "cmdstanr_fit",
      label = "Centered",
      inputs = n_data,
      params = list(
        stan_file = stan_centered,
        data_input = n_data,
        chains = 4,
        iter_warmup = 500,
        iter_sampling = 500,
        seed = 123
      )
    )

    run_res <- bg_run(handle, targets = n_fit)
    expect_equal(run_res$status, "succeeded")

    # The fit must emit an hmc_diagnostics summary with errors (divergences).
    summaries <- bg_read_summaries(handle)
    hmc <- Filter(
      function(s) identical(s$summary_kind, "hmc_diagnostics"),
      summaries
    )
    expect_gte(length(hmc), 1)

    # The centered parameterization is pathological for eight-schools: at the
    # configured seed the observed divergence rate is ~1.5%, above the 1%
    # error threshold, so the fit must report divergences and severity error.
    expect_gt(hmc[[1]]$metrics$divergences, 0)
    expect_equal(hmc[[1]]$severity, "error")

    # The review_computation_validity obligation must be blocking.
    actions <- bg_next_actions(handle)
    review <- Filter(
      function(o) identical(o$kind, "review_computation_validity"),
      actions$obligations
    )
    expect_gte(length(review), 1)
    expect_equal(review[[1]]$severity, "blocking")
  })

  it("zero user-written diagnostic code was required", {
    # This test asserts the structural property: the hmc_diagnostics summary
    # was emitted by the built-in cmdstanr_fit executor (executor_ref =
    # "builtin:cmdstanr_fit"), not by any user-registered executor. We verify
    # by checking that the node kind has a built-in executor_ref.
    tmp <- withr::local_tempdir()
    handle <- bg_init(
      path = tmp,
      workflow_packs = list("bayesgrove.default_bayesian")
    )
    bg_use_cmdstanr(handle)

    kind_reg <- handle@registries$node_kinds[["cmdstanr_fit"]]
    expect_true(startsWith(kind_reg$executor_ref, "builtin:"))
    expect_true(is.function(kind_reg$executor))
  })
})
