# loo_pit and sbc executors (Milestone 4) -----------------------------------

describe("LOO-PIT helpers", {
  it("bg_ks_uniform_stat is ~0 for a uniform sample and large for a skewed one", {
    set.seed(1)
    uniform <- runif(2000)
    skewed <- rbeta(2000, 5, 1) # concentrated near 1, far from Uniform
    expect_true(bg_ks_uniform_stat(uniform) < 0.10)
    expect_true(bg_ks_uniform_stat(skewed) > 0.30)
  })

  it("bg_ks_uniform_stat returns NA for an empty/NA sample", {
    expect_true(is.na(bg_ks_uniform_stat(numeric(0))))
    expect_true(is.na(bg_ks_uniform_stat(c(NA_real_, NA_real_))))
  })

  it("bg_loo_pit_severity grades by KS thresholds and respects overrides", {
    # KS ~0 for uniform => ok; large => error.
    uniform <- runif(2000)
    skewed <- rep(0.99, 100) # degenerate near 1
    expect_equal(bg_loo_pit_severity(uniform), "ok")
    expect_equal(bg_loo_pit_severity(skewed), "error")
    # Override thresholds: a large KS can downgrade to warning.
    expect_equal(bg_loo_pit_severity(skewed, ks_error = 1.5), "warning")
  })
})

describe("bg_executor_loo_pit (cmdstanr/brms shared)", {
  skip_if_not_installed("loo")
  skip_if_not_installed("posterior")

  it("computes PIT values in [0,1] and a loo_pit_calibration summary", {
    # Synthetic fit: well-calibrated yrep centered at the observed y, with a
    # matching log_lik. The PIT values should land near the middle of [0,1].
    set.seed(2)
    n_draws <- 200L
    n_obs <- 6L
    y <- rep(5, n_obs)
    yrep <- matrix(rnorm(n_draws * n_obs, 5, 1), nrow = n_draws)
    log_lik <- matrix(dnorm(yrep, y, 1, log = TRUE), nrow = n_draws)
    colnames(yrep) <- paste0("yrep[", seq_len(n_obs), "]")
    colnames(log_lik) <- paste0("log_lik[", seq_len(n_obs), "]")

    draws <- cbind(log_lik, yrep)
    # Build a draws_matrix-like object bg_extract_draws accepts.
    dm <- posterior::as_draws_matrix(posterior::as_draws_matrix(draws))
    fit <- list(draws = function() dm)
    class(fit) <- c("CmdStanMCMC", class(fit))

    inputs <- list(fit, list(y = y))
    node <- list(params = list())
    res <- bayesgrove:::bg_executor_loo_pit(node, inputs)

    expect_equal(res$summaries[[1]]$summary_kind, "loo_pit_calibration")
    expect_true(all(res$result$pit_values >= 0 & res$result$pit_values <= 1))
    # Well-calibrated: PIT values should average near 0.5 (not all near 0 or 1).
    expect_true(
      mean(res$result$pit_values) > 0.2 && mean(res$result$pit_values) < 0.8
    )
    # Plot-ready data carries observed y and the PIT vector.
    expect_equal(res$result$plot_data$observed_y, y)
    expect_equal(length(res$result$plot_data$pit_values), n_obs)
  })

  it("ignores decoy variables whose name merely contains log_lik_var", {
    # log_lik_saturated must be excluded; only log_lik[i] feed the PSIS step.
    set.seed(3)
    n_draws <- 100L
    n_obs <- 3L
    y <- rep(5, n_obs)
    yrep <- matrix(rnorm(n_draws * n_obs, 5, 1), nrow = n_draws)
    log_lik <- matrix(dnorm(yrep, y, 1, log = TRUE), nrow = n_draws)
    decoy <- matrix(rnorm(n_draws * n_obs, 0, 1), nrow = n_draws)
    draws <- cbind(log_lik, decoy, yrep)
    colnames(draws) <- c(
      paste0("log_lik[", seq_len(n_obs), "]"),
      paste0("log_lik_saturated[", seq_len(n_obs), "]"),
      paste0("yrep[", seq_len(n_obs), "]")
    )
    dm <- posterior::as_draws_matrix(posterior::as_draws_matrix(draws))
    fit <- list(draws = function() dm)
    class(fit) <- c("CmdStanMCMC", class(fit))

    inputs <- list(fit, list(y = y))
    res <- bayesgrove:::bg_executor_loo_pit(list(params = list()), inputs)
    expect_true(length(res$result$pit_values) == n_obs)
    expect_true(all(res$result$pit_values >= 0 & res$result$pit_values <= 1))
  })

  it("errors when log_lik/yrep column count does not match observed y", {
    set.seed(4)
    draws <- matrix(rnorm(100 * 4), nrow = 100)
    colnames(draws) <- c(
      "log_lik[1]",
      "log_lik[2]",
      "yrep[1]",
      "yrep[2]"
    )
    dm <- posterior::as_draws_matrix(posterior::as_draws_matrix(draws))
    fit <- list(draws = function() dm)
    class(fit) <- c("CmdStanMCMC", class(fit))
    # 3 observed values vs 2 log_lik/yrep columns.
    inputs <- list(fit, list(y = c(1, 2, 3)))
    expect_error(
      bayesgrove:::bg_executor_loo_pit(list(params = list()), inputs),
      class = "rlang_error"
    )
  })
})

describe("bg_executor_sbc registration", {
  it("bg_use_cmdstanr and bg_use_brms register the sbc and loo_pit kinds", {
    for (setup in list(bg_use_cmdstanr, bg_use_brms)) {
      tmp <- withr::local_tempdir()
      handle <- bg_init(path = tmp)
      setup(handle)
      kinds <- names(handle@registries$node_kinds)
      expect_true("loo_pit" %in% kinds)
      expect_true("sbc" %in% kinds)
    }
  })
})
