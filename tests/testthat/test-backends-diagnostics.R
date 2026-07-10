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

  it("bg_loo_pit_diagnostics grades with a calibrated uniformity p-value", {
    uniform <- (seq_len(200) - 0.5) / 200
    skewed <- rep(0.99, 100)

    uniform_diag <- bayesgrove:::bg_loo_pit_diagnostics(uniform)
    skewed_diag <- bayesgrove:::bg_loo_pit_diagnostics(skewed)

    expect_equal(uniform_diag$severity, "ok")
    expect_equal(skewed_diag$severity, "error")
    expect_true(uniform_diag$p_value > skewed_diag$p_value)
    expect_equal(uniform_diag$test, "PIET")
  })

  it("SBC histogram defaults keep every expected bin count at least five", {
    ranks <- rep(0:20, length.out = 20)
    histogram <- bayesgrove:::bg_sbc_rank_histogram(ranks, n_draws = 20)

    expect_lte(histogram$n_bins, 4L)
    expect_gte(histogram$n_bins, 2L)
    expect_true(all(histogram$expected >= 5))
    expect_true(histogram$graded)
  })

  it("SBC histogram marks underpowered custom grading as ungraded", {
    ranks <- rep(0:20, length.out = 20)
    histogram <- bayesgrove:::bg_sbc_rank_histogram(
      ranks,
      n_draws = 20,
      n_bins = 20
    )

    expect_false(histogram$graded)
    expect_true(any(histogram$expected < 5))
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
    expect_true(is.numeric(res$summaries[[1]]$metrics$p_value))
    expect_equal(res$summaries[[1]]$metrics$uniformity_test, "PIET")
  })

  it("uses reproducible randomized PIT for discrete outcomes", {
    n_draws <- 100L
    y <- c(0L, 1L)
    yrep <- cbind(rep(c(0L, 1L), each = n_draws / 2), rep(1L, n_draws))
    log_lik <- matrix(0, nrow = n_draws, ncol = 2)
    draws <- cbind(log_lik, yrep)
    colnames(draws) <- c("log_lik[1]", "log_lik[2]", "yrep[1]", "yrep[2]")
    dm <- posterior::as_draws_matrix(draws)
    fit <- list(draws = function() dm)
    class(fit) <- c("CmdStanMCMC", class(fit))
    node <- list(params = list(pit_seed = 123L))

    first <- bayesgrove:::bg_executor_loo_pit(node, list(fit, list(y = y)))
    second <- bayesgrove:::bg_executor_loo_pit(node, list(fit, list(y = y)))

    expect_equal(first$result$pit_values, second$result$pit_values)
    expect_true(first$summaries[[1]]$metrics$discrete)
    expect_equal(first$summaries[[1]]$metrics$pit_method, "randomized")
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
