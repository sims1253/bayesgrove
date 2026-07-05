describe("bg_executor_ppc (cmdstanr backend)", {
  skip_if_not_installed("posterior")

  # Asymmetric fixture where the two apply() margins give different p-values.
  # 3 draws, 2 observations; per-draw means are (0, 10, 20), observed mean 10,
  # so the correct per-draw p-value is 2/3. The wrong margin (per observation)
  # collapses to means (10, 10) and p = 1, hiding the discrepancy.
  yrep <- matrix(c(0, 0, 10, 10, 20, 20), nrow = 3, byrow = TRUE)
  colnames(yrep) <- c("yrep[1]", "yrep[2]")
  inputs <- list(yrep, list(y = c(9, 11)))

  it("computes the p-value on the per-draw margin (T(yrep))", {
    node <- list(params = list(stats = "mean"))
    res <- bayesgrove:::bg_executor_ppc(node, inputs)

    expect_equal(res$result$p_values$mean, 2 / 3)
    expect_equal(res$summaries[[1]]$metrics$mean, 2 / 3)
    expect_equal(res$summaries[[1]]$summary_kind, "posterior_predictive_check")
    # Plot-ready data: observed y and a capped yrep subsample are stored.
    expect_equal(res$result$plot_data$observed_y, c(9, 11))
    expect_true(is.matrix(res$result$plot_data$yrep))
    expect_equal(ncol(res$result$plot_data$yrep), 2L)
  })

  it("rejects statistics outside the allowlist", {
    node_bad <- list(params = list(stats = "var"))
    expect_error(
      bayesgrove:::bg_executor_ppc(node_bad, inputs),
      class = "rlang_error"
    )
  })

  it("accepts allowed statistics and returns a p-value in [0, 1]", {
    node <- list(params = list(stats = "median"))
    res <- bayesgrove:::bg_executor_ppc(node, inputs)

    expect_true(is.numeric(res$result$p_values$median))
    expect_true(res$result$p_values$median >= 0 && res$result$p_values$median <= 1)
  })

  it("ignores decoy variables whose name merely contains yrep_var", {
    # Substring matching would wrongly include yrep_new[i] alongside yrep[i];
    # anchored matching must select only the yrep[i] columns. Build a fit-like
    # matrix carrying both and confirm the executor still produces the expected
    # p-value (2/3 for the mean stat, as in the base fixture).
    draws <- matrix(
      c(0, 0, 10, 10, 20, 20, 99, 99, 99, 99, 99, 99),
      nrow = 3,
      byrow = TRUE
    )
    colnames(draws) <- c("yrep[1]", "yrep[2]", "yrep_new[1]", "yrep_new[2]")
    decoy_inputs <- list(draws, list(y = c(9, 11)))
    node <- list(params = list(stats = "mean"))
    res <- bayesgrove:::bg_executor_ppc(node, decoy_inputs)

    # With the decoy excluded, the mean p-value matches the base fixture.
    expect_equal(res$result$p_values$mean, 2 / 3)

    # And the helper selects exactly the yrep[i] columns.
    sel <- bayesgrove:::bg_indexed_var_cols("yrep", colnames(draws))
    expect_equal(which(sel), 1:2)
  })

  it("caps the yrep subsample to max_yrep_rows for plot-ready storage", {
    # A fit with many draws should be capped to max_yrep_rows in plot_data so
    # the artifact stays small, while all draws still feed the p-value calc.
    # big_yrep: col1 all 0, col2 all 10 -> every per-draw mean is 5; observed
    # mean is 10, so p-value (P(T(yrep) >= T(y))) is 0.
    big_yrep <- matrix(rep(c(0, 10), each = 250), nrow = 250, ncol = 2)
    colnames(big_yrep) <- c("yrep[1]", "yrep[2]")
    big_inputs <- list(big_yrep, list(y = c(9, 11)))
    node <- list(params = list(stats = "mean", max_yrep_rows = 50L))
    res <- bayesgrove:::bg_executor_ppc(node, big_inputs)

    expect_equal(nrow(res$result$plot_data$yrep), 50L)
    # The p-value still uses all 250 draws.
    expect_equal(res$result$p_values$mean, 0)
  })
})
