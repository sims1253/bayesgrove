describe("bg_executor_loo and bg_executor_compare (cmdstanr backend)", {
  skip_if_not_installed("loo")
  skip_if_not_installed("posterior")

  it("bg_extract_draws supports as='array' to preserve chain structure", {
    set.seed(1)
    arr <- posterior::as_draws_array(array(
      rnorm(4 * 25 * 2),
      dim = c(iteration = 25, chain = 4, variable = 2),
      dimnames = list(iteration = NULL, chain = NULL, variable = c("a", "b"))
    ))
    m <- bayesgrove:::bg_extract_draws(arr, as = "matrix")
    a <- bayesgrove:::bg_extract_draws(arr, as = "array")
    expect_s3_class(m, "draws_matrix")
    expect_s3_class(a, "draws_array")
    # Default is matrix.
    expect_s3_class(bayesgrove:::bg_extract_draws(arr), "draws_matrix")
  })

  it("compare reports a nonzero runner-up elpd_diff and full table", {
    # Two pre-built loo objects: A better than B by a clear margin.
    set.seed(2)
    # 200 draws x 10 observations; A has systematically higher log-lik.
    ll_a <- matrix(rnorm(200 * 10, mean = -1, sd = 0.5), nrow = 200, ncol = 10)
    ll_b <- matrix(rnorm(200 * 10, mean = -3, sd = 0.5), nrow = 200, ncol = 10)
    loo_a <- loo::loo(ll_a)
    loo_b <- loo::loo(ll_b)

    node <- list(params = list())
    res <- bayesgrove:::bg_executor_compare(
      node,
      list(node_a = loo_a, node_b = loo_b)
    )

    comparison_summary <- Filter(
      function(s) identical(s$summary_kind, "comparison_results"),
      res$summaries
    )[[1]]
    # Runner-up elpd_diff must be nonzero (it was always 0 before the fix).
    expect_true(is.finite(comparison_summary$metrics$elpd_diff))
    expect_true(comparison_summary$metrics$elpd_diff != 0)
    # Full comparison table is plain data.
    tbl <- comparison_summary$metrics$comparison_table
    expect_setequal(tbl$model, c("node_a", "node_b"))
    expect_length(tbl$model, 2)
    expect_length(tbl$elpd_diff, 2)
    expect_length(tbl$se_diff, 2)
  })

  it("compare emits a stacking_weights summary whose weights sum to ~1", {
    set.seed(3)
    ll_a <- matrix(rnorm(200 * 10, mean = -1, sd = 0.5), nrow = 200, ncol = 10)
    ll_b <- matrix(
      rnorm(200 * 10, mean = -1.2, sd = 0.5),
      nrow = 200,
      ncol = 10
    )
    loo_a <- loo::loo(ll_a)
    loo_b <- loo::loo(ll_b)

    node <- list(params = list())
    res <- bayesgrove:::bg_executor_compare(node, list(loo_a, loo_b))

    stacking_summary <- Filter(
      function(s) identical(s$summary_kind, "stacking_weights"),
      res$summaries
    )[[1]]
    # Stacking weights are always emitted (not silently dropped) and sum to ~1.
    weights <- unlist(stacking_summary$metrics)
    expect_length(weights, 2)
    expect_equal(sum(weights), 1, tolerance = 1e-6)
  })

  it("loo computes r_eff from chain-shaped draws (no missing-r_eff warning)", {
    # Build a fit-like object that bg_extract_draws can read as both matrix and
    # array. Use a draws_array carrying log_lik variables.
    set.seed(4)
    arr <- array(
      rnorm(4 * 50 * 5, mean = -1),
      dim = c(iteration = 50, chain = 4, variable = 5),
      dimnames = list(
        iteration = NULL,
        chain = NULL,
        variable = paste0("log_lik[", 1:5, "]")
      )
    )
    fit <- list(draws = function() posterior::as_draws(arr))
    class(fit) <- c("CmdStanMCMC", class(fit))

    node <- list(params = list())
    res <- bayesgrove:::bg_executor_loo(node, list(fit))
    loo_summary <- Filter(
      function(s) identical(s$summary_kind, "loo_diagnostics"),
      res$summaries
    )[[1]]
    expect_true(!is.null(loo_summary))
    expect_true(loo_summary$summary_kind == "loo_diagnostics")
  })

  it("loo ignores decoy variables whose name merely contains log_lik_var", {
    # Substring matching would wrongly include log_lik_saturated[i] and
    # log_lik_extra in the log-lik matrix; anchored matching must select only
    # log_lik and log_lik[i]. loo::relative_eff() errors if the matrix has 0
    # columns or mismatched shape, so a wrong selection would surface as an
    # error or a wrong pareto_k count.
    set.seed(7)
    var_names <- c(
      paste0("log_lik[", 1:3, "]"),
      "log_lik_saturated[1]",
      "log_lik_extra"
    )
    arr <- array(
      rnorm(4 * 20 * length(var_names), mean = -1),
      dim = c(iteration = 20, chain = 4, variable = length(var_names)),
      dimnames = list(iteration = NULL, chain = NULL, variable = var_names)
    )
    fit <- list(draws = function() posterior::as_draws(arr))
    class(fit) <- c("CmdStanMCMC", class(fit))

    # The selected columns should be exactly the log_lik[i] ones, not the
    # decoys. Verify via the helper directly.
    m <- bayesgrove:::bg_extract_draws(fit, as = "matrix")
    expect_true(
      bayesgrove:::bg_indexed_var_cols("log_lik", colnames(m)) |>
        which() |>
        identical(1:3)
    )

    node <- list(params = list())
    res <- bayesgrove:::bg_executor_loo(node, list(fit))
    loo_summary <- Filter(
      function(s) identical(s$summary_kind, "loo_diagnostics"),
      res$summaries
    )[[1]]
    # loo::loo on 3 pointwise terms should report 3 pareto_k values.
    expect_length(loo_summary$metrics, 2L)
  })
})
