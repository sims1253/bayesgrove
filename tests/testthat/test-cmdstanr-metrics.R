describe("bg_cmdstanr_hmc_metrics (cmdstanr backend)", {
  skip_if_not_installed("posterior")

  it("sums divergences/max_treedepth and takes min finite E-BFMI", {
    set.seed(42)
    draws <- posterior::as_draws_array(
      array(
        rnorm(4 * 100 * 3),
        dim = c(iteration = 100, chain = 4, variable = 3),
        dimnames = list(
          iteration = NULL,
          chain = NULL,
          variable = c("alpha", "beta", "sigma")
        )
      )
    )

    fake_fit <- list(
      diagnostic_summary = function() {
        list(
          num_divergent = c(3L, 0L),
          num_max_treedepth = c(1L, 0L),
          ebfmi = c(0.2, NA)
        )
      },
      draws = function() draws,
      num_chains = function() 4L
    )

    m <- bayesgrove:::bg_cmdstanr_hmc_metrics(fake_fit)

    expect_equal(m$divergences, 3L)
    expect_equal(m$max_treedepth_hits, 1L)
    expect_equal(m$e_bfmi, 0.2)
  })

  it("returns Inf E-BFMI when every chain is NA", {
    set.seed(7)
    draws <- posterior::as_draws_array(
      array(
        rnorm(4 * 100 * 3),
        dim = c(iteration = 100, chain = 4, variable = 3),
        dimnames = list(
          iteration = NULL,
          chain = NULL,
          variable = c("alpha", "beta", "sigma")
        )
      )
    )

    fake_fit <- list(
      diagnostic_summary = function() {
        list(
          num_divergent = c(0L, 0L),
          num_max_treedepth = c(0L, 0L),
          ebfmi = c(NA_real_, NA_real_)
        )
      },
      draws = function() draws,
      num_chains = function() 4L
    )

    m <- bayesgrove:::bg_cmdstanr_hmc_metrics(fake_fit)

    expect_equal(m$divergences, 0L)
    expect_equal(m$e_bfmi, Inf)
  })
})
