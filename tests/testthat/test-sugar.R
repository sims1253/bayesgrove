describe("Practitioner fit helpers", {
  it("bg_fit_stan names its backend setup function before changing the graph", {
    handle <- bg_init(withr::local_tempdir())

    expect_error(
      bg_fit_stan(handle, "missing.stan", list()),
      "bg_use_cmdstanr"
    )
    expect_length(bg_read_graph(handle)$nodes, 0L)
  })

  it("bg_fit_brms names its backend setup function before changing the graph", {
    handle <- bg_init(withr::local_tempdir())

    expect_error(
      bg_fit_brms(handle, y ~ x, data.frame(y = 1, x = 1)),
      "bg_use_brms"
    )
    expect_length(bg_read_graph(handle)$nodes, 0L)
  })
})
