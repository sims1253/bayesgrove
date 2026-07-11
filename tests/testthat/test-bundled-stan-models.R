describe("bundled Stan models", {
  it("generate posterior predictions and pointwise log likelihoods", {
    stan_dir <- system.file("stan", package = "bayesgrove")

    for (model in c("normal_iid.stan", "lognormal_iid.stan")) {
      code <- readLines(file.path(stan_dir, model), warn = FALSE)

      expect_true(any(grepl("array\\[N\\] real yrep;", code)))
      expect_true(any(grepl("vector\\[N\\] log_lik;", code)))
      expect_true(any(grepl("log_lik\\[n\\] = .*_lpdf", code)))
    }
  })
})
