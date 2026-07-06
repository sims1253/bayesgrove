# Precompute vignette outputs
#
# Runs the eight-schools flow locally and prints the chunk outputs so the
# pasted `#>` blocks in vignettes/eight-schools.Rmd can be refreshed from a
# real run. Run with:
#
#   Rscript tools/precompute-vignettes.R
#
# Requires cmdstanr + a working CmdStan installation. Outputs are package- and
# CmdStan-version dependent; re-run when either changes and update the version
# stamp at the top of the vignette.

suppressPackageStartupMessages({
  library(bayesgrove)
})

cat(
  "bayesgrove package version:",
  as.character(packageVersion("bayesgrove")),
  "\n"
)
cat(
  "cmdstanr version:",
  tryCatch(as.character(packageVersion("cmdstanr")), error = function(e) "n/a"),
  "\n"
)

tmp <- tempfile("eight-schools-vignette-")
dir.create(tmp)
handle <- bg_init(
  path = tmp,
  workflow_packs = list("bayesgrove.default_bayesian")
)
bg_use_cmdstanr(handle)

stan_file <- withr::local_tempfile(fileext = ".stan")
writeLines(
  c(
    "data { int<lower=1> J; vector[J] y; vector<lower=0>[J] sigma; }",
    "parameters { real mu; real<lower=0> tau; vector[J] theta; }",
    "model { mu ~ normal(0, 5); tau ~ cauchy(0, 5);",
    "        theta ~ normal(mu, tau); y ~ normal(theta, sigma); }"
  ),
  stan_file
)

schools_data <- list(
  J = 8L,
  y = c(28, 8, -3, 7, -1, 1, 18, 12),
  sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
)

n_data <- bg_add_node(handle, kind = "stan_data", label = "Schools data")
bg_set_node_data(handle, n_data, schools_data)
n_fit <- bg_add_node(
  handle,
  kind = "cmdstanr_fit",
  label = "Centered",
  inputs = n_data,
  params = list(
    stan_file = stan_file,
    data_input = n_data,
    chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 123
  )
)

run_res <- bg_run(handle, targets = n_fit)
print(run_res$status)
print(run_res$summary)

summaries <- bg_read_summaries(handle)
hmc <- Filter(
  function(s) identical(s$summary_kind, "hmc_diagnostics"),
  summaries
)
cat("--- hmc_diagnostics summary ---\n")
str(hmc[[1]][c("severity", "metrics")])

cat("\n--- next actions ---\n")
actions <- bg_next_actions(handle)
print(vapply(actions$obligations, function(o) o$kind, character(1)))
