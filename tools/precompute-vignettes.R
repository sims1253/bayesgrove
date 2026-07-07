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
# stamp at the top of the vignette. The `<<<CHUNK ...>>>` markers name the
# vignette chunk each block of output belongs to.

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
cat(
  "CmdStan version:",
  tryCatch(
    as.character(cmdstanr::cmdstan_version()),
    error = function(e) "n/a"
  ),
  "\n"
)

handle <- bg_init(
  path = tempfile("bg-eight-schools-vignette-"),
  project_name = "Eight Schools"
)
bg_use_workflow_packs(handle, "bayesgrove.default_bayesian")
bg_use_cmdstanr(handle)

schools_data <- list(
  J = 8L,
  y = c(28, 8, -3, 7, -1, 1, 18, 12),
  sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
)

n_data <- bg_add_node(handle, kind = "stan_data", label = "Schools data")
bg_set_node_data(handle, n_data, schools_data)

stan_file <- tempfile(fileext = ".stan")
writeLines(c(
  "data { int<lower=1> J; vector[J] y; vector<lower=0>[J] sigma; }",
  "parameters { real mu; real<lower=0> tau; vector[J] theta; }",
  "model { mu ~ normal(0, 5); tau ~ cauchy(0, 5);",
  "        theta ~ normal(mu, tau); y ~ normal(theta, sigma); }"
), stan_file)

n_fit <- bg_add_node(
  handle,
  kind = "cmdstanr_fit",
  label = "Centered",
  inputs = n_data,
  params = list(
    stan_file = stan_file,
    chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    seed = 42
  )
)

cat("<<<CHUNK run>>>\n")
run <- bg_run(handle, targets = n_fit)

cat("<<<CHUNK summaries>>>\n")
summaries <- bg_read_summaries(handle)
print(summaries[[1]]$severity)
print(summaries[[1]]$metrics[c("divergences", "e_bfmi", "max_rhat")])

cat("<<<CHUNK next-actions>>>\n")
guide <- bg_next_actions(handle)
print(vapply(guide$obligations, `[[`, character(1), "kind"))
print(vapply(guide$actions, `[[`, character(1), "kind"))

cat("<<<CHUNK review-decision>>>\n")
review <- Filter(
  function(a) identical(a$payload$decision_type, "computation_review"),
  guide$actions
)[[1]]

bg_record_decision(
  handle,
  scope = "project",
  prompt = "Is the centered fit acceptable for downstream use?",
  choice = "reject_and_reparametrize",
  rationale = paste(
    "81 of 4000 transitions diverged and E-BFMI is low: the sampler cannot",
    "explore the funnel between tau and theta. Switch to the non-centered",
    "parameterization."
  ),
  kind = "computation_review",
  metadata = review$payload[c("node_ids", "summary_ids")]
)

print(vapply(bg_next_actions(handle)$obligations, `[[`, character(1), "kind"))

cat("<<<CHUNK branch>>>\n")
branch <- bg_branch(handle, n_fit, label = "Non-centered")

bg_set_goal(
  project = handle,
  branch_id = branch$branch_id,
  kind = "latent_inference",
  label = "School effects, non-centered",
  rationale = "Same model; reparameterized so HMC can explore the posterior."
)

stan_file_nc <- tempfile(fileext = ".stan")
writeLines(c(
  "data { int<lower=1> J; vector[J] y; vector<lower=0>[J] sigma; }",
  "parameters { real mu; real<lower=0> tau; vector[J] theta_raw; }",
  "transformed parameters { vector[J] theta = mu + tau * theta_raw; }",
  "model { mu ~ normal(0, 5); tau ~ cauchy(0, 5);",
  "        theta_raw ~ normal(0, 1); y ~ normal(theta, sigma); }"
), stan_file_nc)

bg_update_node(
  handle,
  branch$root_node_id,
  params = list(stan_file = stan_file_nc)
)

cat("<<<CHUNK rerun>>>\n")
run2 <- bg_run(handle, targets = branch$root_node_id)

cat("<<<CHUNK rerun-summaries>>>\n")
for (s in bg_read_summaries(handle, include_stale = FALSE)) {
  cat(s$node_id, "-", s$severity, "- divergences:", s$metrics$divergences, "\n")
}

cat("<<<CHUNK branch-review>>>\n")
branch_guide <- bg_next_actions(
  handle,
  scope = "branch",
  branch_id = branch$branch_id
)
print(vapply(branch_guide$obligations, `[[`, character(1), "kind"))

review_nc <- Filter(
  function(a) identical(a$payload$decision_type, "computation_review"),
  branch_guide$actions
)[[1]]

bg_record_decision(
  handle,
  scope = branch$branch_id,
  prompt = "Is the centered fit acceptable as this branch's baseline?",
  choice = "superseded_by_non_centered",
  rationale = paste(
    "The centered fit's divergences are the reason this branch exists;",
    "the non-centered fit replaces it."
  ),
  kind = "computation_review",
  metadata = review_nc$payload[c("node_ids", "summary_ids")]
)

cat("<<<CHUNK loop-closed>>>\n")
print(bg_next_actions(handle)$obligations)

cat("<<<CHUNK graph>>>\n")
print(bg_read_graph(handle))

cat("<<<CHUNK export>>>\n")
report <- bg_export_report(
  handle,
  format = "md",
  path = file.path(tempdir(), "eight-schools-report.md")
)
cat(readLines(report, n = 8), sep = "\n")
