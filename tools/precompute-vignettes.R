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
writeLines(
  c(
    "data { int<lower=1> J; vector[J] y; vector<lower=0>[J] sigma; }",
    "parameters { real mu; real<lower=0> tau; vector[J] theta; }",
    "model { mu ~ normal(0, 5); tau ~ cauchy(0, 5);",
    "        theta ~ normal(mu, tau); y ~ normal(theta, sigma); }"
  ),
  stan_file
)

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
writeLines(
  c(
    "data { int<lower=1> J; vector[J] y; vector<lower=0>[J] sigma; }",
    "parameters { real mu; real<lower=0> tau; vector[J] theta_raw; }",
    "transformed parameters { vector[J] theta = mu + tau * theta_raw; }",
    "model { mu ~ normal(0, 5); tau ~ cauchy(0, 5);",
    "        theta_raw ~ normal(0, 1); y ~ normal(theta, sigma); }"
  ),
  stan_file_nc
)

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

print(bg_next_actions(handle)$obligations)

cat("<<<CHUNK export>>>\n")
print(bg_read_graph(handle))

report <- bg_export_report(
  handle,
  format = "md",
  path = file.path(tempdir(), "eight-schools-report.md")
)
cat(readLines(report, n = 8), sep = "\n")

# Getting started ---------------------------------------------------------

handle <- bg_init(
  path = tempfile("bg-getting-started-vignette-"),
  project_name = "Getting Started"
)
bg_use_cmdstanr(handle)
bg_use_default_workflow(handle)

set.seed(31)
y <- round(rlnorm(50, meanlog = 0, sdlog = 0.8), 3)

stan_dir <- tempfile("bg-getting-started-stan-")
dir.create(stan_dir)
normal_program <- file.path(stan_dir, "normal_iid.stan")
lognormal_program <- file.path(stan_dir, "lognormal_iid.stan")
file.copy(
  system.file(
    "stan/normal_iid.stan",
    package = "bayesgrove",
    mustWork = TRUE
  ),
  normal_program
)
file.copy(
  system.file(
    "stan/lognormal_iid.stan",
    package = "bayesgrove",
    mustWork = TRUE
  ),
  lognormal_program
)

cat("<<<CHUNK getting-started-fit>>>\n")
n_fit <- bg_fit_stan(
  handle,
  stan_file = normal_program,
  data = list(N = length(y), y = y),
  label = "Normal model",
  seed = 21,
  chains = 4
)

cat("<<<CHUNK getting-started-first-guidance>>>\n")
print(bg_status(handle))

cat("<<<CHUNK getting-started-first-obligations>>>\n")
guide <- bg_next_actions(handle)
print(guide$obligations)

cat("<<<CHUNK getting-started-ppc>>>\n")
graph <- bg_read_graph(handle)
n_data <- Filter(
  function(edge) identical(edge$to, n_fit),
  graph$edges
)[[1]]$from
n_ppc <- bg_add_node(
  handle,
  kind = "ppc",
  label = "PPC: normal model",
  inputs = c(n_fit, n_data),
  params = list(stats = c("mean", "sd", "min", "max"))
)
run <- bg_run(handle, targets = n_ppc)

cat("<<<CHUNK getting-started-ppc-summary>>>\n")
ppc_summary <- bg_read_summaries(handle, include_stale = FALSE)
ppc_summary <- Filter(
  function(s) identical(s$summary_kind, "posterior_predictive_check"),
  ppc_summary
)[[1]]
print(ppc_summary$severity)
print(ppc_summary$metrics)

figure_path <- file.path(
  "vignettes",
  "figures",
  "getting-started-ppc-normal.png"
)
dir.create(dirname(figure_path), recursive = TRUE, showWarnings = FALSE)
png(figure_path, width = 700, height = 500)
print(bg_plot(handle, n_ppc))
dev.off()

cat("<<<CHUNK getting-started-obligation>>>\n")
guide <- bg_next_actions(handle)
print(vapply(guide$obligations, `[[`, character(1), "kind"))
print(vapply(guide$actions, `[[`, character(1), "kind"))

cat("<<<CHUNK getting-started-holds>>>\n")
held_plan <- bg_plan(handle, external_holds = guide$metadata$external_holds)
print(held_plan$external_blocked)

cat("<<<CHUNK getting-started-decision>>>\n")
review <- Filter(
  function(a) identical(a$kind, "record_decision"),
  guide$actions
)[[1]]
bg_record_decision(
  handle,
  scope = "project",
  prompt = "Does the normal model reproduce the data?",
  choice = "reject_normal_model",
  rationale = paste(
    "The observed minimum is far outside the posterior predictive",
    "distribution: the data are strictly positive and right-skewed,",
    "and a normal model cannot express that. Refit on the log scale."
  ),
  kind = review$payload$decision_type,
  metadata = review$payload[c("node_ids", "summary_ids")]
)
print(bg_next_actions(handle)$obligations)

cat("<<<CHUNK getting-started-branch>>>\n")
branch <- bg_branch(handle, n_fit, label = "Lognormal model")
bg_set_goal(
  project = handle,
  branch_id = branch$branch_id,
  kind = "observable_prediction",
  label = "Positive-valued predictions",
  rationale = "The rejected normal fit motivates a lognormal likelihood."
)
bg_update_node(
  handle,
  branch$root_node_id,
  params = list(stan_file = lognormal_program)
)
run2 <- bg_run(handle, targets = branch$root_node_id)

cat("<<<CHUNK getting-started-branch-ppc>>>\n")
n_ppc2 <- bg_add_node(
  handle,
  kind = "ppc",
  label = "PPC: lognormal model",
  inputs = c(branch$root_node_id, n_data),
  params = list(stats = c("mean", "sd", "min", "max"))
)
bg_run(handle, targets = n_ppc2)
ppc2 <- Filter(
  function(s) {
    identical(s$summary_kind, "posterior_predictive_check") &&
      identical(s$node_id, n_ppc2)
  },
  bg_read_summaries(handle, include_stale = FALSE)
)[[1]]
print(ppc2$severity)
print(ppc2$metrics)

cat("<<<CHUNK getting-started-graph>>>\n")
print(bg_read_graph(handle))

cat("<<<CHUNK getting-started-gate>>>\n")
gate <- bg_add_gate(
  project = handle,
  from = n_data,
  to = n_fit,
  prompt = "Were the measurements validated before modeling?",
  alternatives = c("yes", "no")
)
print(bg_pending_gates(handle))
bg_answer_gate(
  project = handle,
  id = gate$id,
  choice = "yes",
  rationale = "Values were cross-checked against the collection protocol."
)
