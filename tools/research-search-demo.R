# Run from the repository root with cmdstanr and CmdStan installed.
# Fits real normal/lognormal models using bayesgrove's existing executors.
# Research judgments below are explicit example choices, not inferred verdicts.
pkgload::load_all(quiet = TRUE)

run_demo <- function() {
  path <- tempfile("bg-research-demo-")
  h <- bg_init(path)
  on.exit(bg_close(h), add = TRUE)
  bg_use_cmdstanr(h)
  human <- list(id = "demo-researcher", type = "human")
  agent <- list(id = "demo-agent", type = "agent")
  rule <- list(
    id = "review-ppc",
    actions = "accept",
    type = "require_review",
    check = "ppc",
    message = "A human must review the predictive evidence before acceptance.",
    interpretation = "Inspect predictive support and the tails for the stated goal.",
    references = "https://arxiv.org/abs/2209.02439"
  )
  bg_research_init(
    h,
    "Which likelihood reproduces these positive measurements?",
    list(kind = "observable_prediction", focus = "support and tails"),
    human,
    "enforce",
    list(rule)
  )
  record <- function(action, rationale, actor = human) {
    bg_research_apply(h, bg_research_propose(h, action, actor, rationale))
  }
  last_id <- function(state) tail(state$history, 1)[[1]]$id
  set.seed(31)
  y <- round(rlnorm(50, 0, 0.8), 3)
  data <- list(N = length(y), y = y)
  settings <- list(
    chains = 2L,
    parallel_chains = 2L,
    iter_warmup = 300L,
    iter_sampling = 300L,
    seed = 21L,
    refresh = 0L
  )
  files <- file.path(path, c("normal.stan", "lognormal.stan"))
  file.copy(
    file.path("inst", "stan", c("normal_iid.stan", "lognormal_iid.stan")),
    files
  )
  p <- function(i) {
    list(stan_source = paste(readLines(files[[i]]), collapse = "\n"))
  }
  baseline <- last_id(record(
    list(
      kind = "create",
      label = "Normal",
      components = list(
        P = p(1),
        A = c(
          list(
            backend = "cmdstanr",
            cmdstan_version = as.character(cmdstanr::cmdstan_version())
          ),
          settings
        ),
        D = data
      )
    ),
    "Use a normal baseline and investigate predictive support."
  ))
  collect <- function(candidate, file) {
    fit <- do.call(
      bg_fit_stan,
      c(list(handle = h, stan_file = file, data = data), settings)
    )
    g <- bg_read_graph(h)
    data_id <- Filter(function(e) identical(e$to, fit), g$edges)[[1]]$from
    ppc <- bg_add_node(
      h,
      "ppc",
      inputs = c(fit, data_id),
      params = list(stats = c("min", "max", "mean"))
    )
    run <- bg_run(h, targets = ppc)
    stopifnot(identical(run$status, "succeeded"))
    summaries <- Filter(
      function(s) identical(s$node_id, ppc),
      bg_read_summaries(h, include_stale = FALSE)
    )
    summary <- tail(summaries, 1)[[1]]
    record(
      list(
        kind = "evidence",
        candidate_id = candidate,
        label = "Posterior predictive check",
        check = "ppc",
        utility = "predictive_performance",
        result = list(
          metrics = summary$metrics,
          severity = summary$severity,
          execution_fingerprint = summary$execution_fingerprint,
          artifact_ref = summary$artifact_ref,
          node_id = ppc
        )
      ),
      "Record the actual executor summary with its fingerprint and artifact reference.",
      agent
    )
  }
  normal_state <- collect(baseline, files[[1]])
  normal_evidence <- last_id(normal_state)
  alternative <- last_id(record(
    list(
      kind = "revise",
      candidate_id = baseline,
      label = "Lognormal",
      components = list(P = p(2)),
      basis = list(evidence_ids = normal_evidence)
    ),
    "Investigate a positive-valued likelihood after the baseline predictive check."
  ))
  alternative_state <- collect(alternative, files[[2]])
  alternative_evidence <- last_id(alternative_state)
  # Close after collection so both candidates retain their existing evidence.
  record(
    list(kind = "close", candidate_id = alternative),
    "Pause this path while comparing the available evidence."
  )
  comparison <- last_id(record(
    list(
      kind = "compare",
      candidate_ids = c(baseline, alternative),
      evidence_ids = c(normal_evidence, alternative_evidence),
      criteria = "Predictive support and extreme observations",
      result = list(
        normal = normal_state$evidence[[normal_evidence]]$result$metrics,
        lognormal = alternative_state$evidence[[
          alternative_evidence
        ]]$result$metrics
      )
    ),
    "Place the check results side by side without declaring a universal winner."
  ))
  record(
    list(
      kind = "reopen",
      candidate_id = alternative,
      basis = list(comparison_ids = comparison)
    ),
    "Resume the alternative for a provisional research decision."
  )
  accept <- list(
    kind = "accept",
    candidate_id = alternative,
    evidence_ids = alternative_evidence,
    basis = list(comparison_ids = comparison)
  )
  blocked <- bg_research_propose(
    h,
    accept,
    agent,
    "Propose the alternative for further work."
  )
  stopifnot(!blocked$allowed)
  record(
    list(
      kind = "review",
      candidate_id = alternative,
      evidence_ids = alternative_evidence
    ),
    "Example review: acknowledge the support check; these statistics do not establish overall adequacy."
  )
  record(
    accept,
    "Example decision: retain the alternative for further investigation under the stated goal."
  )
  state <- bg_research_state(h)
  report <- bg_export_report(h, format = "md")
  bundle <- bg_bundle(h)
  bg_close(h)
  h <- bg_open(path)
  stopifnot(identical(bg_research_state(h), state))
  extract <- tempfile("bg-research-restored-")
  dir.create(extract)
  utils::untar(bundle, exdir = extract)
  restored <- bg_open(file.path(extract, basename(path)), readonly = TRUE)
  stopifnot(identical(bg_research_state(restored), state))
  bg_close(restored)
  cat("Research search completed and survived reopen and bundle restore.\n")
  cat("Normal PPC metrics:\n")
  print(normal_state$evidence[[normal_evidence]]$result$metrics)
  cat("Lognormal PPC metrics:\n")
  print(alternative_state$evidence[[alternative_evidence]]$result$metrics)
  cat("Report:", report, "\n")
  invisible(state)
}
run_demo()
