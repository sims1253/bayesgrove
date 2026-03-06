demo_repl_workflow <- function(start_repl = interactive()) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The repl workflow demo needs the pkgload package.", call. = FALSE)
  }

  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

  fmt <- function(x) {
    format(round(x, 2), nsmall = 2, trim = TRUE)
  }

  project_root <- file.path(tempdir(), "bg-vhs-repl-demo")
  unlink(project_root, recursive = TRUE, force = TRUE)

  handle <- bg_init(
    path = project_root,
    project_name = "repl-workflow-demo",
    workflow_packs = list("bayesguide.default_bayesian")
  )

  bg_register_node_kind(handle, "scenario", executor = function(node, inputs) {
    node$params
  })

  bg_register_node_kind(
    handle,
    "simulate_data",
    executor = function(node, inputs) {
      scenario <- inputs[[1]]
      set.seed(scenario$seed + node$params$seed_offset)

      n <- scenario$n
      z <- rnorm(n)
      treatment_prob <- plogis(
        scenario$assignment_intercept + scenario$confounding * z
      )
      treatment <- rbinom(n, size = 1, prob = treatment_prob)
      outcome <- (0.5 +
        scenario$beta_treat * treatment +
        scenario$beta_z * z +
        rnorm(n, sd = scenario$sigma))

      list(
        data = data.frame(
          z = z,
          treatment = treatment,
          outcome = outcome
        ),
        truth = list(
          beta_treat = scenario$beta_treat,
          beta_z = scenario$beta_z,
          confounding = scenario$confounding
        ),
        split = node$params$split
      )
    }
  )

  bg_register_node_kind(
    handle,
    "descriptive_check",
    executor = function(node, inputs) {
      sim <- inputs[[1]]
      data <- sim$data

      treated <- data[data$treatment == 1, , drop = FALSE]
      untreated <- data[data$treatment == 0, , drop = FALSE]
      pooled_sd <- stats::sd(data$z)

      list(
        split = sim$split,
        treated_share = mean(data$treatment),
        z_smd = (mean(treated$z) - mean(untreated$z)) / pooled_sd,
        cor_treatment_z = stats::cor(data$treatment, data$z)
      )
    }
  )

  bg_register_node_kind(handle, "fit_lm", executor = function(node, inputs) {
    train <- inputs[[1]]
    test <- inputs[[2]]
    formula <- stats::as.formula(node$params$formula)

    fit <- stats::lm(formula, data = train$data)
    predictions <- stats::predict(fit, newdata = test$data)
    rmse <- sqrt(mean((test$data$outcome - predictions)^2))
    estimate <- unname(stats::coef(fit)[["treatment"]])

    list(
      model_label = node$label,
      formula = node$params$formula,
      estimate = estimate,
      absolute_effect_error = abs(estimate - train$truth$beta_treat),
      predictive_rmse = rmse
    )
  })

  bg_register_node_kind(
    handle,
    "compare_models",
    executor = function(node, inputs) {
      naive <- inputs[[1]]
      adjusted <- inputs[[2]]
      descriptive <- inputs[[3]]

      ranking <- data.frame(
        model = c(naive$model_label, adjusted$model_label),
        absolute_effect_error = c(
          naive$absolute_effect_error,
          adjusted$absolute_effect_error
        ),
        predictive_rmse = c(
          naive$predictive_rmse,
          adjusted$predictive_rmse
        )
      )

      ranking <- ranking[
        order(
          ranking$absolute_effect_error,
          ranking$predictive_rmse
        ),
      ]

      list(
        imbalance = descriptive,
        ranking = ranking,
        recommended = ranking$model[[1]]
      )
    }
  )

  set.seed(20260306)

  scenario <- bg_add_node(
    handle,
    kind = "scenario",
    label = "Confounded assignment scenario",
    params = list(
      n = 250,
      beta_treat = 1.0,
      beta_z = 0.8,
      sigma = 1.0,
      assignment_intercept = 0,
      confounding = 1.1,
      seed = 202
    )
  )

  train <- bg_add_node(
    handle,
    kind = "simulate_data",
    label = "Training draw",
    inputs = scenario,
    params = list(split = "train", seed_offset = 1)
  )

  test <- bg_add_node(
    handle,
    kind = "simulate_data",
    label = "Test draw",
    inputs = scenario,
    params = list(split = "test", seed_offset = 1001)
  )

  descriptive <- bg_add_node(
    handle,
    kind = "descriptive_check",
    label = "Treatment imbalance check",
    inputs = train
  )

  naive_fit <- bg_add_node(
    handle,
    kind = "fit_lm",
    label = "Naive outcome model",
    inputs = c(train, test),
    params = list(formula = "outcome ~ treatment")
  )

  adjusted_fit <- bg_add_node(
    handle,
    kind = "fit_lm",
    label = "Adjusted outcome model",
    inputs = c(train, test),
    params = list(formula = "outcome ~ treatment + z")
  )

  compare <- bg_add_node(
    handle,
    kind = "compare_models",
    label = "Model comparison checkpoint",
    inputs = c(naive_fit, adjusted_fit, descriptive)
  )

  gate <- bg_add_gate(
    handle,
    from = descriptive,
    to = compare,
    prompt = paste(
      "Does the imbalance check justify treating the adjusted model",
      "as the trustworthy explanatory branch?"
    ),
    options = c("yes", "no"),
    refs = list(
      list(
        citekey = "gelman2020workflow",
        note = "Model checking should drive revision."
      )
    )
  )

  initial_run <- suppressMessages(bg_run(handle, mode = "sync"))
  descriptive_result <- bg_result(handle, descriptive)
  naive_result <- bg_result(handle, naive_fit)
  adjusted_result <- bg_result(handle, adjusted_fit)

  demo <- list(
    handle = handle,
    project_root = project_root,
    initial_run = initial_run,
    ids = list(
      scenario = scenario,
      train = train,
      test = test,
      descriptive = descriptive,
      naive_fit = naive_fit,
      adjusted_fit = adjusted_fit,
      compare = compare,
      gate = gate$id
    ),
    summaries = list(
      descriptive = descriptive_result,
      naive_fit = naive_result,
      adjusted_fit = adjusted_result
    )
  )

  assign("bg_repl_demo", demo, envir = .GlobalEnv)

  cat("\nBayesGrove REPL demo\n")
  cat("====================\n")
  cat("This recording resumes from a checkpoint instead of starting empty.\n")
  cat(
    "Upstream simulation, checking, and both model fits are already cached.\n\n"
  )
  cat(sprintf("Project root: %s\n\n", project_root))
  cat("Checkpoint summary\n")
  cat(sprintf("- z imbalance (SMD): %s\n", fmt(descriptive_result$z_smd)))
  cat(sprintf(
    "- corr(treatment, z): %s\n",
    fmt(descriptive_result$cor_treatment_z)
  ))
  cat(sprintf(
    "- naive effect error: %s\n",
    fmt(naive_result$absolute_effect_error)
  ))
  cat(sprintf(
    "- adjusted effect error: %s\n\n",
    fmt(adjusted_result$absolute_effect_error)
  ))
  cat("Useful ids for the tape\n")
  cat(sprintf("- adjusted fit node: %s\n", adjusted_fit))
  cat(sprintf("- comparison node: %s\n", compare))
  cat(sprintf("- gate id: %s\n\n", gate$id))

  if (isTRUE(start_repl)) {
    bg_repl(handle)
  }

  invisible(demo)
}

if (interactive()) {
  demo_repl_workflow(start_repl = TRUE)
}
