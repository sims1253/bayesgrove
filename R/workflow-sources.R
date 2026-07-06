bg_workflow_source_registry <- function() {
  list(
    workflow_core = paste0(
      "Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter, B., ",
      "Yao, Y., Kennedy, L., Gabry, J., Burkner, P.-C., & Modrak, M. (2020). ",
      "Bayesian Workflow. Journal of the Royal Statistical Society Series A. ",
      "(https://doi.org/10.1111/rssa.12378)"
    ),
    prior_predictive = paste0(
      "Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). ",
      "Visualization in Bayesian Workflow: Using R and Stan to build, evaluate, ",
      "and understand Bayesian models. ",
      "(https://doi.org/10.1080/10618600.2018.1473781)"
    ),
    posterior_predictive = paste0(
      "Gelman, A., Meng, X.-L., & Stern, H. (1996). Posterior Predictive ",
      "Assessment of Model Fitness via Realized Discrepancies. Statistica ",
      "Sinica, 6(4), 733-760. (https://www.jstor.org/stable/24306036)"
    ),
    sbc = paste0(
      "Talts, S., Betancourt, M., Simpson, D., Vehtari, A., & Gelman, A. (2018). ",
      "Validating Bayesian Inference Algorithms with Simulation-Based ",
      "Calibration. (https://arxiv.org/abs/1804.06788)"
    ),
    model_comparison = paste0(
      "Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model ",
      "evaluation using leave-one-out cross-validation and WAIC. Statistics ",
      "and Computing, 27(5), 1413-1432. ",
      "(https://doi.org/10.1007/s11222-016-9696-4)"
    ),
    stacking = paste0(
      "Yao, Y., Vehtari, A., Simpson, D., & Gelman, A. (2018). Using Stacking ",
      "to Average Bayesian Predictive Distributions (with Discussion). ",
      "Bayesian Analysis, 13(3), 917-1007. (https://doi.org/10.1214/17-BA1091)"
    ),
    loo_pit = paste0(
      "Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). ",
      "Visualization in Bayesian Workflow. ",
      "(https://doi.org/10.1080/10618600.2018.1473781)"
    ),
    stan_diagnostics = c(
      paste0(
        "Betancourt, M., & Girolami, M. (2015). Hamiltonian Monte Carlo for ",
        "Hierarchical Models. In S. K. Upadhyay, U. Singh, D. K. Dey, & ",
        "A. Loganathan (Eds.), Current Trends in Bayesian Methodology with ",
        "Applications. (https://arxiv.org/abs/1312.0906)"
      ),
      paste0(
        "Betancourt, M. (2017). A Conceptual Introduction to Hamiltonian Monte ",
        "Carlo. (https://arxiv.org/abs/1701.02434)"
      )
    ),
    rhat_ess = paste0(
      "Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Burkner, P.-C. ",
      "(2021). Rank-Normalization, Folding, and Localization: An Improved ",
      "R-hat for Assessing Convergence of MCMC. Bayesian Analysis, 16(2), ",
      "667-718. (https://doi.org/10.1214/20-BA1221)"
    ),
    taxonomy = paste0(
      "Burkner, P.-C., Scholz, M., & Radev, S. (2023). Some models are useful, ",
      "but how do we know which ones? Towards a unified Bayesian model ",
      "taxonomy. (https://arxiv.org/abs/2306.12727)"
    ),
    causal_scaffold = paste0(
      "McElreath, R. (2020). Statistical Rethinking: A Bayesian Course with ",
      "Examples in R and Stan (2nd ed.). CRC Press. ",
      "(https://xcelab.net/rm/statistical-rethinking/)"
    ),
    causal_identification = c(
      paste0(
        "Pearl, J. (2009). Causality: Models, Reasoning and Inference (2nd ",
        "ed.). Cambridge University Press. ",
        "(https://doi.org/10.1017/CBO9780511803161)"
      ),
      paste0(
        "Textor, J., van der Zander, B., Gilthorpe, M. S., Liszkiewicz, M., & ",
        "Ellison, G. T. H. (2016). Robust causal inference using directed ",
        "acyclic graphs: the R package 'dagitty'. International Journal of ",
        "Epidemiology, 45(6), 1887-1894. ",
        "(https://doi.org/10.1093/ije/dyw341)"
      )
    ),
    projection_predictive = paste0(
      "Piironen, J., Paasiniemi, M., & Vehtari, A. (2020). Projective ",
      "Inference in High-Dimensional Problems: Prediction and Feature ",
      "Selection. Electronic Journal of Statistics, 14(1), 2155-2197. ",
      "(https://doi.org/10.1214/20-EJS1711)"
    )
  )
}

bg_workflow_references <- function(source_keys = NULL, references = NULL) {
  registry <- bg_workflow_source_registry()

  keyed_refs <- unlist(
    lapply(source_keys %||% character(), function(key) {
      registry[[key]] %||% key
    }),
    use.names = FALSE
  )

  unique(c(keyed_refs, references %||% character()))
}
