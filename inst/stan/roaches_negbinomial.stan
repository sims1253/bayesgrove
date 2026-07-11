// Negative-binomial regression for the roaches pest-management data
// (Gelman and Hill, 2007). Overdispersed counts with a log-exposure offset.
data {
  int<lower=0> N;
  array[N] int<lower=0> y;
  vector[N] roach1s;
  vector[N] treatment;
  vector[N] senior;
  vector<lower=0>[N] exposure2;
}
transformed data {
  vector[N] log_exposure = log(exposure2);
}
parameters {
  real alpha;
  real b_roach1s;
  real b_treatment;
  real b_senior;
  real<lower=0> phi;
}
transformed parameters {
  vector[N] eta = alpha + b_roach1s * roach1s + b_treatment * treatment
                  + b_senior * senior + log_exposure;
}
model {
  alpha ~ normal(0, 5);
  b_roach1s ~ normal(0, 2.5);
  b_treatment ~ normal(0, 2.5);
  b_senior ~ normal(0, 2.5);
  phi ~ exponential(1);
  y ~ neg_binomial_2_log(eta, phi);
}
generated quantities {
  array[N] int yrep;
  vector[N] log_lik;
  for (n in 1:N) {
    // Draw via the gamma-Poisson mixture with the gamma draw capped, because
    // neg_binomial_2_log_rng overflows when the heavy tail exceeds the RNG
    // range. The cap only truncates astronomically implausible draws.
    real g = gamma_rng(phi, phi / exp(eta[n]));
    yrep[n] = poisson_rng(fmin(g, 1e8));
    log_lik[n] = neg_binomial_2_log_lpmf(y[n] | eta[n], phi);
  }
}
