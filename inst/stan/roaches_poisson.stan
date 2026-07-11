// Poisson regression for the roaches pest-management data
// (Gelman and Hill, 2007). Counts y with a log-exposure offset.
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
  y ~ poisson_log(eta);
}
generated quantities {
  array[N] int yrep;
  vector[N] log_lik;
  for (n in 1:N) {
    yrep[n] = poisson_log_rng(eta[n]);
    log_lik[n] = poisson_log_lpmf(y[n] | eta[n]);
  }
}
