// Zero-inflated negative-binomial regression for the roaches pest-management
// data (Gelman and Hill, 2007). A structural-zero mixture (intercept-only
// zero-inflation) on top of the negative-binomial count component.
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
  real zi_alpha; // logit probability of a structural zero
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
  zi_alpha ~ normal(0, 1.5);
  for (n in 1:N) {
    if (y[n] == 0) {
      target += log_sum_exp(
        bernoulli_logit_lpmf(1 | zi_alpha),
        bernoulli_logit_lpmf(0 | zi_alpha)
          + neg_binomial_2_log_lpmf(0 | eta[n], phi)
      );
    } else {
      target += bernoulli_logit_lpmf(0 | zi_alpha)
        + neg_binomial_2_log_lpmf(y[n] | eta[n], phi);
    }
  }
}
generated quantities {
  array[N] int yrep;
  vector[N] log_lik;
  for (n in 1:N) {
    if (y[n] == 0) {
      log_lik[n] = log_sum_exp(
        bernoulli_logit_lpmf(1 | zi_alpha),
        bernoulli_logit_lpmf(0 | zi_alpha)
          + neg_binomial_2_log_lpmf(0 | eta[n], phi)
      );
    } else {
      log_lik[n] = bernoulli_logit_lpmf(0 | zi_alpha)
        + neg_binomial_2_log_lpmf(y[n] | eta[n], phi);
    }
    if (bernoulli_logit_rng(zi_alpha) == 1) {
      yrep[n] = 0;
    } else {
      // Capped gamma-Poisson mixture; see roaches_negbinomial.stan.
      real g = gamma_rng(phi, phi / exp(eta[n]));
      yrep[n] = poisson_rng(fmin(g, 1e8));
    }
  }
}
