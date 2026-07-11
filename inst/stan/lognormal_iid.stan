// Minimal iid lognormal model for the getting-started vignette.
data {
  int<lower=0> N;
  vector<lower=0>[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 2.5);
  sigma ~ exponential(1);
  y ~ lognormal(mu, sigma);
}
generated quantities {
  array[N] real yrep;
  vector[N] log_lik;
  for (n in 1:N) {
    yrep[n] = lognormal_rng(mu, sigma);
    log_lik[n] = lognormal_lpdf(y[n] | mu, sigma);
  }
}
