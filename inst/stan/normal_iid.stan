// Minimal iid normal model for the getting-started vignette.
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 5);
  sigma ~ exponential(1);
  y ~ normal(mu, sigma);
}
generated quantities {
  array[N] real yrep;
  for (n in 1:N) {
    yrep[n] = normal_rng(mu, sigma);
  }
}
