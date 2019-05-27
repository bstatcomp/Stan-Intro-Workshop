data {
  int<lower=0> n; // total number of measurements
  vector[n] x; // year
  vector[n] y; // temperature
}

parameters {
  real a;
  real b;
  real<lower=0> sigma;
}

model {
  for (i in 1:n) {
    y[i] ~ normal(a + b * x[i], sigma);
  }
}
