data {
  int<lower=0> n; // number of data entries
  int<lower=0> k; // number of attributes (predictors)
  matrix[n, k] X; // year
  vector[n] y; // profit
}

parameters {
  real a;
  vector[k] b;
  real<lower=0> sigma;
}

model {
  for (i in 1:n) {
    for (j in 1:k) {
      y[i] ~ normal(a + b[j] * X[i, j], sigma);
    }
  }
}
