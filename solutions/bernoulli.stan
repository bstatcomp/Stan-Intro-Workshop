data {
  int<lower=1> n;
  int y[n];
}

parameters {
  real<lower=0,upper=1> theta;
}

model {
  // prior
  theta ~ beta(1,1);

  for (i in 1:n) {
    y[i] ~ bernoulli(theta);
  }
}
