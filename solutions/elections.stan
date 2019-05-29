data {
  int<lower=1> n; // number of voters
  int<lower=1> k; // number of parties
  int Y[n, k]; // votes matrix
}

parameters {
  simplex[k] theta;
}

model {
  // prior
  theta ~ beta(1,1);

  for (i in 1:n) {
    for(j in 1:k) {
      Y[i, j] ~ bernoulli(theta[j]);
    }
  }
}
