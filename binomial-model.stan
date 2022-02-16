data {
  int<lower=0> n; // number of games
  int<lower=0> k; // number of wins

}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  theta ~ beta(4, 4);
  k ~ binomial(n, theta);
}
