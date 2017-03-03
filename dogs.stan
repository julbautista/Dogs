data{
  int<lower = 0> N;
  int<lower = 0> shocks[N];
  int<lower = 0> dog_id[N];
  real prevshocks[N];
  real avoidances[N];
}

parameters{
  real b0;
  real<upper=0> b_prevshocks[30];
  real<upper=0> b_avoidances[30];
  real mu_prevshocks;
  real<lower=0> tau_prevshocks;
  real mu_avoidances;
  real<lower=0> tau_avoidances;
}

model{
  for(n in 1:30){
    for(i in 1:N){
      shocks[i] ~ bernoulli_logit(b0 + 
      b_prevshocks[dog_id[n]] * prevshocks[i]
      + b_avoidances[dog_id[n]] * avoidances[i]);
    }
  }
  b_prevshocks~normal(mu_prevshocks, tau_prevshocks);
  b_avoidances~normal(mu_avoidances, tau_avoidances);
  mu_prevshocks~normal(0,5);
  tau_prevshocks~normal(1,2);
  mu_avoidances~normal(0,5);
  tau_avoidances~normal(1,2);
}

generated quantities{
  int<lower = 0, upper = 1> sim_avoid[N];
  for(n in 1:30){
    for(i in 1:N){
      sim_avoid[i] = bernoulli_rng(exp(b0+ 
                  b_prevshocks[dog_id[n]] * prevshocks[i]
                  + b_avoidances[dog_id[n]] * avoidances[i]));
    }
  }
}
