//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
data {
  int<lower=0> Ny;
  int<lower=0> Nx;
  vector[Ny] y;
  vector[Nx] x;
  real shift_a;                    // Translation parameter
  real threshold;                  // threshold parameter to indicator function

}

parameters {
  real mu;
  real<lower=0> sigma;             // Standard deviation parameter
}

transformed parameters {
  real data_mean_x = mean(x);  // Calculate the mean of the input data
  real data_sd_y = sd(y);      // Calculate the standard deviation of the input data
  real data_sd_x = sd(x);      // Calculate the standard deviation of the input data
}

model {
  y ~ normal(mu, data_sd_y/sqrt(Ny));
  
  // Indicator function on the prior variance
  if (data_mean_x + shift_a > threshold*data_mean_x) {
    mu ~ normal(data_mean_x + shift_a, data_sd_x/sqrt(Nx));  //prior variance normal
  } else {
    mu ~ normal(data_mean_x + shift_a,1e6);  // as indicator tends to infinity
  }
  
}

