library(magrittr)
library(rstan)
library(dplyr)
library(tidyr)
rstan_options(auto_write = TRUE)

# Create a stan model, see empirical_profile_Bayes.stan.
stan_model <- rstan::stan_model(file = "./code/empirical_profile_Bayes.stan")

# Mimic what is done in the paper, and create the value for null hypothesis model

# Simulate data for null hypothesis 
mean = 0
sd = 10
X25 = rnorm(25,mean = mean,sd = sd)
X50 = rnorm(50,mean = mean,sd = sd)
X100 = rnorm(100,mean = mean,sd = sd)
X200 = rnorm(200,mean = mean,sd = sd)
X400 = rnorm(400,mean = mean,sd = sd)
X500 = rnorm(500,mean = mean,sd = sd)
X1000 = rnorm(1000,mean = mean,sd = sd)
Y = rnorm(1000,mean = 1,sd = 10^2)

for(sample_size in c(25,50,100,200,400,500,1000)){
  
  data_get = get(paste0("X",sample_size))
  
# Fit Stan model
assign(paste0("stan_model_",sample_size),stan("./code/empirical_profile_Bayes.stan", 
           data = list(Nx = sample_size, 
                       Ny = 1000, 
                       x = data_get, 
                       y = Y,
                       shift_a = 0.3,
                       threshold = 0.5 # information borrowing only when pediatric mean > 50% of adult's
                       )))

}


# Calculate type I error for each
paste0("stan_model_",c(25,50,100,200,400,500,1000))




# PLot all the type I error

