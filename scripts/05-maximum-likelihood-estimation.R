##################################################
### Load libraries
##################################################

library(tidyverse)
library(bbmle)
library(broom)

# Set options to suppress scientific notation
options(scipen = 99)



##################################################
### Joint probability density
##################################################

dnorm(x = 60, mean = 50, sd = 10) *
  dnorm(x = 65, mean = 50, sd = 10) * 
  dnorm(x = 67, mean = 50, sd = 10)

# Shortcut
prod(dnorm(x = c(60, 65, 67), mean = 50, sd = 10))



##################################################
### Likelihood
##################################################

# What is the likelihood (probability) that the mean is 20 and the standard deviation is 4?
prod(dnorm(x = c(30, 20, 24, 27), mean = 20, sd = 4))

# What is the likelihood (probability) that the mean is 25 and the standard deviation is 4?
prod(dnorm(x = c(30, 20, 24, 27), mean = 25, sd = 4))



##################################################
### Maximum likelihood
##################################################

expand.grid(
  mu = seq(from = 10, to =30, by = 0.1),
  sigma = seq(from = 0, to = 10, by = 0.1)
  ) %>%
  rowwise() %>%
  mutate( lik = prod(dnorm(c(30, 20, 24, 27), mean = mu, sd = sigma)) ) %>%
  arrange(desc(lik))



##################################################
### Log-Likelihood
##################################################

log(.00001829129)


expand.grid(
  mu = seq(from = 10, to =30, by = 0.1),
  sigma = seq(from = 0, to = 10, by = 0.1)
  ) %>%
  rowwise() %>%
  mutate( 
    log_lik = sum(dnorm(c(30, 20, 24, 27), mean = mu, sd = sigma, log = TRUE)) 
    ) %>%
  arrange(desc(log_lik))



##################################################
### Maximum likelihood for regression
##################################################

# Enter data into vectors
x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)





##################################################
### Function to compute log-likelihood
##################################################

log_likelihood = function(b0, b1){
  
  # Use the following x and y values
  x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
  y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)
  
  # Compute the yhat and residuals based on the two input values
  yhats = b0 + b1*x
  errors = y - yhats
  
  # Compute the sd of the residuals
  sigma = sd(errors)
  
  # Compute the log-likelihood
  log_lik = sum(dnorm(errors, mean = 0, sd = sigma, log = TRUE))
  
  # Output the log-likelihood
  return(log_lik)
}


# Use the function
log_likelihood(b0 = 10, b1 = 3)


##################################################
### Use function in grid search
##################################################

expand.grid(
  b0 = seq(from = 30, to = 50, by = 0.1),
  b1 = seq(from = -5, to = 5, by = 0.1)
  ) %>%
  rowwise() %>%
  mutate( log_lik = log_likelihood(b0 = b0, b1 = b1) ) %>%
  arrange(desc(log_lik))

# Compute estimate of RMSE
errors = y - 40.1 - 2.7*x
sd(errors)



##################################################
### Use ML to estimate regression coefficients and RMSE
##################################################

# Obtain OLS estimates
lm.1 = lm(y ~ 1 + x)
tidy(lm.1)
glance(lm.1)

# Function to compute negative log-likelihood
regress.ll = function(b0, b1, rmse) { 
  # Use the following x and y values
  x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
  y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)
  
  # Compute yhats and residuals
  yhats = b0 + b1 * x
  errors = y - yhats
  
  # Compute the negative log-likelihood
  neg_log_lik = -sum(dnorm(errors, mean = 0, sd = rmse, log = TRUE))
  return(neg_log_lik)
} 

# Use function to compute ML estiates
mle.results = mle2(
  minuslogl = regress.ll, 
  start = list(b0 = 40.0, b1 = 2.7, rmse = 13.98)
  )

# View results
summary(mle.results)



##################################################
### Obtain log-likelihood and likelihood in practice
##################################################

# Fit OLS regression
lm.1 = lm(y ~ 1 + x)

# Log-likelihood
logLik(lm.1)

# Likelihood
exp(-39.45442)
