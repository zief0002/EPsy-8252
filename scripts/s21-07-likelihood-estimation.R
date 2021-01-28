##################################################
### Load libraries
##################################################

library(broom)
library(tidyverse)



##################################################
### Import data
##################################################

# Read in data
mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mn-schools.csv")


# View data
head(mn)



##################################################
### Likelihood
##################################################

# Compute likelihood mu=20, sigma=4
prod(dnorm(x = c(30, 20, 24, 27), mean = 20, sd = 4))


# Compute likelihood mu=25, sigma=4
prod(dnorm(x = c(30, 20, 24, 27), mean = 25, sd = 4))



##################################################
### Use MLE to estimate mean
##################################################

# Assume Normal distribution and sigma=4.272; 

# Compute L(mu = 10)
prod(dnorm(c(30, 20, 24, 27), mean = 10, sd = 4.272))


# Compute L(mu = 15)
prod(dnorm(c(30, 20, 24, 27), mean = 15, sd = 4.272))


# Compute L(mu = 20)
prod(dnorm(c(30, 20, 24, 27), mean = 20, sd = 4.272))


# Compute L(mu = 25)
prod(dnorm(c(30, 20, 24, 27), mean = 25, sd = 4.272))


# Compute L(mu = 30)
prod(dnorm(c(30, 20, 24, 27), mean = 30, sd = 4.272))


# Plot the likelihood vs. the mean value
# Create data
dat = data.frame(
  mu = c(10, 15, 20, 25, 30)
) %>%
  rowwise() %>%
  mutate(
    L = prod(dnorm(c(30, 20, 24, 27), mean = mu, sd = 4.272))
  )


# Plot
ggplot(data = dat, aes(x = mu, y = L)) +
  geom_line(color = "darkgrey", size = 0.5) +
  geom_point() +
  xlab(expression(mu)) +
  ylab("Likelihood") +
  theme_light()



##################################################
### Computational (grid) search
##################################################

# Set up parameter search space
# Compute likelihood
dat = data.frame(
  mu = seq(from = 10, to = 30, by = 0.01)
) %>%
  rowwise() %>%
  mutate(
    L = prod(dnorm(c(30, 20, 24, 27), mean = mu, sd = 4.272))
  ) %>%
  ungroup()


# View results
head(dat)


# Plot the likelihood versus the parameter values
ggplot(data = dat, aes(x = mu, y = L)) +
  geom_line() +
  xlab(expression(mu)) +
  ylab("Likelihood") +
  theme_light()


# Find mu with maximum likelihood
dat %>%
  slice_max(L, n = 1) 



##################################################
### Use MLE to estimate mu and sigma; assuming normal dist.
##################################################

# Set up search space
# Compute the likelihood
dat = crossing(
  mu = seq(from = 10, to = 30, by = 0.1),
  sigma = seq(from = 0, to = 10, by = 0.1)
) %>%
  rowwise() %>%
  mutate(
    L = prod(dnorm(c(30, 20, 24, 27), mean = mu, sd = sigma))
  ) %>%
  ungroup()


# Find row with highest likelihood  
dat %>%
  slice_max(L, n = 1)




##################################################
### Computational (grid) search using log-Likelihood
##################################################

# Set up parameter search space
# Compute likelihood and 
dat = data.frame(
  mu = seq(from = 10, to = 30, by = 0.01)
) %>%
  rowwise() %>%
  mutate(
    L = prod(dnorm(c(30, 20, 24, 27), mean = mu, sd = 4.272)),
    ln_L = log(L) 
  ) %>%
  ungroup()


# Find mu with maximum log-likelihood
dat %>%
  slice_max(ln_L, n = 1)


# Plot the log-likelihood versus the parameter values
ggplot(data = dat, aes(x = mu, y = ln_L)) +
  geom_line() +
  xlab(expression(mu)) +
  ylab("Log-likelihood") +
  theme_light()



##################################################
### Maximum likelihood for regression - toy example
##################################################

# Enter data into vectors
x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)



##################################################
### Function to compute log-likelihood
##################################################

# Function to compute the log-likelihood
ll = function(b_0, b_1){
  
  # Use the following x and y values
  x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
  y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)
  
  # Compute the yhat and residuals based on the two input values
  yhats = b_0 + b_1*x
  errors = y - yhats
  
  # Compute the sd of the residuals
  sigma = sd(errors)
  
  # Compute the log-likelihood
  log_lik = sum(dnorm(errors, mean = 0, sd = sigma, log = TRUE))
  
  # Output the log-likelihood
  return(log_lik)
}



##################################################
### Use the function to compute log-likelihood
##################################################

# Compute log-likelihood for b_0=10 and b_1=3
ll(b_0 = 10, b_1 = 3)

# Compute log-likelihood for b_0=20 and b_1=10
ll(b_0 = 20, b_1 = 10)




##################################################
### Use function in grid search
##################################################

# Create data set of search values and log-likelihoods
dat = crossing(
  B0 = seq(from = 30, to = 50, by = 0.1),
  B1 = seq(from = -5, to = 5, by = 0.1)
) %>%
  rowwise() %>%
  mutate(
    ln_L = ll(b_0 = B0, b_1 = B1)
  ) %>%
  ungroup()

# Find parameters that produce highest log-likelihood
dat %>%
  slice_max(ln_L, n = 1)


# Compute residuals using MLE estimate
errors = y - 40.1 - 2.7*x


# Compute estimate of RMSE
sd(errors)



##################################################
### Computing MLEs in Practice
##################################################

# Load library
library(bbmle)


# Function to output the negative log-likelihood
neg_ll = function(b_0, b_1, rse){
  
  # Use the following x and y values
  x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
  y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)
  
  # Compute the yhat and residuals based on the two input values
  yhats = b_0 + b_1*x
  errors = y - yhats
  
  # Compute the negative log-likelihood
  neg_log_lik = -sum(dnorm(errors, mean = 0, sd = rse, log = TRUE))
  
  # Output the log-likelihood
  return(neg_log_lik)
  
}


# Use function to compute ML estimates
# Fit model using ML
mle.results = mle2(
  minuslogl = neg_ll, 
  start = list(
    b_0 = 20.0, 
    b_1 = 5.0, 
    rse = 10)
  )

# View results
summary(mle.results)



##################################################
### Compare ML estimates with OLS estimates
##################################################

# Obtain OLS estimates
lm.1 = lm(y ~ 1 + x)
tidy(lm.1)
glance(lm.1)



##################################################
### Obtain log-likelihood and likelihood in practice using OLS estimated model
##################################################

# Log-likelihood
logLik(lm.1)


# Negative log-likelihood
-logLik(lm.1)


# Likelihood
exp(-39.45442)

