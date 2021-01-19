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
### Model 1: Classical Framework of Evidence
##################################################

# Fit Model 1
lm.1 = lm(grad ~ 1 + sat, data = mn)


# Coefficient-level output
tidy(lm.1)



##################################################
### Joint Probability Density
##################################################

# Compute joint density
dnorm(x = 60, mean = 50, sd = 10) * 
  dnorm(x = 65, mean = 50, sd = 10) * 
  dnorm(x = 67, mean = 50, sd = 10)


# Compute joint density: Shortcut
prod(dnorm(x = c(60, 65, 67), mean = 50, sd = 10))



##################################################
### Computing and evaluating likelihood
##################################################

# L(mu=20, sigma = 4 | x and ~N)
prod(dnorm(x = c(30, 20, 24, 27), mean = 20, sd = 4))


# L(mu=25, sigma = 4 | x and ~N)
prod(dnorm(x = c(30, 20, 24, 27), mean = 25, sd = 4))


# Likelihood ratio
0.00001774012 / 0.0000005702554



##################################################
### Back to regression example
##################################################

# Compute likelihood for Model 1
prod(dnorm(x = resid(lm.1), mean = 0, sd = 7.79))


# Fit Model 2
lm.2 = lm(grad ~ 1 + sat + public, data = mn)


# Get RSE for use in likelihood
glance(lm.2)


# Compute likelihood for Model 2
prod(dnorm(x = resid(lm.2), mean = 0, sd = 6.86))


# Compute likelihood ratio
5.231164e-48 / 4.71647e-50



##################################################
### Log-likelihood
##################################################

# Log-likelihood for Model 1
log(4.71647e-50)


# Log-likelihood for Model 2
log(5.231164e-48)


# Difference in log-likelihoods
log(5.231164e-48) - log(4.71647e-50)


# Equivalent to ln(LR)
log(5.231164e-48 / 4.71647e-50)


# Exponentiate the difference in log-likelihoods to get LR
exp(4.708743)



##################################################
### Shortcut: logLik()
##################################################

# Compute log-likelihood for Model 1
logLik(lm.1)


# Compute log-likelihood for Model 2
logLik(lm.2)


# Compute likelihood for Model 2
exp(logLik(lm.2)[1])


# Compute LR
exp( logLik(lm.2)[1] - logLik(lm.1)[1] )



##################################################
### Likelihood Ratio Test for Nested Models (Model 1 vs. Model 2)
##################################################

# Compute chi-squared
-2 * (logLik(lm.1)[1] - logLik(lm.2)[1])


# Compute the deviance for Model 1
-2 * logLik(lm.1)[1]


# Compute the deviance for Model 2
-2 * logLik(lm.2)[1]


# Compute difference in deviances
227.0944 - 217.5929


# Compute p-value for X^2 = 9.5015
1 - pchisq(q = 9.5015, df = 1)


# Alternative method
pchisq(q = 9.5015, df = 1, lower.tail = FALSE)



##################################################
### LRT (Model 2 vs. Model 3)
##################################################

# Fit Model 3
lm.3 = lm(grad ~ 1 + sat + public + sat:public, data = mn)


# Log-likelihood for Model 3
logLik(lm.3)


# Likelihood for Model 3
exp(logLik(lm.3)[1])


# Deviance for Model 3
-2 * logLik(lm.3)[1]


# Likelihood ratio
7.552619e-48 / 5.627352e-48


# Chi-squared; Difference in deviance
-2 * logLik(lm.2)[1] - (-2 * logLik(lm.3)[1])


# Compute p-value
pchisq(q = 0.588511, df = 1, lower.tail = FALSE)



##################################################
### Use ML to estimate regression coefficients and RMSE
##################################################

# Obtain OLS estimates
lm.1 = lm(y ~ 1 + x)
tidy(lm.1)
glance(lm.1)



##################################################
### Using lrtest()
##################################################

# Load library
library(lmtest)


# LRT to compare Model 1 and Model 2
lrtest(lm.1, lm.2)


# LRT to compare Model 2 and Model 3
lrtest(lm.2, lm.3)


# Multiple LRTs
# First LRT to compare Model 1 and Model 2
# Second LRT to compare Model 2 and Model 3
lrtest(lm.1, lm.2, lm.3)


