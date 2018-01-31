##################################################
### Load libraries
##################################################

library(tidyverse)
library(mosaic)
library(sm)



##################################################
### Read in data
##################################################

iq_data = read.csv(file = "~/Dropbox/epsy-8252/data/iq.csv")
head(iq_data)



##################################################
### Explore IQ scores
##################################################

iq_data %>%
  summarize(M = mean(iq), SD = sd(iq), N = n())

sm.density(iq_data$iq)



##################################################
### Parametric (normal theory) confidence interval
##################################################

t.test(iq_data$iq)



##################################################
### Draw one bootstrap sample and find the mean
##################################################

# Randomly sample 20 IQ scores with replacement
sample(x = iq_data$iq, size = 20, replace = TRUE)

# Compute the mean
mean(sample(x = iq_data$iq, size = 20, replace = TRUE))



##################################################
### Draw 10 bootstrap samples and find the mean
##################################################

do(10) * {
  mean(sample(x = iq_data$iq, size = 20, replace = TRUE))
  }



##################################################
### Draw 1000 bootstrap samples and find the mean
##################################################

boot_means = do(1000) * {
  mean(sample(x = iq_data$iq, size = 20, replace = TRUE))
  }

# Examine bootstrap distribution
sm.density(boot_means$result)

# Summary statistics
boot_means %>%
  summarize(M = mean(result), SD = sd(result))



##################################################
### Compute bootstrap-based CI: Add/subtract 2 SEs
##################################################

# Lower limit
mean(iq_data$iq) - 2 * sd(boot_means$result)

# Upper limit
mean(iq_data$iq) + 2 * sd(boot_means$result)



##################################################
### Compute bootstrap-based CI: Percentile method
##################################################

# Lower limit
quantile(boot_means$result, prob = .025)

# Upper limit
quantile(boot_means$result, prob = .975)



##################################################
### Bootstrapping the mean: Regression model
##################################################

# Fit the marginal mean model
lm.0 = lm(iq ~ 1, data = iq_data)

# Obtain observed mean (marginal mean) IQ score
lm.0

# Obtain parametric (normal theory) CI
confint(lm.0)



##################################################
### Resample rows from a data frame
##################################################

resample(iq_data)

# Use resample() in the lm() function
lm(iq ~ 1, data = resample(iq_data))



##################################################
### Draw 5 bootstrap samples
##################################################

do(5) * lm(iq ~ 1, data = resample(iq_data))



##################################################
### Draw 1000 bootstrap samples
##################################################

boot_reg = do(1000) * lm(iq ~ 1, data = resample(iq_data))

# Plot bootstrap distribution
sm.density(boot_reg$Intercept)



##################################################
### Compute bootstrap-based CIs
##################################################

# Add/subtract SE method
se = sd(boot_reg$Intercept)
mean(iq_data$iq) - 2 * se
mean(iq_data$iq) + 2 * se

# Percentile method
quantile(boot_reg$Intercept, prob = c(.025, .975))







