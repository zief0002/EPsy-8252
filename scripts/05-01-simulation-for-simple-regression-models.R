###################################################
### Read in and examine data
###################################################

jun_high = read.csv("~/Documents/GitHub/EPsy-8251/data/homework-education-gpa.csv")
head(jun_high)
nrow(jun_high)



###################################################
### Load libraries
###################################################

library(ggplot2)
library(MASS)
library(sm)



###################################################
### Use matrix algebra to obtain beta estimates 
###################################################


y = jun_high$gpa                                          # Vector of the outcome
x = matrix(c(rep(1, 100), jun_high$homework), ncol = 2)   # Design (X) matrix
b = solve(t(x) %*% x) %*% t(x) %*% y                      # Beta estimates
b



###################################################
### Predict GPA for a student with HW = 6
###################################################

74.289677 + 1.214209 * 6
b[1] + b[2] * 6    # Can also index the estimated values from b directly



###################################################
### Obtain estimate for RMSE
###################################################

residuals = y - (x %*% b)
sse = t(residuals) %*% residuals
df = 98
mse = sse / df
rmse = sqrt(mse)
rmse



###################################################
### Simulate a single prediction for HW = 6
###################################################

b[1] + b[2] * 6 + rnorm(n = 1, mean = 0, sd = rmse)



###################################################
### Simulate a prediction for HW = 6
###################################################

# Create function to generate a Y value
yhat = function(x){
  b[1] + b[2] * x + rnorm(n = 1, mean = 0, sd = rmse)
  }

#Run the function
yhat(6)



###################################################
### Simulate MANY predictions for HW = 6
###################################################

# Carry out 10 trials
replicate(10, yhat(6))

# Carry out 1000 trials
preds = replicate(1000, yhat(6))
head(preds)

# Examine the distribution of the predicted values (needs sm library)
sm.density(preds)

mean(preds)
sd(preds)



###################################################
### Interval estimates for the prediction uncertainty
###################################################

# Compute interval using simulated mean and SD
mean(preds) - 2 * sd(preds)
mean(preds) + 2 * sd(preds)

# Compute the percentile interval
quantile(preds, probs = c(0.025, 0.975))



###################################################
### Predicting the mean value with sampling uncertainty
###################################################

# Variance-covariance matrix 
Vb = solve(t(x) %*% x)
vc = as.numeric(rmse ^ 2) * Vb 
vc

# Compute SEs
sqrt(diag(vc))

# Intuitive....but WRONG!!!!
b0 = rnorm(n = 1, mean = 74.289677, sd = 1.9419562)
b1 = rnorm(n = 1, mean =  1.214209, sd = 0.3540204)
b0 + b1 * 6



###################################################
### Sample from the multivariate normal distribution
###################################################

library(MASS)
sampled_b = mvrnorm(n = 1, b, vc)
sampled_b

# Generate an average y from the DGP
sampled_b[1] + sampled_b[2] * 6



###################################################
### Function to simulate a single average y from the DGP
###################################################

yhat2 = function(x){
  sampled_b = mvrnorm(1, b, vc)
  sampled_b[1] + sampled_b[2] * x
  }


# Use the function to randomly generate an average y value for x = 6
yhat2(6)
yhat(20)


###################################################
### Use replicate() to generate MANY average y values for x = 6
###################################################

preds2 = replicate(1000, yhat2(x = 6))


# Examine distribution of randomly generated averages
# sm.density(preds2)
# mean(preds2)
# sd(preds2)


# 95% percentile interval
quantile(preds2, probs = c(0.025, 0.975))




###################################################
### Account for both sampling and prediction uncertainty
###################################################

# Sample an errors from the appropriate normal distribution
e = rnorm(n = 1, mean = 0, sd = rmse)

# Use the vectors to produce predicted values
sampled_b[1] + sampled_b[2] * 6 + e



###################################################
### Randomly generate RMSE and use that in the prediction
###################################################

rmse = 7.23961 * sqrt((100 - 2) / rchisq(n = 1, df = 98))  # Randomly generate the RMSE
vc_sim = as.numeric(rmse ^ 2) * Vb                         # Compute the variance-covariance matrix using the randomly generated RMSE
sampled_b = mvrnorm(n = 1, b, vc_sim)                      # Randomly generate betas
e = rnorm(n = 1, mean = 0, sd = rmse)                      # Randomly generate an error term


# Use the randomly generated parameters to produce predicted values
sampled_b[1] + sampled_b[2] * 6 + e



###################################################
### Function to randomly generate RMSE, betas, and error then make a prediction from a given value of x
###################################################

yhat3 = function(x){
  n = nrow(jun_high)
  k = 2
  rmse = 7.23961 * sqrt((n - k) / rchisq(n = 1, df = (n - k)))
  vc_sim = as.numeric(rmse ^ 2) * Vb 
  sampled_b = mvrnorm(n = 1, b, vc_sim)
  e = rnorm(n = 1, mean = 0, sd = rmse)
  sampled_b[1] + sampled_b[2] * x + e
  }

yhat3(x = 6)



###################################################
### Carry out MANY predictions
###################################################

preds3 = replicate(1000, yhat3(x = 6))

# Examine the distribution of the predicted values
# sm.density(preds3)
# mean(preds3)
# sd(preds3)

# Compute the percentile interval
quantile(preds3, probs = c(0.025, 0.975))



###################################################
### Pro Tip: Replicating simulation results
###################################################


set.seed(314)                     # Set the simulation seed
rnorm(n = 1, mean = 0, sd = 1)    # Randomly generate something
