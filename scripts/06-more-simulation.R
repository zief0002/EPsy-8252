###################################################
### Read in  data
###################################################

jun_high = read.csv("~/Documents/GitHub/EPsy-8252/data/homework-education-gpa.csv")



###################################################
### Load libraries
###################################################

library(ggplot2)
library(MASS)
library(sm)



###################################################
### Matrix algebra
###################################################

y = jun_high$gpa                                         # Vector of the outcome
x = matrix(c(rep(1, 100), jun_high$homework), ncol = 2)  #  Predictor (X) matrix
b = solve(t(x) %*% x) %*% t(x) %*% y                     # Obtain the beta estimates
Vb = solve(t(x) %*% x)



###################################################
### Load the yhat() function from the sim_pred.R script
###################################################

source("~/Documents/GitHub/EPsy-8252/scripts/sim-pred.R")

#  Use the yhat() function
yhat(x = 6, data = jun_high)



###################################################
### Simulation for the difference between two students; accounting for sampling and prediction uncertainty
###################################################

# Predict difference one time
yhat(x = 9, data = jun_high) - yhat(x = 6, data = jun_high)


# Predict difference MANY times

g = replicate(10000, yhat(x = 9, data = jun_high) - yhat(x = 6, data = jun_high))
head(g)

# Examine distribution of the simulated outcomes
sm.density(g)
mean(g)
sd(g)

# Compute the percentile interval
quantile(g, probs = c(0.025, 0.975))



###################################################
### Simulation for the difference between two students; accounting for sampling and prediction uncertainty
###################################################

g9 = replicate(10000, yhat(x = 9, data = jun_high))
g6 = replicate(10000, yhat(x = 6, data = jun_high))

# Compute differences
g.diff = g9 - g6
head(g.diff)

# Examine distribution of the simulated outcomes
sm.density(g.diff)


# Examine summary measures of the simulated outcomes
mean(g.diff)
sd(g.diff)

# Compute the percentile interval
quantile(g.diff, probs = c(0.025, 0.975))



###################################################
### Compare the two methods
###################################################

plot(density(g.diff))
lines(density(g), col = "red", lty = "dashed")

mean(g.diff)
mean(g)

sd(g.diff)
sd(g)



###################################################
### Simulation for multiple regression
###################################################

y = jun_high$gpa           # Vector of the outcome
x = matrix(                #  Predictor (X) matrix
  c(rep(1, 100), 
  jun_high$homework, 
  jun_high$parentEd), 
    ncol = 3
  )  
b = solve(t(x) %*% x) %*% t(x) %*% y   # Obtain the beta estimates
Vb = solve(t(x) %*% x)


residuals = y - (x %*% b)
sse = t(residuals) %*% residuals
df = 97
mse = sse / df
rmse = sqrt(mse)
rmse


###################################################
### Function to predict after accounting for sampling and prediction uncertainty
###################################################

# Changed the following parameters: k, value in the rmse line, regression equation

yhat2 = function(homework, parentEd, data = d){
  n = nrow(data)
  k = 3
  rmse = 7.091574 * sqrt((n - k) / rchisq(n = 1, df = (n - k)))
  vc_sim = as.numeric(rmse ^ 2) * Vb 
  sampled_b = mvrnorm(n = 1, b, vc_sim)
  e = rnorm(n = 1, mean = 0, sd = rmse)
  sampled_b[1] + sampled_b[2] * homework + sampled_b[3] * parentEd + e
}



###################################################
### Use the function to predict for a student who spends 6 hours 
### on HW and parent education is 16 years.
###################################################

yhat2(homework = 6, parentEd = 16, data = jun_high)


# Carry out 10,000 trials
preds = replicate(10000, yhat2(homework = 6, parentEd = 16, data = jun_high))
head(preds)

# Examine the distribution of the predicted values
sm.density(preds)
mean(preds)
sd(preds)

# Compute the percentile interval
quantile(preds, probs = c(0.025, 0.975))



###################################################
### Write a more generalizable function
###################################################

# Write a function that takes a fitted model, and the predictor values 
# as input and then outputs the predicted value accounting for sampling 
# and prediction uncertainty. The command would look something like this:

lm.2 = lm(gpa ~ homework + parentEd, data = jun_high)
yhat3(model = lm.2, homework = 6, parentEd = 16)

# The outcome and predictor matrix would be generated from the fitted model.
# These could then be used to compute b and Vb. OR, the coefficients and 
# SEs could be indexed directly from the fitted model's output. Additionally,
# the RMSE and k could be computed from the fitted model. 




