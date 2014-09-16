###################################################
### Read in the heights.dta data
###################################################

# Data are in Stata's binary format
library(foreign)

heights = read.dta(
	file = "/Users/andrewz/Documents/Data/Gelman-Regression/earnings/heights.dta"
	)



## Simulation to represent predictive uncertainty

###################################################
### Model of log earnings with interactions
###################################################

## Clean data
ok = !is.na(heights$earn + heights$height + heights$sex) & (heights$earn > 0)
heights = as.data.frame (cbind (heights$earn, heights$height, heights$sex)[ok, ])
names(heights) = c("earn", "height", "sex")

# Prepare variables
heights$Learn = log(heights$earn)
heights$male = ifelse(heights$sex == 1, 1, 0)


lm.a = lm(Learn ~ height, data = heights)

library(arm)
display(lm.a)



###################################################
### Predictions and prediction intervals
###################################################

x.new = data.frame(
	height = 68, 
	male = 1
	)

pred.interval = predict(lm.a, newdata = x.new, interval = "prediction", level = 0.95)

print(exp(pred.interval))



###################################################
### Constructing the predictive interval using simulation
###################################################

pred = exp(rnorm(1000, 9.95, 0.88))
pred.original.scale = rnorm(1000, 9.95, 0.88)

library(sm)
sm.density(pred.original.scale, xlab = "log(earnings)")
sm.density(pred, xlab = "Earnings")

## Why do we need simulation for predictive inferences?

pred.man = exp(rnorm(1000, 8.4 + 0.17*68 - 0.079*1 + .007*68*1, 0.88))
pred.woman = exp(rnorm(1000, 8.4 + 0.17*68 - 0.079*0 + .007*68*0, 0.88))
pred.diff = pred.man - pred.woman
pred.ratio = pred.man / pred.woman



###################################################
### Simulation to represent uncertainty in regression coefficients
###################################################

trials = 1000
sim.1 = sim (lm.a, trials)

height.coef = sim.1$coef[ , 2]
mean(height.coef)
sd(height.coef)
quantile(height.coef, c(0.025, 0.975))

height.for.men.coef = sim.1$coef[ , 2] + sim.1$coef[ , 4]
quantile(height.for.men.coef, c(0.025, 0.975))

## Inside the sim function

#for (s in 1: n.sims){
#  sigma[s] <- sigma.hat*sqrt((n-k)/rchisq (1, n-k))
#  beta[s] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
#}
#return (list (coef=beta, sigma=sigma))

