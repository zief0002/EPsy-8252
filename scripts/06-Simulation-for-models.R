###################################################
### Read in the heights.dta data
###################################################

# Data are in Stata's binary format
library(foreign)

heights = read.dta(
	file = "/Users/andrewz/Documents/Data/Gelman-Regression/earnings/heights.dta"
	)

## Clean data
ok = !is.na(heights$earn + heights$height + heights$sex) & (heights$earn > 0)
heights = as.data.frame (cbind (heights$earn, heights$height, heights$sex)[ok, ])
names(heights) = c("earn", "height", "sex")

# Prepare variables
heights$Learn = log(heights$earn)
heights$male = ifelse(heights$sex == 1, 1, 0)



###################################################
### Model of log earnings 
###################################################

lm.a = lm(Learn ~ height, data = heights)

library(arm)
display(lm.a)



###################################################
### Predictions and prediction intervals
###################################################

x.tilde = data.frame(
	height = 68
	)

pred.interval = predict(lm.a, newdata = x.tilde, interval = "prediction", level = 0.95)
pred.interval

# Convert to raw scale
exp(pred.interval)



###################################################
### Constructing the prediction interval using simulation
###################################################

pred.log.scale = rnorm(1000, 9.79, 0.88)
head(pred.log.scale)

library(sm)
sm.density(pred.log.scale, xlab = "log(Earnings)")

quantile(pred.log.scale, c(0.025, 0.975))


# Convert to raw values
pred = exp(rnorm(1000, 9.79, 0.88))
head(pred)
sm.density(pred, xlab = "Earnings")



## Why do we need simulation for predictive inferences?

# Difference between average earnings between person who is 68" tall and person who is 60" tall
pred.68 = exp(rnorm(1000, 5.78 + 0.06*68, 0.89))
pred.60 = exp(rnorm(1000, 5.78 + 0.06*60, 0.89))

# Find CIs
pred.diff = pred.68 - pred.60
quantile(pred.diff, c(0.025, 0.975))

pred.ratio = pred.68 / pred.60
quantile(pred.ratio, c(0.025, 0.975))



###################################################
### Simulation to represent uncertainty in regression coefficients
###################################################

trials = 1000
sim.1 = sim (lm.a, trials)

## Inside the sim function

#for (s in 1: n.sims){
#  sigma[s] <- sigma.hat*sqrt((n-k)/rchisq (1, n-k))
#  beta[s] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
#}
#return (list (coef=beta, sigma=sigma))


# Two slots: "coef" and "sigma"
head(sim.1@coef)
head(sim.1@sigma)

int = sim.1@coef[ , 1]
height.coef = sim.1@coef[ , 2]
sig = sim.1@sigma

# Uncertainty in beta_0
mean(int)
sd(int)
quantile(int, c(0.025, 0.975))

# Uncertainty in beta_1
mean(height.coef)
sd(height.coef)
quantile(height.coef, c(0.025, 0.975))


# Uncertainty in model (coefficients) and in the error
preds = int[1] + height.coef[1]*68 + rnorm(1, 0, sig[1])
preds


preds = rep(NA, 1000)
for(i in 1:1000){
	preds[i] = int[i] + height.coef[i]*68 + rnorm(1, 0, sig[i])
}

quantile(preds, c(0.025, 0.975))
exp(quantile(preds, c(0.025, 0.975)))


###################################################
### Interaction model
###################################################

lm.b = lm(Learn ~ height + male + height:male, data = heights)
display(lm.b)


# Compute PI for interaction model (male who is 68")
x.new = data.frame(
	height = 68, 
	male = 1
	)

pred.interval = predict(lm.b, newdata = x.new, interval = "prediction", level = 0.95)
pred.interval

exp(pred.interval) # Convert to raw scale

# Simulation for coefficient uncertainty
sim.2 = sim (lm.b, trials)

head(sim.2@coef)
head(sim.2@sigma)

# Predict for 68" tall man
yhat = sim.2@coef[1 , 1] + sim.2@coef[1 , 2]*68 + sim.2@coef[1 , 3]*1 + 
	sim.2@coef[1 , 4]*1*68 + rnorm(1, 0, sim.2@sigma[1])
yhat

# PI for 68" tall men 
yhat = rep(NA, 1000)
for(i in 1:1000){
	yhat[i] = sim.2@coef[i , 1] + sim.2@coef[i , 2]*68 + sim.2@coef[i , 3]*1 + 
	sim.2@coef[i , 4]*1*68 + rnorm(1, 0, sim.2@sigma[i])
}

sm.density(yhat)
quantile(yhat, c(0.025, 0.975))
exp(quantile(yhat, c(0.025, 0.975)))




