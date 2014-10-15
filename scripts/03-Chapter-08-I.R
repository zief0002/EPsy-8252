###################################################
### Create dataset
###################################################

law = data.frame(
  school = 1:15, 
  lsat = c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594), 
  gpa = c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43, 3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 2.96)
  )

# Display law dataframe
law



###################################################
### Fit regression model
###################################################

lm.1 = lm(gpa ~ 1 + lsat, data = law)
summary(lm.1)

library(car)
influencePlot(lm.1)

# Check residual plots from the fitted regression model
par(mfrow = c(2, 2))
plot(lm.1)
par(mfrow = c(1, 1))



###################################################
### Simulate random errors
### (Check the parmaters/properties of a model)
###################################################

pred = fitted(lm.1)

y = pred + rnorm(15, mean = 0, sd = 0.1593)
x = law$lsat

lm(y ~ x)
coef(lm(y ~ x))[[2]]



###################################################
### Simulate random errors 1000 times
### (Check the parameters/properties of a model)
###################################################
slopes = rep(NA, 1000)

for(i in 1:10000){
  y = pred + rnorm(15, mean = 0, sd = 0.1593)
  slopes[i] = coef(lm(y ~ x))[[2]]
}

library(sm)
sm.density(slopes)

# Compute simulated SE
sd(slopes)


# Compute lower and upper limits on CI
ll = mean(slopes) - qt(.975, df = 13) * sd(slopes)
ul = mean(slopes) + qt(.975, df = 13) * sd(slopes)

# Compute coverage
sum(slopes < ll)
sum(slopes > ul)



###################################################
### Bootstrap regression coefficients
### (Compute SE of a parameter)
###################################################

samp = sample(1:15, size = 15, replace = TRUE)
samp

law[samp, ]

lm(gpa ~ 1 + lsat, data = law[samp, ])

slopes = rep(NA, 1000)

for(i in 1:1000){
  samp = sample(1:15, size = 15, replace = TRUE)
  slopes[i] = coef(lm(gpa ~ 1 + lsat, data = law[samp, ]))[[2]]
}

sm.density(slopes)

# Compute simulated SE
sd(slopes)



###################################################
### Is the distribution of LSAT scores "normal"?
### (Check the fit of a model)
###################################################

plot(density(law$lsat), ylim =c(0, .02))

# Get mean and SD
mean(law$lsat)
sd(law$lsat)

# Plot density of first random sample
plot(density(rnorm(15, mean = 600.2667, 41.79451), bw = 15), ylim =c(0, .02), xlim = c(400, 750))

# Plot density of 999 other random samples
for(i in 1:999){
  lines(density(rnorm(15, mean = 600.2667, 41.79451), bw = 15))
}

# Add density of LSAT scores
lines(density(law$lsat, bw = 19.21), col = "red", lwd = 2)




###################################################
### Is observation 1 a regression outlier?
### (Check the fit of a model)
###################################################

pred = fitted(lm.1)

y = pred + rnorm(15, mean = 0, sd = 0.1593)
x = law$lsat

lm.sim = lm(y ~ x)

plot(x = fitted(lm.sim), y = rstandard(lm.sim), pch = 20, cex = 2, ylim = c(-5, 5))
points(x = fitted(lm.1)[1], y = rstandard(lm.1)[1], col = "red", pch = 20, cex = 2)

for(i in 1:100){
  y = pred + rnorm(15, mean = 0, sd = 0.1593)
  lm.sim = lm(y ~ x)
  plot(x = fitted(lm.sim), y = rstandard(lm.sim), pch = 20, cex = 2, ylim = c(-5, 5), xlim = c(2.6, 3.8))
  points(x = fitted(lm.1)[1], y = rstandard(lm.1)[1], col = "red", pch = 20, cex = 2) 
}
