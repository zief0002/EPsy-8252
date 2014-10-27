###################################################
### Read in data
###################################################

students = read.csv(file = "/Users/andrewz/Documents/Data/HSB-Level-1.csv")
head(students)



###################################################
### Load libraries
###################################################

library(ggplot2)
library(lme4)
library(psych)
library(sm)



###################################################
### Fit the unconditional means model
###################################################

lmer.u = lmer(math ~ 1 +  (1 | school), data = students)
summary(lmer.u)

deviance(lmer.u)
AIC(lmer.u)
BIC(lmer.u)



###################################################
### Fit the conditional means model
###################################################

lmer.c = lmer(math ~ 1 + ses + (1|school), data = students)
summary(lmer.c)

deviance(lmer.c)
AIC(lmer.c)
BIC(lmer.c)



###################################################
### Random intercept and slope model
###################################################

lmer.ri.rs = lmer(math ~ 1 + ses + (1 + ses |school), data = students)
summary(lmer.ri.rs)

deviance(lmer.ri.rs)
AIC(lmer.ri.rs)
BIC(lmer.ri.rs)



###################################################
### Extras
###################################################

# Get estimates of the random-effects
ranef(lmer.ri.rs)

# Estimates of the variance-covariance matrix of the random effects
VarCorr(lmer.ri.rs)$school




###################################################
### Examine assumptions
###################################################

# Residuals
sm.density(resid(lmer.ri.rs), model = "normal")

# Examine the random-effects for intercept
eta_0 = ranef(lmer.ri.rs)$school[ , 1]
sm.density(eta_0, model = "normal")

# Examine the random-effects for slope
eta_1 = ranef(lmer.ri.rs)$school[ , 2]
sm.density(eta_1, model = "normal")

pdf(file="~/Desktop/plot.pdf")
dev.off()

