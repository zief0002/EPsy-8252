###################################################
### Example Portland cement data
###################################################

# Load library for Multi-Model Inference
library(MuMIn)

# Load in Cement data from library
data(Cement)

# View Cement data
Cement











###################################################
### Fit mean model
###################################################

lm.1 = lm(y ~ 1, data = Cement)



###################################################
### ANOVA decomposition
###################################################

anova(lm.1)



###################################################
### ML Estimates
###################################################

# Mean
mean(Cement$y)

# Variance
2715.8 / 13



###################################################
### Examine estimates from R functions
###################################################

var(Cement$y)

summary(lm.1)




###################################################
### Compute Deviance
###################################################

-2 * logLik(lm.1)[1] 


# Fit slope model
lm.2 = lm(y ~ X1, data = Cement)   ## Fit slope model
-2 * logLik(lm.2)[1] 

# NOT the same!!!! The deviance() function returns the SSE.
deviance(lm.1)
deviance(lm.2)



###################################################
### Compute AIC
###################################################

# AIC for model 1 (k=2 since we are estimating B0 and Sigma^2(e).)
-2 * logLik(lm.1)[1] + 2*2

AIC(lm.1)



# AIC for model 2 (k=3 since we are estimating B0, B1, and Sigma^2(e).)
-2 * logLik(lm.2)[1] + 2*3

AIC(lm.2)



###################################################
### Compute BIC
###################################################

BIC(lm.1)
BIC(lm.2)



###################################################
### Likelihood Ratio Test (LRT)
###################################################

library(lmtest)

lrtest(lm.1, lm.2)

### Very different results
anova(lm.1, lm.2)

