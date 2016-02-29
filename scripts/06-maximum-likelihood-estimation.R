#####################################
# Load Libraries
#####################################

library(ggplot2)
library(dplyr)



#####################################
# Log-likelihood function
#####################################

LL = function(b0){
	-5/2 * log(2 * pi * 15) - 1/(2*15) * ( (12 - b0)^2 + (8 - b0)^2 + (16.26 - b0)^2 + 
	(13.65 - b0)^2 + (8.5 - b0)^2 ) 
	}


## Try function
LL(11)
LL(11.01)
LL(11.02)
LL(11.03)



#####################################
# Grid Search (b0)
#####################################

## Generate candidate values for b0
candidates = data.frame(
	b0 = seq(from = 11, to = 12, by = 0.01)
	)


## Generate the log-likelihood values and store the results
gridSearch = candidates %>% 
  rowwise() %>% 
  mutate(ll = LL(b0))


# Examine the search grid
head(gridSearch)


## Plot log-likelihood vs. b0
ggplot(data = gridSearch, aes(x = b0, y = ll)) + 
    geom_line() +
    theme_bw() +
    xlab(expression(beta[0])) +
    ylab("Log-Likelihood")


## Find the maximum log-likelihood value
gridSearch %>% slice(which.max(ll))



#####################################
# Function to compute deviance (Inputs: b0, b1, sigma2)
#####################################

LL2 = function(b0, b1, sigma2){
  -5/2 * log(2 * pi * sigma2) - 1/(2*sigma2) * (
    (12    - b0 - b1 * 32)^2 + 
    (8     - b0 - b1 * 33)^2 + 
    (16.26 - b0 - b1 * 32)^2 + 
    (13.65 - b0 - b1 * 33)^2 + 
    (8.5   - b0 - b1 * 26)^2 
  ) 
}


# OR...compute deviance
dev = function(b0, b1, sigma2){
  5 * log(2 * pi * sigma2) * (
    (12    - b0 - b1 * 32)^2 + 
    (8     - b0 - b1 * 33)^2 + 
    (16.26 - b0 - b1 * 32)^2 + 
    (13.65 - b0 - b1 * 33)^2 + 
    (8.5   - b0 - b1 * 26)^2 
  ) 
}



## Try function
LL2(b0 = 0, b1 = 0, sigma2 = 15)
dev(b0 = 0, b1 = 0, sigma2 = 15)

-2 * LL2(b0 = 0, b1 = 0, sigma2 = 15)



#####################################
# Grid Search
#####################################

## Generate candidate values for the unknown parameters
candidates = expand.grid(
    b0 = seq(from = -4, to = -3, by = 0.01),
    b1 = seq(from = 0, to = 1, by = 0.01),
    sigma2 = seq(from = 0, to = 20, by = 0.01)
    )


## Time the computation (Note...must use <- rather than =)
system.time(
  gridSearch <- candidates %.% rowwise() %.% mutate(ll = LL2(b0, b1, sigma2))
  )

# gridSearch <- candidates %.% rowwise() %.% mutate(deviance = dev(b0, b1, sigma2))


## Find the maximum log-likelihood value
gridSearch %>% slice(which.max(ll))

## OR...Find the minimum deviance value
# gridSearch %>% slice(which.min(deviance))


# Examine the search grid...Note that getting NAs does not necessarily mean your function failed
# head(gridSearch)



#####################################
# Use lm() function
#####################################

## Create data
x = c(32, 33, 32, 33, 26)
y = c(12, 8, 16.26, 13.65, 8.5)


## Fit linear model
lm.a = lm(y ~ x)
summary(lm.a)


## Compute log-likelihood
logLik(lm.a)


## Compute deviance
-2 * logLik(lm.a)[1]



#####################################
# Compute derivative
#####################################

g = expression(x^2 + 2*x - 1)
D(g, "x")


g = expression(log(mu) - log(1 - mu))
D(g, "mu")



#####################################
# Compute partial derivatives of deviance
#####################################

g = 5 * log(2 * pi * sigma2) + 1/sigma2 * ( 
    (12    - b0 - b1 * 32)^2 + 
    (8     - b0 - b1 * 33)^2 + 
    (16.26 - b0 - b1 * 32)^2 + 
    (13.65 - b0 - b1 * 33)^2 + 
    (8.5   - b0 - b1 * 26)^2 
) 
D(g, "b0")
D(g, "b1")
D(g, "sigma2")



