#####################################
# Load Libraries
#####################################

library(ggplot2)
library(dplyr)



#####################################
# Log-likelihood function
#####################################

LL = function(b0) {
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
  mutate(logLikelihood = LL(b0))


## Plot log-likelihood vs. b0
ggplot(data = gridSearch, aes(x = b0, y = logLikelihood)) + 
    geom_line() +
    theme_bw() +
    xlab(expression(beta[0])) +
    ylab("Log-Likelihood")

## Arrange from smallest to largest log-likelihood
tail(arrange(gridSearch, logLikelihood))




#####################################
# Grid Search (b0, b1, sigma2)
#####################################

## Create the function
dev = function(b0, b1, sigma2) {
	5 * log(2 * pi * sigma2) + 1/sigma2 * ( (12 - b0 - b1 * 32)^2 + 
	(8 - b0 - b1 * 33)^2 + (16.26 - b0 - b1 * 32)^2 + 
	(13.65 - b0 - b1 * 33)^2 + (8.5 - b0 - b1 * 26)^2 ) 
	}
	
## Try function
dev(b0 = 0, b1 = 0, sigma2 = 15)

## Generate candidate values for the unknown parameters
candidates = expand.grid(
    b0 = seq(from = -4, to = -3, by = 0.01),
    b1 = seq(from = 0, to = 1, by = 0.01),
    sigma2 = seq(from = 0, to = 20, by = 0.01)
    )

## Generate the deviance values and store the results
gridSearch = candidates %>% 
  rowwise() %>% 
  mutate(deviance = dev(b0, b1, sigma2))


## Order the data frame by deviance
head(arrange(gridSearch, dev))






#####################################
# Compute derivative
#####################################

g = expression(x^2 + 2*x - 1)
D(g, "x")

g = expression(log(mu) - log(1 - mu))
D(g, "mu")


