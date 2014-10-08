## @knitr libraries, echo=F, message=F, warning=F
library(ggplot2)
library(lattice)
library(plyr)


## Create data
myData = data.frame(
	wage = c(12, 8, 16.26, 13.65, 8.5),
	educ = c(12, 12, 12, 16, 17),
	sex = c("M", "F", "M", "M", "M"),
	status = c("Married", "Married", "Single", "Married", "Single"),
	age = c(32, 33, 32, 33, 26),
	sector = c("manuf", "service", "service", "prof", "clerical")
	)

myData


## Probability density for epsilon = -1
dnorm(-1, mean = 0, sd = sqrt(1))


## Create the function
dev = function(b0) {
     5 * log(2 * pi * 15) + 1/15 * ( (12 - b0)^2 +
     (8 - b0)^2 + (16.26 - b0)^2 +
     (13.65 - b0)^2 + (8.5 - b0)^2 )
     }

## Try function
dev(0)	


## Generate values for b0
new = data.frame( 
	b0 = seq(from = 11, to = 12, by = 0.01)
	)

## Generate the deviance values and store the results
library(plyr)
new = mdply(new, dev)
head(new)

## Change the name of the second column
names(new)[2] = "deviance"


## Plot deviance vs. b0
ggplot(data = new, aes(x = b0, y = deviance)) +
	geom_line() +
    theme_bw() +
    xlab(expression(beta[0])) +
    ylab("Deviance")

## Arrange from smallest to largest deviance
head(arrange(new, deviance))


## Create the function
dev = function(b0, s) {
     5 * log(2 * pi * s^2) + 1/s^2 * ( (12 - b0)^2 +
     (8 - b0)^2 + (16.26 - b0)^2 +
     (13.65 - b0)^2 + (8.5 - b0)^2 )
     }


## Try function
dev(b0 = 0, s = 1)

## Generate independent search grids for b0 and b1
b0 = seq(from = 11, to = 12, by = 0.01) 

## Create combined search grid
new = expand.grid(b0 = b0, s = s)

## Generate the deviance values and store the results
new = mdply(new, dev)

## Change the name of the second column
names(new)[3] = "deviance"

## Arrange from smallest to largest deviance
head(arrange(new, deviance))

## Create the function
dev = function(b1) {
     5 * log(2 * pi * 15) + 1/15 * ( (12 + 4 - b1 * 32)^2 +
     (8 + 4 - b1 * 33)^2 + (16.26 + 4 - b1 * 32)^2 +
     (13.65 + 4 - b1 * 33)^2 + (8.5 + 4 - b1 * 26)^2 )
     }

## Try function
dev(0)

## Generate values for b1
new = data.frame(
	b1 = seq(from = 0, to = 1, by = 0.01)
	)

## Generate the deviance values and store the results
library(plyr)
new = mdply(new, dev)

## Change the name of the second column
names(new)[2] = "deviance"

## Create the function
dev = function(b0, b1) { 

## Try function
dev(b0 = 0, b1 = 0)

## Generate values for b1
new2 = expand.grid(
    b0 = seq(from = -4, to = -3, by = 0.01),
    b1 = seq(from = 0, to = 1, by = 0.01)
    )

## Generate the deviance values and store the results
new2 = mdply(new2, dev)

## Change the name of the third column
names(new)[2] = "deviance"

## Order the data frame by deviance
arrange(new2, dev)





## Fit linear model
lm.1 = lm(wage ~ age, data = myData)

## Compute log-likelihood
logLik(lm.1)

## Compute deviance
-2 * logLik(lm.1)[1]

lm.0 = lm(wage ~ 1, data = myData)  ## Fit intercept-only model
-2 * logLik(lm.0)[1]            ## Compute deviance


lm.1 = lm(wage ~ age, data = myData)  ## Fit slope model
-2 * logLik(lm.1)[1]              ## Compute deviance


AIC(lm.0)
AIC(lm.1)

BIC(lm.0)
BIC(lm.1)