###################################################
### Create data
###################################################

myData = data.frame(
	wage = c(12, 8, 16.26, 13.65, 8.5),
	age = c(32, 33, 32, 33, 26)
	)

myData



###################################################
### Load libraries
###################################################

library(ggplot2)
library(dplyr)



###################################################
### Function to compute SSE given b0 and b1
###################################################

## Create the function
sse = function(b0, b1) {
     y = myData$wage
     yhat = b0 + b1 * myData$age
     e = y - yhat
     sum(e ^ 2)
     }

## Try function
sse(b0 = 1, b1 = 3)	



###################################################
### Search over many values of b0 and b1 to find the minimum SSE
###################################################

## Generate values for b0 and b1
search.space = expand.grid(
    b0 = seq(from = -10, to = 10, by = 1),
    b1 = seq(from = -10, to = 10, by = 1)
    )

## Generate the SSE values and store the results
new = search.space %>% 
  rowwise() %>% 
  mutate(SSE = sse(b0, b1))

head(new)

# Time to compute n = 21*21 = 441 values of SSE
# system.time(search.space %>% rowwise() %>% mutate(SSE = sse(b0, b1)))
#   user  system elapsed 
#  0.031   0.001   0.033

## Arrange from smallest to largest SSE
head(arrange(new, sse))



###################################################
### Search over many values of b0 and b1 to find the minimum SSE
###################################################

search.space = expand.grid(
    b0 = seq(from = -10, to = 10, by = 0.1),
    b1 = seq(from = -10, to = 10, by = 0.1)
    )

## Generate the SSE values and store the results
new = search.space %>% 
  rowwise() %>% 
  mutate(SSE = sse(b0, b1))

# Time to compute n = 201*201 = 40,401 values of SSE
# system.time(search.space %>% rowwise() %>% mutate(SSE = sse(b0, b1)))
#   user  system elapsed 
#  1.346   0.017   1.357 

head(arrange(new, SSE))



###################################################
### Search over many values of b0 and b1 to find the minimum SSE
###################################################

search.space = expand.grid(
    b0 = seq(from = -10, to = 10, by = 0.01),
    b1 = seq(from = -10, to = 10, by = 0.01)
    )

# Time to compute n = 2001*2001 = 4,004,001 values of SSE
# system.time(search.space %>% rowwise() %>% mutate(SSE = sse(b0, b1)))
#   user  system elapsed 
#126.057   1.035 127.105 





