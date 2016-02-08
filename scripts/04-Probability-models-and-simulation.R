###################################################
### A simple example of discrete predictive simulations
###################################################

# Simulate rolling a die 5 times, and you wished to count the number of 3's you observe.

rbinom(1, size = 5, prob = (1/6))    
### rbinom(number of experiments, number of observations per experiment, probability of success)

# simulate a class of 30 students rolling a dice 10 times, and you wished to count the number of 3's you observe for each student.
rbinom(30, size = 10, prob = (1/6)) 


myDist = rbinom(1000000, size = 10, prob = (1/6))

counts = table(myDist)
barplot(counts) 

mean(myDist) # very close to 1/6 * 10 = 1.66667
var(myDist) # very close to 1/6 * 5/6 * 10



myDist = rbinom(1000000, size = 10, prob = (3/6))
counts = table(myDist)
barplot(counts)


myDist = rbinom(1000000, size = 10, prob = (5/6))
counts = table(myDist)
barplot(counts)



myDist = rbinom(1000000, size = 10, prob = 0.7)
counts = table(myDist)
barplot(counts)




###################################################
### Poisson distribution
###################################################

# Compute mean
(0*4 + 1*3 + 2*5 + 3*2 + 4*4 + 5*1 + 6*1) / 20

y = rpois(1000000, lambda = 2.3)

mean(y)
var(y)

table(y)
table(y >= 3)
table(y >= 3) / 1000000

# P(X = 0) 
p0 = (2.3^0 * exp(-2.3)) / factorial(0)

# P(X = 1) 
p1 = (2.3^1 * exp(-2.3)) / factorial(1)

# P(X = 1) 
p2 = (2.3^2 * exp(-2.3)) / factorial(2)

# Compute probability
1 - (p0 + p1 + p2)






y = rnorm(1000000, mean = 0, sd = 1)

mean(y)
var(y)

hist(y, freq = FALSE)

plot(function(x) dnorm(x, mean = 0, sd = 1), -3.5, 3.5)


pnorm(q = -2, mean = 0, sd = 1)
pnorm(q = 0, mean = 0, sd = 1)

qnorm(0.5, mean =0, sd = 1)

dnorm(x = 1, mean = 0, sd = 1)
dnorm(1)

plot(function(x) pnorm(x, mean = 0, sd = 1), -3.5, 3.5, ylab = "Cumulative Density")


###################################################
### Another simple example of discrete predictive simulations
###################################################

## A single trial of the simulation
n.girls = rbinom(1, 400, 0.488)
print(n.girls)



## Many trials of the simulation
## Note: Gelman uses n.sims rather than trials
trials = 1000
n.girls = rep(NA, trials)

## Note: Gelman uses s for indexing rather than i
for (i in 1:trials){
  n.girls[i] = rbinom(1, 400, 0.488)
  }

library(sm)
sm.density(n.girls)

## equivalently

trials = 1000
n.girls = rbinom(trials, 400, 0.488)
sm.density(n.girls)





###################################################
### Accounting for twins
###################################################

birth.type = sample(
  c("fraternal twin", "identical twin", "single birth"),
  prob = c(1/25, 1/300, 1 - 1/25 - 1/300),
  size = 400, 
  replace = TRUE
  )

girls = rep(NA, 400)

for (i in 1:400){
  if (birth.type[i] == "single birth"){
    girls[i] = rbinom(1, 1, 0.488)
  }
  else if (birth.type[i] == "identical twin"){
    girls[i] = 2 * rbinom(1, 1, 0.495)
  }
  else if (birth.type[i] == "fraternal twin"){
    girls[i] = rbinom(1, 2, 0.495)
  }
}

n.girls = sum(girls)



###################################################
### Putting in a loop
###################################################

trials = 1000
n.girls = rep(NA, trials)

for (s in 1:trials){

  birth.type = sample(
    c("fraternal twin", "identical twin", "single birth"),
    prob = c(1/25, 1/300, 1 - 1/25 - 1/300),
    size = 400, 
    replace = TRUE
    )

  girls = rep(NA, 400)

  for (i in 1:400){
    if (birth.type[i] == "single birth"){
      girls[i] = rbinom(1, 1, 0.488)
    }
    else if (birth.type[i] == "identical twin"){
      girls[i] = 2 * rbinom(1, 1, 0.495)
    }
    else if (birth.type[i] == "fraternal twin"){
      girls[i] = rbinom(1, 2, 0.495)
    }
  }

  n.girls[s] = sum(girls)
}


###################################################
### Alternate Syntax
###################################################

girls = ifelse(birth.type == "single birth", rbinom(400, 1, 0.488),
          ifelse(birth.type == "identical twin", 2*rbinom(400, 1, 0.495),
          rbinom(400, 2, 0.495)
          )
        )



###################################################
### A simple example of continuos predictive simulations
###################################################

woman = rbinom(10, 1, 0.52)
height = ifelse(woman == 0, rnorm(10, 69.1, 2.9), rnorm(10, 64.5, 2.7))

avg.height = mean(height)
print(avg.height)


## Simulation for the average height

trials = 1000
avg.height = rep (NA, trials)

for(s in 1:trials){
  sex = rbinom(10, 1, 0.52)
  height = ifelse(sex == 0, rnorm(10, 69.1, 2.9), rnorm(10, 64.5, 2.7))
  avg.height[s] = mean (height)
}

sm.density(avg.height) 



## Simulation for the maximum height

trials = 1000
max.height = rep (NA, trials)

for(s in 1:trials){
  sex = rbinom(10, 1, 0.52)
  height <= ifelse(sex == 0, rnorm(10, 69.1, 2.9), rnorm(10, 64.5, 2.7))
  max.height[s] = max(height)
}

sm.density(max.height)



###################################################
### Simulation using custom-made functions
###################################################

Height.sim = function (n.adults){
  sex = rbinom (n.adults, 1, 0.52)
  height = ifelse (sex == 0, rnorm(10, 69.1, 2.9), rnorm(10, 64.5, 2.7))
  return (mean(height))
}

avg.height = replicate (1000, Height.sim(n.adults = 10))
sm.density(avg.height)
