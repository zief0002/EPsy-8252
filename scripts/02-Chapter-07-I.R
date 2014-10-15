###################################################
### A simple example of discrete predictive simulations
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
