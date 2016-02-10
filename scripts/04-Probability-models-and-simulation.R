###################################################
### Sample a single student from MN
###################################################

sample(
  x = c("proficient", "not proficient"),
  size = 1,
  replace = TRUE,
  prob = c(0.602, 0.398)
  )


# Use {0,1} rather than {failure, success}
sample(
  x = c(0, 1),
  size = 1,
  replace = TRUE,
  prob = c(0.398, 0.602)
  )



###################################################
### Sample 10 students from MN
###################################################

sample(
  x = c(0, 1),
  size = 10,
  replace = TRUE,
  prob = c(0.398, 0.602)
  )



###################################################
### Binomial distributions
###################################################

# Sample 100 students from MN
X = sample(
  x = c(0, 1),
  size = 100,
  replace = TRUE,
  prob = c(0.398, 0.602)
  )

# Sum the results
sum(X)

# Sample 100 students from MN (one time) with probability of success = 0.602
rbinom(1, size = 100, prob = 0.602)

# Carry out a Bernoulli trial
#Sample a single student from MN (ten times) with probability of success = 0.602
rbinom(10, size = 1, prob = 0.602)


# Sample 100 students from MN (25 times) with probability of success = 0.602
rbinom(25, size = 100, prob = 0.602)



###################################################
### Sample 100 students from MN (1,000,000 times) with probability of success = 0.602
###################################################

y = rbinom(1000000, size = 100, prob = 0.602)

# Examine mean and sd (compare to theoretical results)
mean(y)
sd(y)

# Plot the frequency distribution (1) obtain counts and (2) plot
counts = table(y)
barplot(counts)

# Plot the (relative) probability distribution (1) obtain proportions from counts
# and (2) plot
prop = counts / 1000000
barplot(prop)

# Or...do it all in one step
barplot(table(y) / 1000000)



###################################################
### Calculate p(x = 59)
###################################################

# Theoretically
choose(100, 59) * 0.602^59 * 0.398^41

# Empirically from the simulation
# Look at TRUE
table(y==59) / 1000000



###################################################
### Poisson distribution
###################################################

# Compute mean number of incidents
(0*4 + 1*3 + 2*5 + 3*2 + 4*4 + 5*1 + 6*1) / 20

# Simulate 1000000 values from the Poisson distribution with lambda = 2.3
y = rpois(1000000, lambda = 2.3)

# Compute mean and sd (compare to theoretical values)
mean(y)
var(y)

# Compute the frequency of incidents
table(y)

# Compute the frequency of incidents >= 3
table(y >= 3)

# Compute the relative frequency (probability) of incidents >= 3
table(y >= 3) / 1000000



###################################################
### Poisson distribution: Theoretic calculation to compute p(X >= 3)
###################################################

# P(X = 0) 
p0 = (2.3^0 * exp(-2.3)) / factorial(0)

# P(X = 1) 
p1 = (2.3^1 * exp(-2.3)) / factorial(1)

# P(X = 1) 
p2 = (2.3^2 * exp(-2.3)) / factorial(2)

# Compute probability
1 - (p0 + p1 + p2)



###################################################
### Normal distribution
###################################################

# Simulate 1,000,000 observations from the standard normal distribution
y = rnorm(1000000, mean = 0, sd = 1)

# Compute mean and sd
mean(y)
var(y)

# Plot the probability distribution
hist(y, freq = FALSE)



###################################################
### Normal distribution: Densities
###################################################

# Density for x = 1 using the standard normal distribution
dnorm(1, mean = 0, sd = 1)

# Cumulative density for -2 using the standard normal distribution
pnorm(-2, mean = 0, sd = 1)



###################################################
### Normal distribution: Cumulative densities
###################################################

# Cumulative density for 0 using the standard normal distribution
pnorm(0, mean = 0, sd = 1)

#Compute quantile (x) given a cumulative density using the standard normal distribution
qnorm(0.5, mean = 0, sd = 1)



###################################################
### Plotting densities
###################################################

# Plot the density for the standard normal distribution between -3.5 and +3.5
plot(function(x) dnorm(x, mean = 0, sd = 1), -3.5, 3.5)

# Plot the cumulative density for the standard normal distribution between -3.5 and +3.5
plot(function(x) pnorm(x, mean = 0, sd = 1), -3.5, 3.5, ylab = "Cumulative Density")




###################################################
### Use ggplot to plot a histogram and draw a density curve
###################################################

# Randomly generate the data and store it in a dataframe
x2 = data.frame(myObs = rnorm(1000000, mean = 0, sd = 1))
head(x2)

library(ggplot2)

# Plot the histogram using densities on the y-axis;
# Use stat_function() to add the density, arguments for dnorm go in the args= list
ggplot(data = x2, aes(x = myObs)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, sd = 1), 
    geom="line", 
    color = "blue", 
    lwd = 2
    ) +
  theme_bw()
