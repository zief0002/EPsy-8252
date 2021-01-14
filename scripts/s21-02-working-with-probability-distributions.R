##################################################
### Load libraries
##################################################

library(tidyverse)



##################################################
### Probability density
##################################################

# Compute p(65) in N(50, 10)
(1 / (10 * sqrt(2 * pi))) * exp(-(225) / 200)


# Shortcut
dnorm(x = 65, mean = 50, sd = 10)


# Compute p(65) in N(30, 20)
dnorm(x = 65, mean = 30, sd = 20)



##################################################
### Cumulative probability density
##################################################

# Find P(x<=65 | N(50,10) )
pnorm(q = 65, mean = 50, sd = 10)


# Compute p-value for z = 2.5
2 * pnorm(q = -2.5, mean = 0, sd = 1)



##################################################
### Compute quantile
##################################################

# Find the quantile that has a cumulative density of 0.5 in the N(50, 10) distribution
qnorm(p = 0.5, mean = 50, sd = 10)



##################################################
### Generating random observations
##################################################

# Generate 15 observations from N(50,10)
set.seed(100)
rnorm(n = 15, mean = 50, sd = 10)



##################################################
### Student's t-distribution
##################################################

# Compare probability densities at z/t=-2
dnorm(q = -2, mean = 0, sd = 1) #Standard normal distribution (z)
dt(q = -2, df = 3)              #t-distribution with 3 df
dt(q = -2, df = 5)              #t-distribution with 5 df
dt(q = -2, df = 10)             #t-distribution with 10 df
dt(q = -2, df = 25)             #t-distribution with 25 df


# Compare cumulative probability densities for z/t<=-2
pnorm(q = -2, mean = 0, sd = 1) #Standard normal distribution (z)
pt(q = -2, df = 3)              #t-distribution with 3 df
pt(q = -2, df = 5)              #t-distribution with 5 df
pt(q = -2, df = 10)             #t-distribution with 10 df
pt(q = -2, df = 25)             #t-distribution with 25 df



##################################################
### F-distribution
##################################################

# Compute the p-value based on F(5,25)=2
1 - pf(q = 2, df1 = 5, df2 = 25)



##################################################
### Creating a probability density function (PDF)
##################################################

# Create F-value and compute probability densities
fig_01 = data.frame(
  X = seq(from = 0, to = 10, by = 0.01)
) %>% 
  mutate(
    Y = df(x = X, df1 = 5, df2 = 25)
  )


# View data
head(fig_01)


# Create plot
ggplot(data = fig_01, aes(x = X, y = Y)) +
  geom_line() +
  xlab("F") +
  ylab("Probability density") +
  theme_light() 



##################################################
### Add shading under PDF
##################################################

# Filter data included in the shaded region
shade_01 = fig_01 %>%
  filter(X >= 2)


# View data
head(shade_01)


# Create plot
ggplot(data = fig_01, aes(x = X, y = Y)) +
  geom_line() +
  xlab("F") +
  ylab("Probability density") +
  theme_light() +
  geom_ribbon(data = shade_01, ymin = 0, aes(x = X, ymax = Y), color = "#bbbbbb", alpha = 0.4)

