#########################
# Load libraries
#########################

library(tidyverse)
library(broom)
library(sm)


#########################
# Compute p(65) in N(50, 10)
#########################

(1/(10*sqrt(2*pi))) * exp(-(225)/200)

dnorm(x = 65, mean = 50, sd = 10)


#########################
# Plot density for N(50, 10); 
# Add point at p(65)
#########################

data.frame(
  X = seq(from = 10, to = 90, by = 0.01)
  ) %>%
    rowwise() %>%
    mutate( Y = dnorm(x = X, mean = 50, sd = 10) ) %>% 
    ggplot(data = ., aes(x = X, y = Y)) +
      geom_line() +
      theme_bw() +
      geom_point(x = 65, y = 0.01295176, size = 3)


#########################
# Cumulative probability
#########################

pnorm(q = 65, mean = 50, sd = 10)


#########################
# p-value for z = 2.5
#########################

2 * pnorm(q = -2.5, mean = 0, sd = 1)


#########################
# Compute quantile
#########################

qnorm(p = 0.5, mean = 50, sd = 10)


#########################
# Compare probability densities
#########################

# Standard normal distribution (z)
pnorm(q = 2, mean = 0, sd = 1)

# t-distribution with 3 df
pt(q = 2, df = 3)

# t-distribution with 5 df
pt(q = 2, df = 5)

# t-distribution with 10 df
pt(q = 2, df = 10)

# t-distribution with 25 df
pt(q = 2, df = 25)


#########################
# Compare cumulative densities/p-values
#########################

2 * pnorm(q = -2.5, mean = 0, sd = 1)

2 * pt(q = -2.5, df = 14)


#########################
# Regression example
#########################

# Read in data
city = read_csv(file = "~/Dropbox/epsy-8252/data/riverside.csv")
head(city)


# Fit regression model
lm.1 = lm(income ~ 1 + education + seniority, data = city)


#########################
# Coefficient-level inference
#########################

tidy(lm.1)

# Compute education t, p
2252/335
2 * pt(q = -6.72, df = 29)

# Intercept t, p
6769/5373
2 * pt(q = -1.26, df = 29)

# Seniority t, p
739/210
2 * pt(q = -3.52, df = 29)


#########################
# Model-level inference
#########################

glance(lm.1)

# ANOVA decomposition
anova(lm.1)

# MS_model
(4147330492 + 722883649) / (1 + 1)

# MS_error
1695313285 / 29

# F
2435107070 / 58459079

# p-value
1 - pf(41.7, df1 = 2, df2 = 29)




