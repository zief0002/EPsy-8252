##################################################
### Load libraries
##################################################

library(broom)
library(tidyverse)



##################################################
### Import data
##################################################

city = read_csv(file = "~/Documents/github/epsy-8252/data/riverview.csv")


# View data
head(city)


# Count number of rows (n)
nrow(city)



##################################################
### Fit regression model
##################################################

lm.1 = lm(income ~ 1 + education + seniority, data = city)



##################################################
### Examine regression output
##################################################

# Model-level output
glance(lm.1)


# Partition the variation
anova(lm.1)


# Coefficient-level output
tidy(lm.1)



##################################################
### Model-level inference
##################################################

# Partition SS
4147.3 + 722.9  #SS_model
1695.3          #SS_residual
4870.2 + 1695.3 #SS_Total


# Compute R^2
48070.2 / 6565.5


# Convert R^2 to F
0.742 / (1 - 0.742) * 29 / 2



##################################################
### Compute F from mean squares
##################################################

# Compute MS_model
(4147330492 + 722883649) / (1 + 1)


# Compute MS_error
1695313285 / 29


# Compute F
2435107070 / 58459079



##################################################
### Test model-level null hypothesis
##################################################

# Compute p-value for F(2,29) = 41.7
1 - pf(41.7, df1 = 2, df2 = 29)



##################################################
### Residual standard error / Root mean square error
##################################################

# Compute RSE/RMSE
sqrt(58.5)



##################################################
### Coefficient-level inference
##################################################

# Compute t-statistic for effect of education
2252 / 335


# Compute p-value for t(29) = 6.72
2 * pt(q = -6.72, df = 29)



##################################################
### Regression model and simulation
##################################################

# Generate Y-values using the same X values that are in the data
new_city = city %>%
  select(education, seniority) %>%
  mutate(
    y_hat = 6.77 + 2.25*education + 0.739*seniority,
    e_i = rnorm(n = 32, mean = 0, sd = 7.65),
    income = y_hat + e_i
  )


# View generated data
new_city







