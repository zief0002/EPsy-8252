##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate) #Need version 0.1.0.1
library(tidyverse)



##################################################
### Import data
##################################################

mn = read_csv(file = "~/Documents/github/epsy-8252/data/mn-schools.csv")
head(mn)



##################################################
### Scatterplot - graduation rate vs. median SAT
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Fit linear effect model
##################################################

# Fit linear model
lm.1 = lm(grad ~ 1 + sat, data = mn)

# Obtain residuals
out = augment(lm.1)

# Examine residuals for linearity
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Fit quadratic effect model
##################################################

# Create quadratic term in the data
mn = mn %>%
  mutate(
    sat_quadratic = sat * sat
  )

# View data
head(mn)

# Fit model
lm.2 = lm(grad ~ 1 + sat + sat_quadratic, data = mn)

# Model-level output
glance(lm.2)

# Coefficient-level output
tidy(lm.2)



##################################################
### Examine residuals from quadratic effects model
##################################################

# Obtain residuals
out_2 = augment(lm.2)

# Examine residuals for linearity
ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")

# Examine residuals for normality (linear)
ggplot(data = out, aes(x = .std.resid)) +
  stat_density_confidence() +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")



##################################################
### Plot the fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  stat_function(fun = function(x) {-366.34 + 62.72*x - 2.15 * x^2} ) +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Find vetex
##################################################

# x-coordinate
- 62.722 / (2 * -2.15)

# y-coordinate
-366.34 + 62.72 * 14.58 - 2.15 * 14.58^2



##################################################
### Fit quadratic effects model: Take 2
##################################################

# Fit model using I() function
lm.2 = lm(grad ~ 1 + sat + I(sat ^ 2), data = mn)

glance(lm.2) # Model-level output
tidy(lm.2)   # Coefficient-level output



##################################################
### Adding covariates
##################################################

# Fit model
lm.3 = lm(grad ~ 1 + sat + I(sat^2) + public, data = mn)

glance(lm.3) # Model-level output
tidy(lm.3)   # Coefficient-level output



##################################################
### Plot the fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  stat_function(
    fun = function(x) {-384.16 + 67.04*x - 2.37 * x^2}, 
    color = "blue", 
    linetype = "dashed"
  ) +
  stat_function(
    fun = function(x) {-393.29 + 67.04*x - 2.37 * x^2}, 
    color = "red", 
    linetype = "solid" 
  ) +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Interaction models
##################################################

# Interaction between sector and linear effect of SAT
lm.4 = lm(grad ~ 1 + sat + I(sat^2) + public + public:sat, data = mn)

# Interaction between sector and linear and quadratic effects of SAT
lm.5 = lm(grad ~ 1 + sat + I(sat^2) + public + public:sat + public:I(sat^2), data = mn)



##################################################
### Model-level output
##################################################

# Main-effects model
glance(lm.3)

# Interaction model (linear term)
glance(lm.4)

# Interaction model (linear and quadratic terms)
glance(lm.5)



##################################################
### Test nested models
##################################################

# Compare Model 1 to Model 2
anova(lm.3, lm.4)

# Compare Model 1 to Model 3
anova(lm.3, lm.5)

# Compare all nested models in a hierarchy
anova(lm.1, lm.2, lm.3, lm.4, lm.5)

