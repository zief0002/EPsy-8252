##################################################
### Load libraries
##################################################

library(broom)
library(educate) 
library(lmtest)
library(patchwork)
library(tidyverse)


##################################################
### Import data
##################################################

mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mn-schools.csv")
head(mn)



##################################################
### Scatterplot - graduation rate vs. median SAT
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Fit linear effect model and look at residuals
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
  theme_light() +
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


# Fit quadratic model
lm.2 = lm(grad ~ 1 + sat + sat_quadratic, data = mn)


# Likelihood ratio test to compare linear and quadratic models
lrtest(lm.1, lm.2)


# Model-level output
glance(lm.2)



##################################################
### Examine residuals from quadratic effects model
##################################################

# Obtain residuals
out_2 = augment(lm.2)


# Examine residuals for normality (linear)
p1 = ggplot(data = out, aes(x = .std.resid)) +
  stat_density_confidence() +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Standardized residuals") +
  ylab("Probability density")


# Examine residuals for linearity
p2 = ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals")


# Display plots side-by-side
p1 | p2



##################################################
### Plot the fitted curve
##################################################

# Coefficient-level output
tidy(lm.2)


# Scatterplot
ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(fun = function(x) {-366.34 + 62.72*x - 2.15 * x^2}) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Find vertex
##################################################

# x-coordinate
- 62.722 / (2 * -2.15)

# y-coordinate
-366.34 + 62.72 * 14.58 - 2.15 * 14.58^2



##################################################
### Alternative syntax to fit quadratic model
##################################################

# Fit model using I() function
lm.2 = lm(grad ~ 1 + sat + I(sat ^ 2), data = mn)

glance(lm.2) # Model-level output
tidy(lm.2)   # Coefficient-level output



##################################################
### Adding covariates: Main effects model
##################################################

# Fit model
lm.3 = lm(grad ~ 1 + sat + I(sat^2) + public, data = mn)


# Compare Model 2 and Model 3
lrtest(lm.2, lm.3)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)



##################################################
### Plot the fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  # Public schools
  geom_function(
    fun = function(x) {-384.16 + 67.04*x - 2.37 * x^2},
    color = "#2ec4b6",
    linetype = "dashed"
  ) +
  # Private schools
  geom_function( 
    fun = function(x) {-393.29 + 67.04*x - 2.37 * x^2},
    color = "#ff9f1c",
    linetype = "solid"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Interaction models
##################################################

# Interaction between sector and linear effect of SAT
lm.4 = lm(grad ~ 1 + sat + I(sat^2) + public + public:sat, data = mn)


# Interaction between sector and linear and quadratic effects of SAT
lm.5 = lm(grad ~ 1 + sat + I(sat^2) + public + public:sat + public:I(sat^2), data = mn)


# Likelihood ratio tests
lrtest(lm.3, lm.4, lm.5)



##################################################
### Summarizing Model 4
##################################################

# Model-level output
glance(lm.4)


# Coefficient-level output
tidy(lm.4)


# Plot of the fitted model
ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  # Public schools
  geom_function(
    fun = function(x) {-378.73 + 67.54 * x - 2.54 * x^2},
    color = "#2ec4b6",
    linetype = "dashed"
  ) +
  # Private schools
  geom_function(
    fun = function(x) {-413.80 + 71.65 * x - 2.54 * x^2},
    color = "#ff9f1c",
    linetype = "solid"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Regression table
##################################################

# Load library
library(stargazer)


# Create the table
# Output is raw html code
# If you include this in an RMD file include the chunk option: results='asis'
stargazer(
  lm.1, lm.2, lm.3, lm.4, lm.5,
  type = "html",
  title = "Five candidate models predicting variation in six-year graduation rates.",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  colnames = FALSE,
  model.numbers = FALSE,
  dep.var.caption = "",
  dep.var.labels = "",
  covariate.labels = c("Median SAT score (Linear)", "Median SAT score (Quadratic)", "Public", "Median SAT score (Linear) x Public", "Median SAT score (Quadratic) x Public"),
  keep.stat = c("rsq", "ser"),
  notes.align = "l",
  star.cutoffs = NA, #No p-value stars
  digits = 2
)

