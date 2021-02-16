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
### Relationship between graduation rates and median SAT scores
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Logarithms
##################################################

# log (base-2) of 32
log(32, base = 2)

# Shortcut
log2(32)



##################################################
### Create log2 transformed SAT scores
##################################################

# Create base-2 log-transformed median SAT scores
mn = mn %>%
  mutate(
    l2sat = log(sat, base = 2)
  )


# View data
head(mn)



##################################################
### Relationship between graduation rates and log2(median SAT scores)
##################################################

ggplot(data = mn, aes(x = l2sat, y = grad)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_light() +
  xlab("Base-2 log-transformed median SAT score") +
  ylab("Six-year graduation rate")



##################################################
### Fit regression model
##################################################

lm.log = lm(grad ~ 1 + l2sat, data = mn)



##################################################
### Evaluate residuals for avg. residual = 0 (linearity)
##################################################

# Obtain residuals for log-transformed SAT
out_log = augment(lm.log)

# Check linearity assumptions
ggplot(data = out_log, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Interpret regression output
##################################################

# Raw SAT
print(glance(lm.log), width = Inf) # Model-level output
tidy(lm.log)                       # Coefficient-level output



##################################################
### Understanding slope
##################################################

#Augsburg
#Raw SAT = 10.3
#L2SAT = log2(10.3) = 3.36

# Compute predicted grad rate
-306.7 + 106.4 * 3.36


# Compute predicted grad rate for 1-unit change in L2SAT
-306.7 + 106.4 * 4.36



##################################################
### Alternative method to fitting the model
##################################################

lm.log = lm(grad ~ 1 + log(sat, base = 2), data = mn)

glance(lm.log) # Model-level output
tidy(lm.log)   # Coefficient-level output



##################################################
### Plot fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(
    fun = function(x) {-306.7 + 106.4*log(x, base = 2)}
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")


# Plot using alternative form of fitted equation
ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  geom_function(
    fun = function(x) {log(2^-306.7 * x^106.4 , base = 2)}
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Base-10 log SAT
##################################################

mn = mn %>%
  mutate(
    l10sat = log(sat, base = 10)
  )


# Examine data
head(mn)


# Fit model
lm.log10 = lm(grad ~ 1 + l10sat, data = mn)
lm.log10 = lm(grad ~ 1 + log(sat, base = 10), data = mn)


print(glance(lm.log10), width = Inf) # Model-level output
tidy(lm.log10)                       # Coefficient-level output



##################################################
### Compare residuals
##################################################

augment(lm.log)
augment(lm.log10)



##################################################
### Natural logarithm (Base-e) of SAT
##################################################

# e^1
exp(1)


# Natural log for Augsburg's SAT score
log(10.3)


# Fit regression model
lm.ln = lm(grad ~ 1 + log(sat), data = mn)


glance(lm.ln) # Model-level output
tidy(lm.ln)   # Coefficient-level output


# Residuals
augment(lm.ln)



##################################################
### Interpreting slope from natural log-transformed model
##################################################

# Compute predicted values for three SAT scores that are 1% different
-306.7055 + 153.5593 * log(c(10, 10.1, 10.201))


# Difference between predicted value for 10.201 and 10.1
49.9338 - 48.4058


# Difference between predicted value for 10.201 and 10.1
49.9338 - 48.4058


# Compute exact effect
153.6 * log(1.01)



##################################################
### Add covariate to model
##################################################

# Fit model
lm.me = lm(grad ~ 1 + public + log(sat), data = mn)

glance(lm.me) # Model-level output
tidy(lm.me)   # Coefficient-level output



##################################################
### Plot the fitted curves
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {-286.1 + 146.0*log(x)},
    color = "blue",
    linetype = "solid"
  ) +
  geom_function(
    fun = function(x) {-294.6 + 146.0*log(x)},
    color = "red",
    linetype = "dashed"
  ) +
  theme_light() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Likelihood ratio test
##################################################

# Fit models (use natural log of SAT)
lm.1 = lm(grad ~ 1,                     data = mn)
lm.2 = lm(grad ~ 1 + log(sat),          data = mn)
lm.3 = lm(grad ~ 1 + public, data = mn)
lm.4 = lm(grad ~ 1 + log(sat) + public, data = mn)

# Effect of SAT?
lrtest(lm.1, lm.2)


# Effect of public?
lrtest(lm.1, lm.3)


# Effect of public after controlling for SAT?
lrtest(lm.2, lm.4)


# Effect of SAT after controlling for public?
lrtest(lm.3, lm.4)

