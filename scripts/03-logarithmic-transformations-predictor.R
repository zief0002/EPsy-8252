##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate) #Need version 0.1.0.1
library(patchwork)
library(tidyverse)



##################################################
### Import data
##################################################

mn = read_csv(file = "~/Documents/github/epsy-8252/data/mn-schools.csv")
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

log(32, base = 2)

# Shortcut
log2(32)



##################################################
### Create log2 transformed SAT scores
##################################################

mn = mn %>% 
  mutate(
    L2sat = log(sat, base = 2)
  )

# View data
head(mn)



##################################################
### Relationship between graduation rates and log2(median SAT scores)
##################################################

ggplot(data = mn, aes(x = L2sat, y = grad)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Log-transformed SAT score") +
  ylab("Six-year graduation rate")



##################################################
### Use log() function
##################################################

log(32, base = 2)


# Use log2() function
log2(32)



##################################################
### Log-transform the median SAT scores
##################################################

mn = mn %>%
  mutate(
    L2sat = log(sat, base = 2)
  )


# View data
head(mn)



##################################################
### Scatterplot - graduation rates vs. log-transformed SAT scores
##################################################

ggplot(data = mn, aes(x = L2sat, y = grad)) +
  geom_point(size = 5) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Log-transformed SAT score") +
  ylab("Six-year graduation rate")



##################################################
### Fit regression model
##################################################

lm.1 = lm(grad ~ 1 + L2sat, data = mn)



##################################################
### EValuate residuals
##################################################

# Obtain residuals for untransformed SAT
lm_raw = lm(grad ~ 1 + sat, data = mn)
out_raw = augment(lm_raw)

# Obtain residuals for log-transformed SAT
out_log = augment(lm.1)

# Check linearity assumptions
p1 = ggplot(data = out_raw, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ggtitle("Untransformed Predictor")

p2 = ggplot(data = out_log, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ggtitle("Log-Transformed Predictor")

# Plot (requires patchwork package)
p1 + p2



##################################################
### Interpret regression output
##################################################


glance(lm.1) # Model-level output
tidy(lm.1)   # Coefficient-level output



##################################################
### Alternative method to fitting the model
##################################################

lm.1 = lm(grad ~ 1 + log(sat, base = 2), data = mn)

glance(lm.1) # Model-level output
tidy(lm.1)   # Coefficient-level output



##################################################
### Plot fitted curve
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0.3) +
  stat_function(fun = function(x) {log( 2^-306.7 * x^106.4 , base = 2)} ) +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



##################################################
### Base-10 log SAT
##################################################

# Create base-10 log SAT
mn = mn %>%
  mutate(
    L10sat = log(mn$sat, base = 10)
  )

# Examine data
head(mn)

# Fit model
lm.2 = lm(grad ~ 1 + log(sat, base = 10), data = mn)

glance(lm.2) # Model-level output
tidy(lm.2)   # Coefficient-level output



##################################################
### Compare residual fit
##################################################

p1 = ggplot(data = out_log, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ggtitle("Log(SAT): Base-2")

out2 = augment(lm.2)
p2 = ggplot(data = out2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ggtitle("Log(SAT): Base-10")

p1 + p2



##################################################
### Natural logarithm of SAT
##################################################

# e^1
exp(1)


# Natural log for Augsburg's SAT score
log(10.3)


# Fit regression model
lm.3 = lm(grad ~ 1 + log(sat), data = mn)

glance(lm.3) # Model-level output
tidy(lm.3)   # Coefficient-level output



##################################################
### Interpreting slope from natural log-transformed model
##################################################

# Three SAT scores that are 1% different
data.frame(
  sat = c(10, 10.1, 10.201)
  ) %>%
  mutate(
    grad = -306.7055 + 153.5593 * log(sat)
  )


# Differences in the fitted values
48.40582 - 46.87786
49.93379 - 48.40582



##################################################
### Add covariate to model
##################################################

# Fit model
lm.4 = lm(grad ~ 1 + public + log(sat), data = mn)

glance(lm.4) # Model-level output
tidy(lm.4)   # Coefficient-level output



##################################################
### Plot the fitted curves
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point(alpha = 0) +
  stat_function(fun = function(x) {log(2^-286.1 * x^146.0)}, color = "blue") +
  stat_function(fun = function(x) {log(2^-294.6 * x^146.0)}, color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Estimated median SAT score (in hundreds)") +
  ylab("Six-year graduation rate")



