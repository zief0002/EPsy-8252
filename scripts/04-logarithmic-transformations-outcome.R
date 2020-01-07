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

movies = read_csv(file = "~/Documents/github/epsy-8252/data/movies.csv")
head(movies)



##################################################
### Exploration
##################################################

# Marginal distribution of budget (outcome)
p1 = ggplot(data = movies, aes(x = budget)) +
  stat_density() +
  theme_bw() +
  xlab("Movie budget (in millions of dollars)") +
  ylab("Probability density")

# Marginal distribution of running time (predictor)
p2 = ggplot(data = movies, aes(x = length)) +
  stat_density() +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Probability density")

# Scatterplot
p3 = ggplot(data = movies, aes(x = length, y = budget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Movie budget (in millions of dollars)") +
  annotate(geom = "text", x = 90, y = 679, label = "The Time Machine (1960)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 114, y = 542, label = "Sabrina (1960)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 108, y = 443, label = "The Nutty Professor (1963)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 181, y = 23, label = "1776 (1972)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 187, y = 92, label = "The Green Mile (1999)", size = 3, hjust = 1)

# Place figures side-by-side (requires patchwork package)
p1 + p2 + p3



##################################################
### Fit model and evaluate residuals
##################################################

# Fit model
lm.1 = lm(budget ~ 1 + length, data = movies)

# Obtain residuals and fitted values
out_1 = augment(lm.1)

# Density plot of the residuals
p1 = ggplot(data = out_1, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")

# Residuals versus fitted values
p2 = ggplot(data = out_1, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")

# Plot the figures side-by-side
p1 + p2



##################################################
### Log-transform budget
##################################################

# Create log-transformed budget
movies = movies %>% 
  mutate(
    Lbudget = log(budget)
  )

# Examine data
head(movies)



##################################################
### Scatterplot - log-transformed budget vs. running time
##################################################

ggplot(data = movies, aes(x = length, y = Lbudget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Movie budget (in millions of dollars)") +
  annotate(geom = "text", x = 90, y = 6.53, label = "The Time Machine (1960)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 114, y = 6.31, label = "Sabrina (1960)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 108, y = 6.10, label = "The Nutty Professor (1963)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 181, y = 3.17, label = "1776 (1972)", size = 3, hjust = 0) +
  annotate(geom = "text", x = 187, y = 4.51, label = "The Green Mile (1999)", size = 3, hjust = 1)



##################################################
### Fit regression model and evaluate residuals
##################################################

# Fit model
lm.2 = lm(Lbudget ~ 1 + length, data = movies)

# Obtain residuals and fitted values
out_2 = augment(lm.2)

# Density plot of the residuals
p1 = ggplot(data = out_2, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")

# Residuals versus fitted values
p2 = ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")

# Plot the figures side-by-side
p1 + p2



##################################################
### Interpret output
##################################################

glance(lm.2) # Model-level output
tidy(lm.2)   # Coefficient-level output



##################################################
### Back-transform coefficients
##################################################

exp(coef(lm.2))



##################################################
### Plot the fitted curve
##################################################

ggplot(data = movies, aes(x = length, y = budget)) +
  geom_point(alpha = 0) +
  stat_function(fun = function(x) {exp(-0.59) * exp(0.034*x)} ) +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Predicted budget (in millions of dollars)") 



##################################################
### Model remaining non-linearity
##################################################

# Fit log-log model
lm_log = lm(Lbudget ~ 1 + log(length), data = movies)

# Fit polynomial model
lm_poly = lm(Lbudget ~ 1 + length + I(length^2), data = movies)

# Obtain residuals
out_log = augment(lm_log)
out_poly = augment(lm_poly)

# Log-log residuals
p1 = ggplot(data = out_log, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  ggtitle("Log-Log Model")

# Polynomial model residuals
p2 = ggplot(data = out_poly, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  ggtitle("Polynomial Model")

# Plot side-by-side
p1 + p2



##################################################
### Relationship between log-budget and running time for both models
##################################################

# Log-log model
p1 = ggplot(data = movies, aes(x = length, y = Lbudget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  stat_function(fun = function(x) {log(exp(-15.2) * x^(3.92))}, 
                color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Movie budget (in millions of dollars)") +
  ggtitle("Log-Log Model")

# polynomial model
p2 = ggplot(data = movies, aes(x = length, y = Lbudget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  stat_function(fun = function(x) {-6.28 + 0.135*x - 0.000434*x^2}, 
                color = "red", linetype = "dashed") +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Movie budget (in millions of dollars)")

# Plot side-by-side
p1 + p2



##################################################
### Polynomial model output
##################################################

glance(lm_poly) # Model-level output
tidy(lm_poly)   # Coefficient-level output



##################################################
### Plot back-transformed fitted curve
##################################################

ggplot(data = movies, aes(x = length, y = budget)) +
  geom_point(alpha = 0) +
  stat_function(fun = function(x) {exp(-6.28) * exp(0.135*x) * exp(-0.0004*x^2)} ) +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Predicted budget (in millions of dollars)") 



##################################################
### Relationship between log-budget and genre
##################################################

# Create dummy variable
movies = movies %>%
  mutate(
    action = if_else(genre == "Action", 1, 0)
  )

# Plot the observed data
ggplot(data = movies, aes(x = action, y = Lbudget)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun.y = 'mean', geom = "point", size = 4, color = "darkred") +
  theme_bw() +
  xlab("Genre") +
  ylab("ln(Movie Budget)")

# Compute summary statistics
movies %>%
  group_by(action) %>%
  summarize(
    M = mean(Lbudget),
    SD = sd(Lbudget)
  )

# Fit the model (non-action is reference group)
lm.3 = lm(Lbudget ~ 1 + action, data = movies)

glance(lm.3) # Model-level output
tidy(lm.3)   # Coefficient-level output



##################################################
### Model interpretation
##################################################

# Untrustworthy interpretation of percent change
coef(lm.3)

# Actual percent change
abs(1 - exp(1.002587))



##################################################
### Include effects of running time and genre
##################################################

# Fit the model (action is reference group)
lm.4 = lm(Lbudget ~ 1 + length + I(length^2) + action, data = movies)

glance(lm.4) # Model-level output
tidy(lm.4)   # Coefficient-level output



##################################################
### Plot the fitted curves
##################################################

ggplot(data = movies, aes(x = length, y = budget)) +
  geom_point(alpha = 0) +
  stat_function(fun = function(x) {exp(-5.56) * exp(0.12*x) * exp(-0.0004*x^2)}, 
                color = "black", linetype = "dashed") +
  stat_function(fun = function(x) {exp(-4.75) * exp(0.12*x) * exp(-0.0004*x^2)}, 
                color = "red", linetype = "solid") +
  theme_bw() +
  xlab("Running time (in minutes)") +
  ylab("Predicted budget (in millions of dollars)") 


