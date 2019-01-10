##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



##################################################
### Import data
##################################################

movies = read_csv(file = "~/Documents/github/epsy-8252/data/movies.csv")
head(movies)



##################################################
### Scatterplot - budget vs. age
##################################################

ggplot(data = movies, aes(x = age, y = budget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("Movie Budget (in millions of dollars)")



##################################################
### Fit model and evaluate residuals
##################################################

# Fit model
lm.1 = lm(budget ~ 1 + age, data = movies)


# Obtain residuals and fitted values
out_1 = augment(lm.1)


# Density plot of the residuals
sm.density(out_1$.std.resid, model = "normal", xlab = "Standardized residuals")


# Residuals versus fitted values
ggplot(data = out_1, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



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
### Scatterplot - log-transformed budget vs. age
##################################################

ggplot(data = movies, aes(x = age, y = Lbudget)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Movie age") +
  ylab("ln(Movie Budget)")



##################################################
### Fit regression model and evaluate residuals
##################################################

# Fit model
lm.2 = lm(Lbudget ~ 1 + age, data = movies)


# Obtain residuals and fitted values
out_2 = augment(lm.2)


# Density plot of the residuals
sm.density(out_2$.std.resid, model = "normal", xlab = "Standardized residuals")


# Residuals versus fitted values
ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Interpret output
##################################################

# Model-level output
glance(lm.2)

# Coefficient-level output
tidy(lm.2)



##################################################
### Back-transform coefficients
##################################################

exp(coef(lm.2))



##################################################
### Obtain back-transformed effect
##################################################

1 - exp(-0.04)



##################################################
### Plot the fitted model
##################################################

# Set up data
plot_data = crossing(
  age = seq(from = 13, to = 80, by = 1)
  ) %>%
  mutate(
    # Predict
    yhat = predict(lm.2, newdata = .)
  )


# Examine data
head(plot_data)


# Back-transform the log-budgets
plot_data = plot_data %>%
  mutate(
    budget = exp(yhat)
  )


# Examine data
head(plot_data)


# Plot
ggplot(data = plot_data, aes(x = age, y = budget)) +
  geom_line() +
  theme_bw() +
  xlab("Age") +
  ylab("Predicted budget (in millions of U.S. dollars)")



##################################################
### Relationship between MPAA rating and budget
##################################################

# Plot the observed data
ggplot(data = movies, aes(x = mpaa, y = Lbudget)) +
  geom_jitter(alpha = 0.2) +
  stat_summary(fun.y = 'mean', geom = "point", size = 4, color = "darkred") +
  theme_bw() +
  xlab("MPAA rating") +
  ylab("ln(Movie Budget)")


# Compute summary statistics
movies %>%
  group_by(mpaa) %>%
  summarize(
    M = mean(Lbudget),
    SD = sd(Lbudget)
  )



##################################################
### Fit regression model
##################################################

# Create dummy variables
movies = movies %>%
  mutate(
    pg   = if_else(mpaa == "PG", 1, 0),
    pg13 = if_else(mpaa == "PG-13", 1, 0),
    r    = if_else(mpaa == "R", 1, 0)
  )


# Fit the model (pg is reference group)
lm.3 = lm(Lbudget ~ 1 + pg13 + r, data = movies)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)



##################################################
### Main-effects model
##################################################

# Fit model (PG is reference group)
lm.4 = lm(Lbudget ~ 1 + age + pg13 + r, data = movies)


# Model-level output
glance(lm.4)


# Coefficient-level output
tidy(lm.4)



##################################################
### Nested F-test (Delta-F)
##################################################

# Fit models
lm.2 = lm(Lbudget ~ 1 + age,            data = movies)
lm.4 = lm(Lbudget ~ 1 + age + pg13 + r, data = movies)


# Nested F-test
anova(lm.2, lm.4)



##################################################
### Model interpretation
##################################################

exp(coef(lm.4))


# Re-fit the model
lm.5 = lm(Lbudget ~ 1 + age + mpaa, data = movies)


# Model-level output
glance(lm.5)


# Coefficient-level output
tidy(lm.5)


# Set up data for plotting
plot_data = crossing(
  age = seq(from = 13, to = 80, by = 1),
  mpaa = c("PG", "PG-13", "R")
) %>%
  mutate(
    yhat = predict(lm.5, newdata = .),
    budget = exp(yhat)
  )


# Examine data
head(plot_data)


# Plot
ggplot(data = plot_data, aes(x = age, y = budget, color = mpaa, linetype = mpaa)) +
  geom_line() +
  theme_bw() +
  xlab("Age") +
  ylab("Predicted budget (in millions of U.S. dollars)") +
  ggsci::scale_color_d3(name = "MPAA rating") +
  scale_linetype_manual(name = "MPAA rating", values = 1:3)



##################################################
### Interaction model
##################################################

# Fit the models
lm.5 = lm(Lbudget ~ 1 + age + mpaa,            data = movies)
lm.6 = lm(Lbudget ~ 1 + age + mpaa + age:mpaa, data = movies)


# Nested F-test to test interaction effects
anova(lm.5, lm.6)



