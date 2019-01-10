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

mn = read_csv(file = "~/Documents/github/epsy-8252/data/mn-schools.csv")
head(mn)



##################################################
### Scatterplot - graduation rate vs. median SAT
##################################################

ggplot(data = mn, aes(x = sat, y = grad)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Median SAT score (in hundreds)") +
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
  geom_point() +
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

# Obtain residuals
out = augment(lm.1)


# Check normality assumption
sm.density(out$.std.resid)


# Check linearity and homogeneity of variance assumptions
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw()



##################################################
### Interpret regression output
##################################################

# Model-level output
glance(lm.1)


# Coefficient-level output
tidy(lm.1)



##################################################
### Compute fitted values for Augsburg
##################################################

-306.7 + 106.4 * 3.36


# Increase Lsat by 1
-306.7 + 106.4 * 4.36



##################################################
### Alternative method to fitting the model
##################################################

lm.1 = lm(grad ~ 1 + log(sat, base = 2), data = mn)


# Model-level output
glance(lm.1)


# Coefficient-level output
tidy(lm.1)



##################################################
### Plotting the fitted model
##################################################

# Set up data
plot_data = crossing(
  sat = seq(from = 8.9, to = 14.0, by = 0.1)
  ) %>%
  mutate(
    # Predict
    yhat = predict(lm.1, newdata = .)
  )


# Examine data
head(plot_data)


# Plot
ggplot(data = plot_data, aes(x = sat, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Median SAT score (in hundreds)") +
  ylab("Predicted graduation rate")



##################################################
### Use base-10 logarithm
##################################################

# Create log-transformed SAT; base-10
mn = mn %>%
  mutate(
    L10sat = log(mn$sat, base = 10)
  )


# Examine data
head(mn)


# Fit regression model
lm.2 = lm(grad ~ 1 + log(sat, base = 10), data = mn)


# Model-level output
glance(lm.2)


# Coefficient-level output
tidy(lm.2)



##################################################
### Use natural logarithm
##################################################

# e
exp(1)


# Natural log for Augsburg's SAT score
log(10.3)


# Fit regression model
lm.3 = lm(grad ~ 1 + log(sat), data = mn)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)



##################################################
### Interpreting slope from natural log-transformed model
##################################################

# Three SAT scores that are 1% different
data.frame(
  sat = c(10, 10.1, 10.201)
  ) %>%
  mutate(
    grad = predict(lm.3, newdata = .)
  )


# Differences in the fitted values
48.4058 - 46.8778
49.9338 - 48.4058



##################################################
### Add covariate to model
##################################################

# Fit model
lm.4 = lm(grad ~ 1 + public + log(sat), data = mn)


# Model-level output
glance(lm.4)


# Coefficient-level output
tidy(lm.4)


# Set up data for plotting
plot_data = crossing(
  sat = seq(from = 8.9, to = 14.0, by = .1),
  public = c(0, 1)
) %>%
  mutate(
    yhat = predict(lm.4, newdata = .),
    public = factor(public, levels = c(0, 1), labels = c("Private", "Public"))
  )


#Examine data
head(plot_data)


# Plot
ggplot(data = plot_data, aes(x = sat, y = yhat, color = public, linetype = public)) +
  geom_line() +
  theme_bw() +
  xlab("Median SAT score (in hundreds)") +
  ylab("Predicted graduation rate") +
  ggsci::scale_color_d3(name = "Sector") +
  scale_linetype_manual(name = "Sector", values = c("solid", "dashed"))




