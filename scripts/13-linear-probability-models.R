##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Read in data
##################################################

grad = read_csv(file = "~/Documents/github/epsy-8252/data/graduation.csv")
head(grad)



##################################################
### Explore outcome
##################################################

grad %>%
  group_by(degree) %>%
  summarize(
    Count = n(),
    Prop = n() / nrow(grad)
  )




##################################################
### Explore predictor (ACT)
##################################################

# Density plot
sm.density(grad$act, xlab = "ACT score")


# Summary measures
grad %>%
  summarize(
    M = mean(act),
    SD = sd(act)
  )



##################################################
### Relationship between act and degree
##################################################

ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated")


# Jittered scatterplot
ggplot(data = grad, aes(x = act, y = jitter(degree))) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated")


# Correlation
grad %>%
  select(degree, act) %>%
  correlate()



##################################################
### Fit linear probability model
##################################################

# Fit the model
lm.1 = lm(degree ~ 1 + act, data = grad)


# Model-level- output
glance(lm.1)


# Coefficient-level- output
tidy(lm.1)



##################################################
### Evaluate assumptions
##################################################

out = augment(lm.1)
head(out)


# Examine normality assumption
sm.density(out$.std.resid, xlab = "Standardized residuals")


# Examine linearity and homoskedasticity
ggplot(data = out, aes(x = .fitted, y = .std.resid)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



