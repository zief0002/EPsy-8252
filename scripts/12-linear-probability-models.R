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

grad = read_csv(file = "~/Dropbox/epsy-8252/data/graduation.csv")
head(grad)



##################################################
### Explore outcome (degree): Compute counts and proportions
##################################################

grad %>% 
  group_by(degree) %>% 
  summarize(
    Count = n(), Prop = n() / nrow(grad)
    )



##################################################
### Explore predictor (ACT)
##################################################

# Density plot
sm.density(grad$act, xlab = "ACT score")

# Compute mean and sd
grad %>% 
  summarize(
    M = mean(act),
    SD = sd(act)
    )



##################################################
### Scatterplot of outcome vs. predictor
##################################################

# Scatterplot
ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated")


# Jittered scatterplot w/regression smoother
ggplot(data = grad, aes(x = act, y = jitter(degree))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Graduated")



##################################################
### Correlation b/w outcome and predictor
##################################################

grad %>% 
  select(degree, act) %>% 
  correlate()



##################################################
### Fit linear probability model
##################################################

lm.1 = lm(degree ~ 1 + act, data = grad)
summary(lm.1)



##################################################
### Examine model assumptions
##################################################

# Obtain residuals and fitted values
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


