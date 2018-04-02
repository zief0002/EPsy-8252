##################################################
### Load libraries
##################################################

library(dplyr)
library(ggplot2)
library(lme4) #for fitting mixed-effects models
library(readr)
library(tidyr)



##################################################
### Read in data
##################################################

seasonal = read_csv(file = "~/Dropbox/epsy-8252/data/seasonal-depression.csv")
head(seasonal, 12)



##################################################
### Format data: Long to wide format
##################################################

seasonal_wide = seasonal %>% 
  spread(season, beck)



##################################################
### Format data: Wide to long format
##################################################

seasonal_long = seasonal_wide %>%
  gather(key = season, value = "beck", s1:s4) %>%
  arrange(subject, season)

head(seasonal_long, 12)



##################################################
### Unconditional means model
##################################################

lmer.0 = lmer(beck ~ 1 + (1|subject), data = seasonal, REML = FALSE)
summary(lmer.0)



##################################################
### Unconditional growth model
##################################################

lmer.1 = lmer(beck ~ 1 + season + (1|subject), data = seasonal, REML = FALSE)
summary(lmer.1)



##################################################
### Replicating RM-ANOVA results
##################################################

lmer.1.reml = lmer(beck ~ 1 + season + (1|subject), data = seasonal, REML = TRUE)
summary(lmer.1.reml)

anova(lmer.1.reml)


##################################################
### Likelihood ratio test (LRT)
##################################################

anova(lmer.0, lmer.1)

# Mimic p-value
1 - pchisq(8.72, df = 3)



##################################################
### Use numeric time predictor
##################################################

# Map seasons to continuous values
lookup_table = data.frame(
  season = c("s1", "s2", "s3", "s4"),
  season2 = c(0, 1, 2, 3)
)

# Join the data
seasonal = left_join(seasonal, lookup_table, by = "season")
head(seasonal)


##################################################
### Plot Beck depression scores over time
##################################################

ggplot(data = seasonal, aes(x = season2, y = beck)) +
  geom_line(aes(group = subject), alpha = 0.3) +
  geom_smooth(group = 1, se = FALSE) +
  theme_bw() +
  xlab("Time") +
  ylab("Beck depression score")



##################################################
### Fit linear, quadratic, and cubi growth models
##################################################

# Linear growth
lmer.l = lmer(beck ~ 1 + season2 + (1|subject), data = seasonal, REML = FALSE)

# Quadratic growth
lmer.q = lmer(beck ~ 1 + season2 + I(season2^2) + (1|subject), 
              data = seasonal, REML = FALSE)

# Cubic growth
lmer.c = lmer(beck ~ 1 + season2 + I(season2^2) + I(season2^3) + (1|subject), data = seasonal, REML = FALSE)

# LRT to examine growth patterns
anova(lmer.0, lmer.l, lmer.q, lmer.c)

# Quartic growth model - Produces error
lmer.quartic = lmer(beck ~ 1 + season2 + I(season2^2) + I(season2^3) + I(season2^4) + (1|subject), data = seasonal, REML = FALSE)


##################################################
### Interpret output
##################################################

summary(lmer.l)



##################################################
### Plot effects of cubic growth model
##################################################

plot_data = expand.grid(
  season2 = seq(from = 0, to = 3, by = 0.01)
)

plot_data$yhat = predict(lmer.c, newdata = plot_data, re.form = NA)

ggplot(data = plot_data, aes(x = season2, y = yhat)) +
  geom_line() +
  theme_bw() +
  xlab("Time") +
  ylab("Beck depression score")



##################################################
### Use different time metric (weeks rather than months)
##################################################

lookup_table = data.frame(
  season2 = c(0, 1, 2, 3),
  season3 = c(0, 13, 26, 39)
)

seasonal = left_join(seasonal, lookup_table, by = "season2")
head(seasonal)


lmer.l2 = lmer(beck ~ 1 + season3 + (1|subject), data = seasonal, REML = FALSE)
summary(lmer.l2)


