##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(corrr)
library(tidyverse)



##################################################
### Read in data
##################################################

grad = read_csv(file = "~/Documents/github/epsy-8252/data/graduation.csv")
head(grad)



##################################################
### Proportion who graduate by ACT score
##################################################

graduates = grad %>%
  group_by(act, degree) %>%
  summarize( N = n() ) %>%
  mutate( Prop = N / sum (N) ) %>%
  filter(degree == 1) %>%
  ungroup() #Makes the resulting tibble regular


# View data
head(graduates, 10)


# Plot of proportion who graduate by ACT score
ggplot(data = graduates, aes(x = act, y = Prop)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Proportion of graduates")



##################################################
### Logistic transformation example
##################################################

example = tibble(
  w = seq(from = -4, to = 4, by = 0.01)
  ) %>%
  mutate(
    Lambda = 1 / (1 + exp(-w)),
    Odds = Lambda / (1 - Lambda),
    Log_odds = log(Odds)
    ) 


# View data
example


# Plot the results
ggplot(data = example, aes(x = w, y = Lambda)) +
  geom_line() +
  theme_bw()



##################################################
### Odds
##################################################

# Odds of getting an A
0.7 / 0.3

# Log-odds of getting an A
log(0.7 / 0.3)



# Odds of Canadian team winning Stanley Cup
0.17 / 0.83

# Log-odds of Canadian team winning Stanley Cup
log(0.17 / 0.83)



##################################################
### Fit binomial logistic model
##################################################

# Fit the model
glm.0 = glm(degree ~ 1,       data = grad, family = binomial(link = "logit"))
glm.1 = glm(degree ~ 1 + act, data = grad, family = binomial(link = "logit"))


# Coefficient-level output
tidy(glm.1)



##################################################
### Back-transform coefficients to odds interpretation
##################################################

exp(coef(glm.1))



##################################################
### Plot of the fitted model - Odds
##################################################

ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {exp(-1.611 + 0.108*x)}
  ) +
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted odds of graduating")



##################################################
### Back-transform intercept value to probability of graduating
##################################################

exp(-1.61) / (1 + exp(-1.61))



##################################################
### Plot of the fitted model - Probability
##################################################

ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point(alpha = 0) +
  stat_function(
    fun = function(x) {exp(-1.611 + 0.108*x) / (1 + exp(-1.611 + 0.108*x))}
  ) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") +
  ylim(0, 1)



##################################################
### Model-level summaries
##################################################

# Model-level output
glance(glm.1)


# Fit intercept-only model (baseline)
glm.0 = glm(degree ~ 1, data = grad, family = binomial(link = "logit"))


# Compute residual deviance for intercept-only model
-2 * logLik(glm.0)[[1]]


# Compute residual deviance for ACT predictor model
-2 * logLik(glm.1)[[1]]



##################################################
### Likelihood ratio test to compare residual deviances
##################################################

anova(glm.0, glm.1, test = "LRT")



##################################################
### Model evidence
##################################################

aictab(
  cand.set = list(glm.0, glm.1),
  modnames = c("Intercept-Only", "Effect of ACT")
)



##################################################
### Pseudo R^2
##################################################

# Baseline residual deviance: 2722.6                          
# Model residual deviance: 2633.2

# Compute pseudo R-squared
(2722.6 - 2633.2) / 2722.6



##################################################
### Control for first generation students
##################################################

# Fit model
glm.2 = glm(degree ~ 1 + act + firstgen, data = grad, family = binomial(link = "logit"))


# Fit model with only firstgen for completeness
glm.3 = glm(degree ~ 1 + firstgen, data = grad, family = binomial(link = "logit"))


# Model evidence
aictab(
  cand.set = list(glm.0, glm.1, glm.2, glm.3),
  modnames = c("Intercept-Only", "ACT", "ACT + First Gen.", "First Gen.")
)


# Obtain coefficient-level output
tidy(glm.2)


# Back-transform coefficients for odds interpretation
exp(coef(glm.2))



##################################################
### Plot the results from the fitted model - Odds
##################################################

ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point(alpha = 0) +
  # Non-first generation students
  geom_function(
    fun = function(x) {exp(-1.48 + 0.0881*x + 0.516*0)},
    linetype = "dashed",
    color = "blue"
  ) +
  # First generation students
  geom_function(
    fun = function(x) {exp(-1.48 + 0.0881*x + 0.516*1)},
    linetype = "solid",
    color = "red"
  )+
  theme_light() +
  xlab("ACT score") +
  ylab("Predicted odds of graduating")



##################################################
### Plot the results from the fitted model - Probability
##################################################

ggplot(data = grad, aes(x = act, y = degree)) +
  geom_point(alpha = 0) +
  # Non-first generation students
  stat_function(
    fun = function(x) {exp(-1.48 + 0.0888*x) / (1 + exp(-1.481 + 0.088*x))},
    linetype = "dashed",
    color = "blue"
  ) +
  # First generation students
  stat_function(
    fun = function(x) {exp(-0.965 + 0.0888*x) / (1 + exp(-0.965 + 0.088*x))},
    linetype = "solid",
    color = "red"
  ) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") +
  ylim(0, 1)



##################################################
### Model-level summaries
##################################################

# Model-level output
glance(glm.2)



##################################################
### Table of regression models
##################################################

library(stargazer)

# Table
stargazer(
  glm.0, glm.1, glm.3, glm.2,
  type = "html",
  title = "Coefficients and standard errors for a taxonomy of models fitted to predict graduation status. All models were fitted using a logistic regression and assuming binomial errors.",
  column.labels = c("Model A", "Model B", "Model C", "Model D"),
  colnames = FALSE,
  model.numbers = FALSE,
  dep.var.caption = "Outcome: Dummy-Coded Indicator of Graduation",
  dep.var.labels.include = FALSE,
  covariate.labels = c("ACT score", "First Generation Indicator"),
  keep.stat = NULL,
  notes.align = "l",
  add.lines = list(
    c("Residual Deviance", 2722.6, 2633.2, 2662.1, 2609.2),
    c("Corrected AIC", round(AICc(glm.0), 1), round(AICc(glm.1), 1), 
      round(AICc(glm.3), 1), round(AICc(glm.2), 1))
  ),
  star.cutoffs = NA, # Omit stars
  omit.table.layout = "n" #Don't show table notes
)
