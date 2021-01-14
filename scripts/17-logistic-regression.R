##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(MuMIn)
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

example = data.frame(
  w = seq(from = -4, to = 4, by = 0.01)
  ) %>%
  mutate(
    Lambda = 1 / (1 + exp(-w)),
    Odds = Lambda / (1 - Lambda),
    Log_odds = log(Odds)
    ) 


# View data
head(example)


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
### Plot of the fitted model
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

model.sel(
  list(glm.0, glm.1)
)



##################################################
### Control for first generation students
##################################################

# Fit model
glm.2 = glm(degree ~ 1 + act + firstgen, data = grad, family = binomial(link = "logit"))
glm.3 = glm(degree ~ 1 + act + firstgen + act:firstgen, data = grad, family = binomial(link = "logit"))

# Obtain coefficient-level output
tidy(glm.3)


# Back-transform coefficients for odds interpretation
exp(coef(glm.2))



##################################################
### Plot the results from the fitted model
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


