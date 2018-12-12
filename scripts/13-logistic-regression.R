##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
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
### Explore proportion of students who graduate by ACT score
##################################################

# Compute proportions of people who graduate (degree = 1)
graduates = grad %>% 
  group_by(act, degree) %>% 
  summarize( 
    N = n() 
    ) %>% 
  mutate( 
    Prop = N / sum(N) 
    ) %>%
  filter(degree == 1) %>%
  ungroup() #Makes the resulting tibble regular


# View data
head(graduates, 10)


# Plot proportions
ggplot(data = graduates, aes(x = act, y = Prop)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("ACT score") +
  ylab("Proportion of graduates")



##################################################
### Logistic transformation: Example
##################################################

example = tibble(
  w = seq(from = -4, to = 4, by = 0.01)  # Set up values
  ) %>%
  mutate(
    phi = 1 / (1 + exp(-w))  # Transform using logistic function
  )

head(example)


# Plot the results
ggplot(data = example, aes(x = w, y = phi)) +
  geom_line() +
  theme_bw()



##################################################
### Fit logistic regression model
##################################################

glm.1 = glm(degree ~ 1 + act, data = grad, family = binomial(link = "logit"))
summary(glm.1)



##################################################
### Back-transform coefficients to get odds
##################################################

exp(coef(glm.1))



##################################################
### Plot predicted probability of graduating as a function of ACT
##################################################

# Set up data
plotData = expand.grid(
  act = seq(from = 10, to = 36, by = 1)
  ) %>%
  mutate(
    pi_hat = predict(glm.1, newdata = ., type = "response")
  )


# Plot the results
ggplot(data = plotData, aes(x = act, y = pi_hat)) +
  geom_line() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") +
  ylim(0, 1)



##################################################
### Null deviance: Deviance from the intercept-only model
##################################################

glm.0 = glm(degree ~ 1, data = grad, family = binomial(link = "logit"))

-2 * logLik(glm.0)[[1]]



##################################################
### Residual deviance: Deviance from the full model
##################################################

-2 * logLik(glm.1)[[1]]



##################################################
### Likelihood ratio test
##################################################

anova(glm.0, glm.1, test = "LRT")



##################################################
### Model evidence
##################################################

myAIC = aictab(
  cand.set = list(glm.0, glm.1),
  modnames = c("Intercept-only", "ACT score")
)

myAIC



##################################################
### Including covariates
##################################################

glm.2 = glm(degree ~ 1 + act + firstgen, data = grad, family = binomial(link = "logit"))
summary(glm.2)



##################################################
### Back-transform coefficients to odds
##################################################

exp(coef(glm.2))



##################################################
### Plot probability of graduating as a function of ACT and firstgen
##################################################

# Create the dataset to plot
plotData = expand.grid(
  act = seq(from = 10, to = 36, by = 1),
  firstgen = c(0, 1)
  ) %>%
  mutate(
    pi_hat = predict(glm.2, newdata = ., type = "response"),
    firstgen = factor(firstgen, 
                      levels = c(0, 1), 
                      labels = c("Non First Generation Students", "First Generation Students")
                      )
  )


# Plot the data
ggplot(data = plotData, aes(x = act, y = pi_hat, color = firstgen)) +
  geom_line() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") +
  ylim(0, 1) +
  scale_color_brewer(name = "", palette = "Set1")


