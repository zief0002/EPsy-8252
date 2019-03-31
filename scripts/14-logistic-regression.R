##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



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
    Lambda = 1 / (1 + exp(-w))
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
glm.1 = glm(degree ~ 1 + act, data = grad, family = binomial(link = "logit"))


# Output
summary(glm.1)



##################################################
### Back-transform coefficients to odds interpretation
##################################################

exp(coef(glm.1))



##################################################
### Plot of the fitted model
##################################################

# Create the data to plot
plotData = crossing(
  act = seq(from = 10, to = 36, by = 1) 
  ) %>%
  mutate(
    pi_hat = predict(glm.1, newdata = ., type = "response") #Predicted probabilities
    )


# Plot the data
ggplot(data = plotData, aes(x = act, y = pi_hat)) +
  geom_line() +
  theme_bw() +
  xlab("ACT score") +
  ylab("Predicted probability of graduating") + 
  ylim(0, 1)



##################################################
### Residual deviance
##################################################

# Fit intercept-only model
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
  modnames = c("Intercept-only", "ACT score")
) 



##################################################
### Control for first generation students
##################################################

# Fit model
glm.2 = glm(degree ~ 1 + act + firstgen, data = grad, family = binomial(link = "logit"))


# Obtain summary output
summary(glm.2)


# Back-transform coefficients for odds interpretation
exp(coef(glm.2))



##################################################
### Plot the results from the fitted model
##################################################

# Create data to plot
plotData = crossing(
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

