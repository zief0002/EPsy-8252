##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(car)
library(countreg) #Install from R-Forge
library(corrr)
library(dplyr)
library(ggplot2)
library(MASS)
library(pscl)
library(readr)
library(sm)




##################################################
### Read in data
##################################################

frisk = read_csv(file = "~/Documents/github/epsy-8252/data/stop-and-frisk.csv")
head(frisk)



##################################################
### Spelling test example
##################################################

# Avg = 4 errors per test
# Probabaility of 7 errors?

exp(-4) * (4^7) / factorial(7)


# Alternative
dpois(x = 7, lambda = 4)



##################################################
### Different Poisson distributions
##################################################

data.frame(
  x = seq(from = 0, to = 20, by = 1)
) %>%
  mutate(
    y_01 = dpois(x = x, lambda = 1),
    y_04 = dpois(x = x, lambda = 4),
    y_10 = dpois(x = x, lambda = 10) 
  ) %>%
  ggplot(aes(x = x, y = y_04)) +
    geom_line(linetype = "dotted") +
    geom_line(aes(y = y_01), color = "blue") +
    geom_line(aes(y = y_10), linetype = "dashed", color = "purple") +
    theme_bw() +
    scale_x_continuous(name = "k", limits = c(0, 20)) +
    annotate(geom = "text", x = 2.1 , y = 0.3, label = expression(mu == 1), color = "blue") +
    annotate(geom = "text", x = 5 , y = 0.2, label = expression(mu == 4), color = "black") +
    annotate(geom = "text", x = 10.1 , y = 0.14, label = expression(mu == 10), color = "purple")



##################################################
### Model 1: Ethnicity
##################################################

glm.1 = glm(stops ~ 1 + relevel(factor(ethnicity), ref = "White"), data = frisk, family = poisson(link = "log"))


# Obtain coefficient- and model-level output
summary(glm.1)


# Exponentiate coefficients
exp(coef(glm.1))



##################################################
### Rate of occurrence
##################################################

# Fit the model
glm.1.2 = glm(stops ~ 1 + relevel(factor(ethnicity), ref = "White"), data = frisk, 
              family = poisson(link = "log"), offset = log(past_arrests+1))


# Output
summary(glm.1.2)


# Exponentiate coefficients
exp(coef(glm.1.2))



##################################################
### Include precinct covariates
##################################################

glm.2 = glm(stops ~ 1 + relevel(factor(ethnicity), ref = "White") + factor(precinct), 
            data = frisk, family = poisson(link = "log"), offset = log(past_arrests+1))


summary(glm.2)


exp(c(0.42, 0.43)) - 1

exp(coef(glm.2)[1:3])


##################################################
### Check Assumptions
##################################################

out_2 = augment(glm.2, type.residual = "pearson")

head(out_2)


# Residual plot
ggplot(data = out_2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Pearson residuals")


# Alternatively
residualPlot(glm.2)



##################################################
### Overdispersion
##################################################

# Conditional means and variances
frisk %>%
  filter(precinct %in% c(1:3)) %>%
  group_by(ethnicity, precinct) %>%
  summarize(
    M = mean(stops),
    V = var(stops)
  )


# Ratio of residual deviance to df
140788 / 823



##################################################
### Quasi-likelihood estimation
##################################################

glm.3 = glm(stops ~ 1 + relevel(factor(ethnicity), ref = "White") + factor(precinct), data = frisk, 
            family = quasipoisson(link = "log"), offset = log(past_arrests+1))


# Output
summary(glm.3)


# Interpret coefficients
exp(c(0.4221, 0.4304)) - 1


# Check the residuals
residualPlot(glm.3)



##################################################
### Negative binomial model
##################################################

glm.4 = glm.nb(stops ~ 1 + relevel(factor(ethnicity), ref = "White") + factor(precinct) + offset(log(past_arrests+1)), 
               data = frisk)


# Output
summary(glm.4)


# Interpret coefficients
exp(c(0.367, 0.371)) - 1


# Check the residuals
residualPlot(glm.4)



##################################################
### Zero inflation
##################################################

# Examine outcome by ethnicity
ggplot(data = frisk, aes(x = stops)) +
  geom_histogram(color = "black", fill = "skyblue") +
  facet_wrap(~ethnicity) +
  theme_bw() +
  xlab('"Stop and frisk" stops') +
  ylab("Counts")



##################################################
### Rootograms for examining residual fit
##################################################

# Install the countreg package
# install.packages("countreg", repos="http://R-Forge.R-project.org")


# Standing rootogram of the fitted Poisson model
rootogram(glm.2, style = "standing")


# Standing rootogram of the fitted negative binomial model
rootogram(glm.4, style = "standing")



##################################################
### Fit zero-inflated model
##################################################

# Fit model
glm.5 = zeroinfl(stops ~ 1 + relevel(factor(ethnicity), ref = "White") + factor(precinct) + 
                   offset(log(past_arrests+1)), data = frisk, dist = "negbin")


# Standing rootogram
rootogram(glm.5, style = "standing")



##################################################
### Example interpretation of zero-inflated model
##################################################

# Fit example model
glm.example = zeroinfl(stops ~ 1 + relevel(factor(ethnicity), ref = "White") + offset(log(past_arrests+1)), 
                       data = frisk, dist = "poisson")


# Output
summary(glm.example)


# Interpret count model coefficients
exp(c(0.15, 0.22)) - 1


# Odds of zero
exp(c(-7.94, -18.2, -10.9))


# Probability of zero
exp(c(-7.94, -18.2, -10.9)) / (1 + exp(c(-7.94, -18.2, -10.9)))



