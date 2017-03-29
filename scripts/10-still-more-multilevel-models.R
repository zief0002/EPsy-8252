#########################################
## Read in data
#########################################

library(haven)
nbaL1 = read_sav(file = "~/Google Drive/Documents/EPsy-8252/data/nbaLevel1.sav")
nbaL2 = read_sav(file = "~/Google Drive/Documents/EPsy-8252/data/nbaLevel2.sav")



#########################################
## Merge the datasets
#########################################

library(dplyr)
nba = left_join(nbaL1, nbaL2, by = "Team_ID")
head(nba)



#########################################
## Load libraries
#########################################

library(AICcmodavg)
library(broom)
library(ggplot2)
library(lme4)
library(sm)



#########################################
## Fit random intercepts model using REML to get ICC
#########################################

lmer.0 = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba)
summary(lmer.0)



#########################################
## Examine level-1 structure using ML
#########################################

# Refit lmer.0 using ML
lmer.0 = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba, REML = FALSE)

# Linear effect of player success
lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), data = nba, REML = FALSE)

# Quadratic effect of player success
lmer.2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + (1 | Team_ID), data = nba, REML = FALSE)


# Compare the AICc values of the three models
aictab(
cand.set =  list(lmer.0, lmer.1, lmer.2),
modnames = c("Unconditional model", "Linear Player success", "Quadratic Player success")
)



#########################################
## Examine level-2 structure using ML
#########################################

# No level-2 predictors
lmer.l1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), data = nba, REML = FALSE)

# Level-2 predictor for intercept
lmer.l2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + (1 | Team_ID), data = nba, REML = FALSE)

# Level-2 predictor for intercept and slope
lmer.l3 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience +  Shots_on_five:Coach_Experience + (1 | Team_ID), data = nba, REML = FALSE)

## ------------------------------------------------------------------------

# No level-2 predictors
lmer.q1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + (1 | Team_ID), data = nba, REML = FALSE)

# Level-2 predictor for intercept
lmer.q2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + Coach_Experience +
                 (1 | Team_ID), data = nba, REML = FALSE)

# Level-2 predictor for intercept and slope
lmer.q3 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + Coach_Experience + 
                 Shots_on_five:Coach_Experience + (1 | Team_ID), data = nba, REML = FALSE)

# Level-2 predictor for intercept, slope, and quadratic
lmer.q4 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + Coach_Experience + 
                 Shots_on_five:Coach_Experience +  I(Shots_on_five ^ 2):Coach_Experience + 
                 (1 | Team_ID), data = nba, REML = FALSE)


# AICc evidence
aictab(
cand.set =  list(lmer.l1, lmer.l2, lmer.l3, lmer.q1, lmer.q2, lmer.q3, lmer.q4),
modnames = c("L1", "L2", "L3", "Q1", "Q2", "Q3", "Q4")
)



#########################################
## Examine random-effects structure using REML
#########################################

# RE for intercept
lmer.re1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + Coach_Experience + 
    Shots_on_five:Coach_Experience +  I(Shots_on_five ^ 2):Coach_Experience +
    (1 | Team_ID), data = nba)

# RE for intercept and linear slope
lmer.re2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + Coach_Experience + 
    Shots_on_five:Coach_Experience +  I(Shots_on_five ^ 2):Coach_Experience +
    (1 + Shots_on_five | Team_ID), data = nba)

# RE for intercept, linear slope, and quadratic slope
lmer.re3 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + I(Shots_on_five ^ 2) + Coach_Experience + 
    Shots_on_five:Coach_Experience +  I(Shots_on_five ^ 2):Coach_Experience +
    (1 + Shots_on_five + I(Shots_on_five ^ 2) | Team_ID), data = nba)


# AICc evidence
aictab(
cand.set =  list(lmer.re1, lmer.re2, lmer.re3),
modnames = c("RE1", "RE2", "RE3")
)



#########################################
## Obtain REML estimates of the fixed-effects and variance components
#########################################

summary(lmer.re1)


#########################################
## Plot average model
#########################################

plotdata = expand.grid(
  Shots_on_five = seq(from = 0, to = 5, by = 0.1),
  Coach_Experience = c(1, 2, 3)
)

# Predict life satisfaction
plotdata$yhat = predict(lmer.re1, newdata = plotdata, re.form = NA)
head(plotdata)

# Coerce coach experience into a factor for better plotting
plotdata$Coach_Experience = factor(plotdata$Coach_Experience, levels = c(1, 2, 3), 
      labels = c("Little experience", "Some experience", "Quite a bit of experience"))

# Plot
ggplot(data = plotdata, aes(x = Shots_on_five, y = yhat, color = Coach_Experience)) +
  geom_line() +
  theme_bw() +
  xlab("Player success") +
  ylab("Predicted life satisfaction") +
  scale_color_brewer(name = "", palette = "Set1")



#########################################
## Check assumptions
#########################################

out = augment(lmer.re1)
head(out)


## Normality of level-1 residuals
sm.density(out$.resid, model = "normal", xlab = "Level-1 residuals")


## Linearity and homogeneity of variance of level-1 residuals
ggplot(data = out, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


## Normality of random-effects
sm.density(ranef(lmer.re1)$Team_ID[ , 1], model = "normal", xlab = "Level-2 residuals")

