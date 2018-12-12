##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(arm)
library(broom)
library(dplyr)
library(ggplot2)
library(haven) #for reading in the .sav files
library(lme4) #for fitting mixed-effects models
library(sm)



##################################################
### Read in data
##################################################

# Read in player-level data
nbaL1 = read_sav(file = "~/Dropbox/epsy-8252/data/nbaLevel1.sav")

# Read in team-level data
nbaL2 = read_sav(file = "~/Dropbox/epsy-8252/data/nbaLevel2.sav")

# Join/merge the two files together
nba = left_join(nbaL1, nbaL2, by = "Team_ID")
head(nba)



##################################################
### Fit unconditional means model
##################################################

lmer.0 = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba, REML = FALSE)
summary(lmer.0)



##################################################
### Fit model with play-level predictor
##################################################

lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), 
              data = nba, REML = FALSE)
summary(lmer.1)



##################################################
### Random-effect of shots-on-five
##################################################

lmer.2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), 
              data = nba, REML = FALSE)
summary(lmer.2)



##################################################
### Choosing random-effects
##################################################

# Plot of OLS lines by team
ggplot(data = nba, aes(x = Shots_on_five, y = Life_Satisfaction)) +
  geom_line(aes(group = Team_ID), stat = "smooth", method = "lm", se = FALSE, 
            color = "blue", alpha = 0.7) +
  geom_line(aes(group = 1), stat = "smooth", method = "lm", se = FALSE, 
            color = "black", size = 2) +
  theme_bw()


# Panel plot of OLS regression lines by team
ggplot(data = nba, aes(x = Shots_on_five, y = Life_Satisfaction)) +
  geom_point() +
  geom_smooth(aes(group = Team_ID), method = "lm", se = FALSE, 
              color = "blue") +
  theme_bw() +
  facet_wrap(~Team_ID)


# Use model evidence
aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("B0 and b0", "B0, B1, and b0", "B0, B1, b0, and b1")
)



##################################################
### Include team-level predictor
##################################################

lmer.3 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + (1 | Team_ID), 
              data = nba, REML = FALSE)


lmer.4 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + 
                (1 + Shots_on_five | Team_ID), data = nba, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2, lmer.3, lmer.4),
  modnames = c("B0 and b0", "B0, B1, and b0", "B0, B1, b0, and b1", 
               "B0, B1, B2, and b0", "B0, B1, B2, b0, and b1")
)


summary(lmer.3)



##################################################
### Cross-level interaction
##################################################

lmer.5 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + 
                Shots_on_five:Coach_Experience + (1 | Team_ID), 
              data = nba, REML = FALSE)


lmer.6 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + 
                Shots_on_five:Coach_Experience + (1 + Shots_on_five | Team_ID), 
              data = nba, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2, lmer.3, lmer.4, lmer.5, lmer.6),
  modnames = c("B0 and b0", "B0, B1, and b0", "B0, B1, b0, and b1", 
               "B0, B1, B2, and b0", "B0, B1, B2, b0, and b1",
               "B0, B1, B2, B1:B2, and b0", 
               "B0, B1, B2, B1:B2, b0, and b1")
)


summary(lmer.5)



##################################################
### Table of fitted models
##################################################

library(texreg)

# Fit the models you want to present 
lmer.0 = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba, REML = FALSE)

lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), 
              data = nba, REML = FALSE)

lmer.2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + 
                (1 | Team_ID), data = nba, REML = FALSE)

lmer.3 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + 
                Shots_on_five:Coach_Experience + (1 | Team_ID), data = nba, REML = FALSE)

# Use extract.lmerMod() to remove number of observations
# for better table formatting
tr0 = extract.lmerMod(lmer.0, include.nobs = FALSE, include.groups = FALSE, 
                      include.bic = FALSE)
tr1 = extract.lmerMod(lmer.1, include.nobs = FALSE, include.groups = FALSE, 
                      include.bic = FALSE)
tr2 = extract.lmerMod(lmer.2, include.nobs = FALSE, include.groups = FALSE, 
                      include.bic = FALSE)
tr3 = extract.lmerMod(lmer.3, include.nobs = FALSE, include.groups = FALSE, 
                      include.bic = FALSE)

texreg(
  list(tr0, tr1, tr2, tr3), 
  custom.coef.names = c("Intercept", "Player success", "Coach experience", 
                        "Player success x Coach experience"), 
  reorder.coef = c(2, 3, 4, 1),
  caption = "Coefficients (Standard Errors) for a Taxonomy of Fitted Mixed-Effects Models to Predict Life Satisfaction for 300 NBA Players from 30 Teams. All Models are Fitted using Maximum Likelihood.",
  caption.above = TRUE
)



##################################################
### Plot model results
##################################################

# Set up plotting data for lmer.2
my_data2 = expand.grid(
  Shots_on_five = seq(from = 0, to = 5, by = 1),
  Coach_Experience = c(1, 3),
  model = "Main-effects"
)


# Set up plotting data for lmer.3
my_data3 = expand.grid(
  Shots_on_five = seq(from = 0, to = 5, by = 1),
  Coach_Experience = c(1, 3),
  model = "Interaction"
)


# Predict life satisfaction
my_data2$yhat = predict(lmer.2, newdata = my_data2, re.form = NA)
my_data3$yhat = predict(lmer.3, newdata = my_data3, re.form = NA)


# Combine both data sets into one 
my_data = rbind(my_data2, my_data3)
head(my_data)


# Turn coach experience into a factor for better plotting
my_data$Coach_Experience = factor(my_data$Coach_Experience)


ggplot(data = my_data, aes(x = Shots_on_five, y = yhat, color = Coach_Experience)) +
  geom_line() +
  theme_bw() +
  xlab("Shots-on-five") +
  ylab("Predicted life satisfaction") +
  ggsci::scale_color_d3(name = "Coach experience (in years)") +
  facet_wrap(~model)




# Set up plotting data for lmer.2
my_data2 = expand.grid(
  Shots_on_five = seq(from = 0, to = 5, by = 1),
  Coach_Experience = c(1, 3)
)


my_data2$yhat = predict(lmer.3, newdata = my_data2, re.form = NA)
head(my_data2)


my_data2$Coach_Experience = factor(my_data2$Coach_Experience)

ggplot(data = my_data2, aes(x = Shots_on_five, y = yhat, color = Coach_Experience)) +
  geom_line() +
  theme_bw() +
  xlab("Shots-on-five") +
  ylab("Predicted life satisfaction") +
  ggsci::scale_color_d3(name = "Coach experience (in years)")



lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience +
                (1 | Team_ID), data = nba, REML = FALSE)
out = augment(lmer.1)
head(out)

sm.density(out$.resid, model = "normal")

ggplot(data = out, aes(x = .fitted, y = .resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0)


b0j = ranef(lmer.1)$Team_ID[ , 1]
sm.density(b0j, model="normal")

yhat = 3.947370 + 1.553284 * nbaL2$Coach_Experience


