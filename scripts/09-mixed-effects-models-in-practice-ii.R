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
### Fit mixed-effects model
##################################################

lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience +
                (1 | Team_ID), data = nba, REML = FALSE)



##################################################
### Get level-1 residuals, fitted values, etc.
##################################################

out = augment(lmer.1)
head(out)



##################################################
### Examine normality of the level-1 residuals
##################################################

sm.density(out$.resid, model = "normal")



##################################################
### Examine linearity and homoscedasticity of the level-1 residuals
##################################################

ggplot(data = out, aes(x = .fitted, y = .resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0)



##################################################
### Get level-2 fitted values and residuals (random-effects)
##################################################

# Extract level-2 residuals
b0j = ranef(lmer.1)$Team_ID[ , 1]

# Compute y-hat for the level-2 model
yhat = 3.947370 + 1.553284 * nbaL2$Coach_Experience

# Put these in a data frame
level_2 = data.frame(
  yhat, b0j
)

head(level_2)


##################################################
### Examine normality of the level-2 residuals
##################################################

sm.density(level2$b0j, model = "normal")



##################################################
### Examine linearity and homoscedasticity of the level-2 residuals
##################################################

ggplot(data = level_2, aes(x = yhat, y = b0j)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0)





