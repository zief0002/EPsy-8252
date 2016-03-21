###################################################
### Load libraries
###################################################

library(dplyr)
library(foreign)
library(ggplot2)
library(lme4)
library(sm)




###################################################
### Read in the SPSS data - Requires the foreign package
###################################################

nbaL1 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel2.sav", to.data.frame = TRUE)

head(nbaL1)
head(nbaL2)



###################################################
### Merge the player-level data and the team-level data
###################################################

nba = left_join(nbaL1, nbaL2, by = "Team_ID")

head(nba)
tail(nba)



###################################################
### Fit mixed-effects regression
###################################################

lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), data = nba)
summary(lmer.1)




###################################################
### Evaluating models
###################################################

AICc(lmer.1)
AICc(lm.1)
AICc(lm.2)


