###################################################
### Read in the data
###################################################

library(foreign)

nbaL1 = read.spss(file = "http://www.tc.umn.edu/~zief0002/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "http://www.tc.umn.edu/~zief0002/data/nbaLevel2.sav", to.data.frame = TRUE)

nba = merge(nbaL1, nbaL2, by = "Team_ID")

head(nba)
tail(nba)



###################################################
### Load libraries
###################################################

library(ggplot2)
library(lmerTest)
library(dplyr)


lmer.b = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), data = nba)
summary(lmer.b)
AIC(lmer.b)
BIC(lmer.b)
cor(nba$Life_Satisfaction, fitted(lmer.b)) ^ 2



###################################################
### Grand mean centering
###################################################

# Center the level-1 predictor using the grand mean
nba$so5 = nba$Shots_on_five - mean(nba$Shots_on_five)

# Fit LMER model using centered predictor
lmer.b2 = lmer(Life_Satisfaction ~ 1 + so5 + (1 + so5 | Team_ID), data = nba)
summary(lmer.b2)

AIC(lmer.b2)
BIC(lmer.b2)
cor(nba$Life_Satisfaction, fitted(lmer.b2)) ^ 2



###################################################
### Group mean centering
###################################################

# Compute group means of level-1 predictor (requires dplyr library)
teams = nba %.%
	group_by(Team_ID) %.%
	summarise(meanShots = mean(Shots_on_five))

# Merge the group means back into the level-1 data set
nba2 = merge(nba, teams, by = "Team_ID")	

# Center the level-1 predictor using the group means
nba2$gcShots = nba2$Shots_on_five - nba2$meanShots
head(nba2)

# Fit LMER model using centered predictor
lmer.b3 = lmer(Life_Satisfaction ~ 1 + gcShots + (1 + gcShots | Team_ID), data = nba2)
summary(lmer.b3)

# Random effects
ranef(lmer.b3)

# Get the predicted values 
fixef(lmer.b3)[[1]] + ranef(lmer.b3)$Team_ID[ , 1]

# G matrix
VarCorr(lmer.b3)$Team_ID

AIC(lmer.b3)
BIC(lmer.b3)
cor(nba$Life_Satisfaction, fitted(lmer.b3)) ^ 2
