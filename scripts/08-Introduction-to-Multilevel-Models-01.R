###################################################
### Load libraries
###################################################

library(dplyr)
library(foreign)





###################################################
### Read in the SPSS data - Requires the foreign package
###################################################

nbaL1 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel2.sav", to.data.frame = TRUE)

head(nbaL1)
head(nbaL2)



###################################################
### Merge the player-level data and the team-level data - Requires the dplyr package
###################################################

nba = left_join(nbaL1, nbaL2, by = "Team_ID")

head(nba)
tail(nba)



###################################################
### To-Do
###################################################

# Explain what left_join() does.

# Fit linear models
# Response variable: Life_Satisfaction

# (1) Fit an intercept-only model 
# (2) Fit a model that includes intercept and the Shots_on_five predictor

# Select "best" model using AICc

# Interpret "best" model's coefficients

# Evaluate all model assumptions for the selected "best" model


