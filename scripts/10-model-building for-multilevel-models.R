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
#head(nbaL1)

nbaL2 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel2.sav", to.data.frame = TRUE)
#head(nbaL2)



###################################################
### Merge the player-level data and the team-level data - Requires the dplyr package
###################################################

nba = left_join(nbaL1, nbaL2, by = "Team_ID")

head(nba)



###################################################
### Fit unconditional random intercepts model - Requires lme4 package
###################################################

lmer.a = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba)
summary(lmer.a)

# Get estimates of the random-effects
ranef(lmer.a)



###################################################
### Maximum likelihood (ML) vs. Restricted maximum likelihood (REML) estimation
###################################################

# ML estimates
lmer.a_ml = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba, REML = FALSE)
summary(lmer.a_ml)

# Compare to REML estimates (fitted previously by default)
summary(lmer.a)



###################################################
### Selecting fixed effects
### Use ML estimation to compare models with different fixed effects
###################################################

# Fit model with fixed and random effects of intercept using ML estimation 
lmer.a_ml = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba, REML = FALSE)

# Fit model with fixed and random effects of intercept AND fixed effect of slope using ML estimation
lmer.b_ml = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), data = nba, REML = FALSE)


# Compare models using evidence from the AICc
 aictab(
  cand.set =  list(lmer.a_ml, lmer.b_ml),
  modnames = c("Intercept", "Intercepts + Slopes")
)

aictab(mod)



###################################################
### Selecting random effects
### Use REML estimation to compare models with different random effects
###################################################

# Fixed and random effects of intercept AND fixed and random effects of slope
lmer.b = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), data = nba)

# Examine model for pedgogical purposes only
# summary(lmer.b)

# Fixed and random effects of intercept AND fixed effect of slope
lmer.b0 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), data = nba)


# Compare models using evidence from the AICc
aictab(
  cand.set =  list(lmer.b0, lmer.b),
  modnames = c("Varying Intercepts", "Varying Intercepts and Slopes")
  )

# Examine varying intercepts model
summary(lmer.b0)



###################################################
### Adding level-2 predictors
###################################################

# No level-2 predictors
lmer.c1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 | Team_ID), data = nba, REML = FALSE)


# Level-2 predictor for intercept
lmer.c2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + (1 | Team_ID), data = nba, REML = FALSE)


# Level-2 predictor for intercept and slope
lmer.c3 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + Shots_on_five:Coach_Experience + 
                 (1 | Team_ID), data = nba, REML = FALSE)

# AICc evidence
aictab(
  cand.set =  list(lmer.c1, lmer.c2, lmer.c3),
  modnames = c("None", "Intercept", "Intercept and Slopes")
)




###################################################
### Interpreting "final" model
###################################################

# Re-fit model using REML estimation
lmer.c2 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + (1 | Team_ID), data = nba)
summary(lmer.c2)

ranef(lmer.c2)
