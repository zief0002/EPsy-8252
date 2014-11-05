###################################################
### Read in and prepare the data
###################################################
library(foreign)

nbaL1 = read.spss(file = "http://www.tc.umn.edu/~zief0002/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "http://www.tc.umn.edu/~zief0002/data/nbaLevel2.sav", to.data.frame = TRUE)

nba = merge(nbaL1, nbaL2, by = "Team_ID")

head(nba)
tail(nba)


###################################################
### Load the libraries
###################################################

library(ggplot2)
library(lmerTest)
library(dplyr)



###################################################
### Create group centered level-1 predictor
###################################################

teams = nba %.%
	group_by(Team_ID) %.%
	summarize(teamMean = mean(Shots_on_five))

nba = merge(nba, teams, by = "Team_ID")	
nba$SO5 = nba$Shots_on_five - nba$teamMean

head(nba)
tail(nba)



###################################################
### Examine functional form of the relationship at level-1
###################################################

# Linear
ggplot(data = nba, aes(x = SO5, y = Life_Satisfaction)) +
	geom_point() +
	geom_smooth(se = FALSE) +
	theme_bw() +
	facet_wrap(~Team_ID)	

# Quadratic
ggplot(data = nba, aes(x = SO5, y = Life_Satisfaction, group = Team_ID)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2)) +
	theme_bw() +
	facet_wrap(~Team_ID)



###################################################
### Examine functional form of the relationship at level-1
###################################################

# Fit linear model
lmer.l = lmer(Life_Satisfaction ~ 1 + SO5 + (1  | Team_ID), 
	data = nba, REML = FALSE)

# Fit quadratic model
lmer.q = lmer(Life_Satisfaction ~ 1 + SO5 + I(SO5 ^ 2) + (1  | Team_ID), 
	data = nba, REML = FALSE)

anova(lmer.l, lmer.q)


###################################################
### RE of intercept and slope
###################################################

lmer.l3 = lmer(Life_Satisfaction ~ 1 + SO5 + (1 + SO5 | Team_ID), data = nba, REML = FALSE)

# Difference in deviance test
anova(lmer.l, lmer.l3)



###################################################
### Indepedent RE of intercept and slope
###################################################

lmer.l4 = lmer(Life_Satisfaction ~ 1 + SO5 + (1 | Team_ID) + (0 + SO5 | Team_ID), 
	data = nba, REML = FALSE)

# Estimates of the variance-covariance matrix of the random effects
VarCorr(lmer.l4)$Team_ID


# Difference in deviance test
anova(lmer.l, lmer.l4)

# Difference in deviance test
anova(lmer.l4, lmer.l3)



###################################################
### Add the level-2 predictor
###################################################

# Level-2 predictor for intercept only
lmer.i = lmer(Life_Satisfaction ~ 1 + SO5 + Coach_Experience + 
	(1 | Team_ID) + (0 + SO5 | Team_ID), data = nba, REML = FALSE)

anova(lmer.l4, lmer.i)


# Level-2 predictor for intercept and slope
lmer.i.s = lmer(Life_Satisfaction ~ 1 + SO5 + Coach_Experience + SO5:Coach_Experience +
	(1 | Team_ID) + (0 + SO5 | Team_ID), data = nba, REML = FALSE)

anova(lmer.i, lmer.i.s)




###################################################
### "Final" model
###################################################

# Refit with REML for better variance estimates
lmer.i = lmer(Life_Satisfaction ~ 1 + SO5 + Coach_Experience + 
	(1 | Team_ID) + (0 + SO5 | Team_ID), data = nba)



###################################################
### Examine the level-1 residuals
###################################################

library(sm)
sm.density(resid(lmer.i), model = "normal")



###################################################
### Examine the level-2 residuals
###################################################

# Intercept
re.i = ranef(lmer.i)$Team_ID[ , 1]
sm.density(re.i, model = "normal")


# Slope
re.s = ranef(lmer.i)$Team_ID[ , 2]
sm.density(re.s, model = "normal")



###################################################
### Fit models for changes in proportion of variance
###################################################

lmer.u1 = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba)
lmer.u2 = lmer(Life_Satisfaction ~ 1 + SO5 + (1 | Team_ID) + (0 + SO5 | Team_ID), data = nba)

summary(lmer.u1)
AIC(lmer.u1)
BIC(lmer.u1)
cor(nba$Life_Satisfaction, fitted(lmer.u1)) ^ 2

summary(lmer.u2)
AIC(lmer.u2)
BIC(lmer.u2)
cor(nba$Life_Satisfaction, fitted(lmer.u2)) ^ 2


summary(lmer.i)
AIC(lmer.i)
BIC(lmer.i)
cor(nba$Life_Satisfaction, fitted(lmer.i)) ^ 2



###################################################
### Plot of the Fitted Model
###################################################

myData = expand.grid(
	SO5 = seq(from = -3.5, to = 2.5, by = 0.1),
	Coach_Experience = c(1, 2, 3)
	)

fixef(lmer.i)

myData$fitted = fixef(lmer.i)[[1]] + fixef(lmer.i)[[2]] * myData$SO5 + 
    fixef(lmer.i)[[3]] * myData$Coach_Experience

head(myData)

myData$Coach_Experience = factor(
	myData$Coach_Experience,
	levels = c(1, 2, 3),
	labels = c("Low", "Moderate", "High")
	)

head(myData)

ggplot(data = myData, aes(x = SO5, y = fitted, color = Coach_Experience)) +
	geom_line(lwd = 1.5) +
	scale_color_brewer(palette = "Paired", name = "Coaching Experience") +
	theme_bw() +
	xlab("Shooting Success") +
	ylab("Life Satisfaction Score")

