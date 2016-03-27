###################################################
### Load libraries
###################################################

library(AICcmodavg)
library(dplyr)
library(foreign)
library(ggplot2)
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
### Analysis Option #1: Aggregate group-level analysis
###################################################

# Mean SO5 and LS for each team
nba2 = nba %>%
	group_by(Team_ID) %>%
	summarize(
		Shots = mean(Shots_on_five),
		LS = mean(Life_Satisfaction)
		)


ggplot(data = nba2, aes(x = Shots, y = LS)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()


lm.group = lm(LS ~ Shots, data = nba2)
summary(lm.group)


# Fortify the model
out.g = fortify(lm.group)

### Residual analysis
sm.density(out.g$.stdresid, model = "normal")

ggplot(data = out.g, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()



###################################################
### Analysis Option #2: Include team as a covariate
###################################################

# Make sure Team_ID is a factor; if not coerce it into a factor
str(nba)

lm.cov = lm(Life_Satisfaction ~ 1 + Shots_on_five + Team_ID, data = nba)
summary(lm.cov)


# Fortify model and add Team_ID variable to fotified data
out.c = fortify(lm.cov)
out.c$Team_ID = nba$Team_ID
head(out.c)


### Show residuals by team
ggplot(data = out.c, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  facet_wrap(~Team_ID)



###################################################
### Fit regression for each team
###################################################

library(lme4)
lmList(Life_Satisfaction ~ Shots_on_five | Team_ID, data = nba)

# Fit regression for each group
regressions = nba %>% 
	group_by(Team_ID) %>%
    do(model = lm(Life_Satisfaction ~ Shots_on_five, .))

# Pull out intercepts and slopes
new = regressions %>% summarise(
	Intercept = coef(model)[1],
	Slope = coef(model)[2],
	RMSE = summary(model)[[6]]
	)

# Show plot of fitted lines (extrapolated)
ggplot(data = nba, aes(x = Shots_on_five, y = Life_Satisfaction, group = Team_ID)) +
	geom_point(size = 0) +
	geom_abline(data = new, aes(intercept = Intercept, slope = Slope)) +
	theme_bw() +
	xlim(-1, 6) +
	ylim(0, 40)


mean(new$Intercept)
mean(new$Slope)
mean(new$RMSE) ^ 2



###################################################
### Fit regression for each team
###################################################

# Easier plot of each team's linear fitted model
ggplot(data = nba, aes(x = Shots_on_five, y = Life_Satisfaction, group = Team_ID)) +
	geom_smooth(se = FALSE, method = "lm") +
	theme_bw()


