###################################################
### Read in the SPSS data
###################################################

library(foreign)

nbaL1 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "~/Documents/EPsy-8252/data/nbaLevel2.sav", to.data.frame = TRUE)

head(nbaL1)
head(nbaL2)



###################################################
### Merge the player-level data and the team-level data
###################################################

nba = merge(nbaL1, nbaL2, by = "Team_ID")

head(nba)
tail(nba)



###################################################
### Fit linear model
###################################################

lm.a = lm(Life_Satisfaction ~ 1 + Shots_on_five, data = nba)
summary(lm.a)



###################################################
### Examine residuals
###################################################

library(ggplot2)
out.a = fortify(lm.a)

# Normality
library(sm)
sm.density(out.a$.stdresid)

# Everything else
ggplot(data = out.a, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	theme_bw()


