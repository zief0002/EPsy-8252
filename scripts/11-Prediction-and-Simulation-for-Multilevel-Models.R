###################################################
### Read in the NFL FCI Data
###################################################

nfl = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/FCI-NFL-2014.csv")
meta = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/NFL-Meta-Data.csv")

# Merge meta data into nfl data frame
nfl = merge(nfl, meta, by = "team")

# Create age of stadium variable
nfl$ageStadium = 2014 - nfl$yearOpened

# Create log of outcome
nfl$Lfci = log(nfl$fci)

# Create log of coachYrswTeam
nfl$LcoachYrswTeam = log(nfl$coachYrswTeam + 1)




###################################################
### Fit model
###################################################

lm.a = lm(Lfci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam, data = nfl)
summary(lm.a)



###################################################
### Predict for 2016 Vikings
###################################################

myData = data.frame(
	ageStadium = c(7, 59),
	LcoachYrswTeam = c(1.098612, 2.397895)
	)

myData

predict(lm.a, newdata = myData)


myMatrix = cbind(
     rep(1, 2),
	c(7, 59),
	c(49, 3481),
	c(1.098612, 2.397895)
	)
myMatrix

coef1 = matrix(coef(lm.a))
coef1

myMatrix %*% coef1

library(arm)
mySim = sim(lm.a, 1)
mySim

mySim@sigma


# Get predicted values
myYhats = myMatrix %*% mySim@coef[1, ]

rnorm(2, mean = 0, sd = mySim@sigma)





###################################################
### Fit LMER model
###################################################

library(foreign)

nbaL1 = read.spss(file = "http://www.tc.umn.edu/~zief0002/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "http://www.tc.umn.edu/~zief0002/data/nbaLevel2.sav", to.data.frame = TRUE)
nba = merge(nbaL1, nbaL2, by = "Team_ID")
teams = nba %>%
	group_by(Team_ID) %>%
	summarize(teamMean = mean(Shots_on_five))

nba = merge(nba, teams, by = "Team_ID")	
nba$SO5 = nba$Shots_on_five - nba$teamMean

lmer.1 = lmer(Life_Satisfaction ~ 1 + SO5 + Coach_Experience + 
	(1 | Team_ID) + (0 + SO5 | Team_ID), data = nba)

# Get variance estimates
sigma.hat(lmer.1)


within.team.error = sigma.hat(lmer.1)$sigma$data

coef(lmer.1)



###################################################
### Predict for new observation from existing group (Team 10)
###################################################

# Get team-level regression coefficients for team 10
coef(lmer.1)$Team_ID[10, ]

# Put those coefficients into a vector
team.betas = as.matrix(coef(lmer.1)$Team_ID[10, ])
team.betas

myMatrix = cbind(1, -1.2, 2 )
myMatrix

yhat = myMatrix %*% t(team.betas)
yhat

rnorm(1, yhat, within.team.error)

mySim = rnorm(1000, yhat, within.team.error)
quantile(mySim, probs = c(0.025, 0.975))



###################################################
### Predict for new observation from new group
###################################################

Coach_Experience = mean(nba$Coach_Experience)

# Get estimated coefficients
fixef(lmer.1)

# Simulate B*0
b00 = fixef(lmer.1)["(Intercept)"]
b01 = fixef(lmer.1)["Coach_Experience"]

s0j = sigma.hat(lmer.1)$sigma$Team_ID

bstar0 = rnorm(1, mean = (b00 + b01*Coach_Experience), sd = s0j)
bstar0

# SImulate B*1
b10 = fixef(lmer.1)["SO5"]

s1j = sigma.hat(lmer.1)$sigma$Team_ID.1

bstar1 = rnorm(1, mean = (b10), sd = s1j)
bstar1

# Simulate Y
SO5 = mean(nba$SO5)

within.team.error = sigma.hat(lmer.1)$sigma$data

Y = rnorm(1, mean = (bstar0 + bstar1*SO5), sd = within.team.error)
Y

