###################################################
### Read in data
###################################################

mplsWide = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/minneapolis.csv")
mplsWide


###################################################
### Examine wide data
###################################################

summary(mplsWide[2:5])
sapply(mplsWide[2:5], mean, na.rm = TRUE)
sapply(mplsWide[2:5], sd, na.rm = TRUE)

sapply(mplsWide[2:5], sd)



# Examiine variance-covariance matrix
var(mplsWide[2:5], use = "pairwise.complete.obs")

# Examine correlation matrix
cor(mplsWide[2:5], use = "pairwise.complete.obs")



###################################################
### Load libraries
###################################################

library(reshape2)
library(ggplot2)
library(lme4)
library(sm)




###################################################
### Prepare data
###################################################

# Create long data from wide data (need reshape2 package loaded)
mpls = melt(
	mplsWide, 
	id = c("studentID", "atRisk", "female", "minority", "ell", "sped", "att"),
	measure = c("read.5", "read.6", "read.7", "read.8")
	)

library(dplyr)
mpls = mpls %>% arrange(studentID)
head(mpls, 12)

# Rename the columns
names(mpls)[8] = "grade"
names(mpls)[9] = "read"

# Change grade predictor to an integer
mpls$grade = as.integer(mpls$grade)
head(mpls)


# Add 4 to get back to 5, 6, 7, and 8
mpls$grade = mpls$grade + 4
head(mpls)

# Remove missing data
mpls = na.omit(mpls)


head(mpls, 12)


summary(lm(read ~ grade, data = mpls))



###################################################
### Examine Functional Form for Level-1 Model
###################################################

# Plot trajectories using raw data
ggplot(data = mpls, aes(x = grade, y = read)) +
	geom_point() +
	geom_line(aes(group = studentID)) +
	facet_wrap(~studentID) +
	theme_bw()

# Show regression lines
ggplot(data = mpls, aes(x = grade, y = read)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~studentID) +
	theme_bw()	

# Put all of the regression lines on one plot
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()	



###################################################
### Fit the unconditional varying intercepts model
### Initial status: (no predictors)
###################################################

model.a = lmer(read ~ 1  + (1 | studentID), data = mpls, REML = FALSE)
summary(model.a)



###################################################
### Fit the growth model with varying intercepts and slopes
### Intercept: (no predictors)
### Slope: (no predictors)
###################################################

# Uncentered model
model.b = lmer(read ~ 1 + grade  + (1 + grade | studentID), data = mpls, REML = FALSE)
summary(model.b)

# Get covariance
VarCorr(model.b)$studentID



###################################################
### Center the grade predictor at grade 5
###################################################

mpls$c.grade = mpls$grade - 5
head(mpls)

# Fit the model
model.b2 = lmer(read ~ 1 + c.grade  + (1 + c.grade | studentID), data = mpls, REML = FALSE)
summary(model.b2)

# Get variance-covariance matrix of the random effects
VarCorr(model.b2)$studentID        




###################################################
### Plot of Fixed-Effects Model B2
###################################################

plotData = data.frame(
	c.grade = 0:3
	)

# The predict() function WILL NOT WORK to plot the fixed-effects model.
# Use fixef() function
fixef(model.b2)

# Compute y-hat values for the fixed-effects (average) model
plotData$yhat = fixef(model.b2)["(Intercept)"] + fixef(model.b2)["c.grade"] * plotData$c.grade

# View plotData
plotData

ggplot(data = plotData, aes(x = c.grade, y = yhat)) +
	geom_line() +
	theme_bw() +
	xlab("Grade (Centered at Grade 5)") +
	ylab("Predicted Reading Score")



###################################################
### Plot of Model B2 for each student
###################################################

# Get each students' RE
ranef(model.b2)

# What associated output do we get with an lmer() object? 
slotNames(model.b2)

# To set up the data, we grab the 
plotData = model.b2@frame

# Use predict()
plotData$yhat = predict(model.b2, newdata = plotData)

ggplot(data = plotData, aes(x = c.grade, y = yhat, group = studentID)) +
	geom_line() +
	theme_bw() +
	xlab("Grade (Centered at Grade 5)") +
	ylab("Predicted Reading Score")



###################################################
### Each student's fitted model
###################################################

# Get each students' RE
ranef(model.b2)

# Compute intercepts
205.745119 + ranef(model.b2)$studentID[1]

# Compute slopes
4.882322 + ranef(model.b2)$studentID[2]



###################################################
### Examine level-1 (within-student) residuals for 
### the unconditional and growth models
###################################################

# Unconditional model
resid.a = data.frame(
	studentID = model.a@frame$studentID,
	residual = resid(model.a),
	model = "Model A"
	)

# Growth model
resid.b2 = data.frame(
	studentID = model.b2@frame$studentID,
	residual = resid(model.b2),
	model = "Model B2"
	)

both.resid = rbind(resid.a, resid.b2)
head(both.resid)

## Plot the residuals by subject ID to 
ggplot(data = both.resid, aes(x = studentID, y = residual, group = studentID)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	geom_hline(yintercept = 0) +
	facet_grid(. ~ model) + 
	xlab("Subject") +
	ylab("Residual") + 
	coord_flip()






###################################################
### Fit the quadratic growth model with varying intercepts, linear terms, and quadratic terms
### Initial status: (no predictors)
### Linear Rate of change: (no predictors)
### Quadratic rate of change (no predictors)
###################################################

model.c = lmer(read ~ 1 + c.grade + I(c.grade ^ 2) + 
	(1 + c.grade + I(c.grade ^ 2) | studentID), data = mpls, REML = FALSE)

summary(model.c)

model.c = lmer(read ~ 1 + c.grade + I(c.grade ^ 2) + 
	(1 + c.grade | studentID), data = mpls, REML = FALSE)



###################################################
### Examine within-person variation for 
### the three unconditional models
###################################################

# Growth (quadratic) model
resid.c = data.frame(
	studentID = model.c@frame$studentID,
	residual = resid(model.c),
	model = "Model C"
	)

## Stack the variables into a new data frame
growth.resid = rbind(resid.b2, resid.c)

## Plot the residuals by subject ID to 
ggplot(data = growth.resid, aes(x = studentID, y = residual, group = studentID)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	geom_hline(yintercept = 0) +
	facet_grid(. ~ model) + 
	xlab("Subject") +
	ylab("Residual") + 
	coord_flip()




###################################################
### Level-2 Predictors: at Risk
###################################################

# atRisk
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~atRisk) +
	theme_bw()

# minority
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~minority) +
	theme_bw()

# female
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~female) +
	theme_bw()

# ell
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~ell) +
	theme_bw()

# sped
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~sped) +
	theme_bw()



###################################################
### Fit the linear growth model (level-1) w/varying intercepts and slopes
### Initial status: atRisk
### Rate of change: atRisk
###################################################

model.d = lmer(read ~ 1 + c.grade + atRisk  + atRisk:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
summary(model.d)

model.e = lmer(read ~ 1 + c.grade + atRisk  + (1 + c.grade | studentID), data = mpls, REML = FALSE)
summary(model.e)



plotData = expand.grid(
	c.grade = 0:3,
	atRisk = 0:1
	)

# Use fixef() function
fixef(model.e)

# Compute y-hat values for the fixed-effects (average) model
plotData$yhat = fixef(model.e)["(Intercept)"] + 
                fixef(model.e)["c.grade"] * plotData$c.grade +
                fixef(model.e)["atRisk"] * plotData$atRisk

# View plotData
plotData

plotData$atRisk = factor(plotData$atRisk,
	levels = c(0, 1),
	labels = c("Not at-risk", "At-risk")
	)

ggplot(data = plotData, aes(x = c.grade, y = yhat, color = atRisk)) +
	geom_line() +
	theme_bw() +
	xlab("Grade (Centered at Grade 5)") +
	ylab("Predicted Reading Score") +
	scale_color_brewer(palette = "Set1", name = "")


plotData = model.d@frame

# Use predict()
plotData$yhat = predict(model.e, newdata = plotData)

plotData$atRisk = factor(plotData$atRisk,
	levels = c(0, 1),
	labels = c("Not at-risk", "At-risk")
	)

ggplot(data = plotData, aes(x = c.grade, y = yhat, group = studentID, color = atRisk)) +
	geom_line() +
	theme_bw() +
	xlab("Grade (Centered at Grade 5)") +
	ylab("Predicted Reading Score") +
	scale_color_brewer(palette = "Set1", name = "")



###################################################
### Model comparisons
###################################################

logLik(model.a)
logLik(model.b2)

# Deviance test for the three models
anova(model.a, model.b2, model.d, model.e)

library(MuMIn)

AICc(model.a)
AICc(model.b2)
AICc(model.e)
AICc(model.d)





###################################################
### Plot the fitted line for the unconditional growth model
###################################################

plotData = data.frame(
	time = 0:3
	)

plotData$yhat = fixef(model.c)["(Intercept)"] + fixef(model.c)["time"] * plotData$time
plotData$grade = plotData$time + 5

ggplot(data = plotData, aes(x = grade, y = yhat)) +
	geom_line(color = "blue") +
	xlab("Grade") +
	ylab("Reading Score") +
	theme_bw()



###################################################
### Compute pseudo R-squared values: Unconditional models
###################################################

# R^2_(y,y-hat)
cor(fitted(model.a), mpls$read) ^ 2

cor(fitted(model.a), na.omit(mpls$read)) ^ 2

# ICC
319.3 / (319.3 + 66.2)




###################################################
### Compute pseudo R-squared values: Growth model (b2)
###################################################

# R^2_(y,y-hat)
cor(fitted(model.b2), na.omit(mpls$read)) ^ 2


#R^2_e
(66.2 - 18.315) / 66.2



###################################################
### Compute pseudo R-squared values: Growth model with intercept predictor of risk (Model E)
###################################################

# R^2_(y,y-hat)
cor(fitted(model.e), na.omit(mpls$read)) ^ 2

#R^2_e
(66.2 - 18.129) / 66.2

#R^2_(b0)
(380.586 - 266.912) / 380.586

#R^2_(b1)
(6.966 - 7.164) / 6.966




###################################################
### Other Level-2 Predictors
###################################################

# minority
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~minority) +
	theme_bw()

# female
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~female) +
	theme_bw()

# ell
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~ell) +
	theme_bw()

# sped
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~sped) +
	theme_bw()














###################################################
### Examine the within-person residuals from the final model
###################################################

# Examine normality of residuals 
# (replace model.i with the name of your adopted model)
sm.density(resid(model.i), model = "norm")



###################################################
### Evaluating Normality Assumption of model random effects
###################################################

## Get model random effects and put in data frame
# (replace model.i with the name of your adopted model)
library(plyr)
ranI = ranef(model.i)$studentID

## Give the columns a name
names(ranI) = c("eta0", "eta1")


## Mean of the Random Effects
## round(colMeans(my.c.re), 8)

## Density plot b0i
sm.density(ranI$eta0, model = "norm")

## Density plot b1i
sm.density(ranI$eta1, model = "norm")




