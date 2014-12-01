###################################################
### Read in data
###################################################

mpls = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/minneapolis.csv")



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

# Create long data from wide data
mpls = melt(
	mpls, 
	id = c("studentID", "atRisk", "female", "minority", "ell", "sped", "att"),
	measure = c("read.5", "read.6", "read.7", "read.8")
	)

# Rename the columns
names(mpls)[8] = "grade"
names(mpls)[9] = "read"

# Change grade predictor to an integer
mpls$grade = as.integer(mpls$grade)
mpls$grade = mpls$grade + 4

# Remove missing data
mpls = na.omit(mpls)


head(mpls)




###################################################
### Examine Functional Form for Level-1 Model
###################################################

ggplot(data = mpls, aes(x = grade, y = read)) +
	geom_point() +
	geom_line(aes(group = studentID)) +
	facet_wrap(~studentID) +
	theme_bw()



###################################################
### Fit the unconditional means model
### Initial status: (none)
###################################################

model.a = lmer(read ~ 1  + (1 | studentID), data = mpls, REML = FALSE)
summary(model.a)



###################################################
### Fit the unconditional growth model
### Initial status: (none)
### Rate of change: (none)
###################################################

# Uncentered model
model.b = lmer(read ~ 1 + grade  + (1 + grade | studentID), data = mpls, REML = FALSE)
summary(model.b)


# Center the grade predictor at grade 5
mpls$time = mpls$grade - 5

model.c = lmer(read ~ 1 + time  + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.c)



###################################################
### Fit the unconditional growth model
### Initial status: (none)
### Linear Rate of change: (none)
### Quadratic rate of change (none)
###################################################

model.d = lmer(read ~ 1 + time + I(time ^ 2) + (1 + time + I(time ^ 2) | studentID), data = mpls, REML = FALSE)
summary(model.d)



###################################################
### Examine within-person variation for 
### the three unconditional models
###################################################

# Unconditional means model
my.resid.a = data.frame(
	studentID = model.a@frame$studentID,
	residual = resid(model.a),
	model = "Model A"
	)

# Unconditional growth model
my.resid.c = data.frame(
	studentID = model.c@frame$studentID,
	residual = resid(model.c),
	model = "Model C"
	)

# Unconditional growth (quadratic) model
my.resid.d = data.frame(
	studentID = model.d@frame$studentID,
	residual = resid(model.d),
	model = "Model D"
	)

## Stack the variables into a new data frame
my.resid = rbind(my.resid.a, my.resid.c, my.resid.d)

## Plot the residuals by subject ID to 
ggplot(data = my.resid, aes(x = studentID, y = residual, group = studentID)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	geom_hline(yintercept = 0) +
	facet_grid(. ~ model) + 
	xlab("Subject") +
	ylab("Residual") + 
	coord_flip()




###################################################
### Deviance test for the three models
###################################################

anova(model.a, model.c, model.d)



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
### Compute pseudo R-squared values
###################################################

# R^2_(y,y-hat)
cor(fitted(model.b), mpls$read) ^ 2

#R^2_e = Var(e, unconditional means) - Var(e, unconditional growth) / Var(e, unconditional means)
(66.2 - 18.315) / 66.2



###################################################
### Level-2 Between-Subjects Model
###################################################

# Risk
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~atRisk) +
	theme_bw()

# Ethnicity
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_smooth(method = "lm", se = FALSE) +
	facet_wrap(~minority) +
	theme_bw()




###################################################
### Fit the growth model w/uncontrolled effects of risk
### Initial status: risk
### Rate of change: risk
###################################################

model.e = lmer(read ~ 1 + time + atRisk  + atRisk:time + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.e)


# R^2_(y,y-hat)
cor(fitted(model.e), mpls$read) ^ 2

#R^2_e
(66.2 - 18.244) / 66.2

#R^2_(b0)
(380.586 - 266.711) / 380.586

#R^2_(b1)
(6.966 - 6.949) / 6.966



###################################################
### Examine within-person variation for 
### Model C vs. Model B
###################################################

my.resid.e = data.frame(
	studentID = model.e@frame$studentID,
	residual = resid(model.e),
	model = "Model E"
	)

my.resid = rbind(my.resid.c, my.resid.e)

ggplot(data = my.resid, aes(x = studentID, y = residual, group = studentID)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	coord_flip() + 
	geom_hline(yintercept = 0) +
	facet_grid(. ~ model) + 
	ylab("Residual") + 
	xlab("Subject")



###################################################
### Examine between-person variation for 
### Model C vs. Model E
###################################################

## Get model random effects from Model C and put in data frame
ranC = ranef(model.c)$studentID
names(ranC) = c("b0i", "b1i")
ranC$model = "Model C"

## Get model random effects from Model E and put in data frame
ranE = ranef(model.e)$studentID
names(ranE) <- c("b0i", "b1i")
ranE$model <- "Model E"

## Put both in the same data frame
myRE = rbind(ranC, ranE)

## Compare intercept random effects
ggplot(data = myRE, aes(x = model, y = b0i)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	geom_hline(yintercept = 0) +
	xlab("Model") +
	ylab("Intercept Random Effect") + 
	coord_flip()

## Compare slope random effects
ggplot(data = myRE, aes(x = model, y = b1i)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	geom_hline(yintercept = 0) +
	xlab("Model") +
	ylab("Slope Random Effect") + 
	coord_flip()




###################################################
### Fit the growth model w/controlled effects of risk
### Initial status: risk, ethnicity
### Rate of change: risk, ethnicity
###################################################

model.f = lmer(read ~ 1 + time + atRisk + minority + atRisk:time + minority:time + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.f)

# Covariance
13.7386 * 2.5548 * -0.269

# R^2_(y,y-hat)
cor(fitted(model.f), mpls$read) ^ 2

#R^2_e
(66.2 - 18.2310) / 66.2

#R^2_(b0)
(380.5857 - 188.7491) / 380.5857

#R^2_(b1)
(6.9662 - 6.5268) / 6.9662



###################################################
### Fit the growth model w/controlled effects of risk
### Initial status: risk, ethnicity
### Rate of change: ethnicity
###################################################

model.g = lmer(read ~ 1 + time + atRisk + minority + time:minority + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.g)

# Covariance
13.7404 * 2.5476 * -0.271

# R^2_(y,y-hat)
cor(fitted(model.g), mpls$read) ^ 2

#R^2_e
(66.203 - 18.2723) / 66.203

#R^2_(b0)
(380.5857 - 188.7986) / 380.5857

#R^2_(b1)
(6.9662 - 6.4905) / 6.9662



###################################################
### Fit the growth model w/controlled effects of risk
### Initial status: risk, ethnicity
### Rate of change: (none)
###################################################

model.h = lmer(read ~ 1 + time + atRisk + minority + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.h)

# Covariance
13.7975 * 2.6907 * -0.287

# R^2_(y,y-hat)
cor(fitted(model.h), mpls$read) ^ 2

#R^2_e
(66.203 - 18.1078) / 66.203

#R^2_(b0)
(380.5857 - 190.3723) / 380.5857

#R^2_(b1)
(6.9662 - 7.2398) / 6.9662



###################################################
### Fit the growth model w/controlled effects of risk
### Initial status: ethnicity, attendance
### Rate of change: ethnicity, attendance
###################################################

model.i = lmer(read ~ 1 + minority + att + time + time:minority + time:att + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.i)

# Covariance
1.6576 * 14.0446 * -0.122

# R^2_(y,y-hat)
cor(fitted(model.i), mpls$read) ^ 2

#R^2_e
(66.203 - 17.9258) / 66.203

#R^2_(b0)
(380.5857 - 197.2500) / 380.5857

#R^2_(b1)
(6.9662 - 2.7478) / 6.9662




###################################################
### Examine the within-person residuals from the final model
###################################################

# Examine normality of residuals
sm.density(resid(model.i), model = "norm")



###################################################
### Evaluating Normality Assumption of model random effects
###################################################

## Get model random effects and put in data frame
library(plyr)
ranI = ranef(model.i)$studentID

## Give the columns a name
names(ranI) = c("b0i", "b1i")


## Mean of the Random Effects
## round(colMeans(my.c.re), 8)

## Density plot b0i
sm.density(ranI$b0i, model = "norm")

## Density plot b1i
sm.density(ranI$b1i, model = "norm")




###################################################
### Plot Fitted Model
### Model I
###################################################

# Find prototypical values for attendance
quantile(mpls$att, probs = c(.25, .75))


plotData = expand.grid(
	time = 0:3,
	minority = c(0, 1),
	att = c(0.95, 0.98)
	)

plotData$yhat = fixef(model.i)["(Intercept)"] + 
	fixef(model.i)["minority"] * plotData$minority +
	fixef(model.i)["att"] * plotData$att +
	fixef(model.i)["time"] * plotData$time +
	fixef(model.i)["minority:time"] * plotData$minority * plotData$time +
	fixef(model.i)["att:time"] * plotData$att * plotData$time

plotData$grade = plotData$time + 5

plotData$minority = factor(plotData$minority,
	levels = c(0, 1),
	labels = c("White Students", "Non-White Students")
	)

plotData$att = factor(plotData$att,
	levels = c(0.95, 0.98),
	labels = c("Low", "High")
	)


ggplot(data = plotData, aes(x = grade, y = yhat, color = att, linetype = minority)) +
	geom_line() +
	xlab("Grade") +
	ylab("Reading Score") +
	annotate("text", x = 6.5, y = 235, label = "White Students", size = 3) +
	annotate("text", x = 6.5, y = 210, label = "Non-White Students", size = 3) +
	theme_bw() +
	scale_linetype(guide = FALSE) +
	scale_color_brewer(name = "Attendance Rate", palette = "Set1")





###################################################
### Centering continuous predictors
### Model J
###################################################

# Center the attendance predictor
mpls$c.att = mpls$att - mean(mpls$att)

model.j = lmer(read ~ 1 + minority + c.att + time + time:minority + time:c.att + (1 + time | studentID), data = mpls, REML = FALSE)
summary(model.j)

# Covariance
1.6576 * 14.0446 * -0.122

# R^2_(y,y-hat)
cor(fitted(model.h), mpls$read) ^ 2

#R^2_e
(66.203 - 17.9258) / 66.203

#R^2_(b0)
(380.5857 - 197.2500) / 380.5857

#R^2_(b1)
(6.9662 - 2.7478) / 6.9662



###################################################
### Get estimates for random effects
###################################################

model.i = lmer(read ~ 1 + minority + att + time + time:minority + time:att + (1 + time | studentID), data = mpls)
summary(model.i)

model.j = lmer(read ~ 1 + minority + c.att + time + time:minority + time:c.att + (1 + time | studentID), data = mpls)
summary(model.j)

