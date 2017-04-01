#########################################
## Read in data
#########################################

library(readr)
mplsWide = read_csv(file = "~/Google Drive/Documents/EPsy-8252/data/minneapolis.csv")
head(mplsWide)



#########################################
## Load libraries
#########################################

library(AICcmodavg)
library(broom)
library(dplyr)
library(ggplot2)
library(lme4)
library(sm)
library(tidyr)



#########################################
## Create long data from wide data (need tidyr package loaded)
#########################################

mpls = mplsWide %>% gather(
  key = time, # Name of the variable that delineates the time points
  value = read, # Name of the outcome
  c(read.5, read.6, read.7, read.8) # The old variables you will use as values
  )


# Arrange by ID number
mpls = mpls %>% arrange(studentID)
head(mpls, 12)


# Turn time into a factor
mpls = mpls %>% mutate(time = as.factor(time))

# Use as.integer() to turn factor into numbers
mpls = mpls %>% mutate(grade = as.integer(time))
head(mpls)

# Use as.integer() to turn factor into numbers
mpls = mpls %>% mutate(grade = as.integer(time) + 4)
head(mpls)



#########################################
## Exploratory analysis of reading scores over time
#########################################

mpls %>% 
  group_by(time) %>% 
  summarize(M = mean(read), SD = sd(read))



#########################################
## Missingness
#########################################

mpls %>% 
  group_by(time) %>% 
  summarize(
    Miss = sum(is.na(read)), 
    Miss_Percent = sum(is.na(read))/length(read)
    )



#########################################
## Means and SDs
#########################################

mpls %>% 
  filter(!is.na(read)) %>%
  group_by(time) %>% 
  summarize(M = mean(read), SD = sd(read))



#########################################
## Correlation
#########################################

cor(mplsWide[2:5], use = "pairwise.complete.obs")



#########################################
## Spaghetti plot
#########################################

ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	#geom_point() + 
  geom_smooth(aes(group = 1), se = FALSE, lwd = 1.5) +
	geom_line(alpha = 0.3) +
	theme_bw()


# Facet on student ID
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
	geom_point() +
	geom_line() +
  geom_smooth(aes(group = 1), se = FALSE, method = "lm") +
	theme_bw() +
  facet_wrap(~studentID)



#########################################
## Fit unconditional random intercepts model
#########################################

lmer.0 = lmer(read ~ 1  + (1 | studentID), data = mpls)
summary(lmer.0)



#########################################
## Explore level-1 fixed-effects
#########################################

# Fit models using ML
lmer.0.ml = lmer(read ~ 1  + (1 | studentID), data = mpls, REML = FALSE)
lmer.1.ml = lmer(read ~ 1  + grade + (1 | studentID), data = mpls, REML = FALSE)
lmer.2.ml = lmer(read ~ 1  + grade + I(grade^2) + (1 | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0.ml, lmer.1.ml, lmer.2.ml),
  modnames = c("Unconditional", "Grade level (L)", "Grade level (Q)")
)

myAIC



#########################################
## Explore model residuals for fit
#########################################

# Unconditional means model level-1 residuals
resid.0 = augment(lmer.0.ml) %>% select(studentID, .resid) %>% mutate(model = "lmer.0")

# Unconditional growth model (linear) level-1 residuals
resid.1 = augment(lmer.1.ml) %>% select(studentID, .resid) %>% mutate(model = "lmer.1")

# Unconditional growth model (quadratic) level-1 residuals
resid.2 = augment(lmer.2.ml) %>% select(studentID, .resid) %>% mutate(model = "lmer.2")

# Stack the three sets of residuals
all.resid = rbind(resid.0, resid.1, resid.2)

# Obtain only Student 1's residuals
res.1 = all.resid %>% filter(studentID == 1)
res.1

# COmpute SD of residuals
res.1 %>% group_by(model) %>% summarize(SD = sd(.resid))



#########################################
## Boxplots of models' residuals
#########################################

ggplot(data = all.resid, aes(x = studentID, y = .resid, group = studentID)) + 
	theme_bw() +
	geom_boxplot(fill = "grey80") + 
	geom_hline(yintercept = 0) +
	facet_wrap(~ model) + 
	xlab("Student ID") +
	ylab("Level-1 Residual") + 
	coord_flip()



#########################################
## Explore random-effects structure
#########################################

lmer.1.int = lmer(read ~ 1  + grade + (1 | studentID), data = mpls)
lmer.1.both = lmer(read ~ 1  + grade + (1 + grade | studentID), data = mpls)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.1.int, lmer.1.both),
  modnames = c("Random Intercepts", "Random Intercepts + Slopes")
)

myAIC


#########################################
## Examine unconditional growth model
#########################################

summary(lmer.1.both)

# Get variance-covariance matrix of the random effects
VarCorr(lmer.1.both)$studentID



#########################################
## Center grade level
#########################################

mpls$c.grade = mpls$grade - 5
head(mpls)  

# Fit unconditional growth model with centered grade level
lmer.1.c = lmer(read ~ 1 + c.grade  + (1 + c.grade | studentID), data = mpls)
summary(lmer.1.c)

# Variance-covariance matrix of random-effects
VarCorr(lmer.1.c)$studentID 



#########################################
## Plot of the fixed-effects
#########################################

plotData = expand.grid(
	c.grade = 0:3
	)

# Compute y-hat values for the fixed-effects (average) model
plotData$yhat = predict(lmer.1.c, newdata = plotData, re.form = NA)
head(plotData)

# Plot
ggplot(data = plotData, aes(x = c.grade, y = yhat)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Grade level", breaks = 0:3, labels = c("5", "6", "7", "8")) +
  ylab("Predicted reading score")



#########################################
## Plot of the individual growth trajectories and the fixed-effects
#########################################

student_data = expand.grid(
	c.grade = 0:3,
	studentID = 1:22
	)

# Use predict() to add the y-hat values
student_data$yhat = predict(lmer.1.c, newdata = student_data)  

# Plot
ggplot(data = student_data, aes(x = c.grade, y = yhat, group = studentID)) +
	geom_line(alpha = 0.3) +
  geom_line(data = plotData, aes(x = c.grade, y = yhat, group = 1), color = "blue", lwd = 1.5) +
  theme_bw() +
  scale_x_continuous(name = "Grade level", breaks = 0:3, labels = c("5", "6", "7", "8")) +
  ylab("Predicted reading score")



#########################################
## Explore atRisk predictor
#########################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + atRisk + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + atRisk + atRisk:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "atRisk - Intercept", "atRisk - Intercept + Slope")
)

myAIC



#########################################
## Explore female predictor
#########################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + female + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + female + female:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "female - Intercept", "female - Intercept + Slope")
)

myAIC



#########################################
## Explore minority predictor
#########################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + minority + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + minority + minority:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "minority - Intercept", "minority - Intercept + Slope")
)

myAIC



#########################################
## Explore ELL predictor
#########################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + ell + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + ell + ell:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "ell - Intercept", "ell - Intercept + Slope")
)

myAIC



#########################################
## Explore special education predictor
#########################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + sped + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + sped + sped:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "sped - Intercept", "sped - Intercept + Slope")
)

myAIC



#########################################
## Explore attendance rate predictor
#########################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + att + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + att + att:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)

# AICc selection
myAIC = aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "att - Intercept", "att - Intercept + Slope")
)

myAIC



#########################################
## Fit model with adopted effects
#########################################

lmer.final = lmer(read ~ 1 + c.grade + atRisk + minority + att + 
      att:c.grade + (1 + c.grade | studentID), data = mpls)
summary(lmer.final)$coefficients



#########################################
## Fit other models dropping terms
#########################################

# Drop att:time interaction term
lmer.final2 = lmer(read ~ 1 + c.grade + atRisk + minority + att + 
      + (1 + c.grade | studentID), data = mpls)

# Drop att:time and att main-effect
lmer.final3 = lmer(read ~ 1 + c.grade + atRisk + minority + 
      + (1 + c.grade | studentID), data = mpls)

# Drop att:time and att and atRisk main-effects
lmer.final4 = lmer(read ~ 1 + c.grade +  minority + 
      (1 + c.grade | studentID), data = mpls)


# AICc selection
myAIC = aictab(
  cand.set = list(lmer.final, lmer.final2, lmer.final3, lmer.final4),
  modnames = c("Full model", "-att:c.grade", "-att:c.grade, -att", "-att:c.grade, -att, -atRisk")
)

myAIC



#########################################
## Check assumptions of final adopted model
#########################################

out = augment(lmer.final)
#head(out)

# Normality of level-1 residuals
sm.density(out$.resid, model = "normal", xlab = "Level-1 residuals")

# Linearity and homoskedasticity of level-1 residuals
ggplot(data = out, aes(x = .fitted, y = .resid)) +
  geom_point(size = 3) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Normaility of level-2 residuals
u0 = ranef(lmer.final)$studentID[ , 1]
u1 = ranef(lmer.final)$studentID[ , 2]


sm.density(u0, model = "normal", xlab = "Level-2 residuals (intercept)")
sm.density(u1, model = "normal", xlab = "Level-2 residuals (slope)")



#########################################
## Interpret effects
#########################################

summary(lmer.final)



#########################################
## Plot of the fixed-effects
#########################################

# Create data set
fixed_effects = expand.grid(
	c.grade = 0:3,
	att = c(0.85, 0.95),
	atRisk = c(0, 1),
	minority = c(0, 1)
	)

# Use predict() to add the y-hat values
fixed_effects$yhat = predict(lmer.final, newdata = fixed_effects, re.form = NA)  

# Coerce any predictors not on the x-axis into factors for better plotting
fixed_effects$att = factor(fixed_effects$att, levels = c(0.85, 0.95), 
                               labels = c("85% attendance rate", "95% attendance rate"))

fixed_effects$atRisk = factor(fixed_effects$atRisk, levels = c(0, 1), 
                               labels = c("Non at-risk students", "At-risk students"))

fixed_effects$minority = factor(fixed_effects$minority, levels = c(0, 1), 
                               labels = c("White students", "Minority students"))


# Plot
ggplot(data = fixed_effects, aes(x = c.grade, y = yhat, color = att)) +
	geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Grade level", breaks = 0:3, labels = c("5", "6", "7", "8")) +
  ylab("Predicted reading score") +
  facet_grid(minority ~ atRisk) +
  scale_color_brewer(name = "Attendance rate", palette = "Set1")

