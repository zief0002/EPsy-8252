###################################################
### Load libraries
###################################################

library(AICcmodavg)
library(dplyr)
library(ggplot2)
library(lme4)
library(sm)
library(tidyr)



###################################################
### Read in the data
###################################################

mplsWide = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/minneapolis.csv")
head(mplsWide)




###################################################
### Create long data from wide data -- need tidyr package
###################################################

mpls = mplsWide %>% gather(
  key = time, # Name of the variable that delineates the time points
  value = read, # Name of the outcome
  c(read.5, read.6, read.7, read.8) # The old variables you will use as values
)

# Arrange by ID number
mpls = mpls %>% arrange(studentID)
head(mpls, 12)



###################################################
### Get grade level into integers that can be analyzed in lmer
###################################################

str(mpls)


# Turn time into a factor
mpls$time = as.factor(mpls$time)
str(mpls)


# Use as.integer() to turn factor into numbers
mpls = mpls %>% mutate(grade = as.integer(time))
head(mpls)


# Use as.integer() to turn factor into numbers
mpls = mpls %>% mutate(grade = as.integer(time) + 4)
head(mpls)



###################################################
### Explore reading scores over time -- numerical summaries
###################################################

# Obtain means and SDs
mpls %>% 
  group_by(time) %>% 
  summarize(M = mean(read), SD = sd(read))


# Missingness
mpls %>% 
  group_by(time) %>% 
  summarize(
    Miss = sum(is.na(read)), 
    Miss_Percent = sum(is.na(read))/length(read)
  )


# Recompute summaries after filtering
mpls %>% 
  filter(!is.na(read)) %>%
  group_by(time) %>% 
  summarize(M = mean(read, na.rm = TRUE), SD = sd(read))


# Correlation of reading scores over time
cor(mplsWide[2:5], use = "pairwise.complete.obs")



###################################################
### Explore reading scores over time -- graphical summaries
###################################################

# Spaghetti plots
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
  geom_point() +
  geom_smooth(aes(group = 1), se = FALSE, lwd = 1.5) +
  geom_line(alpha = 0.3) +
  theme_bw()


# Facet by Student ID
ggplot(data = mpls, aes(x = grade, y = read, group = studentID)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~studentID)



###################################################
### Fit models to examine functional form of the change over time -- ML estimation
###################################################

lmer.0.ml = lmer(read ~ 1  + (1 | studentID), data = mpls, REML = FALSE)
lmer.1.ml = lmer(read ~ 1  + grade + (1 | studentID), data = mpls, REML = FALSE)
lmer.2.ml = lmer(read ~ 1  + grade + I(grade^2) + (1 | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0.ml, lmer.1.ml, lmer.2.ml),
  modnames = c("LMER0", "LMER1", "LMER2")
)



###################################################
### Examine the level-1 residuals as another way to pick predictors
###################################################

# What associated output do we get with an lmer() object? 
slotNames(lmer.1.c) 
head(lmer.1.c@frame)


# Unconditional means model level-1 residuals
resid.0 = data.frame(
  studentID = lmer.0.ml@frame$studentID,
  residual = resid(lmer.0.ml),
  model = "Model 0"
)


# Unconditional growth model (linear) level-1 residuals
resid.1 = data.frame(
  studentID = lmer.1.ml@frame$studentID,
  residual = resid(lmer.1.ml),
  model = "Model 1"
)


# Unconditional growth model (quadratic) level-1 residuals
resid.2 = data.frame(
  studentID = lmer.2.ml@frame$studentID,
  residual = resid(lmer.2.ml),
  model = "Model 2"
)


# Stack the three sets of residuals into a single data frame
all.resid = rbind(resid.0, resid.1, resid.2)

# Only Student 1's residuals
res.1 = all.resid %>% filter(studentID == 1)
res.1

## Plot the residuals by subject ID
ggplot(data = all.resid, aes(x = studentID, y = residual, group = studentID)) + 
  theme_bw() +
  geom_boxplot(fill = "grey80") + 
  geom_hline(yintercept = 0) +
  facet_wrap(~ model) + 
  xlab("Subject") +
  ylab("Residual") + 
  coord_flip()



###################################################
### Fit models to examine structure of the random effects -- REML estimation
###################################################

lmer.1.int = lmer(read ~ 1  + grade + (1 | studentID), data = mpls)
lmer.1.both = lmer(read ~ 1  + grade + (1 + grade | studentID), data = mpls)


# AICc selection
aictab(
  cand.set = list(lmer.1.int, lmer.1.both),
  modnames = c("Random Intercepts", "Random Intercepts + Slopes")
)



###################################################
### Model interpretation
###################################################

summary(lmer.1.both)

# Get variance-covariance matrix of the random effects
VarCorr(lmer.1.both)$studentID



###################################################
### Centering grade level for better interpretation
###################################################

mpls$c.grade = mpls$grade - 5
head(mpls)  


# Re-fit model
lmer.1.c = lmer(read ~ 1 + c.grade  + (1 + c.grade | studentID), data = mpls)
summary(lmer.1.c)


# Get variance-covariance matrix of the random effects
VarCorr(lmer.1.c)$studentID 



###################################################
### Plotting the fixed effects model
###################################################

# Create data set 
plotData = data.frame(
  c.grade = 0:3
)


# Predict
plotData$yhat = predict(lmer.1.c, newdata = plotData, re.form = NA)

# Plot
ggplot(data = plotData, aes(x = c.grade, y = yhat)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Grade", breaks = 0:3, labels = c("5", "6", "7", "8")) +
  ylab("Predicted Reading Score")



###################################################
### Plotting the mixed effects model
###################################################

# Set up the data for plotting
plotData = expand.grid(
  c.grade = 0:3,
  studentID = unique(mpls$studentID)
)

# Use predict() to add the y-hat values
plotData$yhat = predict(lmer.1.c, newdata = plotData)  

# Plot
ggplot(data = plotData, aes(x = c.grade, y = yhat, group = studentID)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Grade", breaks = 0:3, labels = c("5", "6", "7", "8")) +
  ylab("Predicted Reading Score")



###################################################
### Level-2 predictor: at-risk
###################################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + atRisk + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + atRisk + atRisk:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "atRisk - Intercept", "atRisk - Intercept + Slope")
)



###################################################
### Level-2 predictor: female
###################################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + female + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + female + female:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "female - Intercept", "female - Intercept + Slope")
)



###################################################
### Level-2 predictor: minority
###################################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + minority + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + minority + minority:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "minority - Intercept", "minority - Intercept + Slope")
)



###################################################
### Level-2 predictor: English language learner
###################################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + ell + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + ell + ell:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "ell - Intercept", "ell - Intercept + Slope")
)



###################################################
### Level-2 predictor: special education
###################################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + sped + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + sped + sped:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "sped - Intercept", "sped - Intercept + Slope")
)



###################################################
### Level-2 predictor: attendance
###################################################

lmer.0 = lmer(read ~ 1 + c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.1 = lmer(read ~ 1 + c.grade + att + (1 + c.grade | studentID), data = mpls, REML = FALSE)
lmer.2 = lmer(read ~ 1 + c.grade + att + att:c.grade + (1 + c.grade | studentID), data = mpls, REML = FALSE)


aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Unconditional Growth", "att - Intercept", "att - Intercept + Slope")
)


###################################################
### Level-2 predictor: final model
###################################################
lmer.final = lmer(read ~ 1 + c.grade + atRisk + minority + att + att:c.grade + (1 + c.grade | studentID), data = mpls)
summary(lmer.final)$coefficients



###################################################
### Drop level-2 predictors for parsimony
###################################################

# Drop att:c.grade
lmer.final2 = lmer(read ~ 1 + c.grade + atRisk + minority + att + (1 + c.grade | studentID), data = mpls)
summary(lmer.final2)$coefficients

# Drop att
lmer.final3 = lmer(read ~ 1 + c.grade + atRisk + minority + (1 + c.grade | studentID), data = mpls)
summary(lmer.final3)$coefficients

# Drop at-risk
lmer.final4 = lmer(read ~ 1 + c.grade +  minority + (1 + c.grade | studentID), data = mpls)
summary(lmer.final4)$coefficients



###################################################
### Check assumptions -- Level-1 residuals
###################################################

# Will produce an error because of missing data
out = fortify(lmer.final4)

# Remove missing values
mpls2 = mpls %>% na.omit()

# Re-fit model
lmer.final5 = lmer(read ~ 1 + c.grade +  minority + (1 + c.grade | studentID), data = mpls2)

# Fortify 
out = fortify(lmer.final5)
head(out)

# Density plot of the scaled residuals
sm.density(out$.scresid, model = "normal")

# Scatterplot of the scaled residuals vs. the fitted values
ggplot(data = out, aes(x = .fitted, y = .scresid)) +
  geom_point(size = 3) +
  theme_bw() +
  geom_hline(yintercept = 0)



###################################################
### Check assumptions -- Level-2 residuals
###################################################

# Compute the residuals
res0 = ranef(lmer.final5)$studentID[ , 1]
res1 = ranef(lmer.final5)$studentID[ , 2]

# Density plot of the intercept residuals
sm.density(res0, model = "normal")

# Density plot of the slope residuals
sm.density(res1, model = "normal")

