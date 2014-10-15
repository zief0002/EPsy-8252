###################################################
### Read in data
###################################################

students = read.csv(file = "/Users/andrewz/Documents/Data/HSB-Level-1.csv")
head(students)




###################################################
### Load libraries
###################################################

library(ggplot2)
library(psych)
library(sm)



###################################################
### Examine outcome
###################################################

sm.density(students$math)
describe(students$math)




###################################################
### Relationship w/math
###################################################

# Create dummies for sex and minority
students$female = ifelse(students$sex == "Female", 1, 0)
students$minority = ifelse(students$minority == "Yes", 1, 0)

# Correlation matrix
cor(students[c("math", "female", "minority", "ses")])


ggplot(data = students, aes(x = ses, y = math)) +
	geom_point(size = 3) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()



###################################################
### Fit LM to all 7185 cases
### Complete pooling
###################################################

lm.1 = lm(math ~ ses, data = students)
summary(lm.1)

par(mfrow = c(2, 2))
plot(lm.1)
par(mfrow = c(1, 1))



###################################################
### Get group-level data
###################################################

library(dplyr)

grp = students %>%
  group_by(school) %>%
  summarise(
  	n = length(math),
  	math = mean(math),
  	ses = mean(ses)
  	)
head(grp)


ggplot(data = grp, aes(x = ses, y = math)) +
	geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
	theme_bw()


###################################################
### Fit the LM to the group-level data
### No pooling
###################################################

lm.2 = lm(math ~ ses, data = grp)
summary(lm.2)

par(mfrow = c(2, 2))
plot(lm.2)
par(mfrow = c(1, 1))




###################################################
### Compare the group-level data and models to the individual-level data and models
###################################################

ggplot(data = grp, aes(x = ses, y = math)) +
  #geom_point(pch = "x", size = 6, alpha = 0.3) +
  geom_point(data = students, size = 3, alpha = 0.4, color = "purple") +
  geom_point(size = 3) +
  geom_smooth(data = students, method = "lm", se = FALSE, color = "yellow") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()




###################################################
### Fit the LM with school (group) as a covariate
###################################################

lm.3 = lm(math ~ ses + factor(school), data = students)
summary(lm.3)

par(mfrow = c(2, 2))
plot(lm.2)
par(mfrow = c(1, 1))




###################################################
### Fit the LM to each group
###################################################
lm.1224 = lm(math ~ ses, data = students, subset = school == 1224)
summary(lm.1224)

library(nlme)
bySchool = lmList(math ~ ses | school, data = students)

summary(bySchool)
summary(bySchool)$coefficients

# Get intercepts, slopes, and school numbers
intercept = summary(bySchool)$coefficients[ , 1, 1]
slope = summary(bySchool)$coefficients[ , 2, 1]
school = rownames(summary(bySchool)$coefficients)

# Put these in a data frame
myRegs = data.frame(intercept, slope, school)

# Add a column of the sample sizes
myRegs = merge(myRegs, grp[c("school", "n")], by = "school")



###################################################
### Fit the mixed-effects model
###################################################

library(lme4)

# Varying intercept model
lmer.1 = lmer(math ~ 1 + ses + (1|school), data = students)
summary(lmer.1)

# Varying intercept and varying slope model
lmer.2 = lmer(math ~ 1 + ses + (1 + ses|school), data = students)
summary(lmer.2)



###################################################
### If you need p-values...
###################################################

library(lmerTest)

# Need to refit te model
lmer.1 = lmer(math ~ 1 + ses + (1|school), data = students)
summary(lmer.1)

anova(lmer.1, ddf = "Kenward-Roger")




