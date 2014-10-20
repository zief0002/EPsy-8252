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
### Relationship w/latitude
###################################################

cor(students[c("ses", "math")])

ggplot(data = students, aes(x = ses, y = math)) +
	geom_point(size = 3) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()



###################################################
### Fit LM to all 7185 cases
###################################################

lm.1 = lm(math ~ ses, data = students)

summary(lm.1)
anova(lm.1)

# Fortify the data
out1 = fortify(lm.1)

# Add schools to the fortified data
out1$school = students$school

# Select a random sample of schools
schools = unique(students$school)
sampled_schools = sample(schools, size = 15, replace = FALSE)

# Plot the randomly sampled schools studentized residuals vs. their fitted values
ggplot(data = out1[out1$school %in% sampled_schools, ], aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  facet_wrap(~school)




###################################################
### Compute group-level data
###################################################

library(dplyr)

school_level = students %.%
  group_by(school) %.%
  summarise(
  	n = length(math),
  	meanMath = mean(math),
  	meanSES = mean(ses)
  	)


ggplot(data = school_level, aes(x = meanSES, y = meanMath)) +
	geom_point(size = 3) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()

pdf(file = "~/Desktop/plot.pdf")
dev.off()

###################################################
### Fit the LM to the group-level data
###################################################

lm.2 = lm(meanMath ~ meanSES, data = school_level)
summary(lm.2)

plot(lm.2)



###################################################
### Fit the mixed-effects model
###################################################

library(lme4)

lmer.1 = lmer(math ~ 1 + ses + (1 + ses|school), data = students)
summary(lmer.1)

anova(lmer.1)



###################################################
### p-value??? -- Part I
###################################################

# Best-case
1 - pf(410.7, df1 = 1, df2 = 1783)

# Worst-case
1 - pf(410.7, df1 = 1, df2 = 158)



###################################################
### p-value??? -- Part II
###################################################

library(lmerTest)
lmer.1 = lmer(math ~ 1 + ses + (1 + ses|school), data = students)
summary(lmer.1)

anova(lmer.1, ddf = "Kenward-Roger")


