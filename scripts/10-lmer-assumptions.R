##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(dplyr)
library(ggplot2)
library(lme4)
library(readr)
library(sm)
library(tidyr)



##################################################
### Read in and join the data
##################################################

# Read in student-level data
student_data = read_csv(file = "~/Documents/github/epsy-8252/data/netherlands-students.csv")

# Read in school-level data
school_data = read_csv(file = "~/Documents/github/epsy-8252/data/netherlands-schools.csv")


# Join the two datasets together
joined_data = left_join(student_data, school_data, by = "school_id")
head(joined_data)



##################################################
### Fit Model 3
##################################################

lmer.3 = lmer(language_post ~ 1 + verbal_iq + ses + public + (1 | school_id),
              data = joined_data, REML = FALSE)




##################################################
### Augment the model to get the Level-1 residuals and fitted values
##################################################

out_3 = augment(lmer.3)
head(out_3)



##################################################
### Plots of the level-1 residuals
##################################################

# Density plot of the level-1 residuals
sm.density(out_3$.resid, model = "normal")


# Scatterplot of the Level-1 residuals versus the fitted values
ggplot(data = out_3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")



##################################################
### Density plot of the random-effects
##################################################

# Obtain the intercept random-effects
re_int = ranef(lmer.3)$school_id[ , 1]


# Density plot of the RE for intercept
sm.density(re_int, model = "normal", xlab = "RE for intercept")


