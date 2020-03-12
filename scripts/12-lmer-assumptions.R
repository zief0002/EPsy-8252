##################################################
### Load libraries
##################################################

library(broom)
library(educate)
library(lme4) #for fitting mixed-effects models
library(patchwork)
library(tidyverse)



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
p1 = ggplot(data = out_3, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals")


# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = out_3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")

# Plot side-by-side
p1 + p2



##################################################
### Density plot of the random-effects
##################################################

# Obtain a data frame of the random-effects
level_2 = ranef(lmer.3)$school_id

# View the data frame
head(level_2)

# Density plot of the RE for intercept
ggplot(data = level_2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals")



##################################################
### Log-transform the outcome
##################################################

# Fit model
lmer.4 = lmer(log(language_post) ~ 1 + verbal_iq + ses + public + (1 | school_id),
              data = joined_data, REML = FALSE)


# Augment model
out_4 = augment(lmer.4)


# Obtain a data frame of the random-effects
level_2 = ranef(lmer.4)$school_id


# Density plot of the level-1 residuals
p1 = ggplot(data = out_4, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals")


# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = out_4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Density plot of the level-2 residuals (RE of intercept)
p3 = ggplot(data = level_2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals")


# Plot side-by-side
p1 + p2 + p3


# Interpret coefficients
tidy(lmer.4)


# Back-transform the coefficients
exp(fixef(lmer.4))



##################################################
### Fit baseline models to look at reducation in variance
##################################################

# Fit baseline model (raw outcome)
lmer.3_0 = lmer(language_post ~ 1 + (1 | school_id), data = joined_data, REML = FALSE)


# Compute reduction in variance (raw outcome)
(4.41^2 - 2.93^2) / 4.41^2 #School-level
(8.04^2 - 6.33^2) / 6.33^2 #Student-level


# Fit baseline model (log-transformed outcome)
lmer.4_0 = lmer(log(language_post) ~ 1 + (1 | school_id), data = joined_data, REML = FALSE)


# Compute reduction in variance (log-transformed outcome)
(0.128^2 - 0.087^2) / 0.128^2 #School-level
(0.223^2 - 0.179^2) / 0.223^2 #Student-level
