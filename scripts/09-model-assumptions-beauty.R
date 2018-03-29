##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(lme4) #for fitting mixed-effects models
library(sm)



##################################################
### Read in data
##################################################

beauty = readr::read_csv(file = "~/Dropbox/epsy-8252/data/beauty.csv")


beauty %>%
  arrange(prof)



##################################################
### Fit mixed-effects model
##################################################

lmer.1 = lmer(avgeval ~ 1 + btystdave + female + nonenglish + (1 | prof),
              data = beauty, REML = FALSE)



##################################################
### Get level-1 residuals, fitted values, etc.
##################################################

out = augment(lmer.1)
head(out)



##################################################
### Examine normality of the level-1 residuals
##################################################

sm.density(out$.resid, model = "normal")



##################################################
### Examine linearity and homoscedasticity of the level-1 residuals
##################################################

ggplot(data = out, aes(x = .fitted, y = .resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0)



##################################################
### Get level-2 fitted values and residuals (random-effects)
##################################################

# Extract level-2 residuals
b0j = ranef(lmer.1)$prof[ , 1]

# Create a classroom level data set by grouping by the level-2 grouping variable,
# select the first row, and select the level-2 predictors
beautyL2 = beauty %>%
  group_by(prof) %>%
  filter(row_number() == 1) %>%
  select(prof, btystdave, female, nonenglish)

beautyL2

# Compute y-hat for the level-2 model
yhat = 4.0538 + 0.1330 * beautyL2$btystdave - 0.2042 * beautyL2$female - 0.3588 * beautyL2$nonenglish

# Put these in a data frame
level_2 = data.frame(
  yhat, b0j
)

head(level_2)


##################################################
### Examine normality of the level-2 residuals
##################################################

sm.density(level_2$b0j, model = "normal")



##################################################
### Examine linearity and homoscedasticity of the level-2 residuals
##################################################

ggplot(data = level_2, aes(x = yhat, y = b0j)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0)





