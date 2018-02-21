##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(arm)
library(broom)
library(dplyr)
library(ggplot2)
library(haven) #for reading in the .sav files
library(lme4) #for fitting mixed-effects models
library(sm)



##################################################
### Read in data
##################################################

# Read in player-level data
nbaL1 = read_sav(file = "~/Dropbox/epsy-8252/data/nbaLevel1.sav")
head(nbaL1)


# Read in team-level data
nbaL2 = read_sav(file = "~/Dropbox/epsy-8252/data/nbaLevel2.sav")
head(nbaL2)



##################################################
### Merge player and team datasets
##################################################

nba = left_join(nbaL1, nbaL2, by = "Team_ID")
head(nba)



##################################################
### Fit fixed-effects model
##################################################

lm.1 = lm(Life_Satisfaction ~ 1 + Shots_on_five, data = nba)
display(lm.1)



##################################################
### Examine residuals
##################################################

# Obtain the fortified data frame
out = augment(lm.1)
head(out)

# Normality
sm.density(out$.std.resid, model = "normal")

# All other assumptions
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals")



##################################################
### Examine whether residuals are independent within teams
##################################################

# Add Team_ID variable to fotified data
out$Team_ID = nba$Team_ID

### Show residuals by team
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals") +
  facet_wrap(~Team_ID, nrow = 5)



##################################################
### Conceptual idea of mixed-effects models
##################################################

models = nba %>%
  group_by(Team_ID) %>%
  do( mod = lm(Life_Satisfaction  ~ Shots_on_five, data = .) ) %>%
  broom::tidy(mod)

models



##################################################
### Fit mixed-effects model in practice
##################################################

lmer.1 = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), 
              data = nba)


# Display fixed-effects
fixef(lmer.1)


# Display random-effects
ranef(lmer.1)



##################################################
### Example 2: Beauty
##################################################

# Read in data
beauty = readr::read_csv(file = "~/Dropbox/epsy-8252/data/beauty.csv")
head(beauty)



##################################################
### Fixed-effects model
##################################################

lm.1 = lm(avgeval ~ 1 + btystdave + female + nonenglish, data = beauty)


# Get model residuals, fitted values, etc.
out = augment(lm.1)

# Add professor ID to the data; Turn it into a factor for better plotting
out$prof = as.factor(beauty$prof)

# Randomly sample 25 professors
set.seed(1001)
my_profs = sample(unique(out$prof), size = 25, replace = FALSE)

# Filter the data to only select data from those professors
my_sample = out %>%
  filter(prof %in% my_profs)

# Show residuals by professor
ggplot(data = my_sample, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals") +
  facet_wrap(~prof, nrow = 5)



##################################################
### Fit mixed-effects model
##################################################

# Fit model
lmer.1 = lmer(avgeval ~ 1 + btystdave + female + nonenglish + (1 | prof),
              data = beauty)


# Display fixed-effects
fixef(lmer.1)


# Display random-effects
ranef(lmer.1)



