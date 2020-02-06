##################################################
### Load libraries
##################################################

library(broom)
library(educate)
library(lme4) #for fitting mixed-effects models
library(patchwork)
library(tidyverse)



##################################################
### Read in data
##################################################

# Read in student-level data
student_data = read_csv(file = "~/Documents/github/epsy-8252/data/netherlands-students.csv")
head(student_data)


# Read in school-level data
school_data = read_csv(file = "~/Documents/github/epsy-8252/data/netherlands-schools.csv")
head(school_data)



##################################################
### Join student-level and school-level datasets
##################################################

joined_data = left_join(student_data, school_data, by = "school_id")
head(joined_data)



##################################################
### Fit fixed-effects model
##################################################

lm.1 = lm(language_post ~ 1 + verbal_iq, data = joined_data)


# Model-level output
glance(lm.1)


# Coefficient-level output
tidy(lm.1)



##################################################
### Examine residuals
##################################################

# Obtain the augmented data frame
out = augment(lm.1)
head(out)


# Normality
p1 = ggplot(data = out, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")


# All other assumptions
p2 = ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")


p1 + p2



##################################################
### Examine whether residuals are independent within schools
##################################################

# Make random sample reproducible
set.seed(100)


# Draw random sample of 25 schools without replacement
my_sample = sample(school_data$school_id, size = 25, replace = FALSE)


# Mutate on school ID and draw random sample
out = out %>%
  mutate(school_id = joined_data$school_id) %>%
  filter(school_id %in% my_sample)


### Show residuals by school
ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Studentized residuals") +
  facet_wrap(~school_id, nrow = 5)



##################################################
### Conceptual idea of mixed-effects models
##################################################

# Fit school models
school_models = joined_data %>%
  group_by(school_id) %>%
  do(mod = lm(language_post  ~ 1 + verbal_iq, data = .)) %>%
  tidy(mod) %>%
  head(., 10)


# View coefficients from fitted models
school_models



##################################################
### Fit mixed-effects model in practice
##################################################

# Fit mixed-effects regression model
lmer.1 = lmer(language_post ~ 1 + verbal_iq + (1 | school_id), data = joined_data)


# Display fixed-effects
fixef(lmer.1)


# Display random-effects
ranef(lmer.1)



##################################################
### Example 2: NBA life satisfaction
##################################################

# Read in player-level data
nba_players = read_csv(file = "~/Documents/github/epsy-8252/data/nba-player-data.csv")

# Read in team-level data
nba_teams = read_csv(file = "~/Documents/github/epsy-8252/data/nba-team-data.csv")


# Join the datasets together
nba = nba_players %>%
  left_join(nba_teams, by = "team")

head(nba)



##################################################
### Fixed-effects model
##################################################

# Fit model
lmer.1 = lmer(life_satisfaction ~ 1 + success + (1 | team), data = nba)


# Display fixed-effects
fixef(lmer.1)


# Display random-effects
ranef(lmer.1)



