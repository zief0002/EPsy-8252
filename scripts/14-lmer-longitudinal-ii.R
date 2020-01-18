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
### Read in data
##################################################

vocabulary = read_csv(file = "~/Documents/github/epsy-8252/data/vocabulary.csv")
head(vocabulary)



##################################################
### Convert from wide to long structured data
##################################################

# Create lookup table
lookup_table = data.frame(
  grade = c("vocab_08", "vocab_09", "vocab_10", "vocab_11"), 
  c_grade = c(0, 1, 2, 3)
)


vocabulary_long = vocabulary %>%
  gather(key = "grade", value = "vocab_score", vocab_08:vocab_11) %>% 
  arrange(id, grade) %>%
  left_join(lookup_table, by = "grade")


# View first 12 cases in the long data
head(vocabulary_long, 12)




##################################################
### Fit models using centered grade
##################################################

#Fit model with random-effect of intercept
lmer.2 = lmer(vocab_score ~ 1 + c_grade + (1|id), data = vocabulary_long, REML = FALSE)


# Fit model with random-effect of intercept and slope
lmer.3 = lmer(vocab_score ~ 1 + c_grade + (1 + c_grade|id), data = vocabulary_long, REML = FALSE)


# Model-evidence
aictab(
  cand.set = list(lmer.2, lmer.3),
  modnames = c("Intercept RE", "Intercept + Slope RE")
)



##################################################
### Output 
##################################################

summary(lmer.3)

# Random-effects
ranef(lmer.3)



##################################################
### Fixed-effect of sex
##################################################

# Main-effect of sex
lmer.4 = lmer(vocab_score ~ 1 + c_grade + female + (1 + c_grade|id), data = vocabulary_long, REML = FALSE)


# Interaction-effect between sex and time
lmer.5 = lmer(vocab_score ~ 1 + c_grade + female + female:c_grade + (1 + c_grade|id), data = vocabulary_long, REML = FALSE)


# Table of model evidence
aictab(
  cand.set = list(lmer.3, lmer.4, lmer.5),
  modnames = c("Unconditional growth (RE for Int and Slope)", 
               "Main-effect of sex", 
               "Interaction b/w sex and grade-level")
  )


# Output
summary(lmer.4)

# Random-effects
ranef(lmer.4)



##################################################
### Evaluate level-1 residuals
##################################################

# Obtain level-1 residuals and fitted values
out_4 = augment(lmer.4)


# Residuals vs fitted values
ggplot(data = out_4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Density plot
sm.density(out_4$.resid, model = "normal", xlab = "Level-1 residuals")



##################################################
### Evaluate level-2 residuals
##################################################

# RE of intercept
sm.density(ranef(lmer.4)$id[ , 1], model = "normal", xlab = "Random effects of intercept") 

# RE of slope
sm.density(ranef(lmer.4)$id[ , 2], model = "normal", xlab = "Random effects of slope")



