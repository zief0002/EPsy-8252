##################################################
### Load libraries
##################################################

library(broom)
library(educate)
library(lme4)
library(MuMIn)
library(patchwork)
library(tidyverse)



##################################################
### Read in data
##################################################

vocabulary = read_csv(file = "~/Documents/github/epsy-8252/data/vocabulary.csv")
head(vocabulary)



##################################################
### Prepare data
##################################################

# Create lookup table
lookup_table = data.frame(
  grade = c("vocab_08", "vocab_09", "vocab_10", "vocab_11"),
  grade_quant = c(8, 9, 10, 11),
  grade_quant_center = c(0, 1, 2, 3)
)


# Convert from wide to long structured data
vocabulary_long = vocabulary %>%
  pivot_longer(
    cols = vocab_08:vocab_11, 
    names_to = "grade", 
    values_to = "vocab_score"
    ) %>%
  left_join(lookup_table, by = "grade") %>%
  arrange(id, grade)


# View first 12 cases in the long data
head(vocabulary_long, 12)




##################################################
### Evaluate linear effect of grade-level
##################################################

# Fit unconditional random intercepts model
lmer.0 = lmer(vocab_score ~ 1 + (1|id), data = vocabulary_long, REML = FALSE)

# Fit unconditional linear growth model
lmer.1 = lmer(vocab_score ~ 1 + grade_quant_center + (1|id), data = vocabulary_long, REML = FALSE)

# LRT
anova(lmer.0, lmer.1)



##################################################
### Evaluate ceffect of grade-level (categorical representation)
##################################################

# Fit model with categorical grade predictor
lmer.1.cat = lmer(vocab_score ~ 1 + grade + (1|id), data = vocabulary_long, REML = FALSE)


# LRT
anova(lmer.0, lmer.1.cat)



##################################################
### Evaluate quadratic effect of grade-level
##################################################

# Fit unconditional quadratic growth model
lmer.quad = lmer(vocab_score ~ 1 + grade_quant_center + I(grade_quant_center ^ 2) + 
                (1|id), data = vocabulary_long, REML = FALSE)


# LRT
anova(lmer.1, lmer.quad)



##################################################
### Evaluate effect of grade-level (log)
##################################################

# Log-linear model
lmer.log = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + (1|id),
                data = vocabulary_long, REML = FALSE)


# LRT
anova(lmer.0, lmer.log)



##################################################
### Evaluate effect of sex
##################################################

# Main effects model
lmer.main = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + female +
                   (1|id), data = vocabulary_long, REML = FALSE)


# Interaction model
lmer.int = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + female + log(grade_quant_center + 1):female +
                  (1|id), data = vocabulary_long, REML = FALSE)


# LRT
anova(lmer.log, lmer.main, lmer.int)



##################################################
### Magnitude of t-values
##################################################

# Coefficient-level output for interaction model
tidy(lmer.int)


# p-values associated with t=2
2 * pt(q = -2, df = 100)
2 * pt(q = -2, df = 10)

