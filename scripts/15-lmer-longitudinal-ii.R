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
  c_grade = c(0, 1, 2, 3)
)


# Convert from wide to long structured data and
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
### Spaghetti plot -- Mean and individual profiles
##################################################

ggplot(data = vocabulary_long, aes(x = grade, y = vocab_score)) +
  geom_line(aes(group = id), alpha = 0.3) +                        #Add individual profiles
  stat_summary(fun.y = mean, geom = "line", size = 2, group = 1) + #Add mean profile line
  stat_summary(fun.y = mean, geom = "point", size = 3) +           #Add mean profile points
  theme_bw() +
  scale_x_discrete(
    name = "Grade-level", 
    labels = c("8th-grade", "9th-grade", "10th-grade", "11th-grade")
  ) +
  ylab("Vocabulary score")



##################################################
### Fit unconditional growth model (intercept and slope random-effects)
##################################################

# Fit model
lmer.2 = lmer(vocab_score ~ 1 + c_grade + (1 + c_grade|id), data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.2)


# Variance components
1.77148 ^ 2 #Intercept
0.00865 ^ 2 #Slope
0.94673 ^ 2 #Level-1 error


# Proportion of unexplained variance
3.14    / (3.14 + .0001 + .90) #Intercept
0.00007 / (3.14 + .0001 + .90) #Slope
0.896   / (3.14 + .0001 + .90) #Level-1 error  


# Random-effects
ranef(lmer.2)



##################################################
### Scatterplot of the random-effects
##################################################

data.frame(
  id = vocabulary$id,
  b0 = ranef(lmer.2)$id[ , 1],
  b1 = ranef(lmer.2)$id[ , 2]
  ) %>%
  ggplot(aes(x = b0, y = b1)) +
    geom_text(aes(label = id)) +
    theme_bw() +
    xlab("Random-effect of intercept") +
    ylab("Random-effect of slopet") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)



##################################################
### Fit potential unconditional growth models
##################################################

# Create log-transformed predictor in the data
vocabulary_long = vocabulary_long %>%
  mutate(
    Lgrade = log(c_grade + 1) 
  )


# View data
head(vocabulary_long)


# Fit unconditional random intercepts model
lmer.0 = lmer(vocab_score ~ 1 + (1|id), data = vocabulary_long, REML = FALSE)


# Fit unconditional growth model (intercept random-effects)
lmer.1 = lmer(vocab_score ~ 1 + Lgrade + (1|id), data = vocabulary_long, REML = FALSE)


# Fit unconditional growth model (intercept and slope random-effects)
lmer.2 = lmer(vocab_score ~ 1 + Lgrade + (1 + Lgrade|id), data = vocabulary_long, REML = FALSE)



##################################################
### Table of model-evidence
##################################################


model.sel(
  object = list(lmer.0, lmer.1, lmer.2),
  rank = "AICc"
)



##################################################
### Output for Model 2
##################################################

# Coefficient-level output
tidy(lmer.2)


# Random-effects
ranef(lmer.2)


# Random-effects for Student 08 and 10
ranef(lmer.2)$id[c(8, 10), ]



##################################################
### Plot predicted profiles based on Model 2
##################################################

ggplot(data = vocabulary_long, aes(x = c_grade, y = vocab_score)) +
  geom_point(alpha = 0) +
  # Mean profile
  stat_function(
    fun = function(x) {1.21 + 1.67 * log(x + 1)},
    color = "black",
    linewidth = 1.5
  ) +
  # Student profiles
  stat_function(
    fun = function(x) {-0.493 + 1.62 * log(x + 1)},
    linetype = "dashed"
  ) +
  stat_function(
    fun = function(x) {2.18 + 1.70 * log(x + 1)},
    linetype = "dashed"
  ) +
  annotate(geom = "text", x = 3, y = 1.2, label = "Student 8",    size = 3, hjust = 1) +
  annotate(geom = "text", x = 3, y = 5,   label = "Student 10",   size = 3, hjust = 1) +
  annotate(geom = "text", x = 3, y = 3,   label = "Mean profile", size = 3, hjust = 1) +
  theme_bw() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")



##################################################
### Examine effect of sex
##################################################

# Main-effects model
lmer.3 = lmer(vocab_score ~ 1 + Lgrade + female + (1 + Lgrade|id),
              data = vocabulary_long, REML = FALSE)


# Interaction model
lmer.4 = lmer(vocab_score ~ 1 + Lgrade + female + female:Lgrade + (1 + Lgrade|id),
              data = vocabulary_long, REML = FALSE)


# Table of model evidence
model.sel(
  object = list(lmer.2, lmer.3, lmer.4),
  rank = "AICc"
)


# Table of model evidence
model.sel(
  object = list(lmer.0, lmer.1, lmer.2, lmer.3, lmer.4),
  rank = "AICc"
)




##################################################
### Main Effect Model
##################################################

# Coefficient-level output
tidy(lmer.3)


# Random-effects
ranef(lmer.3)



##################################################
### Interaction Model
##################################################

# Coefficient-level output
tidy(lmer.4)


# Random-effects
ranef(lmer.4)



##################################################
### Evaluate Level-1 residuals
##################################################

# Obtain the level-1 residuals and fitted values
main_lev1 = augment(lmer.3)
int_lev1 = augment(lmer.4)


##################### MAIN EFFECTS MODEL #####################

# Density plot of the level-1 residuals
p1 = ggplot(data = main_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals") +
  ylab("Probability density") +
  ggtitle("Main Effects Model")


# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = main_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


##################### INTERACTION MODEL #####################

# Density plot of the level-1 residuals
p3 = ggplot(data = int_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-1 residuals") +
  ylab("Probability density") +
  ggtitle("Interaction Model")


# Scatterplot of the Level-1 residuals versus the fitted values
p4 = ggplot(data = int_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")


# Layout plots
(p1 | p2) / (p3 | p4)




##################################################
### Evaluate Level-2 residuals
##################################################

# Obtain a data frame of the random-effects
main_lev2 = ranef(lmer.3)$id
int_lev2 = ranef(lmer.4)$id


##################### MAIN EFFECTS MODEL #####################

# Density plot of the level-2 residuals (RE of intercept)
p5 = ggplot(data = main_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals") +
  ylab("Probability density") +
  ggtitle("Main Effects Model")


# Density plot of the level-2 residuals (RE of intercept)
p6 = ggplot(data = main_lev2, aes(x = Lgrade)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals") +
  ylab("Probability density")


##################### INTERACTION MODEL #####################

# Density plot of the level-2 residuals (RE of intercept)
p7 = ggplot(data = int_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals") +
  ylab("Probability density") +
  ggtitle("Interaction Model")


# Density plot of the level-2 residuals (RE of intercept)
p8 = ggplot(data = int_lev2, aes(x = Lgrade)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Level-2 residuals") +
  ylab("Probability density")


# Layout plots
(p5 | p6) / (p7 | p8)

