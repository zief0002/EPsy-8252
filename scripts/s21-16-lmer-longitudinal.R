##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom.mixed)
library(educate)
library(lme4)
library(patchwork)
library(tidyverse)



##################################################
### Read in data
##################################################

vocabulary = read_csv(file = "~/Documents/github/epsy-8252/data/vocabulary.csv")

vocabulary



##################################################
### Convert from wide to long structured data
##################################################

vocabulary %>%
  pivot_longer(
    cols = vocab_08:vocab_11
  )


vocabulary_long = vocabulary %>%
  pivot_longer(
    cols = vocab_08:vocab_11, 
    names_to = "grade", 
    values_to = "vocab_score"
    ) %>%
  arrange(id, grade)


# View long data
vocabulary_long




##################################################
### Spaghetti plot of the individual and mean profiles
##################################################

ggplot(data = vocabulary_long, aes(x = grade, y = vocab_score)) +
  geom_line(aes(group = id), alpha = 0.3) +                      #Add individual profiles
  stat_summary(fun = mean, geom = "line", size = 2, group = 1) + #Add mean profile line
  stat_summary(fun = mean, geom = "point", size = 3) +           #Add mean profile points
  theme_light() +
  scale_x_discrete(
    name = "Grade-level", 
    labels = c("8th-grade", "9th-grade", "10th-grade", "11th-grade")
  ) +
  ylab("Vocabulary score")



##################################################
### Unconditional random intercepts model
##################################################

# Fit unconditional random intercepts model
lmer.0 = lmer(vocab_score ~ 1 + (1|id), data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.0)


# Compute variance components
1.35 ^ 2 #Residual (Level-1: Time point; 38%)
1.72 ^ 2 #Intercept (Level-2: Students; 62%)

1.8225 / (1.8225 + 2.9584) # Within-student unexplained variance
2.9584 / (1.8225 + 2.9584) # Between-student unexplained variance



##################################################
### Unconditional growth model (categorical grade)
##################################################

# Fit unconditional growth model (RM-ANOVA-like results)
lmer.1 = lmer(vocab_score ~ 1 + grade + (1|id), 
              data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.1)


# Compute variance components
0.899 ^ 2 #Residual
1.791 ^ 2 #Intercept

(1.8225 - 0.808201) / 1.8225  #Explained variance within-students
(2.9584 - 3.207681) / 2.9584  #Explained variance between-students



##################################################
### Lookup table
##################################################

lookup_table = data.frame(
  grade = c("vocab_08", "vocab_09", "vocab_10", "vocab_11"),
  grade_quant = c(8, 9, 10, 11),
  grade_quant_center = c(0, 1, 2, 3)
)


# View lookup table
lookup_table



##################################################
### Join data to lookup table
##################################################

# Join the data with the lookup table
vocabulary_long = vocabulary_long %>%
  left_join(lookup_table, by = "grade")


# View joined data
vocabulary_long



##################################################
### Unconditional growth model (quantitative grade-level predictor)
##################################################

# Fit unconditional growth model
lmer.1_quant = lmer(vocab_score ~ 1 + grade_quant + (1|id), 
                    data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.1_quant)


# Compute variance components
0.947 ^ 2 #Residual
1.784 ^ 2 #Intercept


(1.8225 - 0.896809) / 1.8225  #Explained variance within-students
(2.9584 - 3.182656) / 2.9584  #Explained variance between-students



##################################################
### Unconditional growth model (quantitative grade-level predictor centered on 8th grade)
##################################################

# Fit unconditional growth model with centered grade
lmer.1_quant_cent = lmer(vocab_score ~ 1 + grade_quant_center + (1|id), 
                         data = vocabulary_long, REML = FALSE)


# Coefficient-level output
tidy(lmer.1_quant_cent)


# Compute variance components
0.947 ^ 2 #Residual
1.784 ^ 2 #Intercept


(1.8225 - 0.896809) / 1.8225  #Explained variance within-students
(2.9584 - 3.182656) / 2.9584  #Explained variance between-students



##################################################
### Examine functional form of grade-level
##################################################

# Quadratic model
lmer.quad = lmer(vocab_score ~ 1 + grade_quant_center + I(grade_quant_center^2) + (1|id), 
              data = vocabulary_long, REML = FALSE)


# Log-linear model
lmer.log = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + (1|id), 
              data = vocabulary_long, REML = FALSE)



##################################################
### Evaluate residuals
##################################################

# Obtain level-1 resduals and fitted values
mod1_lev1 = augment(lmer.1_quant_cent)
quad_lev1 = augment(lmer.quad)
log_lev1 = augment(lmer.log)


# Obtain a data frame of the random-effects (level-2 residuals)
mod1_lev2 = ranef(lmer.1_quant_cent)$id
quad_lev2 = ranef(lmer.quad)$id
log_lev2 = ranef(lmer.log)$id


##################### LINEAR MODEL #####################

# Density plot of the level-1 residuals
p1 = ggplot(data = mod1_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-1 residuals") +
  ggtitle("Linear Model")

# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = mod1_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")

# Density plot of the level-2 residuals (RE of intercept)
p3 = ggplot(data = mod1_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-2 residuals")

##################### NONLINEAR (QUADRATIC) MODEL #####################

# Density plot of the level-1 residuals
p4 = ggplot(data = quad_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-1 residuals") +
  ggtitle("Nonlinear Model (Quadratic)")

# Scatterplot of the Level-1 residuals versus the fitted values
p5 = ggplot(data = quad_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")

# Density plot of the level-2 residuals (RE of intercept)
p6 = ggplot(data = quad_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-2 residuals")

##################### NONLINEAR (LOG) MODEL #####################

# Density plot of the level-1 residuals
p7 = ggplot(data = log_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-1 residuals") +
  ggtitle("Nonlinear Model (Log)")

# Scatterplot of the Level-1 residuals versus the fitted values
p8 = ggplot(data = log_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")

# Density plot of the level-2 residuals (RE of intercept)
p9 = ggplot(data = log_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-2 residuals")


# Layoout plots
(p1 | p2 | p3) / (p4 | p5 | p6) / (p7  | p8 | p9)



##################################################
### Table of model-evidence
##################################################

aictab(
  cand.set = list(lmer.1_quant_cent, lmer.quad, lmer.log),
  modnames = c("Linear", "Quadratic", "Log-linear")
)



##################################################
### Log-linear model
##################################################

# Coefficient-level output
tidy(lmer.log)


# Compute variance components
0.907 ^ 2 #Residual
1.790 ^ 2 #Intercept


(1.8225 - 0.822649) / 1.8225  #Explained variance within-students
(2.9584 - 3.2041) / 2.9584    #Explained variance between-students



##################################################
### Plot mean profile for adopted unconditional growth model
##################################################

ggplot(data = vocabulary_long, aes(x = grade_quant_center, y = vocab_score)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {1.21 + 1.67 * log(x + 1)},
    color = "blue"
  ) +
  theme_light() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")



##################################################
### Spaghetti plot by sex
##################################################

# Turn female into factor for better plotting
vocabulary_long %>%
  mutate(
    # Create factor for better plotting
    Sex = factor(female, levels = c(0, 1), labels = c("Non-female", "Female"))
  ) %>%
  ggplot(aes(x = grade_quant, y = vocab_score, color = Sex)) +
    geom_line(aes(group = id), alpha = 0.3) +
    stat_summary(fun = mean, geom = "line", size = 2, group = 1) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    theme_light() +
    xlab("Grade-level") +
    ylab("Vocabulary score") +
    facet_wrap(~Sex) +
    ggsci::scale_color_d3() +
    guides(color = FALSE)



##################################################
### Explore effect of sex
##################################################

# Main effects model
lmer.main  = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + female +
                (1|id), data = vocabulary_long, REML = FALSE)


# Interaction model
lmer.int = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + female + 
                  log(grade_quant_center + 1):female +
                (1|id), data = vocabulary_long, REML = FALSE)


# Table of model-evidence
aictab(
  cand.set = list(lmer.log, lmer.main, lmer.int),
  modnames = c("Unconditional Growth", "Sex Main Effect", "Sex Interaction")
)



##################################################
### Examine residuals
##################################################

# Obtain the level-1 residuals and fitted values
main_lev1 = augment(lmer.main)
int_lev1 = augment(lmer.int)


# Obtain a data frame of the random-effects
main_lev2 = ranef(lmer.main)$id
int_lev2 = ranef(lmer.int)$id


##################### MAIN EFFECTS MODEL #####################

# Density plot of the level-1 residuals
p1 = ggplot(data = main_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-1 residuals") +
  ggtitle("Main Effects Model")

# Scatterplot of the Level-1 residuals versus the fitted values
p2 = ggplot(data = main_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")

# Density plot of the level-2 residuals (RE of intercept)
p3 = ggplot(data = main_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-2 residuals")


##################### INTERACTION MODEL #####################

# Density plot of the level-1 residuals
p4 = ggplot(data = int_lev1, aes(x = .resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-1 residuals") +
  ggtitle("Interaction Model")

# Scatterplot of the Level-1 residuals versus the fitted values
p5 = ggplot(data = int_lev1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light() +
  xlab("Fitted values") +
  ylab("Level-1 residuals")

# Density plot of the level-2 residuals (RE of intercept)
p6 = ggplot(data = int_lev2, aes(x = `(Intercept)`)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_light() +
  xlab("Level-2 residuals")


# Layout plots
(p1 | p2 | p3) / (p4 | p5 | p6)



##################################################
### Main effects model
##################################################

tidy(lmer.main)


# Compute variance components
0.907 ^ 2 #Residual
1.232 ^ 2 #Intercept


(1.8225 - 0.822649) / 1.8225  #Explained variance within-students
(2.9584 - 1.517824) / 2.9584  #Explained variance between-students



##################################################
### Plot mean profile from main effects model
##################################################

ggplot(data = vocabulary_long, aes(x = grade_quant_center, y = vocab_score)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {-0.01 + 1.67 * log(x + 1)},
    color = "blue",
    linetype = "dashed"
  ) +
  geom_function(
    fun = function(x) {2.59 + 1.67 * log(x + 1)},
    color = "orange"
  ) +
  theme_light() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")



##################################################
### Interaction model
##################################################

tidy(lmer.int)


# Compute variance components
0.903 ^ 2 #Residual
1.233 ^ 2 #Intercept


(1.8225 - 0.815409) / 1.8225  #Explained variance within-students
(2.9584 - 1.520289) / 2.9584  #Explained variance between-students



##################################################
### Plot mean profile from interaction model
##################################################

ggplot(data = vocabulary_long, aes(x = grade_quant_center, y = vocab_score)) +
  geom_point(alpha = 0) +
  geom_function(
    fun = function(x) {-0.11 + 1.79 * log(x + 1)},
    color = "blue",
    linetype = "dashed"
  ) +
  geom_function(
    fun = function(x) {2.7 + 1.52 * log(x + 1)},
    color = "orange"
  ) +
  theme_light() +
  scale_x_continuous(
    name = "Grade-level",
    breaks = c(0, 1, 2, 3),
    labels = c("0\n(8th)", "1\n(9th)", "2\n(10th)", "3\n(11th)")
  ) +
  ylab("Vocabulary score")


