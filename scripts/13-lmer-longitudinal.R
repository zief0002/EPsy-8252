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

vocabulary_long = vocabulary %>%
  gather(key = "grade", value = "vocab_score", vocab_08:vocab_11) %>%
  arrange(id, grade)


# View first 12 cases in the long data
head(vocabulary_long, 12)




##################################################
### Spaghetti plot of the individual and mean profiles
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
### Unconditional random intercepts model
##################################################

lmer.0 = lmer(vocab_score ~ 1 + (1|id), data = vocabulary_long, REML = FALSE)
summary(lmer.0)



##################################################
### Unconditional growth model
##################################################

lmer.1 = lmer(vocab_score ~ 1 + grade + (1|id), data = vocabulary_long, REML = FALSE)
summary(lmer.1)



##################################################
### Likelihood-based p-values
##################################################

anova(lmer.0, lmer.1)


# Compute p-value manually
1 - pchisq(156.46, df = 3)



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

vocabulary_long_2 = left_join(vocabulary_long, lookup_table, by = "grade")
head(vocabulary_long_2)



##################################################
### Unconditional growth model (quantitative grade-level predictor)
##################################################

lmer.2 = lmer(vocab_score ~ 1 + grade_quant + (1|id), data = vocabulary_long_2, REML = FALSE)
summary(lmer.2)



##################################################
### Centering grade-level
##################################################

lmer.3 = lmer(vocab_score ~ 1 + grade_quant_center + (1|id), data = vocabulary_long_2, REML = FALSE)
summary(lmer.3)



##################################################
### Examine functional form of grade-level
##################################################

# Quadratic model
lmer.4 = lmer(vocab_score ~ 1 + grade_quant_center + I(grade_quant_center^2) + (1|id), 
              data = vocabulary_long_2, REML = FALSE)


# Log-linear model
lmer.5 = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + (1|id), 
              data = vocabulary_long_2, REML = FALSE)



# Table of model-evidence
aictab(
  cand.set = list(lmer.3, lmer.4, lmer.5),
  modnames = c("Linear", "Quadratic", "Log-linear")
)



##################################################
### Evaluate residuals
##################################################

out_4 = augment(lmer.4)
out_5 = augment(lmer.5)


# Log-linear model
ggplot(data = out_5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals") +
  ggtitle("Log-linear")

sm.density(out_5$.resid, model = "normal", main = "Main-Effect", xlab = "Level-1 residuals")
sm.density(ranef(lmer.5)$id[ , 1], model = "normal", xlab = "Random effects of the intercept")


# Quadratic model
ggplot(data = out_4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals") +
  ggtitle("Quadratic")

sm.density(out_4$.resid, model = "normal", main = "Interaction Effect", xlab = "Level-1 residuals")
sm.density(ranef(lmer.4)$id[ , 1], model = "normal", xlab = "Random effects of the intercept")



##################################################
### Log-linear model
##################################################

summary(lmer.5)



##################################################
### Plot model
##################################################

# Set up data
plot_data = crossing(
  grade_quant_center = seq(from = 0, to = 3, by = 0.01)
) %>%
  mutate(
    yhat = predict(lmer.5, newdata = ., re.form = NA)
  )


head(plot_data)


# Create plot
ggplot(data = plot_data, aes(x = grade_quant_center, y = yhat)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    name = "Grade-level", 
    breaks = c(0, 1, 2, 3), 
    labels = c(8, 9, 10, 11)
  ) +
  ylab("Vocabulary score")



##################################################
### Spaghetti plot by sex
##################################################

vocabulary_long_2 %>%
  mutate(
    Sex = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  ) %>%
  ggplot(aes(x = grade_quant, y = vocab_score, color = Sex)) +
  geom_line(aes(group = id), alpha = 0.3) +
  stat_summary(fun.y = mean, geom = "line", size = 2, group = 1) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  theme_bw() +
  xlab("Grade-level") +
  ylab("Vocabulary score") +
  facet_wrap(~Sex) +
  ggsci::scale_color_d3()



##################################################
### Explore effect of sex
##################################################

# Main-effect of sex
lmer.6 = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + female +
                (1|id), data = vocabulary_long_2, REML = FALSE)


# Interaction-effect between sex and grade-level
lmer.7 = lmer(vocab_score ~ 1 + log(grade_quant_center + 1) + female + log(grade_quant_center + 1):female +
                (1|id), data = vocabulary_long_2, REML = FALSE)


# Table of model-evidence
aictab(
  cand.set = list(lmer.5, lmer.6, lmer.7),
  modnames = c("Unconditional growth", "Main-effect of sex", "Interaction-effect")
)



##################################################
### Examine residuals
##################################################

out_6 = augment(lmer.6)
out_7 = augment(lmer.7)


# Main-effect model
ggplot(data = out_6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals") +
  ggtitle("Main-Effect")

sm.density(out_6$.resid, model = "normal", main = "Main-Effect", xlab = "Level-1 residuals")
sm.density(ranef(lmer.6)$id[ , 1], model = "normal", xlab = "Random effects of the intercept")


# Interaction model
ggplot(data = out_7, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Level-1 residuals") +
  ggtitle("Interaction Effect")

sm.density(out_7$.resid, model = "normal", main = "Interaction Effect", xlab = "Level-1 residuals")
sm.density(ranef(lmer.7)$id[ , 1], model = "normal", xlab = "Random effects of the intercept")



##################################################
### Likelihood ratio test
##################################################

anova(lmer.5, lmer.6, lmer.7)



##################################################
### Main-effect of sex
##################################################

summary(lmer.6)



##################################################
### Plot of the main-effects model
##################################################

# Set up data
plot_data = crossing(
  grade_quant_center = seq(from = 0, to = 3, by = 0.01),
  female = c(0, 1)
) %>%
  mutate(
    yhat = predict(lmer.6, newdata = ., re.form = NA),
    Sex = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  )


head(plot_data)


# Create plot
ggplot(data = plot_data, aes(x = grade_quant_center, y = yhat, color = Sex, linetype = Sex)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Grade-level", breaks = c(0, 1, 2, 3), labels = c(8, 9, 10, 11)) +
  ylab("Vocabulary score") +
  ggsci::scale_color_d3()



##################################################
### Interaction model
##################################################

summary(lmer.7)



##################################################
### Plot of the interaction model
##################################################

# Set up data
plot_data_2 = crossing(
  grade_quant_center = seq(from = 0, to = 3, by = 0.01),
  female = c(0, 1)
) %>%
  mutate(
    yhat = predict(lmer.7, newdata = ., re.form = NA),
    Sex = factor(female, levels = c(0, 1), labels = c("Male", "Female"))
  )


head(plot_data_2)


# Create plot
ggplot(data = plot_data_2, aes(x = grade_quant_center, y = yhat, color = Sex, linetype = Sex)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Grade-level", breaks = c(0, 1, 2, 3), labels = c(8, 9, 10, 11)) +
  ylab("Vocabulary score") +
  ggsci::scale_color_d3()
