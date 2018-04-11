##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(corrr)
library(dplyr)
library(ggplot2)
library(lme4) #for fitting mixed-effects models
library(readr)
library(texreg)
library(tidyr)



##################################################
### Read in data
##################################################

reading_wide = read_csv(file = "~/Dropbox/epsy-8252/data/reading-wide.csv")
head(reading_wide)



##################################################
### Format data: Wide to long format
##################################################

reading_long = reading_wide %>%
  gather(key = "grade_cat", value = "read_score", read.5:read.8) %>%
  arrange(studentID, grade_cat)

head(reading_long, 12)



##################################################
### Use numeric time predictor
##################################################

# Map year_cat to continuous years
lookup_table = tibble(
  grade_cat = c("read.5", "read.6", "read.7", "read.8"),
  grade = c(0, 1, 2, 3)
)

# Join the data
reading = left_join(reading_long, lookup_table, by = "grade_cat")
head(reading)



##################################################
### Summarize data
##################################################

summary(reading)

reading = reading %>%
  drop_na()



##################################################
### Explore reading scores over time
##################################################


# Compute means and SDs by year
read_summaries = reading %>%
  group_by(grade) %>%
  summarize(
    M = mean(read_score),
    SD = sd(read_score)
  )

read_summaries


# Plot reading scores over time
ggplot(data = reading, aes(x = grade, y = read_score)) +
  geom_line(aes(group = studentID), alpha = 0.3) +
  geom_line(data = read_summaries, aes(x = grade, y = M), size = 1.5, color = "blue") +
  geom_point(data = read_summaries, aes(x = grade, y = M), size = 3, color = "blue") +
  theme_bw() +
  xlab("Grade (centered at 5th grade)") +
  ylab("Reading score")


##################################################
### Examine fixed-effects of time (grade)
##################################################

# Unconditional means model
lmer.0 = lmer(read_score ~ 1 + (1|studentID), data = reading, REML = FALSE)

# Linear growth model
lmer.1 = lmer(read_score ~ 1 + grade + (1|studentID), data = reading, REML = FALSE)

# Quadratic growth model
lmer.2 = lmer(read_score ~ 1 + grade + I(grade^2) + (1|studentID), data = reading, REML = FALSE)

# Likelihood ratio tests
anova(lmer.0, lmer.1, lmer.2)



##################################################
### Examine RE of slope
##################################################

lmer.3 = lmer(read_score ~ 1 + grade + (1 + grade|studentID), data = reading, REML = FALSE)

# Table of model evidence
aictab(
  cand.set = list(lmer.1, lmer.3),
  modnames = c("Linear growth model", "Linear growth model (RE = Linear slopes)")
  )



##################################################
### Examine lmer.3 output
##################################################

summary(lmer.3)


#Get variance-covariance matrix of the random-effects
VarCorr(lmer.3)$studentID



##################################################
### Examine covariates
##################################################

# Compute means and SDs by grade:atRisk
atrisk_summaries = reading %>%
  group_by(grade, atRisk) %>%
  summarize(
    M = mean(read_score),
    SD = sd(read_score)
  ) %>%
  arrange(atRisk, grade)

atrisk_summaries

ggplot(data = reading, aes(x = grade, y = read_score, color = factor(atRisk))) +
  geom_line(aes(group = studentID), alpha = 0.4) +
  geom_line(data = atrisk_summaries, aes(x = grade, y = M, color = factor(atRisk)),
            size = 1.5) +
  geom_point(data = atrisk_summaries, aes(x = grade, y = M, color = factor(atRisk)),
             size = 3) +
  theme_bw() +
  xlab("Grade (centered at 5th grade)") +
  ylab("Reading score") +
  scale_color_manual(name = "Risk status", values = c("red", "blue")) +
  facet_wrap(~factor(atRisk))






##################################################
### Examine effect of atRisk
##################################################

# Main-effect of risk
lmer.4 = lmer(read_score ~ 1 + grade + atRisk + (1 + grade|studentID), data = reading, REML = FALSE)

# Interaction-effect of risk:grade
lmer.5 = lmer(read_score ~ 1 + grade + atRisk + atRisk:grade + (1 + grade|studentID), data = reading, REML = FALSE)

# Likelihood ratio tests
anova(lmer.3, lmer.4, lmer.5)



##################################################
### Interpret output
##################################################

summary(lmer.4)



##################################################
### Plot fixed-effects model
##################################################

plot_data = expand.grid(
  grade = seq(from = 0, to = 3, by = 1),
  atRisk = c(0, 1)
  ) %>%
  mutate(
    yhat = predict(lmer.4, newdata = ., re.form = NA)
  ) %>%
  mutate(
    atRisk = factor(atRisk, levels = c(0, 1), labels = c("Not at-risk", "At-risk"))
  )



ggplot(data = plot_data, aes(x = grade, y = yhat, color = atRisk)) +
  geom_line() +
  theme_bw() +
  xlab("Grade (centered at 5th grade") +
  ylab("Reading score") +
  scale_color_manual(name = "Risk status", values = c("red", "blue"))



##################################################
### Add Student 01's line
##################################################

# ranef(lmer.4)$studentID[2, ]


student_02 = expand.grid(
  grade = seq(from = 0, to = 3, by = 1),
  atRisk = 1,
  studentID = 2
  ) %>%
  mutate(
    yhat = predict(lmer.4, newdata = .)
  )

student_02


ggplot(data = plot_data, aes(x = grade, y = yhat, color = atRisk)) +
  geom_line() +
  theme_bw() +
  xlab("Grade (centered at 5th grade") +
  ylab("Reading score") +
  scale_color_manual(name = "Risk status", values = c("red", "blue")) +
  geom_line(data = student_01, aes(x = grade, y = yhat), color = "blue", linetype = "dotted")


##################################################
### Regression tables
##################################################

screenreg(list(lmer.0, lmer.1, lmer.3, lmer.4), stars = NULL)


# Screenreg displays results to screen. For knitting to a PDF use texreg.


