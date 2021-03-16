##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom.mixed)
library(educate)
library(lme4)
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

# View data
head(joined_data)



##################################################
### Fit the unconditional random intercepts model
##################################################

# Fit model
lmer.a = lmer(language_post ~ 1 + (1 | school_id), data = joined_data, REML = FALSE)


# View coefficients and variance components
tidy(lmer.a)

4.41 ^ 2 #Compute var(b_0)
8.04 ^ 2 #Compute var(e)


# Alternative way to view coefficients and variance components
summary(lmer.a)



##################################################
### Compute unexplained variance
##################################################

# Total unexplained variance
19.4 + 64.6


# Proportion of unexplained variance at the school-level
19.4 / (19.4 + 64.6)


# Proportion of unexplained variance at the student-level
64.6 / (19.4 + 64.6)



##################################################
### Fit model with verbal IQ as predictor
##################################################

# Fit model
lmer.b = lmer(language_post ~ 1 + verbal_iq + (1 | school_id), 
              data = joined_data, REML = FALSE)



# View coefficients and variance components
tidy(lmer.b)

3.08 ^ 2 #Compute var(b_0)
6.50 ^ 2 #Compute var(e)


# Compute difference in variance components
(64.6 - 42.2) / 64.6 #Student-level
(19.4 - 9.49) / 19.4 #School-level



##################################################
### Table of model evidence
##################################################

aictab(
  cand.set = list(lmer.a, lmer.b),
  modnames = c("Model A", "Model B")
)



##################################################
### Fit model with verbal IQ, SES, and public as predictors
##################################################

lmer.c = lmer(language_post ~ 1 + verbal_iq + ses + public + (1 | school_id), 
              data = joined_data, REML = FALSE)


# View coefficients and variance components
tidy(lmer.c)

2.93 ^ 2 #Compute var(b_0)
6.33 ^ 2 #Compute var(e)


# Compute difference in variance components
(64.6 - 40.0) / 64.6 #Student-level
(19.4 - 8.57) / 19.4 #School-level



##################################################
### Table of model evidence
##################################################

aictab(
  cand.set = list(lmer.a, lmer.b, lmer.c),
  modnames = c("Model A", "Model B", "Model C")
)



##################################################
### Stargazer table of models
##################################################

library(stargazer)

# For printing to screen
stargazer(
  lmer.0, lmer.1, lmer.2,
  type = "text", #Change to type='html' for markdown
  title = "Fixed-Effects Coefficients and Standard Errors for a Taxonomy of Fitted Models to Predict Post-Test Language Scores for 2,287 Students from 131 Schools. All Models Included a Random-Effect of Intercept and were Fitted using Maximum Likelihood.",
  column.labels = c("Model A", "Model B", "Model C"),
  colnames = FALSE,
  model.numbers = FALSE,
  dep.var.caption = "Outcome: Post-Test Language Scores",
  dep.var.labels.include = FALSE,
  covariate.labels = c("Verbal IQ scores", "SES", "School type"),
  keep.stat = c("ll"),
  notes.align = "l",
  add.lines = list(c("Corrected AIC", round(AICc(lmer.0), 1), round(AICc(lmer.1), 1),
                     round(AICc(lmer.2), 1))),
  star.cutoffs = NA, # Omit stars
  omit.table.layout = "n" #Don't show table notes

)



##################################################
### Table of variance components
##################################################

library(knitr) #Need for kable() function
library(kableExtra) #Need for functions to pretty-up the table

data.frame(
  level = c("School", "Student"),
  var_comp = c("$\\sigma^2_{0}$", "$\\sigma^2_{\\epsilon}$"),
  mod_1 = c(19.40, 64.60),
  mod_2 = c( 9.49, 42.20),
  mod_3 = c( 8.57, 40.00)
) %>%
  kable(
    col.names = c("Level", "Estimate", "Model A", "Model B", "Model C"),
    caption = "Variance estimates from fitting the unconditional random intercepts model (Model A), the conditional random intercepts model with verbal IQ as a fixed-effect (Model B), and the conditional random intercepts model with verbal IQ, SES, and school type as fixed-effects (Model C).",
    format = 'html',
    escape = FALSE,
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c")
  )  %>%
  kable_classic()



##################################################
### Plot of the fitted final model
##################################################

ggplot(data = joined_data, aes(x = verbal_iq, y = language_post)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = 41.2, slope = 2.25, color = "darkblue", linetype = "dashed") +
  geom_abline(intercept = 42.7, slope = 2.25, color = "darkred", linetype = "solid") +
  theme_bw() +
  xlab("Verbal IQ score") +
  ylab("Predicted post-test language score")


