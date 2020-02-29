##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
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
head(joined_data)



##################################################
### Fit the unconditional random intercepts model
##################################################

# Fit model
lmer.0 = lmer(language_post ~ 1 + (1 | school_id), data = joined_data, REML = FALSE)


# View coefficients and variance components
tidy(lmer.0)

4.41 ^ 2 #Compute var(b_0)
8.04 ^ 2 #Compute var(e)


# Alternative wayt oview coefficients and variance components
summary(lmer.0)



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
lmer.1 = lmer(language_post ~ 1 + verbal_iq + (1 | school_id), data = joined_data, REML = FALSE)



# View coefficients and variance components
tidy(lmer.1)

3.08 ^ 2 #Compute var(b_0)
6.50 ^ 2 #Compute var(e)


# Compute difference in variance components
(64.6 - 42.2) / 64.6 #Student-level
(19.4 - 9.49) / 19.4 #School-level



##################################################
### Table of model evidence
##################################################

aictab(
  cand.set = list(lmer.0, lmer.1),
  modnames = c("Model A", "Model B")
)



##################################################
### Fit model with verbal IQ, SES, and public as predictors
##################################################

lmer.2 = lmer(language_post ~ 1 + verbal_iq + ses + public + (1 | school_id), 
              data = joined_data, REML = FALSE)


# View coefficients and variance components
tidy(lmer.2)

2.93 ^ 2 #Compute var(b_0)
6.33 ^ 2 #Compute var(e)


# Compute difference in variance components
(64.6 - 40.0) / 64.6 #Student-level
(19.4 - 8.57) / 19.4 #School-level



##################################################
### Table of model evidence
##################################################

aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
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
  var_comp = c("$\\sigma^2_{0}$", "$\\sigma^2_{\\epsilon}$"),
  mod_1 = c(19.43, 64.57),
  mod_2 = c( 9.50, 42.23),
  mod_3 = c( 8.57, 40.04)
) %>%
  kable(
    col.names = c("Estimate", "Model A", "Model B", "Model C"),
    caption = "Variance Estimates from Fitting the Unconditional Random Intercepts Model (Model A), the Conditional Random Intercepts Model with Verbal IQ as a Fixed-Effect (Model B), and the Conditional Random Intercepts Model with Verbal IQ, SES, and School Type as a Fixed-Effects (Model C)",
    format = 'html',
    align = "c"
  )  %>%
  column_spec(1:4, width = "1in")



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


