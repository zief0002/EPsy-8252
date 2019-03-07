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

lmer.0 = lmer(language_post ~ 1 + (1 | school_id), data = joined_data, REML = FALSE)

summary(lmer.0)

tidy(lmer.0)



##################################################
### Compute unexplained variance
##################################################

# Total unexplained variance
19.43 + 64.57

# Proportion of unexplained variance at the school-level
19.43 / (19.43 + 64.57)

# Proportion of unexplained variance at the student-level
64.57 / (19.43 + 64.57)



##################################################
### Fit model with verbal IQ as predictor
##################################################

lmer.1 = lmer(language_post ~ 1 + verbal_iq + (1 | school_id), data = joined_data, REML = FALSE)

summary(lmer.1)


# Compute difference in variance components
(64.57 - 42.23) / 64.57 #Student-level
(19.43 - 9.50) / 19.43 #School-level



##################################################
### Table of model evidence
##################################################

aictab(
  cand.set = list(lmer.0, lmer.1),
  modnames = c("Model 1", "Model 2")
)



##################################################
### Fit model with verbal IQ, SES, and public as predictors
##################################################

lmer.2 = lmer(language_post ~ 1 + verbal_iq + ses + public + (1 | school_id), 
              data = joined_data, REML = FALSE)

summary(lmer.2)

# Compute difference in variance components
(64.57 - 40.04) / 64.57 #Student-level
(19.43 - 8.57) / 19.43 #School-level



##################################################
### Table of model evidence
##################################################

aictab(
  cand.set = list(lmer.0, lmer.1, lmer.2),
  modnames = c("Model 1", "Model 2", "Model 3")
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
  column.labels = c("Model 1", "Model 2", "Model 3"),
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
  var_comp = c("$\\sigma^2_{\\epsilon}$", "$\\sigma^2_{0}$"),
  mod_1 = c(64.57, 19.43),
  mod_2 = c(42.23, 9.50),
  mod_3 = c(40.04, 8.57)
) %>%
  kable(
    col.names = c("Estimate", "Model 1", "Model 2", "Model 3"),
    caption = "Variance Estimates from Fitting the Unconditional Random Intercepts Model (Model 1), the Conditional Random Intercepts Model with Verbal IQ as a Fixed-Effect (Model 2), and the Conditional Random Intercepts Model with Verbal IQ, SES, and School Type as a Fixed-Effects (Model 3)",
    format = 'html',
    align = "c"
  )  %>%
  column_spec(1:4, width = "1in")



##################################################
### Plot of the fitted final model
##################################################

# Set up plotting data for lmer.2
plot_data = crossing(
  verbal_iq = seq(from = -7.83, to = 6.17, by = 0.01),
  ses = 27.81,
  public = c(0, 1)
)


# Predict life satisfaction and turn school_type into a factor for better plotting
plot_data = plot_data %>%
  mutate(
    yhat = predict(lmer.2, newdata = ., re.form = NA),
    school_type = factor(public, levels = c(0, 1), labels = c("Private", "Public"))
  )


head(plot_data)


ggplot(data = plot_data, aes(x = verbal_iq, y = yhat, color = school_type, linetype = school_type)) +
  geom_line() +
  theme_bw() +
  xlab("Verbal IQ score") +
  ylab("Predicted post-test language score") +
  ggsci::scale_color_d3(name = "School type") +
  scale_linetype_discrete(name = "School type")