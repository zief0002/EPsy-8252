##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



##################################################
### Read in data
##################################################

ed = read_csv(file = "~/Documents/github/epsy-8252/data/ed-schools-2018.csv")


# View data
head(ed)


# Examine data
nrow(ed)
summary(ed)



##################################################
### Drop rows with missing data
##################################################

# Drop rows with missing data
educ = ed %>%
  drop_na()


# Check resulting data
nrow(educ)
summary(educ)


# Create log-transformed peer ratings
educ = educ %>%
  mutate(
    Lpeer = log(peer)
  )



##################################################
### Fit student-related factors model
##################################################

# Fit  model
lm.1 = lm(Lpeer ~ 1 + gre_quant + I(gre_quant^2) + I(gre_quant^3), data = educ)


# Coefficient-level output
tidy(lm.1)


##################################################
### Fit faculty-related factors model
##################################################

#Create log of the faculty-related predictors
educ = educ %>%
  mutate(
    Lfunded_research_per_faculty = log(funded_research_per_faculty),
    Lphd_granted_per_faculty = log(phd_granted_per_faculty + 1)
  )


# Fit model
lm.2 = lm(Lpeer ~ 1 + Lfunded_research_per_faculty + I(Lfunded_research_per_faculty^2) +
            Lphd_granted_per_faculty  + I(Lphd_granted_per_faculty^2),
          data = educ)


# Coefficient-level output
tidy(lm.2)



##################################################
### Fit the institution-related factors model
##################################################

#Create log of the faculty-related predictors
educ = educ %>%
  mutate(
    Ldoc_accept = log(doc_accept),
    Lphd_student_faculty_ratio = log(phd_student_faculty_ratio + 1),
    Lenroll = log(enroll)
  )


# Fit model
lm.3 = lm(Lpeer ~ 1 + Ldoc_accept + Lenroll + Lphd_student_faculty_ratio, data = educ)


# Coefficient-level output
tidy(lm.3)



##################################################
### Fit the institution-related factors model (reduced)
##################################################

# Fit model
lm.4 = lm(Lpeer ~ 1 + Ldoc_accept + Lphd_student_faculty_ratio, data = educ)


# Coefficient-level output
tidy(lm.4)



##################################################
### Log-likelihood values for candidate models
##################################################

logLik(lm.1)
logLik(lm.2)
logLik(lm.3)
logLik(lm.4)


# Log-likelihood available in glance() output
glance(lm.1)



##################################################
### Deviance
##################################################

-2 * logLik(lm.1)[1] #Model 1
-2 * logLik(lm.2)[1] #Model 2
-2 * logLik(lm.3)[1] #Model 3
-2 * logLik(lm.4)[1] #Model 4



##################################################
### Akiake's Information Criteria (AIC)
##################################################

-2 * logLik(lm.1)[1] + 2*5 #Model 1
-2 * logLik(lm.2)[1] + 2*6 #Model 2
-2 * logLik(lm.3)[1] + 2*5 #Model 3
-2 * logLik(lm.4)[1] + 2*4 #Model 4



##################################################
### Use AIC() function
##################################################

# Compute AIC value for Model 4
AIC(lm.4)


# AIC available in glance() output
glance(lm.4)



##################################################
### What if we had done no transforming?
##################################################

# Fit models
lm.1 = lm(peer ~ 1 + gre_quant + gre_verbal, data = educ)
lm.2 = lm(peer ~ 1 + funded_research_per_faculty + phd_granted_per_faculty, data = educ)
lm.3 = lm(peer ~ 1 + doc_accept + enroll + phd_student_faculty_ratio, data = educ)


# Compute AIC values
AIC(lm.1)
AIC(lm.2)
AIC(lm.3)

