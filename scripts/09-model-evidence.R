##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(tidyr)



##################################################
### Read in and prepare data
##################################################

# Read in data
ed = read_csv(file = "~/Documents/github/epsy-8252/data/ed-schools-2018.csv")


# Drop rows with missing data
educ = ed %>%
  drop_na()


# Create log-transformed variables
educ = educ %>%
  mutate(
    Lpeer = log(peer),
    Lfunded_research_per_faculty = log(funded_research_per_faculty),
    Lphd_granted_per_faculty = log(phd_granted_per_faculty + 1),
    Ldoc_accept = log(doc_accept),
    Lphd_student_faculty_ratio = log(phd_student_faculty_ratio + 1),
    Lenroll = log(enroll)
  )



##################################################
### Fit candidate models
##################################################

lm.1 = lm(Lpeer ~ 1 + gre_quant + I(gre_quant^2) + I(gre_quant^3), data = educ)
lm.2 = lm(Lpeer ~ 1 + Lfunded_research_per_faculty + I(Lfunded_research_per_faculty^2) + Lphd_granted_per_faculty  + I(Lphd_granted_per_faculty^2), data = educ)
lm.3 = lm(Lpeer ~ 1 + Ldoc_accept + Lenroll + Lphd_student_faculty_ratio, data = educ)
lm.4 = lm(Lpeer ~ 1 + Ldoc_accept + Lphd_student_faculty_ratio, data = educ)



##################################################
### Compute corrected AIC (AICc) values
##################################################

n = 122
k = 5


# Compute AICc for Model 1
-2 * logLik(lm.1)[[1]] + 2 * k * n / (n - k - 1) #Model 1


# Can also use the AICc() function
AICc(lm.1)
AICc(lm.2)
AICc(lm.3)
AICc(lm.4)



##################################################
### Compute Delta AICc
##################################################

AICc(lm.1) - AICc(lm.4) #Student-related factors
AICc(lm.2) - AICc(lm.4) #Faculty-related factors
AICc(lm.4) - AICc(lm.4) #Institution-related factors



##################################################
### Compute relative likelihoods
##################################################

exp(-1/2 * 12.09) #Student-related factors
exp(-1/2 *  9.56) #Faculty-related factors
exp(-1/2 *  0.00) #Institution-related factors



##################################################
### Model probabilities / AICc weights
##################################################

# Compute sum of relative likelihoods
sum_rel = 1.000000000 + 0.008376636 + 0.002366380


0.002366380 / sum_rel #Student-related factors
0.008376636 / sum_rel #Faculty-related factors
1.000000000 / sum_rel #Institution-related factors



##################################################
### Compute table of model evidence using aictab() function
##################################################

myAIC = aictab(
  cand.set = list(lm.1, lm.2, lm.3),
  modnames = c("Model 1", "Model 2", "Model 3")
)


# View table
myAIC



##################################################
### Compute evidence ratios
##################################################

# Evidence Ratio 1
evidence(
  myAIC,
  model.high = "Institution-related factors (Reduced)",
  model.low = "Faculty-related factors"
)


# Evidence Ratio 2
evidence(
  myAIC,
  model.high = "Institution-related factors (Reduced)",
  model.low = "Student-related factors"
)



##################################################
### Empirical support for candidate models
##################################################

confset(
  cand.set = list(lm.1, lm.2, lm.4),
  modnames = c("Student-related factors", "Faculty-related factors", "Institution-related factors (Reduced)"),
  method = "ordinal"
)



##################################################
### What if we had done no transforming?
##################################################

# Fit candidate models
lm.1 = lm(peer ~ 1 + gre_quant + gre_verbal, data = educ)
lm.2 = lm(peer ~ 1 + funded_research_per_faculty + phd_granted_per_faculty, data = educ)
lm.3 = lm(peer ~ 1 + doc_accept + enroll + phd_student_faculty_ratio, data = educ)


# Empirical support
confset(
  cand.set = list(lm.1, lm.2, lm.3),
  modnames = c("Student-related factors", "Faculty-related factors", "Institution-related factors (Full)"),
  method = "ordinal"
)

