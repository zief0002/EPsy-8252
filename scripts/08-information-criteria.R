##################################################
### Load libraries
##################################################

library(broom)
library(educate) #Need version 0.1.0.1
library(MuMIn)
library(patchwork)
library(tidyverse)



##################################################
### Read in and prepare data
##################################################

# Import data
usnews = read_csv("~/Documents/github/epsy-8252/data/usnews.csv")


# View data
head(usnews)


# Drop rows with missing data
# Create log-transformed variables
educ = usnews %>%
  drop_na() %>%
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

# Fit Model 1
lm.1 = lm(Lpeer ~ 1 + gre_quant + I(gre_quant^2) + I(gre_quant^3), data = educ)

# Fit Model 2
lm.2 = lm(Lpeer ~ 1 + Lfunded_research_per_faculty + I(Lfunded_research_per_faculty^2) + Lphd_granted_per_faculty  + I(Lphd_granted_per_faculty^2), data = educ)

# Fit Model 3
lm.3 = lm(Lpeer ~ 1 + Ldoc_accept + Lenroll + Lphd_student_faculty_ratio, data = educ)



##################################################
### Log-likelihood values for candidate models
##################################################

logLik(lm.1)
logLik(lm.2)
logLik(lm.3)


# Log-likelihood available in glance() output
glance(lm.1)



##################################################
### Deviance
##################################################

-2 * logLik(lm.1)[1] #Model 1
-2 * logLik(lm.2)[1] #Model 2
-2 * logLik(lm.3)[1] #Model 3



##################################################
### Akiake's Information Criteria (AIC)
##################################################

-2 * logLik(lm.1)[1] + 2*5 #Model 1
-2 * logLik(lm.2)[1] + 2*6 #Model 2
-2 * logLik(lm.3)[1] + 2*5 #Model 3



##################################################
### Use AIC() function
##################################################

# Compute AIC value for Model 1
AIC(lm.1)


# AIC available in glance() output
glance(lm.1)



##################################################
### AIC Second-Order Corrected
##################################################

n = 122
k = 5

# Compute AICc for Model 1
-2 * logLik(lm.1)[[1]] + 2 * k * n / (n - k - 1) #Model 1


# Shortcut with function
AICc(lm.1)
AICc(lm.2)
AICc(lm.3)



##################################################
### Delta-AICc values
##################################################

AICc(lm.1) - AICc(lm.3) #Student-related factors
AICc(lm.2) - AICc(lm.3) #Faculty-related factors
AICc(lm.3) - AICc(lm.3) #Institution-related factors



##################################################
### Relative likelihood
##################################################

exp(-1/2 *  0.00) #Institution-related factors
exp(-1/2 *  8.6)  #Faculty-related factors
exp(-1/2 * 11.1)  #Student-related factors



##################################################
### Evidence ratios
##################################################

1 / 0.014 #Institution-related factors / Faculty-related factors
1 / 0.004 #Institution-related factors / Student-related factors



##################################################
### Model probabilities (Akaike Weight)
##################################################

# Compute sum of relative likelihoods
sum_rel = 1.000000000 + 0.01356856 + 0.003887457


# Compute model probabilities
1.000000000 / sum_rel #Institution-related factors
0.013568560 / sum_rel #Faculty-related factors
0.003887457 / sum_rel #Student-related factors



##################################################
### Table of model evidence
##################################################

#Create table of model evidence
model_evidence = model.sel(
  object = list(lm.1, lm.2, lm.3),
  rank = "AICc"
)


# View output
model_evidence



##################################################
### Pretty printing tables of model evidence
##################################################

# Load libraries for formatting
library(knitr)
library(kableExtra)

# Create data frame to format into table
tab_01 = model_evidence %>%
  mutate(
    Hypothesis = c("H1", "H2", "H3"),
    weight = as.numeric(weight)
  ) %>%
  select(Hypothesis, df, logLik, AICc, delta, weight) %>%
  rename(
    # We can include LaTeX math notation in column names
    # Because \ is a special character we need two \\
    '$K$' = df,
    '$LL$' = logLik,
    '$\\Delta$AICc' = delta,
    'AICc Wt.' = weight
  )


# Format table
kable(tab_01,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "Table of Model Evidence for Three Working Hypotheses",
      digits = 3,
      align = "c",
      row.names = FALSE
) %>%
  footnote(
    general = "K = Model df; LL = Log-Likelihood; AIC Wt. = Model Probability",
    general_title = "Note.",
    footnote_as_chunk = TRUE
  ) %>%
  kable_styling(latex_options = "HOLD_position")





##################################################
### What if we didn't consider functional form of models?
##################################################

# Fit models
lm.1_1 = lm(peer ~ 1 + gre_quant + gre_verbal, data = educ)
lm.2_1 = lm(peer ~ 1 + funded_research_per_faculty + phd_granted_per_faculty, data = educ)
lm.3_1 = lm(peer ~ 1 + doc_accept + enroll + phd_student_faculty_ratio, data = educ)


# Compute model evidence
model.sel(
  object = list(lm.1_1, lm.2_1, lm.3_1),
  rank = "AICc"
)


