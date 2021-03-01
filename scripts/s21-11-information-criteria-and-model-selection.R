##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(tidyverse)



##################################################
### Read in and prepare data
##################################################

# Import data
mn = read_csv(file = "https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mn-schools.csv")


# View data
head(mn)



##################################################
### Fit candidate models
##################################################

lm.1 = lm(grad ~ 1 + sat,            data = mn)
lm.2 = lm(grad ~ 1 + sat + I(sat^2), data = mn)
lm.3 = lm(grad ~ 1 + log(sat),       data = mn)



##################################################
### Akiake's Information Criteria (AIC)
##################################################

# Compute AIC for model associated with linear hypothesis
# logLik(lm.1)
-2*-113.5472 + 2*3



##################################################
### Use AIC() and glance() functions
##################################################

AIC(lm.1) #Linear
AIC(lm.2) #Quadratic
AIC(lm.3) #Log-linear


# AIC available in glance() output
glance(lm.1)



##################################################
### AIC Second-Order Corrected
##################################################

n = 33
k = 3

# Compute AICc for Model 1
-2 * logLik(lm.1)[[1]] + 2 * k * n / (n - k - 1)


# Shortcut with function
AICc(lm.1) #Linear
AICc(lm.2) #Quadratic
AICc(lm.3) #Log-linear



##################################################
### Delta-AICc values
##################################################

AICc(lm.1) - AICc(lm.2)  #Linear
AICc(lm.2) - AICc(lm.2)  #Quadratic
AICc(lm.3) - AICc(lm.2)  #Log-linear



##################################################
### Relative likelihood
##################################################

exp(-1/2 * 5.37686)  #Linear
exp(-1/2 * 0)        #Quadratic
exp(-1/2 * 1.845128) #Log-linear



##################################################
### Evidence ratios
##################################################

1 / 0.397  #Quadratic / Log-linear
1 / 0.0068 #Quadratic / Linear



##################################################
### Model probabilities (Akaike Weight)
##################################################

# Compute sum of relative likelihoods
sum_rel = 0.0679876 + 1 + 0.3974985


# Compute model probability for each model
0.0679876 / sum_rel #Linear
1 / sum_rel         #Quadratic
0.3974985 / sum_rel #Log-linear



##################################################
### Table of model evidence
##################################################

#Create table of model evidence
model_evidence = aictab(
  cand.set = list(lm.1, lm.2, lm.3), 
  modnames = c("Linear", "Quadratic", "Log-linear")
)


# View output
model_evidence



##################################################
### Pretty printing tables of model evidence
##################################################

# Create data frame to format into table
tab_01 = model_evidence %>%
  data.frame() %>%
  select(-LL, -Cum.Wt)


# View table
tab_01


# Load libraries for formatting
library(knitr)
library(kableExtra)


# Create knitted table
tab_01 %>%
  kable(
    format = "html",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c("Hypothesis", "k", "AICc", "$\\Delta$AICc", "Rel($\\mathcal{L}$)", "AICc Weight"),
    caption = "Models rank-ordered by the amount of empirical support as measured by the AICc. The sample size used to fit each model was $n=33$.",
    digits = 1,
    table.attr = "style='width:50%;'"
  ) %>%
  footnote(
    general = "Rel($\\mathcal{L}$) = Relative Likelihood",
    general_title = "Note.",
    footnote_as_chunk = TRUE
  ) %>%
  kable_classic()






