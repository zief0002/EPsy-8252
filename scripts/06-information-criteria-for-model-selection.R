##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(dplyr)
library(readr)



##################################################
### Read in data
##################################################

ed = read_csv(file = "~/Dropbox/epsy-8252/data/ed-schools-2010.csv")
head(ed)



##################################################
### Fit candidate models
##################################################

lm.1 = lm(peer_rate ~ 1 + gre, data = ed)
lm.2 = lm(peer_rate ~ 1 + sf_ratio + phd_fac_ratio, data = ed)
lm.3 = lm(peer_rate ~ 1 + doc_accept + avg_res, data = ed)



##################################################
### Compute log-likelihood for the candidate models
##################################################

logLik(lm.1)
logLik(lm.2)
logLik(lm.3)

# Also available from the glance() output
glance(lm.1)



##################################################
### Compute deviance values
##################################################

-2 * logLik(lm.1)[1] #Model 1
-2 * logLik(lm.2)[1] #Model 2
-2 * logLik(lm.3)[1] #Model 3



##################################################
### Compute AIC values
##################################################

-2 * logLik(lm.1)[1] + 2*3 #Model 1
-2 * logLik(lm.2)[1] + 2*4 #Model 2
-2 * logLik(lm.3)[1] + 2*4 #Model 3



##################################################
### Compute corrected AIC (AICc) values
##################################################

n = 52

# Compute AICc for Model 1, B, and C
-2 * logLik(lm.1)[[1]] + 2 * 3 * n / (n - 3 - 1) #Model 1
-2 * logLik(lm.2)[[1]] + 2 * 4 * n / (n - 4 - 1) #Model 2
-2 * logLik(lm.3)[[1]] + 2 * 4 * n / (n - 4 - 1) #Model 3

# Can also use the AICc() function
AICc(lm.1)
AICc(lm.2)
AICc(lm.3)



##################################################
### Compute Delta AICc
##################################################

AICc(lm.2) - AICc(lm.1) #Model 1
AICc(lm.2) - AICc(lm.2) #Model 2
AICc(lm.2) - AICc(lm.3) #Model 3



##################################################
### Compute relative likelihoods
##################################################

exp(-1/2 * 2.96) #Model 1
exp(-1/2 * 0.00) #Model 2
exp(-1/2 * 6.95) #Model 3



##################################################
### Compute evidence ratios
##################################################

1/.228    #H2 vs. H1
1/.031    #H2 vs. H3
.228/.031 #H1 vs. H3



##################################################
### Compute model probabilities
##################################################

# Compute sum of relative probabilities
sum_rel = 1 + .228 + .031 


.228 / sum_rel #Model 1
1 / sum_rel    #Model 2
.031 / sum_rel #Model 3



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
### Compute evidence ratios using evidence() function
##################################################

# Evidence Ratios
evidence(myAIC, model.high = "Model 2", model.low = "Model 1")
evidence(myAIC, model.high = "Model 2", model.low = "Model 2")
evidence(myAIC, model.high = "Model 1", model.low = "Model 3")



##################################################
### Pretty-Printing Model Evidence Tables in Markdown
##################################################

x = data.frame(myAIC) %>%
  select(
    Model = Modnames, 
    LL, K, AICc, Delta_AICc,
    w_i = AICcWt
  ) %>%
  mutate(
    ER = max(w_i) / w_i
  )

# Here we employ indexing to change the fifth column name
# We use LaTeX math notation in the names to use the Greek letter Delta
names(x)[5] = '$\\Delta$AICc'

# Here we employ indexing to change the sixth column name
# We use LaTeX math notation to write a subscript
names(x)[6] = '$w_i$'

kable(x, caption = "Table of Model Evidence for Three Candidate Models. 
                    (LL = Log-Likelihood; K = Model df; $w_i$ = Model 
                     Probability; ER = Evidence Ratio)")



