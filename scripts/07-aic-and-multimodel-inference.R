## ------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(digits = 3)
library(printr)
library(dplyr)



## ------------------------------------------------------------------------
mn = read.csv(file = "/Users/andrewz/Documents/GitHub/EPsy-8252/data/mnSchools.csv")
head(mn)



## ------------------------------------------------------------------------
lm.a = lm(gradRate ~ public + sat + tuition, data = mn)
lm.b = lm(gradRate ~ public + sat + tuition + public:sat, data = mn)
lm.c = lm(gradRate ~ public + sat + tuition + public:tuition, data = mn)
lm.d = lm(gradRate ~ public + sat + tuition + public:sat + public:tuition, data = mn)



## ------------------------------------------------------------------------
# Compute AICc for Model A
n = 33
k = 5
a = -2 * logLik(lm.a)[[1]] + 2 * k * n / (n - k - 1)


# Compute AICc for Model B
n = 33
k = 6
b = -2 * logLik(lm.b)[[1]] + 2 * k * n / (n - k - 1)


# Compute AICc for Model C
n = 33
k = 6
c = -2 * logLik(lm.c)[[1]] + 2 * k * n / (n - k - 1)


# Compute AICc for Model D
n = 33
k = 7
d = -2 * logLik(lm.d)[[1]] + 2 * k * n / (n - k - 1)



## ------------------------------------------------------------------------
modTab = data.frame(
  Model = c("Model A", "Model B", "Model C", "Model D"),
  AICc = c(a, b, c, d)
)
arrange(modTab, AICc)



## ------------------------------------------------------------------------
# Compute dalta values
delta_a = a - a
delta_b = b - a
delta_c = c - a
delta_d = d - a



## ------------------------------------------------------------------------
modTab2 = data.frame(
  Model = c("Model A", "Model B", "Model C", "Model D"),
  Delta = c(delta_a, delta_b, delta_c, delta_d)
)

arrange(modTab2, Delta)




## ------------------------------------------------------------------------
w_a = exp(-1/2 * delta_a) 
w_b = exp(-1/2 * delta_b)
w_c = exp(-1/2 * delta_c)
w_d = exp(-1/2 * delta_d)



## ------------------------------------------------------------------------
modTab3 = data.frame(
  Model = c("Model A", "Model B", "Model C", "Model D"),
  Weight = c(w_a, w_b, w_c, w_d)
)
arrange(modTab3, desc(Weight))



## ------------------------------------------------------------------------
er_a = w_a / w_a 
er_b = w_a / w_b  
er_c = w_a / w_c
er_d = w_a / w_d



## ------------------------------------------------------------------------
modTab4 = data.frame(
  Model = c("Model A", "Model B", "Model C", "Model D"),
  ER = c(er_a, er_b, er_c, er_d)
)
arrange(modTab4, ER)



## ------------------------------------------------------------------------
mp_a = w_a / (w_a + w_b + w_c + w_d)
mp_b = w_b / (w_a + w_b + w_c + w_d)
mp_c = w_c / (w_a + w_b + w_c + w_d)
mp_d = w_d / (w_a + w_b + w_c + w_d)



## -----------------------------------------------------------------------
modTab5 = data.frame(
  Model = c("Model A", "Model B", "Model C", "Model D"),
  Prob = c(mp_a, mp_b, mp_c, mp_d)
)
arrange(modTab5, desc(Prob))



## ------------------------------------------------------------------------
library(AICcmodavg)

# AICc Table for Model Selection
myAIC = aictab(
  cand.set = list(lm.a, lm.b, lm.c, lm.d), 
  modnames = c("Model A", "Model B", "Model C", "Model D")
  )
myAIC

# Evidence Ratios
evidence(myAIC, model.high = "Model A", model.low = "Model B")
evidence(myAIC, model.high = "Model A", model.low = "Model C")
evidence(myAIC, model.high = "Model A", model.low = "Model D")



## ------------------------------------------------------------------------
# Examine object
str(myAIC)



## ------------------------------------------------------------------------
# Create new columns
myAIC$ER = max(myAIC$AICcWt) / myAIC$AICcWt
myAIC$Prob = myAIC$AICcWt / sum(myAIC$AICcWt)
myAIC



## ------------------------------------------------------------------------
myAIC = myAIC[ , c("Modnames", "K", "AICc", "Delta_AICc", "AICcWt", "ER", "Prob")]
names(myAIC) = c("", "K", "AICc", "Delta", "Weight", "Evidence Ratio", "Model Probability")



## ------------------------------------------------------------------------
knitr::kable(myAIC, row.names = FALSE)



## ------------------------------------------------------------------------
AICc(lm.a)

