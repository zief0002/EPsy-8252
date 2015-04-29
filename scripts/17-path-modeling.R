###################################################
### Read in the data
###################################################

pm = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/path-model.csv")

head(pm)
summary(pm)



###################################################
### Correlation matrix for two predictor model
###################################################

cor(pm[c("achieve", "ability", "motivate")])



###################################################
### Compute z-scores
###################################################

pm$z.achieve = scale(pm$achieve)
pm$z.ability = scale(pm$ability)
pm$z.motivate = scale(pm$motivate)

head(pm)



###################################################
### Fit standardized models (using z-scores)
###################################################

summary(lm(z.achieve ~ z.ability + z.motivate, data = pm))
summary(lm(z.motivate ~ z.ability, data = pm))



###################################################
### Method 2: Fit unstandardized models then obtain standardized coefficients
###################################################

lm.achieve = lm(achieve ~ ability + motivate, data = pm)
lm.motivate = lm(motivate ~ ability, data = pm)

library(QuantPsyc)
lm.beta(lm.achieve)
lm.beta(lm.motivate)



###################################################
### Method 3: Convert entire data frame to z-scores fit standardized models
###################################################

pmz = data.frame(Make.Z(pm))
head(pmz)

lm.achieve = lm(achieve ~ ability + motivate, data = pmz)
summary(lm.achieve)

lm.motivate = lm(motivate ~ ability, data = pmz)
summary(lm.motivate)



###################################################
### Estimate direct effects - Simultaneous regression
###################################################

lm.direct = lm(achieve ~ fam_back + ability + motivate + courses, data = pmz)
summary(lm.direct)



###################################################
### Estimate total effects - Sequential regression
###################################################

lm.total.fam = lm(achieve ~ fam_back, data = pmz)
summary(lm.total.fam)

lm.total.ability = lm(achieve ~ fam_back + ability, data = pmz)
summary(lm.total.ability)

lm.total.motivate = lm(achieve ~ fam_back + ability + motivate, data = pmz)
summary(lm.total.motivate)

lm.total.courses = lm(achieve ~ fam_back + ability + motivate + courses, data = pmz)
summary(lm.total.courses)



