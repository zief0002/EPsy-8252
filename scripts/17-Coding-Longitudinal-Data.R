###################################################
### Read in the data
###################################################

dietData = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/Diet-Long-Format.csv")
head(dietData)





###################################################
### Analyze data (measurement wave)
###################################################

# Examine plot
library(ggplot)

ggplot(data = dietData, aes(x = wave, y = weight, group = id)) +
	geom_line(alpha = 0.3) +
	stat_summary(fun.y = "mean", geom = "line", aes(group = 1), lwd = 2) +
	theme_bw() +
	facet_wrap(~diet)


library(lme4)
lmer.1 = lmer(weight ~ 1 + wave + (1 + wave | id), data = dietData)
summary(lmer.1)

lmer.1 = lmer(weight ~ 1 + I(wave-1) + (1 + I(wave-1) | id), data = dietData)
summary(lmer.1)

lmer.2 = lmer(weight ~ 1 + I(wave-1) + diet + (1 + I(wave-1) | id), data = dietData)
summary(lmer.2)

lmer.3 = lmer(weight ~ 1 + I(wave-1) + diet + I(wave-1):diet + (1 + I(wave-1) | id), data = dietData)
summary(lmer.3)




###################################################
### Analyze data (actual time: months)
###################################################

# Examine plot
library(ggplot)

ggplot(data = dietData, aes(x = month, y = weight, group = id)) +
	geom_line(alpha = 0.3) +
	stat_summary(fun.y = "mean", geom = "line", aes(group = 1), lwd = 2) +
	theme_bw() +
	facet_wrap(~diet)


library(lme4)
lmer.4 = lmer(weight ~ 1 + month + (1 + month | id), data = dietData)
summary(lmer.4)

lmer.5 = lmer(weight ~ 1 + month + diet + (1 + month | id), data = dietData)
summary(lmer.5)

lmer.6 = lmer(weight ~ 1 + month + diet + month:diet + (1 + month | id), data = dietData)
summary(lmer.6)




