##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(dplyr)
library(ez)
library(ggplot2)
library(readr)
library(tidyr)
library(lme4)




###################################################
### Read in data
###################################################

seasonal = read_csv(file = "~/Dropbox/epsy-8252/data/seasonal-depression.csv")
head(seasonal)


colMeans(seasonal[2:5])


###################################################
### Prepare data
###################################################

seasonal_long = seasonal %>% 
  gather(season, beck, t1:t4) %>%
  arrange(subject, season)

seasonal = seasonal %>% 
  mutate(season = gsub(x = time, pattern = "t", replacement = "s")) %>%
  dplyr::select(subject, season, beck)



###################################################
### Plot and describe data
###################################################

ggplot(data = seasonal_long, aes(x = time, y = beck, group = subject)) + 
  geom_line() +
  geom_point() +
  #stat_summary(group = 1, fun.y = mean, geom = "line", color = "blue", lwd = 1.5) +
  #stat_summary(group = 1, fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  scale_x_discrete(
    name = "Time",
    labels = c("T1 \n(Winter)", "T2 \n(Spring)", "T3 \n(Summer)", "T4 \n(Fall)")
    ) + 
  scale_y_continuous(
    name = "Beck depression score"
  ) +
  facet_wrap(~subject)


seasonal_long %>%
  na.omit() %>%
  group_by(time) %>%
  summarize(
    M = mean(beck), SD = sd(beck), N = n()
    )


seasonal %>% 
  select(t1:t4) %>%
  correlate() %>%
  shave() %>%
  fashion(decimals = 3)


###################################################
### RM-ANOVA
###################################################

bp = backpain %>% 
  na.omit() %>%
  gather(time, pain, t1:t4) %>%
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("Control", "Treatment")))

  
# Fit RM-ANOVA
ezANOVA(data = seasonal_long, dv = beck, wid = factor(subject),
        within = time, return_aov = TRUE)


# Fit RM-ANOVA with both within- and between-subjects factors
ezANOVA(data = bp, dv = pain, wid = factor(id), 
        within = time, between = treatment, return_aov = TRUE)


ggplot(data = bp, aes(x = time, y = pain, color = treatment)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", group = 1) +
  theme_bw() +
  xlab("Time") +
  ylab("Mean pain") +
  facet_wrap(~treatment) +
  ggsci::scale_color_d3() +
  guides(color = FALSE)


###################################################
### manova
###################################################

outcome = cbind(seasonal_wide$s1, seasonal_wide$s2, seasonal_wide$s3, seasonal_wide$s4)      
mod.mlm = lm(outcome ~ 1)

idata = data.frame(time = c("s1", "s2", "s3", "s4"))
manova1 = car::Anova(mod.mlm, idata = idata, idesign = ~time, type = 3)
summary(manova1)



###################################################
### lmer
###################################################


lmer.0 = lmer(beck ~ 1 + (1 | subject), data = seasonal_long)
lmer.1 = lmer(beck ~ 1  + time + (1 | subject), data = seasonal_long)

summary(lmer.1)



anova(lmer.0, lmer.1)
car::Anova(lmer.1, test.statistic = "F")

library(nlme)
anova(lme(beck ~ 1  + time, random = ~1 | subject, data = seasonal_long))
summary(lme(beck ~ 1  + time, random = ~1 | subject, data = seasonal_long))






seasonal_long = seasonal_long %>%
  mutate(time2 = as.integer(gsub(time, pattern = "t", replacement = "")))

lmer.1 = lmerTest::lmer(beck ~ 1  + time2 + I(time2^2) + (1 | subject), data = seasonal_long)
summary(lmer.1)

anova(lmer.0, lmer.1, refit(NULL))

###################################################
### MANOVA
###################################################

outcome = cbind(backpain$t1, backpain$t2, backpain$t3, backpain$t4)      
mod.mlm = lm(outcome ~ 1)

idata = data.frame(time = c("t1", "t2", "t3", "t4"))
manova1 = Anova(mod.mlm, idata = idata, idesign = ~time, type = 3)
summary(manova1)


# INclude treatment as a between-subject factor
mod.mlm2 = lm(cbind(t1, t2, t3, t4) ~ 1 + treatment, data = backpain)
manova2 = Anova(mod.mlm2, idata = idata, idesign = ~time, type = 3)
summary(manova2)




###########################################################
#                        					                        #
#				            HORMONE THERAPY DATA                  #
#  									                                      #
###########################################################

hormone = read_csv(file = "~/Dropbox/epsy-8282/data/hormone.csv")
head(hormone)

hormone_long = hormone %>% 
  gather(time, hdi, t1:t4) %>%
  arrange(id, time) %>%
  mutate(condition = factor(condition))

hormone_long

ggplot(data = hormone_long, aes(x = time, y = hdi, group = id)) + 
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.4) +
  stat_summary(group = 1, fun.y = mean, geom = "line", color = "blue", lwd = 1.5) +
  stat_summary(group = 1, fun.y = mean, geom = "point", color = "blue", size = 4) +
  theme_bw() +
  facet_wrap(~condition) + 
  xlab("Time") + 
  ylab("Hamilton Depression Inventory score")


# Fit RM-ANOVA with both within- and between-subjects factors
ezANOVA(data = hormone_long, dv = hdi, wid = factor(id), 
        within = time, between = condition, return_aov = TRUE)

# MANOVA
mod.mlm3 = lm(cbind(t1, t2, t3, t4) ~ 1 + condition, data = hormone)
idata = data.frame(time = c("t1", "t2", "t3", "t4"))
manova3 = Anova(mod.mlm3, idata = idata, idesign = ~time, type = 3)
summary(manova3)

