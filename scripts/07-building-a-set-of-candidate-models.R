##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(educate) #Need version 0.1.0.1
library(patchwork)
library(skimr)
library(tidyverse)



##################################################
### Read in data
##################################################

# Import data
usnews = read_csv("~/Documents/github/epsy-8252/data/usnews.csv")


# View data
head(usnews)


# Examine data
usnews %>%
  skim()



##################################################
### Drop rows with missing data
##################################################

# Drop rows with missing data
educ = ed %>%
  drop_na()


# Check resulting data
educ %>%
  skim()



##################################################
### Explore outcome
##################################################

ggplot(data = educ, aes(x = peer)) +
  geom_density() +
  theme_bw() +
  xlab("Peer rating") +
  ylab("Probability density")



##################################################
### Explore relationships with peer ratings for student-related outcomes model
##################################################

p1 = ggplot(data = educ, aes(x = gre_verbal, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Mean Verbal GRE score")

p2 = ggplot(data = educ, aes(x = gre_quant, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Mean Quantitative GRE score")

p1 + p2



##################################################
### Explore relationships with peer ratings for faculty-related outcomes model
##################################################

p1 = ggplot(data = educ, aes(x = funded_research_per_faculty, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Funded research per faculty member (in thousands of dollars)")

p2 = ggplot(data = educ, aes(x = phd_granted_per_faculty, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Ph.D.s granted per faculty member")

p1 + p2



##################################################
### Explore relationships with peer ratings for institution-related outcomes model
##################################################

p1 = ggplot(data = educ, aes(x = doc_accept, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Acceptance rate of Ph.D. students")

p2 = ggplot(data = educ, aes(x = phd_student_faculty_ratio, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Ph.D. student-to-faculty ratio")

p3 = ggplot(data = educ, aes(x = enroll, y = peer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("Total enrollment")

p1 + p2 + p3



##################################################
### Create log-transformed peer ratings
##################################################

educ = educ %>%
  mutate(
    Lpeer = log(peer)
  )



##################################################
### Explore relationships with ln(peer ratings) for student-related outcomes model
##################################################

p1 = ggplot(data = educ, aes(x = gre_verbal, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("ln(Mean Verbal GRE score)")

p2 = ggplot(data = educ, aes(x = gre_quant, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Peer rating") +
  xlab("ln(Mean Quantitative GRE score)")

p1 + p2


# Correlation matrix
educ %>%
  select(Lpeer, gre_verbal, gre_quant) %>%
  correlate()



##################################################
### Fit student-related factors model
##################################################

# Fit cubic model
lm.1 = lm(Lpeer ~ 1 + gre_quant + I(gre_quant^2) + I(gre_quant^3), data = educ)


# Model-level output
glance(lm.1)


# Coefficient-level output
tidy(lm.1)


# Obtain residuals
out_1 = augment(lm.1)

# Examine residuals
p1 = ggplot(data = out_1, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")

p2 = ggplot(data = out_1, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")

p1 + p2



##################################################
### Explore relationships with ln(peer ratings) for faculty-related outcomes model
##################################################

p1 = ggplot(data = educ, aes(x = funded_research_per_faculty, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("Funded research per faculty member (in thousands of dollars)")

p2 = ggplot(data = educ, aes(x = phd_granted_per_faculty, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("Ph.D.s granted per faculty member")

p1 + p2


# Correlation matrix
educ %>%
  select(Lpeer, funded_research_per_faculty, phd_granted_per_faculty) %>%
  correlate()



##################################################
### Log-transform faculty model predictors
##################################################

# Examine predictors for minimum values
educ %>%
  select(funded_research_per_faculty, phd_granted_per_faculty) %>%
  skim()


#Create log of the faculty-related predictors
educ = educ %>%
  mutate(
    Lfunded_research_per_faculty = log(funded_research_per_faculty),
    Lphd_granted_per_faculty = log(phd_granted_per_faculty + 1)
  )


# Re-examine relationships
p1 = ggplot(data = educ, aes(x = Lfunded_research_per_faculty, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("ln(Funded research per faculty member)")

p2 = ggplot(data = educ, aes(x = Lphd_granted_per_faculty, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("ln(Ph.D.s granted per faculty member + 1)")

p1 + p2



##################################################
### Fit faculty-related factors model
##################################################

# Fit model
lm.2 = lm(Lpeer ~ 1 + Lfunded_research_per_faculty + I(Lfunded_research_per_faculty^2) + Lphd_granted_per_faculty  + I(Lphd_granted_per_faculty^2), data = educ)


# Model-level output
glance(lm.2)


# Coefficient-level output
tidy(lm.2)


# Obtain residuals
out_2 = augment(lm.2)


# Examine residuals
p1 = ggplot(data = out_2, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")

p2 = ggplot(data = out_2, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")

p1 + p2



##################################################
### Explore relationships with ln(peer ratings) for institution-related outcomes model
##################################################

p1 = ggplot(data = educ, aes(x = doc_accept, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("Acceptance rate of Ph.D. students")

p2 = ggplot(data = educ, aes(x = phd_student_faculty_ratio, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("Ph.D. student-to-faculty ratio")

p3 = ggplot(data = educ, aes(x = enroll, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("Total enrollment")

p1 + p2 + p3


# Correlation matrix
educ %>%
  select(Lpeer, doc_accept, phd_student_faculty_ratio, enroll) %>%
  correlate()


##################################################
### Log-transform institution model predictors
##################################################

educ %>%
  select(doc_accept, phd_student_faculty_ratio, enroll) %>%
  skim()


#Create log of the institution-related predictors
educ = educ %>%
  mutate(
    Ldoc_accept = log(doc_accept),
    Lphd_student_faculty_ratio = log(phd_student_faculty_ratio + 1),
    Lenroll = log(enroll)
  )


# Re-examine relationships
p1 = ggplot(data = educ, aes(x = Ldoc_accept, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("ln(Acceptance rate of Ph.D. students)")

p2 = ggplot(data = educ, aes(x = Lphd_student_faculty_ratio, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("ln(Ph.D. student-to-faculty ratio + 1)")

p3 = ggplot(data = educ, aes(x = Lenroll, y = Lpeer)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("ln(Peer rating)") +
  xlab("ln(Total enrollment)")

p1 + p2 + p3



##################################################
### Fit the institution-related factors model
##################################################

# Fit model
lm.3 = lm(Lpeer ~ 1 + Ldoc_accept + Lenroll + Lphd_student_faculty_ratio, data = educ)


# Model-level output
glance(lm.3)


# Coefficient-level output
tidy(lm.3)


# Obtain residuals
out_3 = augment(lm.3)


# Examine residuals
p1 = ggplot(data = out_3, aes(x = .std.resid)) +
  stat_density_confidence(model = "normal") +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized residuals") +
  ylab("Probability density")

p2 = ggplot(data = out_3, aes(x = .fitted, y = .std.resid)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0) +
  geom_point() +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")

p1 + p2


