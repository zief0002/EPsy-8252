##################################################
### Load libraries
##################################################

library(tidyverse)
library(broom)
library(gridExtra)
library(sm)



##################################################
### Read in data
##################################################

city = read_csv(file = "~/Dropbox/epsy-8252/data/riverside.csv")
head(city)



##################################################
### Normal theory regression
##################################################

lm.1 = lm(income ~ 1 + education + seniority, data = city)

# Model-level estimates
glance(lm.1)

# Coefficient-level estimates
tidy(lm.1)



##################################################
### Examine assumptions
##################################################

out.1 = augment(lm.1)
head(out.1)

sm.density(out.1$.std.resid, model = "normal")

ggplot(data = out.1, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Bootstrapping coefficient-level estimates
##################################################

boot_reg = city %>% 
  bootstrap(1000) %>%
  do( tidy( lm(income ~ 1 + education + seniority, data = .) ) )

head(boot_reg)



##################################################
### Examine bootstrap distributions
##################################################

boot_reg %>%
  group_by(term) %>%
  summarize(M = mean(estimate), SE = sd(estimate))


p1 = boot_reg %>%
  filter(term == "(Intercept)") %>%
  ggplot(data = ., aes(x = estimate)) +
  geom_density() +
  theme_bw() +
  ggtitle("Intercept")

p2 = boot_reg %>%
  filter(term == "education") %>%
  ggplot(data = ., aes(x = estimate)) +
  geom_density() +
  theme_bw() +
  ggtitle("Education")

p3 = boot_reg %>%
  filter(term == "seniority") %>%
  ggplot(data = ., aes(x = estimate)) +
  geom_density() +
  theme_bw() +
  ggtitle("Seniority")

grid.arrange(p1, p2, p3, nrow = 1)



##################################################
### Bootstrap-based CI: Percentile method
##################################################

boot_reg %>%
  group_by(term) %>%
  summarize(
    LL = quantile(estimate, prob = .025),
    UL = quantile(estimate, prob = .975)
  )



##################################################
### Parametic analysis (intercept)
##################################################

# Compute t
6769 / 5373

# Normal theory p-value
pt(q = -1.26, df = 29) * 2



##################################################
### Semi-parametric analysis (intercept)
##################################################

# Using a bootstrap-based SE to compute t
6769 / 4820

# Compute p-value using modified t
pt(q = -1.4, df = 29) * 2



##################################################
### Nonparametric analysis
##################################################

boot_intercepts = boot_reg %>%
  filter(term == "(Intercept)") %>%
  mutate(centered_estimate = estimate - 6769) %>%
  select(replicate, estimate, centered_estimate)

head(boot_intercepts)

# Count bootstrapped estimates that are at least as extreme as 6769
sum(boot_intercepts$centered_estimate >= 6769)
sum(boot_intercepts$centered_estimate <= -6769)

# Combine these into a single statement
sum(abs(boot_intercepts$centered_estimate) >= 6769)

# Compute p-value
sum(abs(boot_intercepts$centered_estimate) >= 6769) / 1000



##################################################
### Nonparametric analysis (education)
##################################################

# Center the distribution
boot_educ = boot_reg %>%
  filter(term == "education") %>%
  mutate(centered_estimate = estimate - 2252) %>%
  select(replicate, estimate, centered_estimate)

head(boot_intercepts)

# Compute p-value
sum(abs(boot_educ$centered_estimate) >= 2252) / 1000



##################################################
### Adjust p-values for simulation error
##################################################

# Simulation adjusted p-value for intercept
(sum(abs(boot_intercepts$centered_estimate) >= 6769) + 1) / (1000 + 1)

# Simulation adjusted p-value for education
(sum(abs(boot_educ$centered_estimate) >= 2252) + 1) / (1000 + 1)



##################################################
### Bootstrapping model-level estimates
##################################################

boot_reg2 = city %>% 
  bootstrap(1000) %>%
  do( glance( lm(income ~ 1 + education + seniority, data = .) ) )

head(boot_reg2)



##################################################
### Bootstrap distribution for R^2
##################################################

ggplot(data = boot_reg2, aes(x = r.squared)) +
  geom_density() +
  theme_bw() +
  geom_vline(xintercept = 0.742, linetype = "dashed")


# Percentile interval
quantile(boot_reg2$r.squared, prob = c(0.025, 0.975))




