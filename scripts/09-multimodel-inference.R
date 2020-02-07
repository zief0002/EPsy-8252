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

# Read in data
nels = read_csv("~/Documents/github/epsy-8252/data/nels.csv")


# View data
head(nels)



##################################################
### Fit candidate models
##################################################

# Fit Model 1
lm.1 = lm(social10 ~ 1 + ses + gpa + parent_ed, data = nels)

# Fit Model 2
lm.2 = lm(social10 ~ 1 + ses + gpa + parent_ed + self_esteem, data = nels)

# Fit Model 3
lm.3 = lm(social10 ~ 1 + ses + gpa + parent_ed + locus, data = nels)

# Fit Model 4
lm.4 = lm(social10 ~ 1 + ses + gpa + parent_ed + self_esteem + locus, data = nels)

# Fit Model 5
lm.5 = lm(social10 ~ 1 + ses + gpa + parent_ed + self_esteem + locus + 
            self_esteem:locus, data = nels)



##################################################
### Table of model evidence
##################################################

model.sel(list(lm.1, lm.2, lm.3, lm.4, lm.5))



##################################################
### Compute model averaged coefficient for self-esteem effect
##################################################

# Simple average
(0 + 0.3487 + 0.3767 + 0.9476 + 0) / 5


# Weighted average using AICc weights
(0 * 0.505 + 0.3487 * 0.223 + 0.3767 * 0.156 + 0.9476 * 0.087 + 0 * 0.029) / 1



##################################################
### Obtain model averaged coefficient estimates for all effects
##################################################

model.avg(list(lm.1, lm.2, lm.3, lm.4, lm.5))



##################################################
### Computing unconditional variance for the self-esteem estimate
##################################################

# Table of model weights, self-esteem coefficient estimates, and sampling variances
data.frame(
  model = c(1, 2, 3, 4, 5),
  w = c(0.029, 0.087, 0.505, 0.223, 0.156),
  beta = c(0, 0.9476, 0, 0.3487, 0.3767),
  var_beta = c(0, 0.4634, 0, 0.5531, 0.5535)^2
)


# Compute the model selection uncertainty terms
data.frame(
  model = c(1, 2, 3, 4, 5),
  w = c(0.029, 0.087, 0.505, 0.223, 0.156),
  beta = c(0, 0.9476, 0, 0.3487, 0.3767),
  var_beta = c(0, 0.4634, 0, 0.5531, 0.5535)^2
) %>%
  mutate(
    mod_sel_unc = (beta - 0.22)^2
  )


# Multiply the weights by the sum of the variance components
data.frame(
  model = c(1, 2, 3, 4, 5),
  w = c(0.029, 0.087, 0.505, 0.223, 0.156),
  beta = c(0, 0.9476, 0, 0.3487, 0.3767),
  var_beta = c(0, 0.4634, 0, 0.5531, 0.5535)^2
) %>%
  mutate(
    mod_sel_unc = (beta - 0.22)^2,
    w_var = w * (var_beta + mod_sel_unc)
  )


# Sum the weighted sum of the variance components;
# Compute unconditional SE
data.frame(
  model = c(1, 2, 3, 4, 5),
  w = c(0.029, 0.087, 0.505, 0.223, 0.156),
  beta = c(0, 0.9476, 0, 0.3487, 0.3767),
  var_beta = c(0, 0.4634, 0, 0.5531, 0.5535)^2
) %>%
  mutate(
    mod_sel_unc = (beta - 0.22)^2,
    w_var = w * (var_beta + mod_sel_unc)
  ) %>%
  summarize(
    unconditional_variance = sum(w_var),
    unconditional_se = sqrt(unconditional_variance)
  )



##################################################
### Compute model averaged coefficients and unconditional standard errors for all effects
##################################################

coefTable(
  model.avg(list(lm.1, lm.2, lm.3, lm.4, lm.5)), 
  full = TRUE
)



##################################################
### Create table of estimates and CIs
##################################################

# Coerce table into a data frame
tab = coefTable(
  model.avg(list(lm.1, lm.2, lm.3, lm.4, lm.5)), 
  full = TRUE
) %>%
  data.frame()


# View data frame
tab


# Compute lower limit (LL) and upper limit (UL) for CI
tab = tab %>%
  mutate(
    LL = Estimate - 1.96*Std..Error,
    UL = Estimate + 1.96*Std..Error
  ) %>%
  mutate(
    Term = row.names(tab)
  ) %>%
  rename(SE = Std..Error) %>%
  select(Term, Estimate, SE, LL, UL)


# View updated table
tab



##################################################
### Create coefficient plot
##################################################

tab %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    Term = factor(Term, 
                  levels = c("locus:self_esteem", "locus", "self_esteem", "parent_ed", "gpa", "ses"),
                  labels = c("Locus-of-control x Self-esteem", "Locus-of-control", "Self-esteem", 
                             "Parent education", "Prior GPA", "SES"),
    )
  ) %>%
  ggplot(aes(x = Estimate, y = Term)) +
    geom_errorbarh(aes(xmin = LL, xmax = UL)) +
    geom_point() +
    theme_bw() +
    xlab("Coefficient estimate") +
    ylab("Predictor")



##################################################
### Coefficient plot (ungeviz)
##################################################

# Install package
# remotes::install_github("wilkelab/ungeviz")


# Load package
library(ungeviz)


# Create plot
tab %>%
  filter(Term != "(Intercept)") %>%
  mutate(
    Term = factor(Term, 
                  levels = c("locus:self_esteem", "locus", "self_esteem", "parent_ed", "gpa", "ses"),
                  labels = c("Locus-of-control x Self-esteem", "Locus-of-control", "Self-esteem", 
                             "Parent education", "Prior GPA", "SES"),
    )
  ) %>%
  ggplot(aes(x = Estimate, y = Term)) +
    stat_confidence_density(aes(moe = SE, confidence = 0.68, 
                                fill = stat(ndensity)), height = 0.15) +
    geom_point(size = 2) +
    scale_fill_gradient(low = "#eff3ff", high = "#6baed6") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(name = "Coefficient estimate", limits = c(-2, 7)) +
    ylab("Predictor")

