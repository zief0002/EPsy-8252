##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(lme4)
library(readr)
library(sm)
library(tidyr)




##################################################
### Read in data
##################################################

math = read_csv(file = "~/Documents/github/epsy-8252/data/math.csv")
head(math)



##################################################
### Re-structure to long format
##################################################

math_long = math %>%
  gather(key = "item", value = "correct", item_01:item_10) %>%
  arrange(examinee, item)


# View data
head(math_long, 15)



##################################################
### Fit Rasch model
##################################################

# The -1 omits the intercept
glmer.1 = glmer(correct ~ -1 + item + (1|examinee), data = math_long, family = binomial(link = "logit"))


# Model output
summary(glmer.1)



##################################################
### Get item difficulty-levels
##################################################

-fixef(glmer.1)



##################################################
### Examinee abilities
##################################################

# Extract the ability-levels
abilities = ranef(glmer.1)$examinee[ , 1]


# Output the first six examinees' ability-levels
head(abilities)



##################################################
### Dotplot of ability levels
##################################################

# Put abilities into a data frame for plotting
examinees = data.frame(
  examinee = 1:30,
  theta = abilities
)


# Plot
ggplot(data = examinees, aes(x = theta)) +
  geom_dotplot() +
  theme_bw() +
  scale_y_continuous(name = "", labels = NULL) +
  scale_x_continuous(name = "Ability-level", limits = c(-2, 2))



##################################################
### Wright map
##################################################

# Put abilities into a data frame for plotting
difficulties = data.frame(
  item = 1:10,
  beta = -fixef(glmer.1)
)


ggplot(data = examinees, aes(x = theta)) +
  geom_dotplot() +
  theme_bw() +
  scale_y_continuous(name = "", labels = NULL, limits = c(-0.3, 1)) +
  scale_x_continuous(name = "Ability-level", limits = c(-2, 2)) +
  geom_hline(yintercept = -0.02) +
  geom_segment(data = difficulties, aes(x = beta, xend = beta, y = -.04, yend = -0.1)) +
  geom_text(data = difficulties, aes(x = beta, y = -0.14, label = item))



##################################################
### ICC for Item 01
##################################################

data.frame(
  b = seq(from = -4, to = 4, by = 0.1)
) %>%
  mutate(
    log_odds = b - 1.378,
    odds = exp(log_odds),
    prob = odds / (1 + odds)
  ) %>%
  ggplot(aes(x = b, y = prob)) +
    geom_line() +
    theme_bw() +
    xlab("Ability level") +
    ylab("Probability of answering correctly") +
    ylim(0, 1)
    ggtitle("Item 01")
  


##################################################
### ICC for all items
##################################################

# Create individual vectors
item = unique(math_long$item)
beta = -fixef(glmer.1)
b = seq(from = -4, to = 4, by = 0.1)


# Cross abilities with item names/difficulties
all_items = crossing(b, nesting(item, beta))


# Obtain probability of answering correctly for each item and ability  
all_items = all_items %>%
  mutate(
    log_odds = b - beta,
    odds = exp(log_odds),
    prob = odds / (1 + odds)
  )


# View data
head(all_items)


# All items in one plot
ggplot(data = all_items, aes(x = b, y = prob, color = item)) +
  geom_line() +
  theme_bw() +
  xlab("Ability level") +
  ylab("Probability of answering correctly") +
  ylim(0, 1) 


# Items in separate panels
ggplot(data = all_items, aes(x = b, y = prob)) +
  geom_line() +
  theme_bw() +
  xlab("Ability level") +
  ylab("Probability of answering correctly") +
  ylim(0, 1) +
  facet_wrap(~item)



##################################################
### Item information
##################################################

# Obtain item information
all_items = all_items %>%
  mutate(
    info = prob * (1 - prob)
  )


# Plot
ggplot(data = all_items, aes(x = b, y = info)) +
  geom_line() +
  theme_bw() +
  xlab("Ability level") +
  ylab("Information") +
  ylim(0, 1) +
  facet_wrap(~item)



##################################################
### Test information
##################################################

# Obtain total information at each ability-level  
test_info = all_items %>%
  group_by(b) %>%
  summarize(
    total_info = sum(info)
  )


# View data
head(test_info)


# Plot
ggplot(data = test_info, aes(x = b, y = total_info)) +
  geom_line() +
  theme_bw() +
  xlab("Ability level") +
  ylab("Information")



##################################################
### Ability uncertainty (SE)
##################################################

# Compute uncertainty for each ability-level
test_info = test_info %>%
  mutate(
    se = sqrt(1 / total_info)
  )


# View data
head(test_info)


# Plot
ggplot(data = test_info, aes(x = b, y = se)) +
  geom_line() +
  theme_bw() +
  xlab("Ability level") +
  ylab("Uncertainty/Standard Error of Measurement)")



