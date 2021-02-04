##################################################
### Creating a probability density function (PDF)
##################################################

# Create X-value and compute probability densities
fig_01 = data.frame(
  X = seq(from = 20, to = 80, by = 0.01)
) %>% 
  mutate(
    Y = dnorm(x = X, mean = 50, sd = 10)
  )


# View data
head(fig_01)


# Create plot
ggplot(data = fig_01, aes(x = X, y = Y)) +
  geom_line() +
  xlab("X") +
  ylab("Probability density") +
  theme_light() 



##################################################
### Add shading under PDF; Shade X <= 30
##################################################

# Filter data included in the shaded region
shade_01 = fig_01 %>%
  filter(X <= 30)


# View data
head(shade_01)


# Create plot
ggplot(data = fig_01, aes(x = X, y = Y)) +
  geom_line() +
  xlab("X") +
  ylab("Probability density") +
  theme_light() +
  geom_ribbon(data = shade_01, ymin = 0, aes(xmin = X, xmax = X, ymax = Y), color = "#bbbbbb", alpha = 0.4)
