###################################################
### Read in data
###################################################

grad = read.csv(file = "/Users/andrewz/Documents/github/EPsy-8252/data/graduate-admissions.csv")
head(grad)



###################################################
### Load necessary libraries
###################################################

library(arm)
library(broom)
library(ggplot2)
library(sm)



###################################################
### Explore relationship between admission and gre score
###################################################

ggplot(data = grad, aes(x = gre, y = admit)) +
  geom_point() +
  theme_bw()


# Jitter the y-values
ggplot(data = grad, aes(x = gre, y = jitter(admit))) +
  geom_point() +
  theme_bw()


# Jitter the y-values (to a lesser degree) and add in regression line
ggplot(data = grad, aes(x = gre, y = jitter(admit, amount = 0.02))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()



###################################################
### Correlation
###################################################

# Pearson correlation
cor(grad[c("admit", "gre")])


# Spearman correlation
cor(grad[c("admit", "gre")], method = "spearman")




###################################################
### Fit linear probability model
###################################################

lm.badnews = lm(admit ~ gre, data = grad)
summary(lm.badnews)


# Examine residuals
# Use augment() from the broom package rather than fortify()
out = augment(lm.badnews)

sm.density(out$.std.resid, model = "normal")

ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()



###################################################
### Fit logistic model
###################################################

glm.1 = glm(admit ~ gre, data = grad, family = binomial(link = "logit"))
summary(glm.1)

# Profile CIs
confint(glm.1)



###################################################
### Examining assuptions
###################################################

out = augment(glm.1)
head(out)

# Neither of these is informative
sm.density(out$.std.resid)

ggplot(data = out, aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

# Use binnnedplot() from the arm package instead
binnedplot(x = out$.fitted, y = out$.std.resid)



###################################################
### Plotting the fitted logistic model
###################################################

# Set up the data
plotData = expand.grid(
  gre = seq(from = 220, to = 800, by = 10)
  )

# Obtain predicted values
plotData$yhat = predict(glm.1, newdata = plotdata)	
head(plotData)

# Plot the logits vs. the GRE scores
ggplot(data = plotData, aes(x = gre, y = yhat)) +
  geom_line() +
  xlab("GRE Score") +
  ylab("Logit of Admission") +
  theme_bw() 



###################################################
### Better interpretation: Plotting the corresponding probability model
###################################################

# Transform the logits to probabilities
plotData$prob = exp(plotData$yhat) / (1 + exp(plotData$yhat))
head(plotData)

# Plot
ggplot(data = plotData, aes(x = gre, y = prob)) +
  geom_line() +
  xlab("GRE Score") +
  ylab("Probability of Admission") +
  #ylim(0, 1) +
  theme_bw() 



###################################################
### Obtain probabilities from predict()
###################################################

plotData$yhat2 = predict(glm.1, newdata = plotdata, type = "response")	
head(plotData)



###################################################
### Plot CIs
###################################################

# Set up the data
plotData = expand.grid(
  gre = seq(from = 220, to = 800, by = 10)
)

# Compute the probabilities and the SEs
yhat3 = predict(glm.1, newdata = plotdata, type = "response", se.fit = TRUE)	
head(yhat3)


# Bind the SEs to the plotting data
plotData2 = data.frame(
  gre = plotData$gre, 
  prob = yhat3$fit,
  se = yhat3$se.fit
  )

head(plotData2)

# Compute confidence limits
plotData2$lower_limit = plotData2$prob - 2*plotData2$se
plotData2$upper_limit = plotData2$prob + 2*plotData2$se
head(plotData2)

# Plot
ggplot(data = plotData2, aes(x = gre, y = prob)) +
  geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit), color = "grey", alpha = 0.3) +
  geom_line(color = "blue") +
  xlab("GRE") +
  ylab("Probability of Admission") +
  ylim(0, 1) +
  theme_bw()


