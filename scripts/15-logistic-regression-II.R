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
### Coerce admit and rank into a factor for better plotting
###################################################

grad$admit2 = factor(grad$admit, levels = c(0, 1), labels = c("Not Admitted", "Admitted"))




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



#grad$rank2 = factor(grad$rank, levels = c(1, 2, 3, 4), labels = c("Highest", "High", "Low", "Lowest"))



###################################################
### code chunk number 7: EPsy-8252-Logistic-Regression-Notes.Rnw:269-271
###################################################
glm.2 <- glm(admit ~ gre + gpa + rank2, data = grad, family = binomial(link = "logit"))
summary(glm.2)


###################################################
### code chunk number 8: EPsy-8252-Logistic-Regression-Notes.Rnw:278-295
###################################################
plotdata <- expand.grid(
	gre = mean(grad$gre),
	gpa = seq(from = 2.26, to = 4.0, by = 0.01),
	rank2 = c("Highest", "High", "Low", "Lowest")
	)
plotdata$fitted <- predict(glm.2, newdata = plotdata, type = "response")	
library(ggplot2)
p1 <- ggplot(data = grad, aes(x = gpa, y = jitter(admit, factor = 0.25), color = rank2)) +
	geom_point() +
	geom_line(data = plotdata, aes(x = gpa, y = fitted, color = rank2)) +
	scale_color_brewer(palette = "Set1", name = "Prestige") +
	xlab("Undergraduate GPA") +
	ylab("Probability of Admission") +
	theme_bw() 	
pdf(file="Graphics/Figure-01.pdf")
print(p1)
dev.off()		


###################################################
### code chunk number 9: EPsy-8252-Logistic-Regression-Notes.Rnw:307-313
###################################################
plotdata <- expand.grid(
	gre = mean(grad$gre),
	gpa = seq(from = 2.26, to = 4.0, by = 0.01),
	rank2 = c("Highest", "High", "Low", "Lowest")
	)
plotdata$fitted <- predict(glm.2, newdata = plotdata, type = "response")	


###################################################
### code chunk number 10: EPsy-8252-Logistic-Regression-Notes.Rnw:320-327 (eval = FALSE)
###################################################
## ggplot(data = grad, aes(x = gpa, y = jitter(admit, factor = 0.25), color = rank2)) +
## 	geom_point() +
## 	geom_line(data = plotdata, aes(x = gpa, y = fitted, color = rank2)) +
## 	scale_color_brewer(palette = "Set1", name = "Prestige") +
## 	xlab("Undergraduate GPA") +
## 	ylab("Probability of Admission") +
## 	theme_bw() 


###################################################
### code chunk number 11: EPsy-8252-Logistic-Regression-Notes.Rnw:334-335 (eval = FALSE)
###################################################
## quantile(grad$gre, probs = c(0.10, 0.90))	


###################################################
### code chunk number 12: EPsy-8252-Logistic-Regression-Notes.Rnw:339-340
###################################################
quantile(grad$gre, probs = c(0.10, 0.90))	


###################################################
### code chunk number 13: EPsy-8252-Logistic-Regression-Notes.Rnw:344-359 (eval = FALSE)
###################################################
## plotdata <- expand.grid(
## 	gre = c(440, 740),
## 	gpa = seq(from = 2.26, to = 4.0, by = 0.01),
## 	rank2 = c("Highest", "High", "Low", "Lowest")
## 	)
## plotdata$fitted <- predict(g2, newdata = plotdata, type = "response")	
## plotdata$gre <- factor(plotdata$gre)
## ggplot(data = plotdata, aes(x = gpa, y = fitted, color = rank2)) +
## 	geom_line(data = plotdata, aes(x = gpa, y = fitted, color = rank2)) +
## 	scale_color_brewer(palette = "Set1", name = "Prestige") +
## 	xlab("Undergraduate GPA") +
## 	ylab("Probability of Admission") +
## 	facet_wrap(~gre) +
## 	opts(aspect.ratio = 1) +
## 	theme_bw() 


###################################################
### code chunk number 14: EPsy-8252-Logistic-Regression-Notes.Rnw:363-381
###################################################
plotdata <- expand.grid(
	gre = c(440, 740),
	gpa = seq(from = 2.26, to = 4.0, by = 0.01),
	rank2 = c("Highest", "High", "Low", "Lowest")
	)
plotdata$fitted <- predict(glm.2, newdata = plotdata, type = "response")	
plotdata$gre <- factor(plotdata$gre)
p2 <- ggplot(data = plotdata, aes(x = gpa, y = fitted, color = rank2)) +
	geom_line(data = plotdata, aes(x = gpa, y = fitted, color = rank2)) +
	scale_color_brewer(palette = "Set1", name = "Prestige") +
	xlab("Undergraduate GPA") +
	ylab("Probability of Admission") +
	facet_wrap(~gre) +
	opts(aspect.ratio = 1) +
	theme_bw() 
pdf(file="Graphics/Figure-02.pdf")
print(p2)
dev.off()		


