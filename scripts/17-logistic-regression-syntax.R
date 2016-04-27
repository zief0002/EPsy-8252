###################################################
### titanic.csv
###################################################

# The titanic.csv dataset includes data on the survival status of individual passengers on the 
# Titanic. The data does not contain information from the crew, but it does contain 
# actual ages of half of the passengers. The principal source for data about Titanic passengers is 
# the Encyclopedia Titanica. The datasets used here were begun by a variety of researchers. One of 
# the original sources is Eaton & Haas (1994) Titanic: Triumph and Tragedy, Patrick Stephens Ltd, 
# which includes a passenger list created by many researchers and edited by Michael A. Findlay. 

# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3info.txt


# name:      Passenger name
# survived:  Did the passenger survive? (0 = No; 1 = Yes)
# female:    Is the passenger a female? (0 = No; 1 = Yes)
# age:       Passenger's age	
# sibsp:     Number of Siblings/Spouses Aboard
# parch:     Number of Parents/Children Aboard
# pclass:    Passenger Class (proxy for SES)
# ticket:    Ticket Number
# fare:      Passenger Fare
# cabin:     Cabin
# embarked:  Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)
# boat:      Lifeboat number	
# body:      Body Identification Number	
# home_dest: Home/Destination


        
###################################################
### Sex as a predictor of survival
###################################################

# 01. Read in the Titanic data set

titanic = read.csv(file = "/Users/andrewz/Documents/github/EPsy-8252/data/titanic.csv")


# 02. Compute percentages of females and males that survived.

titanic$survived2 = ifelse(titanic$survived == 1, "Survived", "Died")
titanic$female2 = ifelse(titanic$female == 1, "Female", "Male")

nrow(titanic)
table(titanic$female2)
table(titanic$survived2)

table(titanic$female2, titanic$survived2)
table(titanic$survived2, titanic$female2)

library(gmodels)
CrossTable(titanic$survived2, titanic$female2)


# 03. Fit logistic model using female to predict survival.

glm.1 = glm(survived ~ female, data = titanic, family = binomial(link = "logit"))
summary(glm.1)

# odds
exp(coef(glm.1)[1])   # for males
exp(sum(coef(glm.1))) # for females

# odds ratio
exp(sum(coef(glm.1))) / exp(coef(glm.1)[1])

# Compute odds ratio directly
exp(coef(glm.1)[2])


# probability
new = data.frame(female = c(0, 1))
predict(glm.1, newdata = new, type = "response")


# 04. Interpret coefficients in model.



###################################################
### Class as a predictor of survival
###################################################

# 05. Compute percentages of first, second, and third class passengers that survived.

CrossTable(titanic$survived, titanic$pclass)



# 06. Fit logistic model using pclass to predict survival.

glm.2 = glm(survived ~ pclass, data = titanic, family = binomial(link = "logit"))
summary(glm.2)



# 07. Interpret coefficients in model.



###################################################
### Sex and class as a predictor of survival -- Main effects
###################################################

# 08. Compute percentages of first, second, and third class passengers that survived by sex.

table(titanic$survived2, titanic$pclass, titanic$female2)

# 09. Fit main-effects model that includes both pclass and female to predict survival.

glm.3 = glm(survived ~ female + pclass, data = titanic, family = binomial(link = "logit"))
summary(glm.3)



# 10. Interpret coefficients in model.



###################################################
### Sex and class as a predictor of survival -- Interaction
###################################################

# 11. Fit interaction model that includes both pclass and female to predict survival.

glm.4 = glm(survived ~ female + pclass + female:pclass, data = titanic, family = binomial(link = "logit"))
summary(glm.4)



# 12. Interpret coefficients in model.


new = expand.grid(
  female = c(0, 1),
  pclass = c("First", "Second", "Third")
)

new$phat = predict(glm.4, newdata = new, type = "response")

ggplot(data = new, aes(x = female, y = phat, color = pclass)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "", breaks = c(0, 1), labels = c("Male", "Female")) +
  ylab("Probability of Survival") +
  ylim(0, 1) +
  scale_color_brewer(name = "Passenger Class", palette = "Set1")


## For fun: What about age?

glm.5 = glm(survived ~ female + pclass + age + female:pclass + female:age + age:pclass, data = titanic, family = binomial(link = "logit"))
summary(glm.5)


new = expand.grid(
  age = 0:80,
  female = c(0, 1),
  pclass = c("First", "Second", "Third")
)

new$phat = predict(glm.5, newdata = new, type = "response")

new$female = factor(new$female, levels = c(0, 1), labels = c("Males", "Females"))

ggplot(data = new, aes(x = age, y = phat, group = female:pclass, color = pclass)) +
  geom_line() +
  theme_bw() +
  xlab("Age") +
  ylab("Probability of Survival") +
  ylim(0, 1) +
  scale_color_brewer(name = "Passenger Class", palette = "Set1") +
  facet_wrap(~female)

