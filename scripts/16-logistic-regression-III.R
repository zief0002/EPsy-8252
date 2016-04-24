###################################################
### titanic.csv
###################################################

# The titanic and titanic2 data frames describe the survival status of individual passengers on the 
# Titanic. The titanic data frame does not contain information from the crew, but it does contain 
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

# 02. Compute percentages of females and males that survived.

# 03. Fit logistic model using female to predict survival.

# 04. Interpret coefficients in model.



###################################################
### Class as a predictor of survival
###################################################

# 05. Compute percentages of first, second, and third class passengers that survived.

# 06. Fit logistic model using pclass to predict survival.

# 07. Interpret coefficients in model.



###################################################
### Sex and class as a predictor of survival -- Main effects
###################################################

# 08. Compute percentages of first, second, and third class passengers that survived by sex.

# 09. Fit main-effects model that includes both pclass and female to predict survival.

# 10. Interpret coefficients in model.



###################################################
### Sex and class as a predictor of survival -- Interaction
###################################################

# 11. Fit interaction model that includes both pclass and female to predict survival.

# 12. Interpret coefficients in model.




