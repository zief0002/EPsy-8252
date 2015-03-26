###################################################
### Read in the data
###################################################

library(foreign)

nbaL1 = read.spss(file = "/Users/andrewz/Documents/EPsy-8252/data/nbaLevel1.sav", to.data.frame = TRUE)
nbaL2 = read.spss(file = "/Users/andrewz/Documents/EPsy-8252/data/nbaLevel2.sav", to.data.frame = TRUE)

head(nbaL1)
head(nbaL2)



###################################################
### Merge the player-level data and the team-level data
###################################################

nba = merge(nbaL1, nbaL2, by = "Team_ID")

head(nba)
tail(nba)



###################################################
### Unconditional random intercepts model
###################################################

library(lmerTest)

# Fit model
lmer.a = lmer(Life_Satisfaction ~ 1 + (1 | Team_ID), data = nba)
summary(lmer.a)

# Produce estimates of the random-effects
ranef(lmer.a)

# Estimates of the variance-covariance matrix of the random effects
VarCorr(lmer.b)$Team_ID

AIC(lmer.a)
BIC(lmer.a)
cor(nba$Life_Satisfaction, fitted(lmer.a)) ^ 2



###################################################
### Unconditional random intercepts and random slopes model
###################################################

lmer.b = lmer(Life_Satisfaction ~ 1 + Shots_on_five + (1 + Shots_on_five | Team_ID), data = nba)
summary(lmer.b)

AIC(lmer.b)
BIC(lmer.b)
cor(nba$Life_Satisfaction, fitted(lmer.b)) ^ 2

# Get estimates of the random-effects
ranef(lmer.b)

# Estimates of the variance-covariance matrix of the random effects
VarCorr(lmer.b)$Team_ID



###################################################
### Conditional model(s)
###################################################

lmer.c = lmer(Life_Satisfaction ~ 1 + Shots_on_five + Coach_Experience + 
	Shots_on_five:Coach_Experience + (1 + Shots_on_five | Team_ID), data = nba)
summary(lmer.c)

AIC(lmer.c)
BIC(lmer.c)
cor(nba$Life_Satisfaction, fitted(lmer.c)) ^ 2

# Get estimates of the random-effects
ranef(lmer.c)

# Estimates of the variance-covariance matrix of the random effects
VarCorr(lmer.c)$Team_ID



###################################################
### General (matrix) form of the mixed-effects model
###################################################

## Fixed-effects design matrix
X = getME(lmer.a, "X")
head(X)


## Random-effects design matrix
Z = getME(lmer.a, "Z")
head(Z, 20)

## Estimated G matrix
est = as.data.frame(VarCorr(lmer.a))
var.b0 = est$vcov[1]

## Estimated G matrix
I = diag(30)
G = var.b0 * I

G[1:10, 1:10]


## Estimated between-teams matrix
B = Z %*% G %*% t(Z)
dim(B)

B[1:10, 1:10]

head(Z, 20) %*% G[1:10, 1:10] %*% t(head(Z, 20))


## Estimated value of the error variance
var.err = est$vcov[2]
var.err

## Create a 10x10 identity matrix
ident = diag(10)
ident

## Compute the within-teams matrix for team j
W.j = var.err * ident

## Compute the estimated variance-covariance matrix for team 1
V = W.j + B[1:10, 1:10]
V

## Create the diagonal matrix D
D = diag(1 / sqrt(diag(V)))
D

## Compute the estimated standardized variance-covariance matrix
Vstar = D %*% V %*% D
Vstar


## Compute the estimated variance-covariance matrix for team 2
Z = model.matrix(lmer.a)[11:20, ]
G = VarCorr(lmer.a)$Team_ID[1, 1]
sigma2 = summary(lmer.a)$sigma ^ 2
ident = diag(10)
W = sigma2 * ident
Z = as.matrix(Z)
B = Z %*% G %*% t(Z)
V = B + W
V
