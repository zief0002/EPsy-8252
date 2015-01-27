###################################################
### Create data frame
###################################################

myData = data.frame(
	wage = c(12, 8, 16.26, 13.65, 8.5),
	educ = c(12, 12, 12, 16, 17),
	sex = c("M", "F", "M", "M", "M"),
	status = c("Married", "Married", "Single", "Married", "Single"),
	age = c(32, 33, 32, 33, 26),
	sector = c("manuf", "service", "service", "prof", "clerical")
	)

myData



###################################################
### Compute beta estimates
###################################################

Y = myData$wage
X = matrix(c(rep(1, 5), myData$age), ncol = 2)

b = solve(t(X) %*% X) %*% t(X) %*% Y
b

# shortcut
lm.1 = lm(wage ~ 1 + age, data = myData)
summary(lm.1)
model.matrix(lm.1)



###################################################
### Compute fitted values (y-hats)
###################################################

X %*% b

# shortcut
fitted(lm.1)

# compute H matrix
h = X %*% solve(t(X) %*% X) %*% t(X)
h

# compute fitted values
h %*% Y

# H is idempotent
h %*% h



###################################################
### Compute residuals
###################################################

# create identity matrix
i = diag(5)

# compute residuals
(i - h) %*% Y

# shortcut
resid(lm.1)



###################################################
### Variance-covariance matrix
###################################################

# Obtain the estimate of the MSE from the ANOVA table or by squaring the RMSE
MSE = 13.312

# compute var-cov matrix
MSE * (i - h)



###################################################
### Obtain h_ii and residual variance
###################################################

# Obtain the predictor vector for the 1st observation
X1 = X[1, ]

h1 = t(X1) %*% solve(t(X) %*% X) %*% X1
h1

# Compute the residual variance estimate for the 1st observation
MSE * (1 - h1)



###################################################
### Studentized residuals
###################################################

# Compute the residual standard errors for all observations
se = sqrt(diag(MSE * (1 - h)))
se

# Compute studentized residuals
resid(lm.1) / se 

# shortcut
rstandard(lm.1)



###################################################
### Leverage values
###################################################

hatvalues(lm.1)

# Mean hat value
mhat = mean(hatvalues(lm.1))
mhat

hatvalues(lm.1) > mhat




###################################################
### Variance inflation factors (VIF)
###################################################

# Fit a model with multiple predictors
lm.2 = lm(wage ~ age + educ + status, data = myData)

# Get the design matrix
X = model.matrix(lm.2)

# Inverse of the correlation matrix for the predictors
# Omit the column of ones
solve(cor(X[ , -1]))

vif(lm.2)
