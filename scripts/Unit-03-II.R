
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
### Fit age and sex main effects model
###################################################

lm.1 = lm(wage ~ 1 + age + sex, data = myData)
summary(lm.1)

# No intercept
lm.2 = lm(wage ~ age + sex - 1, data = myData)
summary(lm.2)

model.matrix(lm.1)
model.matrix(lm.2)



###################################################
### Fit age main effects model
###################################################

lm.1 = lm(wage ~ 1 + age, data = myData)
summary(lm.1)

Y = myData$wage
X = model.matrix(lm.1)

# Compute Y'Y
t(Y) %*% Y


# Compute X'X
t(X) %*% X

# Compute X'Y
t(X) %*% Y

# Inverse of X'X
solve(t(X) %*% X)

# Variance-Covariance matrix
vcov(lm.1)

