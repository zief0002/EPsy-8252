
###################################################
### Create data frame
###################################################

myData = data.frame(
  wage   = c(12, 8, 16.26, 13.65, 8.5),
  age    = c(32, 33, 32, 33, 26),	
  sex    = c("M", "F", "M", "M", "M")
)

myData



###################################################
### Set up outcome and predictor matrices
###################################################

# Vector of the outcome
Y = myData$wage

#  Predictor (X) matrix
X = matrix(c(rep(1, 5), myData$age), ncol = 2)



###################################################
### Use matrix algebra to compute the regression coefficients
###################################################

b = solve(t(X) %*% X) %*% t(X) %*% Y
b



###################################################
### Use matrix algebra to compute the fitted values
###################################################

fitted_values = X %*% b
fitted_values



###################################################
### Use matrix algebra to compute the H matrix
###################################################
 
h = X %*% solve(t(X) %*% X) %*% t(X)
h

# Another way to get the fitted values
h %*% Y

# Idempotency
h %*% h



###################################################
### Use matrix algebra to compute the residuals
###################################################

i = diag(5)
i

residuals = (i - h) %*% Y
residuals



###################################################
### Use matrix algebra to compute the SSE and MSE
###################################################

SSE = t(residuals) %*% residuals
SSE

# MSE
num_coef = 2
df_residual = nrow(myData) - num_coef

MSE = SSE / df_residual
MSE



###################################################
### Use matrix algebra to compute the SE for the coefficients
###################################################

V_b = solve(t(X) %*% X)
V_b

var_cov = as.numeric(MSE) * V_b 
var_cov

# Compute SEs
sqrt(diag(var_cov))

# Compute correlation between coefficients
V_b[1, 2] / sqrt(prod(diag(V_b)))



###################################################
### Use R's built-in functions
###################################################

lm.1 = lm(wage ~ 1 + age, data = myData)

summary(lm.1)
anova(lm.1)
coef(lm.1)
fitted(lm.1)
resid(lm.1)
model.matrix(lm.1)
vcov(lm.1)


