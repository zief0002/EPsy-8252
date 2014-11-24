###################################################
### Read in the data
###################################################

mpls = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/minneapolis.csv")

# Remove missing data from wide data
mpls2 = mpls[complete.cases(mpls), ]

# Only include the StudentID, 5th and 8th grade scores (two time-point example)
mpls3 = mpls2[ , c(1,2,5)]

# Create long data from wide data
library(reshape2)

mplsLong = melt(
	mpls3, 
	id = c("studentID"),
	measure = c("read.5", "read.8")
	)

# Rename the columns
names(mplsLong)[2] = "grade"
names(mplsLong)[3] = "read"

head(mplsLong)



###################################################
### Fit MANOVA model
###################################################

# Bind together the outcome columns from the wide data
dvm = cbind(mpls3$read.5, mpls3$read.8)

# Fit the multivariate model
mlm.1 = lm(dvm ~ 1)
mlm.1

# Intra-subject design
my.design = factor(c("read.5", "read.8"))

library(car)
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design)

# Obtain different multivariate test statistics
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Wilks")
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Hotelling-Lawley")
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Roy")

# RM-ANOVA is same as MANOVA for two time-point data
library(ez)
aov.2 = ezANOVA(data = mplsLong, dv = read, wid = studentID, within = .(grade), detailed = TRUE)
aov.2




###################################################
### More than Two Time Points
###################################################

# Compute difference scores (these are also the errors for the reduced model)
mpls2$d1 = mpls2$read.6 - mpls2$read.5
mpls2$d2 = mpls2$read.7 - mpls2$read.6
mpls2$d3 = mpls2$read.8 - mpls2$read.7

# Compute the errors for the full model
mpls2$e1 = mpls2$d1 - mean(mpls2$d1)
mpls2$e2 = mpls2$d2 - mean(mpls2$d2)
mpls2$e3 = mpls2$d3 - mean(mpls2$d3)


# Create SSCP matrices
X = as.matrix(mpls2[c("d1", "d2", "d3")])
sscp_r = t(X) %*% X
sscp_r


Y = as.matrix(mpls2[c("e1", "e2", "e3")])
sscp_f = t(Y) %*% Y
sscp_f 

# Compute the determinant
det(sscp_r)
det(sscp_f)

# Compute Delta-F
(det(sscp_r, log = FALSE) - det(sscp_f, log = FALSE)) / (4 - 1) * (14 - 4 + 1) / (det(sscp_f, log = FALSE))




###################################################
### MANOVA directly in R
###################################################

# Bind together the outcome columns from the wide data
dvm = cbind(mpls2$read.5, mpls2$read.6, mpls2$read.7, mpls2$read.8)

# Fit the multivariate model
mlm.1 = lm(dvm ~ 1)
mlm.1

# Set up the intra-subject design
my.design = factor(c("read.5", "read.6", "read.7", "read.8"))

library(car)
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Wilks")

# Obtain different multivariate test statistics
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Hotelling-Lawley")
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Roy")
Anova(mlm.1, idata = data.frame(my.design), idesign = ~ my.design, test = "Pillai")

# Compute effect size (1 - Wilk's Lambda)
1 - 0.212800

