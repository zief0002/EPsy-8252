###################################################
### Read in the data
###################################################

mpls = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/minneapolis.csv")

mpls



###################################################
### Remove missing data
###################################################

mpls2 = mpls[complete.cases(mpls), ]


###################################################
### Descriptively examine data
###################################################

summary(mpls2[2:5])




###################################################
### Reshaping the data to the long format
###################################################

library(reshape2)

mplsLong = melt(
	mpls2, 
	id = c("studentID"),
	measure = c("read.5", "read.6", "read.7", "read.8")
	)

head(mplsLong)

# Rename the columns
names(mplsLong)[2] = "grade"
names(mplsLong)[3] = "read"

head(mplsLong)

# Rename the levels of grade
levels(mplsLong$grade)

levels(mplsLong$grade)[1] = "grade.5"
levels(mplsLong$grade)[2] = "grade.6"
levels(mplsLong$grade)[3] = "grade.7"
levels(mplsLong$grade)[4] = "grade.8"

head(mplsLong)



###################################################
### Plot the reading scores by grade
###################################################

library(ggplot2)

ggplot(data = mplsLong, aes(x = grade, y = read)) +
	geom_point(alpha = 0.4) +
	#stat_summary(fun.y = "mean", geom = "line", aes(group = 1)) +
	stat_summary(fun.y = "mean", geom = "point", pch = 19, size = 3) +
	theme_bw() +
	xlab("Grade") +
	ylab("Reading Score")


ggplot(data = mplsLong, aes(x = grade, y = read)) +
	geom_line(alpha = 0.4, aes(group = studentID)) +
	#stat_summary(fun.y = "mean", geom = "line", aes(group = 1), lwd = 2) +
	#stat_summary(fun.y = "mean", geom = "point", pch = 19, size = 3) +
	theme_bw() +
	xlab("Grade") +
	ylab("Reading Score")




###################################################
### Analyze data with independence assumption
###################################################

lm.1 = lm(read ~ 1 + grade, data = mplsLong)
anova(lm.1)

# Examine assumptions
out1 = fortify(lm.1)

library(sm)
sm.density(out1$.stdresid, model = "normal")

ggplot(data = out1, aes(x = .fitted, y = .stdresid)) +
	geom_point() +
	geom_hline(yintercept = 0) +
	theme_bw()


# Add the student IDs to the fortified data
out1$ID = mplsLong$studentID

# Examine independence of residuals within a "group"
ggplot(data = out1, aes(x = .fitted, y = .stdresid, color = factor(ID))) +
	geom_point() +
	theme_bw() +
	geom_hline(yintercept = 0) +
	facet_wrap(~ID) +
	guides(color = FALSE)




###################################################
### Coerce studentID into a factor
###################################################

mplsLong$student = as.factor(mplsLong$studentID)



###################################################
### Examine data for interaction
###################################################

ggplot(data = mplsLong, aes(x = grade, y = read, group = student)) +
	geom_line(aes(color = student)) +
	theme_bw()



###################################################
### Fit repeated measures ANOVA using lm()
###################################################

lm.2 = lm(read ~ grade + student + grade:student, data = mplsLong)
anova(lm.2)

1 - pf(18.55, df1 = 3, df2 = 39)




###################################################
### Check sphericity (compound symmetry) assumption
###################################################

## Examine the variance-covariance matrix
var(mpls2[2:5])


## Examine the correlation matrix
cor(mpls2[2:5])


## Function to compute Greenhouse-Geisser epsilon estimate
GG = function(s, k){
	d = k ^ 2 * (mean(diag(s)) - mean(s)) ^ 2
	n1 = sum(s ^ 2)
	n2 = 2 * k * sum(apply(s, 1, mean) ^ 2)
	n3 = k ^ 2 * mean(s) ^ 2
	epsi = d / ((k - 1) * (n1 - n2 + n3))
	return(epsi)
	}

S = var(mpls2[2:5])

## Compute epsilon
GG(S, k = 4)

1 - pf(18.55, df1 = 2.032201, df2 = 26.41861)



# Huynh-Feldt correction
HF = function(epsi, k = 3, n = 30){
	epsiHF = (n * (k - 1) * epsi - 2) / ((k - 1) * ((n - 1) - (k - 1) * epsi))
	return(epsiHF)
	}

HF(epsi = 0.6774003, k = 4, n = 14)

1 - pf(18.55, df1 = 2.411679, df2 = 31.35183)



###################################################
### Fit repeated measures ANOVA using ezANOVA()
###################################################

library(ez)

rm.aov = ezANOVA(data = mplsLong, 
	dv = read, 
	wid = student, 
	within = .(grade), 
	detailed = TRUE
	)

rm.aov


###################################################
### Fit repeated measures ANOVA with feamle as a predictor as well
###################################################


mplsLong = melt(
	mpls2, 
	id = c("studentID", "female"),
	measure = c("read.5", "read.6", "read.7", "read.8")
	)

# Rename the columns
names(mplsLong)[3] = "grade"
names(mplsLong)[4] = "read"

library(ez)
aov.2 = ezANOVA(
	data = mplsLong, 
	dv = read, 
	wid = studentID, 
	within = .(grade),
	between = female, 
	detailed = TRUE
	)
aov.2


