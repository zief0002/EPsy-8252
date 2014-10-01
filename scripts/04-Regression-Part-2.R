###################################################
### Read in the NFL FCI Data
###################################################

nfl = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/FCI-NFL-2014.csv")

head(nfl)
tail(nfl)
str(nfl)

meta = read.csv(file = "/Users/andrewz/Documents/EPsy-8252/data/NFL-Meta-Data.csv")
head(meta)
tail(meta)
str(meta)

# Merge meta data into nfl data frame
nfl = merge(nfl, meta, by = "team")

# Create age of stadium variable
nfl$ageStadium = 2014 - nfl$yearOpened



###################################################
### Examine outcome
###################################################

library(sm)
sm.density(nfl$fci)

# Create log of outcome
nfl$Lfci = log(nfl$fci)

sm.density(nfl$Lfci)

library(psych)
describe(nfl$Lfci)



###################################################
### fci vs. ageStadium
###################################################

library(ggplot2)

ggplot(data = nfl, aes(x = ageStadium, y = Lfci)) +
	geom_point() +
	#geom_text(aes(label = team)) +
	geom_smooth(se = FALSE) +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()

lm.test = lm(Lfci ~ ageStadium, data = nfl)
test = fortify(lm.test)
sm.density(test$.stdresid, model = "normal")

# Residual plot
ggplot(data = test, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 3) +
	geom_hline(yintercept = 0) +
	theme_bw()




nfl$ageStadiumQ = nfl$ageStadium ^ 2

summary(lm(Lfci ~ ageStadium + ageStadiumQ, data = nfl))



###################################################
### Other predictors
###################################################

library(car)
scatterplotMatrix(nfl[c("Lfci", "ageStadium", "ageStadiumQ", "pts2013", "ptsAgainst2013", 
	"winPct2013", "coachYrswTeam", "newCoach", "playoffs2013", "playoffWins2013", 
	"lat", "lon", "capacity")])

corMat = cor(nfl[c("Lfci", "ageStadium", "ageStadiumQ", "pts2013", "ptsAgainst2013", 
	"winPct2013", "coachYrswTeam", "newCoach", "playoffs2013", "playoffWins2013", 
	"lat", "lon", "capacity")])

# Needs psych library
#remove effects of age of stadium (L and Q)
partial.r(corMat, c(1, 4:13), c(2, 3)) 

# Examne coachYrswTeam predictor
sm.density(nfl$LcoachYrswTeam)

ggplot(data = nfl, aes(x = coachYrswTeam, y = Lfci)) +
	geom_point() +
	#geom_text(aes(label = team)) +
	geom_smooth(se = FALSE) +
	theme_bw()

nfl$LcoachYrswTeam = log(nfl$coachYrswTeam + 1)
ggplot(data = nfl, aes(x = LcoachYrswTeam, y = Lfci)) +
	geom_point() +
	#geom_text(aes(label = team)) +
	geom_smooth(se = FALSE) +
	theme_bw()

summary(lm(Lfci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam, data = nfl))



###################################################
### Other predictors: Part II
###################################################

corMat = cor(nfl[c("Lfci", "ageStadium", "ageStadiumQ", "pts2013", "ptsAgainst2013", 
	"winPct2013", "LcoachYrswTeam", "newCoach", "playoffs2013", "playoffWins2013", 
	"lat", "lon", "capacity")])

#remove effects of age of stadium (L and Q) and coachYrswTeam
partial.r(corMat, c(1, 4:6, 8:13), c(2, 3, 7)) 

# Examine capacity 
sm.density(nfl$capacity)

ggplot(data = nfl, aes(x = capacity, y = Lfci)) +
	geom_point() +
	#geom_text(aes(label = team)) +
	geom_smooth(se = FALSE) +
	theme_bw()

summary(lm(Lfci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam + capacity, data = nfl))



###################################################
### Check residuals
###################################################

lm.a = lm(fci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam, data = nfl)
out.a = fortify(lm.a)
head(out.a)

# Density plot of residuals
sm.density(out.a$.stdresid, model = "normal")

# Residual plot
ggplot(data = out.a, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 3) +
	geom_hline(yintercept = 0) +
	theme_bw()

# Residual plot (Identify teams)
out.a$team = nfl$team

ggplot(data = out.a, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = team), size = 3) +
	geom_hline(yintercept = 0) +
	theme_bw()

# Residual plot (Identify row number)
ggplot(data = out.a, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = rownames(out.a)), size = 3) +
	geom_hline(yintercept = 0) +
	theme_bw()	



###################################################
### Remove Washington (Row 32)
###################################################

lm.b = lm(fci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam, data = nfl, subset = -c(32))
out.b = fortify(lm.b)
out.b$team = nfl$team[-c(32)]
head(out.b)

# Density plot of residuals
sm.density(out.b$.stdresid, model = "normal")

# Residual plot
ggplot(data = out.b, aes(x = .fitted, y = .stdresid)) +
	geom_point(size = 3) +
	geom_hline(yintercept = 0) +
	theme_bw()

# Residual plot (Identify teams)
ggplot(data = out.b, aes(x = .fitted, y = .stdresid)) +
	geom_text(aes(label = team), size = 3) +
	geom_hline(yintercept = 0) +
	theme_bw()



###################################################
### Is it more expensive to go to a game if the stadium has a retractable roof?
###################################################

# Dmmy code for retractable roof
nfl$roof2 = ifelse(nfl$roof == "Retractable", 1, 0)

lm.c = lm(fci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam + roof2, data = nfl)
display(lm.c)




###################################################
### Plot of Model B (final model)
###################################################

# Create data frame from which to predict
# For LcoachYrswTeam; log(c(0, 2, 5, 10) + 1)
nfl2 = expand.grid(
	ageStadium = 0:90,
	LcoachYrswTeam = c(0, 1.098612, 1.791759, 2.397895) # O, 2, 5, and 10 years with team
	)


# Get predicted values for fci
preds = predict(lm.b, newdata = nfl2)
head(preds)

# Add predicted values as a column in the nfl2 data frame
nfl2 = cbind(nfl2, preds)
head(nfl2)

# Create factor of LcoachYrswTeam
nfl2$LcoachYrswTeam = factor(nfl2$LcoachYrswTeam,
	levels = c(0, 1.098612, 1.791759, 2.397895),
	labels = c("0 years", "2 years", "5 years", "10 years")
	)


# Plot
ggplot(data = nfl2, aes(x = ageStadium, y = preds, group = LcoachYrswTeam)) +
    geom_line(aes(color = LcoachYrswTeam)) +
    xlab("Age of Stadium") +
    ylab("Predicted Cost") +
    theme_bw() +
    scale_color_brewer(name = "Time with team", palette = "Set2")



###################################################
### Plot of Model B (avg. LcoachYrswTeam)
###################################################

nfl2 = expand.grid(
	ageStadium = 0:90,
	LcoachYrswTeam = 1.136643
	)
preds = predict(lm.b, newdata = nfl2)
nfl2 = cbind(nfl2, preds)
head(nfl2)



###################################################
### Save plot as SVG
###################################################

library(RSvgDevice)

devSVG("~/Desktop/NFL-FCI.svg", width = 12, height = 8)
ggplot(data = nfl2, aes(x = ageStadium, y = preds)) +
    geom_line() +
    #geom_text(data = nfl, aes(y = fci, label = team)) +
    geom_point(data = nfl, aes(y = fci)) +
    xlab("Age of Stadium") +
    ylab("Predicted Cost") +
    theme_bw() +
    scale_color_brewer(name = "Time with team", palette = "Set2")
dev.off()

ggplot(data = nfl2, aes(x = ageStadium, y = preds, group = LcoachYrswTeam)) +
    geom_line(aes(color = LcoachYrswTeam)) +
    geom_text(data = nfl, aes(y = fci, label = team)) +
    geom_point(data = nfl, aes(y = fci)) +
    xlab("Age of Stadium") +
    ylab("Predicted Cost") +
    theme_bw() +
    scale_color_brewer(name = "Time with team", palette = "Set2") + xlim(0, 25)



###################################################
### Save plot as SVG
###################################################	

lm.b = lm(fci ~ ageStadium + I(ageStadium ^ 2) + LcoachYrswTeam, data = nfl, subset = -c(32))
out.b = fortify(lm.b)
out.b$team = nfl$team[-c(32)]
head(out.b)

# Compute for Washington
coef(lm.b)
499.92082564 + -7.47318934*(17) + 0.09435957*(17)^2 + 48.83633091*(0)
597.51 - 400.1465

# Residual plot
devSVG("~/Desktop/FCI-Residuals.svg", width = 10, height = 10)
ggplot(data = out.b, aes(x = .fitted, y = .resid)) +
	geom_point(size = 3) +
	geom_text(aes(label = team)) +
	geom_point(x = 597.51, y = 197.3635) +
	geom_hline(yintercept = 0) +
	theme_bw() +
	ylim(-200, 200) +
	xlim(550, 650)
dev.off()








