# Load librarires
library(AICcmodavg)
library(broom)
library(corrr)
library(dplyr)
library(ggplot2)
library(nnet) # Used to fit multinomial models
library(readr)
library(sm)
library(texreg)
library(tidyr)


# Read in italian wine data
wine = read_csv("~/Dropbox/epsy-8252/data/italian-wines.csv")
head(wine)



wine$cultivar = factor(wine$cultivar)


model.1 = multinom(cultivar ~ 1 + intensity, data = wine)
summary(model.1)



############################################################

# Set Nebbiolo as reference group
wine$cultivar = relevel(factor(wine$cultivar), ref = "Nebbiolo")

# Check the ordering of levels; Nebbiolo should now be first
levels(wine$cultivar)

model.1 = multinom(cultivar ~ 1 + intensity, data = wine)
summary(model.1)