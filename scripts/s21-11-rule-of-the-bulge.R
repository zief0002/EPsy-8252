# Load libraries
library(tidyverse)


# Import data
mammal = read_csv("https://raw.githubusercontent.com/zief0002/epsy-8252/master/data/mammal.csv")
head(mammal)


# Plot
ggplot(data = mammal, aes(x = Brain_weight, y = Bodyweight)) +
  geom_point() +
  geom_smooth(se = FALSE)



