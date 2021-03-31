library(tidyverse)


# Create data set a
tab_01 = data.frame(
  Site = c("UMN", "UMN", "Columbia", "Drake"),
  Participant = c("Andy", "Toni", "Mohammed", "Simba")
)


# Create data set b
tab_02 = data.frame(
  Participant = c("Andy", "Toni", "Geri", "Mohammed"),
  Score = c(3, 15, 12, 15)
)


# View the data sets
tab_01
tab_02


# Try different joins
joined_data = left_join(tab_01, tab_02, by = "Participant")

right_join(tab_01, tab_02, by = "Participant")

inner_join(tab_01, tab_02, by = "Participant")

full_join(tab_01, tab_02, by = "Participant")
