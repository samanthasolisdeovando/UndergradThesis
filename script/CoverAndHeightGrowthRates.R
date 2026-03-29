### Written by Samantha Solis de Ovando
tutorial: https://forum.posit.co/t/growth-rate-calculation-in-r/38675
### weekly growth rate calculated with change in canopy height per week (cm)
library(dplyr)
growth_data <- read.csv("shifted_dataset.csv")


growth_rate_height = growth_data %>%
  group_by(PotID) %>%
  arrange(measurement.week, .by_group = TRUE) %>%
  mutate(Diff_height = canopyheight - lag(canopyheight),
       Rate_change_percent = (Diff_height / lag(canopyheight))* 100) %>%
  ungroup()

print(growth_rate_height)
write.csv(growth_rate_height, "shifted_dataset_added_height_growth_rate.csv", row.names = FALSE)
