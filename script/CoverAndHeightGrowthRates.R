### Written by Samantha Solis de Ovando
tutorial: https://forum.posit.co/t/growth-rate-calculation-in-r/38675
### weekly growth rate calculated with change in canopy height per week (cm)
library(dplyr)
growth_data <- read.csv("shifted_dataset.csv")


growth_rate_height = growth_data %>%
  group_by(PotID) %>%
  arrange(measurement.week, .by_group = TRUE) %>%
  mutate(DiffHeight = canopyheight - lag(canopyheight),
       HeightPercentChange = (DiffHeight / lag(canopyheight))* 100) %>%
  ungroup()

print(growth_rate_height)
write.csv(growth_rate_height, "shifted_dataset_added_height_growth_rate.csv", row.names = FALSE)

#### GCC rate change

green_data <- read.csv("shifted_dataset_added_height_growth_rate.csv")
GCC_rate = green_data %>%
  group_by(PotID) %>%
  arrange(measurement.week, .by_group = TRUE) %>%
  mutate(DiffGCC = meanGCC - lag(meanGCC),
         GCCpercentChange = (DiffGCC / lag(meanGCC))* 100) %>%
  ungroup()

print(GCC_rate)
write.csv(GCC_rate, "shifted_dataset_added_gcc.csv", row.names = FALSE)

#### Δ Cover Scatter
cover_data <- read.csv("shifted_dataset_added_gcc.csv")

cover_rate = cover_data %>%
  group_by(PotID) %>%
  arrange(measurement.week, .by_group = TRUE) %>%
  mutate(DiffCover = Cover - lag(Cover),
         CoverPercentChange = (DiffCover / lag(Cover))* 100) %>%
  ungroup()

print(cover_rate)
write.csv(cover_rate, "shifted_dataset_GccCoverHeightChange.csv", row.names = FALSE)

scatter_data <- read.csv("shifted_dataset_GccCoverHeightChange.csv")

library(ggplot2)
ggplot(scatter_data, aes(x = measurement.week, y = CoverPercentChange, color = Treatment)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(aes(fill = Treatment),
              method = "loess",
              se = TRUE,
              alpha = 0.2,
              na.rm = TRUE) +
  scale_color_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "Cover % change"
  ) +
  theme_minimal()

#Δ Height Scatter
library(ggplot2)
ggplot(scatter_data, aes(x = measurement.week, y = DiffHeight, color = Treatment)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(aes(fill = Treatment),
              method = "loess",
              se = TRUE,
              alpha = 0.2,
              na.rm = TRUE) +
  scale_color_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "∆ Height"
  ) +
  theme_minimal()

#Δ GCC Scatter
library(ggplot2)
ggplot(scatter_data, aes(x = measurement.week, y = DiffGCC, color = Treatment)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(aes(fill = Treatment),
              method = "loess",
              se = TRUE,
              alpha = 0.2,
              na.rm = TRUE) +
  scale_color_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "∆ GCC"
  ) +
  theme_minimal()

