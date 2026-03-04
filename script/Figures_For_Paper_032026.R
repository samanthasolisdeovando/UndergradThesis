
############## UPDATED FIGURES ###############
#### Written by Samantha Solis de Ovando #####

# set working directory 
setwd("~/Documents/UndergradThesis/data")

####### shift extended grp by -2 weeks #######
library(dplyr)
original_data <- read.csv("MasterDataSheet_UGthesis_Feb232026.csv")
shifted_data <- original_data %>%
  mutate(measurement.week = ifelse(Treatment == "Extended",
                                   measurement.week - 2,
                                   measurement.week))
#print CSV
#write.csv(shifted_data, "shifted_dataset.csv", row.names = FALSE)

###########################################################################
################################# FIGURES #################################
###########################################################################

### IMPORTANT: USE THIS DATA WHEN PLOTTING BY TIME
plotting_with_time <- read.csv("shifted_dataset.csv")
plotting_with_time$canopyheight <- as.numeric(as.character(plotting_with_time$canopyheight))


############## PLANT PRODUCTIVITY ##############

####### GREENNESS #######

## Scatter plot with linear best fit and SE ribbon ##

### figure file name = GCC_Scatter_Linear_Ribbon

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = meanGCC, color = Treatment)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(aes(fill = Treatment),
              method = "lm",
              se = TRUE,
              alpha = 0.2,
              na.rm = TRUE) +
  scale_color_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  scale_fill_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "GCC"
  ) +
  theme_minimal()

#############################################
## Scatter plot with local regression line ##
#file name of figure: GCC_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = meanGCC, color = Treatment)) +
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
  scale_fill_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "GCC"
  ) +
  theme_minimal()

###################### CANOPY HEIGHT ######################

### trouble shooting - plots not coming out right:
#class(plotting_with_time$canopyheight)
#str(plotting_with_time$canopyheight)
#plotting_with_time$canopyheight <- as.numeric(as.character(plotting_with_time$canopyheight))

### scatter with linear regression line ###
#file name: CanopyHeight_Scatter_LinearRegression
library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = canopyheight, color = Treatment)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(aes(fill = Treatment),
              method = "lm",
              se = TRUE,
              alpha = 0.2,
              na.rm = TRUE) +
  scale_color_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  scale_fill_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "Canopy Height"
  ) +
  theme_minimal()

### Canopy height scatter with local regression line ###
#file name: CanopyHeight_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = canopyheight, color = Treatment)) +
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
  scale_fill_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "Canopy Height"
  ) +
  theme_minimal()

###################### COVER ######################
### cover scatter with linear regression
# file name for figure: Cover_Scatter_LinearRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = CanopyExtent, color = Treatment)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(aes(fill = Treatment),
              method = "lm",
              se = TRUE,
              alpha = 0.2,
              na.rm = TRUE) +
  scale_color_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  scale_fill_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "Cover"
  ) +
  theme_minimal()

### cover scatter with local regression ###
# file name: Cover_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = CanopyExtent, color = Treatment)) +
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
  scale_fill_manual(values = c(
    "Control" = "#1E88E5",
    "Extended" = "#228B22",
    "Heatwave" = "#F57F17"
  )) +
  labs(
    x = "Week",
    y = "Cover"
  ) +
  theme_minimal()

############## SOIL  ##############


####### BOX PLOTS - add significance lines BETWEEN BOXES! ######