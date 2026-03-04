
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


#### multi-panel figure ####

############## ABIOTIC SOIL PROPERTIES  ##############

####### SOIL MOISTURE #######
### scatter with linear regression line ###
#file name: SoilMoisture_Scatter_LinearRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = MeanSoilMoisture, color = Treatment)) +
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
    y = "Soil Moisture"
  ) +
  theme_minimal()

### scatter with local regression line ###
#file name: SoilMoisture_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = MeanSoilMoisture, color = Treatment)) +
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
    y = "Soil Moisture"
  ) +
  theme_minimal()

#################### SOIL PH ####################

### scatter with linear regression line ###
#file name: SoilpH_Scatter_LinearRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = MEANpH, color = Treatment)) +
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
    y = "Soil pH"
  ) +
  theme_minimal()

### scatter with local regression line ###
#file name: SoilpH_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = MEANpH, color = Treatment)) +
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
    y = "Soil pH"
  ) +
  theme_minimal()

################### PLANT PHYSIOLOGICAL RESPONSE #######################

#### EVAPOTRANSPIRATION ###
# Pot water loss / week. ET proxy. 

### scatter with linear regression line ###
#file name: Evapotranspiration_Scatter_LinearRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = EvapotranspirationRate, color = Treatment)) +
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
    y = "Evapotranspiration (Kg lost/week)"
  ) +
  theme_minimal()

### scatter with local regression line ###
#file name: Evapotranspiration_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = EvapotranspirationRate, color = Treatment)) +
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
    y = "Evapotranspiration (Kg lost/week)"
  ) +
  theme_minimal()

###################### ECOSYSTEM GAS FLUXES #######################

############ CO2 respiration #########

### scatter with linear regression line ###
#file name: CO2_Scatter_LinearRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = CO2flux, color = Treatment)) +
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
    y = "CO2 respiration"
  ) +
  theme_minimal()

### Scatter with local regression line ###
#File name: CO2_Scatter_LocalRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = CO2flux, color = Treatment)) +
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
    y = "CO2 respiration"
  ) +
  theme_minimal()


############### CH4 Respiration ###############

### scatter with linear regression line ###
#file name: CH4_Scatter_LinearRegression

library(ggplot2)

ggplot(plotting_with_time, aes(x = measurement.week, y = CH4flux, color = Treatment)) +
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
    y = "CH4 respiration"
  ) +
  theme_minimal()

##########################################
### Scatter with local regression line ###
#File name: CH4_Scatter_LocalRegression###
##### includes dashed season lines (needs to be updaed) - not working:
#mid-summer line is missing, text overlapping

library(ggplot2)
#added for season labels
library(dplyr)
season_blocks <- plotting_with_time %>%
  filter(!is.na(SeasonLabels), !is.na(measurement.week)) %>%
  group_by(SeasonLabels) %>%
  summarise(
    xmin = min(measurement.week),
    xmax = max(measurement.week),
    xmid = (xmin + xmax) / 2,
    .groups = "drop"
  )
heatwave_start <- 5
heatwave_end <- 7

yr <- range(plotting_with_time$CH4flux, na.rm = TRUE)
y_label <- yr[1] - 0.15 * diff(yr)

### scatter original
ggplot(plotting_with_time, aes(x = measurement.week, y = CH4flux, color = Treatment)) +

##### heatwave shaded block ######
  geom_rect(
    aes(xmin = heatwave_start, xmax = heatwave_end,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#ffdbdc",
    alpha = 0.15
  ) +
  
#### dashed season lines #####
geom_vline(
  data = season_blocks,
  aes(xintercept = xmin),
  linetype = "dashed",
  color = "black",
  alpha = 0.6
) +
  
### season labels ###
  geom_text(
    data = season_blocks,
    aes(x = xmid, y = y_label, label = gsub(" ", "\n", SeasonLabels)),
    inherit.aes = FALSE,
    color = "black",
    size = 3,
    lineheight = 0.9
  ) +
  
  expand_limits(y = y_label)+

  ##############################
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
    y = "CH4 respiration"
  ) +
  theme_minimal()

####### BOX PLOTS COMPARING SEASONS ###
####### BOX PLOTS - add significance lines BETWEEN BOXES! ######