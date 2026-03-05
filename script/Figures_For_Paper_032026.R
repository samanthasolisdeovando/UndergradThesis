
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


##### FOR POSTER ###########
#### multi-panel figure ####
library(ggplot2)
install.packages("patchwork")
library(patchwork)
treatment_colours <- c(
  "Control"  = "#1E88E5",
  "Extended" = "#228B22",
  "Heatwave" = "#F57F17"
)

CAGPoster_theme <- theme_minimal(base_size = 26) +
  theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 22),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 22),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

### make same plot for any variable for poster ###
make_poster_plot <- function(df, y_col, y_lab) {
  ggplot(df, aes(x = measurement.week, y = .data[[y_col]], color = Treatment)) +
    geom_point(alpha = 0.6, na.rm = TRUE, size = 3) +
    geom_smooth(aes(fill = Treatment),
                method = "loess",
                se = TRUE,
                alpha = 0.2,
                na.rm = TRUE,
                linewidth = 1.5) +
    scale_color_manual(values = treatment_colours) +
    scale_fill_manual(values = treatment_colours) +
    labs(x = "Week", y = y_lab, color = "Treatment", fill = "Treatment") +
    CAGPoster_theme
}

p1 <- make_poster_plot(plotting_with_time, "meanGCC", "Greenness")
p2 <- make_poster_plot(plotting_with_time, "canopyheight", "Canopy height (cm)")
p3 <- make_poster_plot(plotting_with_time, "CanopyExtent", "Cover")

### 1x3 grid
combined <- p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

combined

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

#############1x2 Grid for Poster################
### save as: 2500x800

CAGPoster_theme <- theme_minimal(base_size = 26) +
  theme(
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 22),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 22),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

### make same plot for any variable for poster ###
poster_plot <- function(df, y_col, y_lab) {
  ggplot(df, aes(x = measurement.week, y = .data[[y_col]], color = Treatment)) +
    geom_point(alpha = 0.6, na.rm = TRUE, size = 3) +
    geom_smooth(aes(fill = Treatment),
                method = "loess",
                se = TRUE,
                alpha = 0.2,
                na.rm = TRUE,
                linewidth = 1.5) +
    scale_color_manual(values = treatment_colours) +
    scale_fill_manual(values = treatment_colours) +
    labs(x = "Week", y = y_lab, color = "Treatment", fill = "Treatment") +
    CAGPoster_theme
}

p1 <- poster_plot(plotting_with_time, "MeanSoilMoisture", "Soil Moisture")
p2 <- poster_plot(plotting_with_time, "MEANpH", "pH")

### 1x3 grid
combined <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

combined


###### side by side box plot (heatwave and autumn only) ##########

################### PLANT PHYSIOLOGICAL RESPONSE #######################
library(ggplot2)
library(dplyr)
library(patchwork)

treat_cols <- c(
  "Control"  = "#1E88E5",
  "Extended" = "#228B22",
  "Heatwave" = "#F57F17"
)

# only Heat wave & Autumn
soil_data <- plotting_with_time %>%
  filter(Season %in% c("Late summer / heatwave", "Autumn")) %>%
  mutate(
    Season = factor(Season, levels = c("Late summer / heatwave", "Autumn")),
    Treatment = factor(Treatment, levels = c("Control", "Heatwave", "Extended"))
  )

# pH plot
pH_plot <- ggplot(soil_data, aes(x = Season, y = MEANpH, fill = Treatment)) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.5,
    na.rm = TRUE,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = treat_cols) +
  labs(
    title = "Soil pH",
    x = NULL,
    y = "Mean pH",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Moisture
moisture_plot <- ggplot(soil_data, aes(x = Season, y = MeanSoilMoisture, fill = Treatment)) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.5,
    na.rm = TRUE,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = treat_cols) +
  labs(
    title = "Soil Moisture",
    x = NULL,
    y = "Mean soil moisture",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#combine plots 1x2 grid + one legend
combined <- (pH_plot + moisture_plot) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

combined

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
  theme_minimal(base_size = 26)

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
  theme_minimal(base_size = 26)


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


################################################################
####### BOX PLOTS - add significance lines BETWEEN BOXES! ######
######################## PLANT RESPONSE ########################


################### FOR POSTER #################
## save as: width = 2500, height = 2100 ##
####### GCC #######
library(ggplot2)
library(dplyr)

box_data <- plotting_with_time %>%
  mutate(
    Treatment = factor(Treatment, levels = c("Control", "Heatwave", "Extended")),
    Season = factor(
    Season, 
    levels = c("Early summer", "Mid-summer", "Late summer / heatwave", "Autumn"),
    labels = c("Early summer", "Mid-summer", "Heat wave", "Autumn"))
  ) %>%
  filter(!is.na(meanGCC))

p <- ggplot(box_data, aes(x = Treatment, y = meanGCC, fill = Treatment)) +
  geom_boxplot(width = 0.7, outlier.alpha = 0.6) +
  facet_grid(. ~ Season) +
  labs(
    title = "GCC by treatment across seasons",
    x = "",
    y = "GCC",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c(
    "Control"  = "#2C6DB2",
    "Extended" = "#2E7D32",
    "Heatwave" = "#D55E00"
  )) +
  theme_minimal(base_size = 50) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p)

#####################################
######### CANOPY HEIGHT #############
############ BOX PLOT ###############

## save as: width = 2500, height = 2100 ##
####### Canopy Height #######
library(ggplot2)
library(dplyr)

box_data <- plotting_with_time %>%
  mutate(
    Treatment = factor(Treatment, levels = c("Control", "Heatwave", "Extended")),
    Season = factor(
      Season, 
      levels = c("Early summer", "Mid-summer", "Late summer / heatwave", "Autumn"),
      labels = c("Early summer", "Mid-summer", "Heat wave", "Autumn"))
  ) %>%
  filter(!is.na(canopyheight))

p <- ggplot(box_data, aes(x = Treatment, y = canopyheight, fill = Treatment)) +
  geom_boxplot(width = 0.7, outlier.alpha = 0.6) +
  facet_grid(. ~ Season) +
  labs(
    title = "Canopy Height by treatment across seasons",
    x = "",
    y = "GCC",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c(
    "Control"  = "#2C6DB2",
    "Extended" = "#2E7D32",
    "Heatwave" = "#D55E00"
  )) +
  theme_minimal(base_size = 50) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p)

#####################################
################# COVER #############
############ BOX PLOT ###############

## save as: width = 2500, height = 2100 ##
####### CO2 #######
library(ggplot2)
library(dplyr)

box_data <- plotting_with_time %>%
  mutate(
    Treatment = factor(Treatment, levels = c("Control", "Heatwave", "Extended")),
    Season = factor(
      Season, 
      levels = c("Early summer", "Mid-summer", "Late summer / heatwave", "Autumn"),
      labels = c("Early summer", "Mid-summer", "Heat wave", "Autumn"))
  ) %>%
  filter(!is.na(CO2flux)) %>%
  filter(Season %in% c("Heat wave", "Autumn"))

p <- ggplot(box_data, aes(x = Treatment, y = CO2flux, fill = Treatment)) +
  geom_boxplot(width = 0.7, outlier.alpha = 0.6) +
  facet_grid(. ~ Season) +
  labs(
    title = "CO2 Respiration during heat wave and autumn",
    x = "",
    y = "CO2 flux",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c(
    "Control"  = "#2C6DB2",
    "Extended" = "#2E7D32",
    "Heatwave" = "#D55E00"
  )) +
  theme_minimal(base_size = 50) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p)