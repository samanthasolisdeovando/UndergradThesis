library(readr)
library(dplyr)
library("ggpubr")
my_data <- read.csv(file.choose("shifted_dataset.csv"))
head(my_data)
levels(my_data$Treatment)
my_data$Treatment <- ordered(my_data$Treatment,
                             levels = c("Extended", "Control", "Heatwave"))

#########################################################
#### UPDATED FILTERED DATABASES (using shifted_data) ####
#########################################################
library(dplyr)
Autumn_data <- my_data %>%
  filter(
    (Season == "Autumn")
  )
 
kruskal.test(CH4flux ~ Treatment, data = Autumn_data)

library(dplyr)

Heatwave_and_autumn <- my_data %>%
  filter(
    (Treatment %in% c("Control", "Heatwave") &
       Season %in% c("Late summer / heatwave", "Autumn")
  ))
  
  
kruskal.test(meanGCC ~ Treatment, data = Heatwave_and_autumn)

##################################################
############### FILTERED DATABASES ###############
library(dplyr)

### HEATWAVE and CTRL only, ALL SEASONS ###
filtered_data_base_heatwaveandcontrolonly <- my_data %>%
  filter(
    (Treatment %in% c("Control", "Heatwave"))
  )

###whole autumn of each treatment group, filtered using dplyr###
filtered_data_base_SenescenceAllGroups <- my_data %>%
  filter(
    (Treatment %in% c("Control", "Heatwave") &
       Season %in% c("Autumn / extended summer", "Regular late autumn / warmer early autumn") |
      (Treatment == "Extended" &
         Season %in% c("Regular late autumn / warmer early autumn", "Regular late autumn (extended group only)"))
  ))

### all Treatments, only during heatwave ###
filtered_data_base_allTreatmentsduringheatwave <- my_data %>%
  filter(
    (Season == "Late summer / heatwave")
  )

### HEATWAVE and CTRL only, whole autumn ###
filtered_data_base_HWvsControl <- my_data %>%
  filter(
    (Treatment %in% c("Control", "Heatwave") &
       Season %in% c("Autumn / extended summer", "Regular late autumn / warmer early autumn")
    ))

### HEATWAVE and CTRL only, autumn onset ### 
filtered_data_base_EarlyAutumnHWvsCtrl <- my_data %>%
  filter(
    (Treatment %in% c("Control", "Heatwave") &
       Season == "Autumn / extended summer")
  )

### EXTENDED and CTRL only, whole respective autumns ###
filtered_data_base_SenescenceExtendedVScontrol <- my_data %>%
  filter(
    (Treatment == "Control" &
       Season %in% c("Autumn / extended summer", "Regular late autumn / warmer early autumn") |
       (Treatment == "Extended" &
          Season %in% c("Regular late autumn / warmer early autumn", "Regular late autumn (extended group only)"))
    ))

### EXTENDED and CTRL only, autumn onset ###
filtered_data_base_extendedCTRLautumnONSET <- my_data %>%
  filter(
    (Treatment == "Control" &
       Season == "Autumn / extended summer" |
       (Treatment == "Extended" &
          Season == "Regular late autumn / warmer early autumn")
    ))

### EXTENDED and CTRL only, peak summers ###
filtered_data_base_SummerControlVSExtendedSummer <- my_data %>%
  filter(
    (Treatment == "Control" &
       Season == "Late summer / heatwave" |
       (Treatment == "Extended" &
          Season == "Autumn / extended summer")
    ))

### EXTENDED and CTRL only, peak summers + whole autumn ###
filtered_data_base_summerandautumncTRLandEXTend <- my_data %>%
  filter(
    (Treatment == "Control" &
       Season %in% c("Late summer / heatwave", "Autumn / extended summer")) |
       (Treatment == "Extended" &
          Season %in% c("Autumn / extended summer", "Regular late autumn / warmer early autumn"))
    )

### filter by treatment only ###
filtered_data_base_control <- my_data %>%
  filter(
    (Treatment == "Control")
  )

############### SUMMARY STATISTICS ###############
library(dplyr)
group_by(filtered_data_base_SenescenceAllGroups, Treatment) %>%
  summarize(
    count = n(),
    mean = mean(canopyheight, na.rm = TRUE),
    sd = sd(canopyheight, na.rm = TRUE),
    median = median(canopyheight, na.rm = TRUE),
    IQR = IQR(canopyheight, na.rm = TRUE),
  )

############### PLOTS ###############

### tools for plotting ###
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")


########## BOX PLOTS ##########

### BOX PLOT for variable by all Treatment groups ###
library("ggpubr")
ggboxplot(filtered_data_base_allTreatmentsduringheatwave, x = "Treatment", y = "CO2flux", 
          color = "Treatment", palette = c("#1F77B4", "#FF7F0E", "#2CA02C"),
          order = c("Control", "Heatwave", "Extended"),
          ylab = "CO2 respiration", xlab = "Treatment",
          title = "CO2 respiration during heatwave")

### BOX PLOT for variable by HEATWAVE and CONTROL groups only ###
library("ggpubr")
ggboxplot(filtered_data_base_EarlyAutumnHWvsCtrl, x = "Treatment", y = "meanGCC", 
          color = "Treatment", palette = c("#1F77B4", "#FF7F0E"),
          order = c("Control", "Heatwave"),
          ylab = "meanGCC", xlab = "Treatment",
          title = "GCC during autumn onset")

### BOX PLOT for variable by EXTENDED and CONTROL groups only ### 
library("ggpubr")
ggboxplot(filtered_data_base_summerandautumncTRLandEXTend, x = "Treatment", y = "CO2flux", 
          color = "Treatment", palette = c("#1F77B4", "#2CA02C"),
          order = c("Control", "Extended"),
          ylab = "CO2 respiration", xlab = "Treatment",
          title = "CO2 respiraation during summer and autumn")


########## LINE GRAPHS ##########

### line graph variable ~ Treatment ###
library("ggpubr")
ggline(filtered_data_base_summernoheatwave, x = "Treatment", y = "CO2flux", 
       add = c("mean_se", "jitter"), 
       order = c("Control", "Heatwave", "Extended"),
       ylab = "CO2flux", xlab = "Treatment")


########## SCATTER PLOTS ##########

### scatter plot ggpubr ###
ggscatter(my_data, x = "averageTemp", y = "CO2flux",
          ylab = "CO2 respiration", xlab = "Temperature",
          title = "CO2 respiration by Temperature")

kruskal.test(CO2flux ~ Treatment, data = filtered_data_base_summerandautumncTRLandEXTend)
