############ source: Coding Club ###########
###### edited by Samantha Solis de Ovando ######

####################### LOAD DATA FOR MODELLING #######################
library(readr)

### this is the ONLY sheet I should use for modelling ##
master_data_modelling <- read_csv("MasterDataSheet_UGthesis_Feb232026.csv")

#show headings
head(master_data_modelling)

############################ VISUALIZE DATA ############################
# response variable histogram
#CO2 flux
hist(master_data_modelling$CO2flux)

#pH

#moisture

#evapotranspiration

#meanGCC

#cover

#canopy height

########################## BASIC LINEAR MODEL ##########################

#### plant productivity #####

#greenness (scale 0-1)
basic.lm <- lm(meanGCC ~ MeanSoilMoisture, data = master_data_modelling)
summary(basic.lm)

#cover (scale 0-1)
basic.lm <- lm(CanopyExtent ~ MeanSoilMoisture, data = master_data_modelling)
summary(basic.lm)

#canopy height (cm)
basic.lm <- lm(canopyheight ~ MeanSoilMoisture, data = master_data_modelling)
summary(basic.lm)