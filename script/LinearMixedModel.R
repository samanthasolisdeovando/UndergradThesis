############ source: Coding Club ###########
###### edited by Samantha Solis de Ovando ######

library(readr)
master_data_modelling <- read_csv("MasterDataSheet_UGthesis_Feb232026.csv")

head(master_data_modelling)

hist(master_data_modelling$CO2flux)

basic.lm <- lm(CH4flux ~ MeanSoilMoisture, data = master_data_modelling)
summary(basic.lm)