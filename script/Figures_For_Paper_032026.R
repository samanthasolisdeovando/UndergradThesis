
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
write.csv(shifted_data, "shifted_dataset.csv", row.names = FALSE)