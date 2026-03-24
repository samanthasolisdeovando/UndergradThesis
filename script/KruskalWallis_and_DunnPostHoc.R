
###### Kruskal Wallis & Dunn's Post-Hoc Test #####
## Tutorial: Data Novia, https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
## Edited by Samantha Solis de Ovando


########################################################################################
########################################################################################

#this data does not meet the assumptions of one-way ANOVA test, therefore alternative = Kruskal-Wallis

thesis_data <- read.csv("shifted_dataset.csv")

library(tidyverse)
library(ggpubr)
library(rstatix)

########################## data #############################
thesis_data <- thesis_data %>%
  reorder_levels(Treatment, order = c("Control", "Heatwave", "Extended"))

#summer only data
summer_data <- thesis_data %>%
  filter(
    (Season == "Summer")
  )
#heat wave only data
heatwave_data <- thesis_data %>%
  filter(
    (Season == "Heatwave")
  )
#autumn only data
autumn_data <- thesis_data %>%
  filter(
    (Season == "Autumn")
  )

########################## stats #############################
#summary stats
heatwave_data %>% 
  group_by(Treatment) %>%
  get_summary_stats(MeanSoilMoisture, type = "common")

#box plot for 1 variable by Treatment
ggboxplot(heatwave_data, x = "Treatment", y = "MeanSoilMoisture")

#Kruskal-Test (rstatix)
res.kruskal <- summer_data %>% kruskal_test(MeanSoilMoisture ~ Treatment)
res.kruskal

#Effect size
#interpretation:
# 0.01- < 0.06 (small effect)
# 0.06 - < 0.14 (moderate effect)
# >= 0.14 (large effect)
summer_data %>% kruskal_effsize(MeanSoilMoisture ~ Treatment)

#Dunn's Test - pairwise comparisons
pwc <- summer_data %>% 
  dunn_test(MeanSoilMoisture ~ Treatment, p.adjust.method = "bonferroni") 
pwc


