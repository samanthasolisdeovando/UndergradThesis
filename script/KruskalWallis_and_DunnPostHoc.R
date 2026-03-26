
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
#autumn warm only data
autumn_warm_data <- thesis_data %>%
  filter(
    (Season == "Autumn warm")
  )

#autumn cold only data
autumn_cold_data <- thesis_data %>%
  filter(
    (Season == "Autumn cold")
  )

########################## stats #############################
#summary stats
thesis_data %>% 
  group_by(Season) %>%
  get_summary_stats(CO2flux, type = "common")

#box plot for 1 variable by Treatment
ggboxplot(thesis_data, x = "Season", y = "CO2flux")

#Kruskal-Test (rstatix)
res.kruskal <- thesis_data %>% kruskal_test(CO2flux ~ Season)
res.kruskal

#Effect size
#interpretation:
# 0.01- < 0.06 (small effect)
# 0.06 - < 0.14 (moderate effect)
# >= 0.14 (large effect)
thesis_data %>% kruskal_effsize(CO2flux ~ Season)

#Dunn's Test - pairwise comparisons
pwc <- thesis_data %>% 
  dunn_test(CO2flux ~ Season, p.adjust.method = "bonferroni") 
pwc


