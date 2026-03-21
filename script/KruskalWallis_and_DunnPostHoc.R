
###### Kruskal Wallis & Dunn's Post-Hoc Test #####
## Tutorial: Data Novia, https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
## Edited by Samantha Solis de Ovando


########################################################################################
########################################################################################

##### Kruskal-Wallis #####
#this data does not meet the assumptions of one-way ANOVA test, therefore alternative = Kruskal-Wallis

thesis_data <- read.csv("shifted_dataset.csv")

library(tidyverse)
library(ggpubr)
library(rstatix)
thesis_data <- thesis_data %>%
  reorder_levels(Treatment, order = c("Control", "Heatwave", "Extended"))

#summary stats
thesis_data %>% 
  group_by(Treatment) %>%
  get_summary_stats(meanGCC, type = "common")

#box plot for 1 variable by Treatment
ggboxplot(thesis_data, x = "Treatment", y = "meanGCC")

#Kruskal-Test (rstatix)
res.kruskal <- thesis_data %>% kruskal_test(Cover ~ Treatment)
res.kruskal

#Effect size
#interpretation:
# 0.01- < 0.06 (small effect)
# 0.06 - < 0.14 (moderate effect)
# >= 0.14 (large effect)
thesis_data %>% kruskal_effsize(Cover ~ Treatment)