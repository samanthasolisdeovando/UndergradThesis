
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
library(ggplot2)

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
autumn_warm_data %>% 
  group_by(Season) %>%
  get_summary_stats(CO2flux, type = "common")

#box plot for 1 variable
ggboxplot(autumn_cold_data, x = "Treatment", y = "CO2flux")

#whole treatment box plot (using ggplot2)
#written by Juliana Dioquino
#edited by Samantha Solis de Ovando

ggplot(data = thesis_data, aes(x = Treatment, y = CO2flux, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) +
  facet_grid(~factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold')))+
  labs(x = "Treatment",
       y = "CO2flux") +
  geom_jitter(width = 0.12, alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Control" = "#1F77B4",
                               "Heatwave" = "#FF7F0E",
                               "Extended" = "#2CA02C")) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        strip.background = element_blank(), #element_rect(colour = "grey", fill = "#EDEDED"),
        strip.text.x = element_text(size = 9, colour = "black"),
        axis.text = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 13, margin = margin(t = 12)),
        axis.title.y = element_text(size = 13, margin = margin(r = 12)),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1))


#Kruskal-Test (rstatix)
res.kruskal <- thesis_data %>% kruskal_test(Cover ~ Season)
res.kruskal

#Effect size
#interpretation:
# 0.01- < 0.06 (small effect)
# 0.06 - < 0.14 (moderate effect)
# >= 0.14 (large effect)
thesis_data %>% kruskal_effsize(Cover ~ Season)

#Dunn's Test - pairwise comparisons
pwc <- thesis_data %>% 
  dunn_test(CO2flux ~ Season, p.adjust.method = "bonferroni") 
pwc


