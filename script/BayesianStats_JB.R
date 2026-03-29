
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

#canopyheight
ggplot(data = thesis_data, aes(x = Treatment, y = canopyheight, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) +
  facet_grid(~factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold')))+
  labs(x = "Treatment",
       y = "canopyheight") +
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

#GCC
ggplot(data = thesis_data, aes(x = Treatment, y = meanGCC, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) +
  facet_grid(~factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold')))+
  labs(x = "Treatment",
       y = "Greenness Chromatic Coordinate (GCC)") +
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

#### bayesian version ####
#### Written by Jeremy Borderieux ####
#### Edited by Samantha Solis de Ovando ####

#### bayesian CO2 ####

library(brms)

## defining my prior knowlkedge
# intercept, baseline flux of the control plot of a given season
my_prior_co2 <- set_prior("normal(1.5,1.5)",class = "b",coef = "SeasonAutumncold")
my_prior_co2 <- c(my_prior_co2,set_prior("normal(1.5,1.5)",class = "b",coef = "SeasonAutumnwarm"))
my_prior_co2 <- c(my_prior_co2,set_prior("normal(1.5,1.5)",class = "b",coef = "SeasonHeatwave"))
my_prior_co2 <- c(my_prior_co2,set_prior("normal(1.5,1.5)",class = "b",coef = "SeasonSummer"))
# treatment effect of autumn cold
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "TreatmentExtended")) #treatment centered around 0 because we dont have any expectsation htat treatment will do anyhting 
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "TreatmentHeatwave"))
# treatmemt effect within a season
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "SeasonAutumnwarm:TreatmentHeatwave"))
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "SeasonHeatwave:TreatmentHeatwave"))
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "SeasonSummer:TreatmentHeatwave"))

my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "SeasonAutumnwarm:TreatmentExtended"))
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "SeasonHeatwave:TreatmentExtended"))
my_prior_co2 <- c(my_prior_co2,set_prior("normal(0,1)",class = "b",coef = "SeasonSummer:TreatmentExtended"))




model_co2 <- brm(CO2flux | trunc(lb = 0)~ 0+Season*Treatment + (1|PotID), # model formula * = interqcti, no negative values
                 data = thesis_data, # data
                 family = gaussian(), # the data looks normally distributed
                 iter = 6000, # number of computation, the more the better
                 warmup = 2000, # number of discarded computation 
                 cores = 3, # this is to speed up the co;putation
                 prior = my_prior_co2,
                 chains = 3,# this is the nu;ber of chain, independant model
                 init = 0) # makes the computation more stable 

plot(model_co2) # assess model convergence, eith fuzzy caterpillar
pp_check(model_co2)
summary(model_co2)

#### Bayesian GCC ###

library(brms)

## defining my prior knowlkedge
# intercept, baseline GCC of the control plot of a given season
my_prior_gcc <- set_prior("normal(0.33,0.04)",class = "b",coef = "SeasonAutumncold") ### first number 1.5 (first) is mean, 1.5 (second) is stdev
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0.33,0.04)",class = "b",coef = "SeasonAutumnwarm"))
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0.32,0.04)",class = "b",coef = "SeasonHeatwave"))
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0.315,0.04)",class = "b",coef = "SeasonSummer"))
# treatment effect of autumn cold
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "TreatmentExtended")) #treatment centered around 0 because we dont have any expectsation htat treatment will do anyhting 
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "TreatmentHeatwave"))
# treatmemt effect within a season (except autumn cold, autumn cold was used as a control!)
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "SeasonAutumnwarm:TreatmentHeatwave"))
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "SeasonHeatwave:TreatmentHeatwave"))
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "SeasonSummer:TreatmentHeatwave"))

my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "SeasonAutumnwarm:TreatmentExtended"))
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "SeasonHeatwave:TreatmentExtended"))
my_prior_gcc <- c(my_prior_co2,set_prior("normal(0,0.04)",class = "b",coef = "SeasonSummer:TreatmentExtended"))

#### should i add residuals or random effects priors??


model_gcc <- brm(meanGCC | trunc(lb = 0)~ 0+Season*Treatment + (1|PotID), # model formula * = interqcti, no negative values--trunc(lb = 0)
                 data = thesis_data, # data
                 family = gaussian(), # the data looks normally distributed
                 iter = 6000, # number of computation, the more the better
                 warmup = 2000, # number of discarded computation 
                 cores = 3, # this is to speed up the computation
                 prior = my_prior_co2,
                 chains = 3,# this is the number of chain, independent model
                 init = 0) # makes the computation more stable 

plot(model_gcc) # assess model convergence, eith fuzzy caterpillar
pp_check(model_gcc)
pp_check(model_gcc, type = "dens_overlay_grouped", group = "Season")

summary(model_gcc)

### bayesian Cover ###

library(brms)

## defining my prior knowlkedge
# intercept, baseline cover of the control plot of a given season
my_prior_cover <- set_prior("normal(0.6,0.3)",class = "b",coef = "SeasonAutumncold")
my_prior_cover <- c(my_prior_co2,set_prior("normal(0.6,0.3)",class = "b",coef = "SeasonAutumnwarm"))
my_prior_cover <- c(my_prior_co2,set_prior("normal(0.4,0.3)",class = "b",coef = "SeasonHeatwave"))
my_prior_cover <- c(my_prior_co2,set_prior("normal(0.2,0.3)",class = "b",coef = "SeasonSummer"))
# treatment effect of autumn cold
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "TreatmentExtended")) #treatment centered around 0 because we dont have any expectsation htat treatment will do anyhting 
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "TreatmentHeatwave"))
# treatmemt effect within a season
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "SeasonAutumnwarm:TreatmentHeatwave"))
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "SeasonHeatwave:TreatmentHeatwave"))
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "SeasonSummer:TreatmentHeatwave"))

my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "SeasonAutumnwarm:TreatmentExtended"))
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "SeasonHeatwave:TreatmentExtended"))
my_prior_cover <- c(my_prior_co2,set_prior("normal(0,0.4)",class = "b",coef = "SeasonSummer:TreatmentExtended"))




model_cover <- brm(Cover | trunc(lb = 0)~ 0+Season*Treatment + (1|PotID), # model formula * = interqcti, no negative values
                 data = thesis_data, # data
                 family = gaussian(), # the data looks normally distributed
                 iter = 6000, # number of computation, the more the better
                 warmup = 2000, # number of discarded computation 
                 cores = 3, # this is to speed up the co;putation
                 prior = my_prior_co2,
                 chains = 3,# this is the nu;ber of chain, independant model
                 init = 0) # makes the computation more stable 

plot(model_cover) # assess model convergence, eith fuzzy caterpillar
pp_check(model_cover)
summary(model_cover)

#### Bayesian canopy height ####

library(brms)

## defining my prior knowlkedge
# intercept, baseline canopy height of the control plot of a given season
my_prior_height <- set_prior("normal(10,40)",class = "b",coef = "SeasonAutumncold")
my_prior_height <- c(my_prior_co2,set_prior("normal(10,40)",class = "b",coef = "SeasonAutumnwarm"))
my_prior_height <- c(my_prior_co2,set_prior("normal(10,40)",class = "b",coef = "SeasonHeatwave"))
my_prior_height <- c(my_prior_co2,set_prior("normal(10,40)",class = "b",coef = "SeasonSummer"))
# treatment effect of autumn cold
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "TreatmentExtended")) #treatment centered around 0 because we dont have any expectsation htat treatment will do anyhting 
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "TreatmentHeatwave"))
# treatmemt effect within a season
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "SeasonAutumnwarm:TreatmentHeatwave"))
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "SeasonHeatwave:TreatmentHeatwave"))
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "SeasonSummer:TreatmentHeatwave"))

my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "SeasonAutumnwarm:TreatmentExtended"))
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "SeasonHeatwave:TreatmentExtended"))
my_prior_height <- c(my_prior_co2,set_prior("normal(0,40)",class = "b",coef = "SeasonSummer:TreatmentExtended"))




model_height <- brm(canopyheight | trunc(lb = 0)~ 0+Season*Treatment + (1|PotID), # model formula * = interqcti, no negative values
                 data = thesis_data, # data
                 family = gaussian(), # the data looks normally distributed
                 iter = 6000, # number of computation, the more the better
                 warmup = 2000, # number of discarded computation 
                 cores = 3, # this is to speed up the co;putation
                 prior = my_prior_co2,
                 chains = 3,# this is the nu;ber of chain, independant model
                 init = 0) # makes the computation more stable 

plot(model_height) # assess model convergence, eith fuzzy caterpillar
pp_check(model_height)
summary(model_height)
