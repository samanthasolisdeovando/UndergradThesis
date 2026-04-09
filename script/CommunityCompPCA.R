#Written by Samantha Solis de Ovando
# inspired by tutorial: https://rpubs.com/brouwern/veganpca
library(readr)
library(dplyr)
library("ggpubr")
master_data <- read.csv(file.choose("MasterDataSheet_UGthesis_Feb232026"))
head(master_data)

########### only 1 timepoint dataset ##########
library(dplyr)
only_june_11 <- master_data %>% filter(measurement.week == "5")

write.csv(only_june_11, file = "june11_2_data_rowIDadded.csv")

#### add row ID ####
only_june_11 <- only_june_11 %>% mutate(row_id = row_number())

########## community matrix using ONLY funct grp ##########
community_matrix <- only_june_11 %>%
  select(shrub.cover, graminoid.cover, bryophyte.cover, forb.cover)

########### run PCA ##########
library(vegan)
pca_model <- rda(community_matrix)

########## extract site scores ##########
pca_scores <- as.data.frame(scores(pca_model, display = "sites"))

pca_scores$row_id <- only_june_11$row_id

######### join PCA scores back to datasheet ##########
June_11_PCA <- only_june_11 %>%
  left_join(pca_scores, by = "row_id")

write.csv(June_11_PCA, file = "June_11_PCA_.csv")

######### PCA score scatter plot ##########
library(ggplot2)

ggplot(June_11_PCA, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_classic() +
  labs(x = "PC1",
  y = "PC2",
  color = "Treatment")

###### PCA funct group biplot ########

species_scores <- as.data.frame(scores(pca_model, display = "species"))
species_scores$FunctionalGroup <- rownames(species_scores)

ggplot(June_11_PCA, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(aes(group = Treatment), linewidth = 1) +
  geom_segment(data = species_scores,aes(x = 0, y = 0, xend = PC1, yend = PC2),arrow = arrow(length = unit(0.02, "mm")), color = "black") +
  geom_text(data = species_scores,aes(x = PC1, y = PC2, label = FunctionalGroup), color = "black") +
  theme_classic()+
  scale_color_manual(values = c("Control" = "#1F77B4",
                                "Heatwave" = "#FF7F0E",
                                "Extended" = "#2CA02C"))
