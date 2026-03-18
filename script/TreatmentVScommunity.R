###########load data##########
library(readr)
library(dplyr)
library("ggpubr")
master_data <- read.csv(file.choose("MasterDataSheet_UGthesis_Feb232026"))
head(master_data)

########### only 1 timepoint dataset ##########
library(dplyr)
only_june_11 <- master_data %>%
  filter(measurement.week == "5")

#write.csv(only_june_11, file = "june11_data_rowIDadded.csv")

### add row ID ###
only_june_11 <- only_june_11 %>%
  mutate(row_id = row_number())

########## create community matrix for ONLY funct grp ##########
community_matrix <- only_june_11 %>%
  select(shrub.cover, graminoid.cover, bryophyte.cover, forb.cover)

######### Hellinger transformation ##########
library(vegan)

community_hellinger <- decostand(community_matrix, method = "hellinger")

########### run PCA ##########
pca_model <- rda(community_hellinger)

########## extract site scores ##########
pca_scores <- as.data.frame(scores(pca_model, display = "sites"))

pca_scores$row_id <- only_june_11$row_id

######### join PCA scores back to datasheet ##########
June_11_PCA <- only_june_11 %>%
  left_join(pca_scores, by = "row_id")

#write.csv(June_11_PCA, file = "June_11_PCA.csv")

######### PCA score scatter plot ##########
library(ggplot2)

ggplot(June_11_PCA, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_classic() +
  labs(
    x = "PC1",
    y = "PC2",
    color = "Treatment"
  )

###### PCA funct group biplot ########

species_scores <- as.data.frame(scores(pca_model, display = "species"))
species_scores$FunctionalGroup <- rownames(species_scores)

ggplot(June_11_PCA, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(aes(group = Treatment), linewidth = 1) +
  geom_segment(data = species_scores,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  geom_text(data = species_scores,
            aes(x = PC1, y = PC2, label = FunctionalGroup),
            color = "black",
            vjust = -0.5) +
  theme_classic()

#############################################################
################ variables by community type ################
#############################################################

#scatter plot
plot(master_data_filtered_with_pca$PC1, master_data_filtered_with_pca$meanGCC,
     main = "Greenness by Community Type",
     xlab = "PC1",
     ylab = "GCC",
     pch = 14)

#add regression line
model <- lm(meanGCC ~ PC1, data = master_data_filtered_with_pca)

plot(master_data_filtered_with_pca$PC1, master_data_filtered_with_pca$meanGCC,
     pch = 16)

abline(model, col = "red", lwd = 2)

###### cleaner plots using ggplot ###############
################## USE THIS ######################
library(ggplot2)

ggplot(master_data_filtered_with_pca, aes(x = PC1, y = meanGCC, color = Treatment)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(color = "Treatment")

ggplot(data, aes(x = x_var, y = y_var, color = Treatment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

####### regression plot ######
ggplot(master_data_filtered_with_pca, aes(x = PC1, y = CanopyExtent, color = Treatment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal()

#### regression lines by treatment #####
ggplot(master_data_filtered_with_pca, aes(x = PC1, y = canopyheight, color = Treatment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

#### exlude NAs (only for variables w NA) ####
ggplot(master_data_filtered_with_pca, aes(x = PC1, y = MEANpH, color = Treatment, group = Treatment)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE) +
  theme_minimal()

##### evapotranspiration filter out above 5 #####
data_clean_evapotranspiration_wPCA <- master_data_filtered_with_pca[master_data_filtered_with_pca$EvapotranspirationRate <= 5, ]

#### evapotranspiration:PCA regression####
ggplot(data_clean_evapotranspiration_wPCA, aes(x = PC1, y = EvapotranspirationRate, color = Treatment, group = Treatment)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE) +
  theme_minimal()

library("ggpubr")
ggboxplot(master_data, x = "Treatment", y = "MEANpH", 
          color = "Treatment", palette = c("#1F77B4", "#FF7F0E", "#2CA02C"),
          ylab = "pH", xlab = "Treatment",
          title = "pH")
