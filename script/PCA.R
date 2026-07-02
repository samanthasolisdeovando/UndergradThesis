################################################################################
################################################################################
############################ PCA using FactoMineR ##############################
################################################################################
################################################################################
#updated March 17 2026
#adapted from STHDA tutorial
#written by Samantha Solis de Ovando

install.packages("FactoMineR")
library("FactoMineR")

library("devtools")
install_github("kassambara/factoextra")

library("factoextra")
PCA_data <- read.csv(file.choose("shifted_dataset.csv"))
head(PCA_data)

PCA_allcols_June11 <- PCA_data[
  PCA_data$measurement.week == "5",
]

PCA_june_11_functgrp <- PCA_allcols_June11[, c("graminoid.cover", "shrub.cover", "forb.cover", "bryophyte.cover")]

###################################
####### descriptive stats #########
###################################
PCA_june_11_functgrp_stats <- data.frame(
  Min = apply(PCA_june_11_functgrp, 2, min),
  Q1 = apply(PCA_june_11_functgrp, 2, quantile, 1/4),
  Med = apply(PCA_june_11_functgrp, 2, median), 
  Q3 = apply(PCA_june_11_functgrp, 2, quantile, 3/4),
  Max = apply(PCA_june_11_functgrp, 2, max)
)
PCA_june_11_functgrp_stats <- round(PCA_june_11_functgrp_stats, 1)
head(PCA_june_11_functgrp_stats)

################################
###### correlation matrix ######
################################
correlation_matrix <- round(cor(PCA_june_11_functgrp), 2)
head(correlation_matrix)

#install.packages("corrplot")
library("corrplot")
corrplot(correlation_matrix, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

#correlation scatter plot
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(PCA_june_11_functgrp, histogram=TRUE, pch=19)

#################################################
####### Principal Component Analysis ############
#################################################

library("FactoMineR")
pca_scores_june11 <- PCA(PCA_june_11_functgrp, graph = FALSE)
print(pca_scores_june11)

eigenvalues <- pca_scores_june11$eig
head(eigenvalues[, 1:2])

#scree plot
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

library("factoextra")
fviz_screeplot(pca_scores_june11, ncp=10)

#####Variables factor map#######

#coordinates of variables on the principal components
head(pca_scores_june11$var$coord)

#contributions of the variables to the principal components
head(pca_scores_june11$var$contrib)

plot(pca_scores_june11, choix = "var")

######## graph of individual monoliths ########
head(pca_scores_june11$ind$coord)


