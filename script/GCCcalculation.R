#install.packages(c("jpeg", "raster", "terra"));
library(jpeg); library(raster); library(terra);

#read JPEG Image
img <- readJPEG("18_Top_08202025.JPG");

#Convert to a Raster Layer
img_r <- raster(img[,,1]); # Red channel
img_g <- raster(img[,,2]); # Green channel
img_b <- raster(img[,,3]); # Blue channel

#calculate GCC
#GCC = G / (R + G + B) for each pixel
total_rgb <- img_r + img_g + img_b;
gcc_raster <- img_g / total_rgb;

#mean GCC for whole image
mean_gcc <- cellStats(gcc_raster, 'mean');
print(paste("Mean GCC:", mean_gcc));

#visualize GCC map
plot(gcc_raster, main="GCC Map");
