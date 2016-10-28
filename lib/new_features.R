library(EBImage)
library(gbm)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADS\\project_3\\data\\images')

list_images <- dir()

##################################
###Load data into sizable format
##################################

#set size to wanted number of pixels per image
size = 28
image_array <- array(0, dim = c(size, size, 3, length(list_images)))

for (i in 1:length(list_images)){
  temp_img <- readImage(list_images[i])
  image_array[ , , , i] <- resize(temp_img, size, size)
  print(i)
}
### Used to save data file
#setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADS\\project_3\\data')
#saveRDS(image_array, file = 'image_array.rds')
#image_array <- readRDS('image_array.rds')

##################################
########## feature processing
##################################

# image_manipulation <- function(img){
#   #manipulate images
#   
# }
# 
# feature_extraction <- function(img){
#   #extract features
#   
# }



image_matrix <- matrix(0, nrow = dim(image_array)[4], ncol = size*size*3)

for (i in 1:dim(image_array)[4]){
  img <- image_array[ , , , i]
  #img <- image_manipulation(img)
  #img <- feature_extraction(img)
  image_matrix[i, ] <- as.vector(img)
  print(i)
}

##############color feature
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
library("EBImage")
list_images <- dir()
rgb_feature <- array(0, dim = c(2000, 800))
i=1
for (i in 1:2000){
  img <- readImage(list_images[i])
  mat <- imageData(img)
  nR <- 10
  nG <- 8
  nB <- 10
  # Caution: the bins should be consistent across all images!
  rBin <- seq(0, 1, length.out=nR)
  gBin <- seq(0, 1, length.out=nG)
  bBin <- seq(0, 1, length.out=nB)
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), 
                                  factor(findInterval(mat[,,2], gBin), levels=1:nG), 
                                  factor(findInterval(mat[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
  i=i+1
  print(i)
}

save(rgb_feature,file="rgb_feature.RData")



