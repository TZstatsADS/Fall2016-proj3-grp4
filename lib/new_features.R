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




