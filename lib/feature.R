#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Author: Weichuan Wu credit to Yuting Ma
### Project 3 Group 4
### ADS Fall 2016

feature <- function(img_dir,output_dir){
  
  ### Construct process features for training/testing images
  ### advanced feature: grb frequency feature
  
  ### Input:  a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  
  ####################################################################################################
  #1.processed advanced feature
  
  ### load libraries
  library("EBImage")
  
  #set the img dir
  setwd(img_dir)
  
  #read the number of images
  img_num <- length(list.files(img_dir))
  
  #get the img list
  list_images <- dir()
  
  #set RGB color 
  nR <- 10
  nG <- 8
  nB <- 10
  
  feature_eval <- array(0, dim = c(img_num, nR*nG*nB))
  #process images
  for (i in 1:img_num){
    img <- readImage(list_images[i])
    mat <- imageData(img)
    # Caution: the bins should be consistent across all images!
    rBin <- seq(0, 1, length.out=nR)
    gBin <- seq(0, 1, length.out=nG)
    bBin <- seq(0, 1, length.out=nB)
    freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), 
                                    factor(findInterval(mat[,,2], gBin), levels=1:nG), 
                                    factor(findInterval(mat[,,3], bBin), levels=1:nB)))
    feature_eval[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
    print(paste(as.character(i/img_num*100),'%',' completed',sep=''))
  }
  
  ####################################################################################################
  
  ### output constructed features
  setwd(output_dir)
  save(feature_eval, file='feature_eval.RData')
  return(feature_eval)
}

###test
#process 2000 images will cost about 19 minutes
#clear environment
#rm(list = ls())
#feature('C:/Study/Columbia/W4243_Applied_Data_Science/Project3/images',
#        'C:/Study/Columbia/W4243_Applied_Data_Science/Project3')


