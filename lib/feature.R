#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Author: Weichuan Wu credit to Yuting Ma
### Project 3 Group 4
### ADS Fall 2016

feature <- function(sift_dir,sift_csv_name,img_dir,output_dir){
  
  ### Construct process features for training/testing images
  ### baseline feature: pca processed sift feature 
  ### advanced feature: grb frequency feature
  
  ### Input: a csv file contain sift feature and a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ####################################################################################################
  #1.process sift feature
  
  #read sift feature csv
  setwd(sift_dir)
  sift_feature<-read.csv(sift_csv_name)
  sift_feature<-t(sift_feature)
  
  #Use the pca to process the sift feature
  Sigma_sift=cov(sift_feature)
  Sigma_eigen_sift=eigen(Sigma_sift)
  Gamma_sift=Sigma_eigen_sift$vectors
  P_sift=t(Gamma_sift)%*%t(sift_feature)
  
  #choose the first k feature as output,we can draw the picture of the explained variance of the first k parameters and choose a good one
  k<-20
  PCA_sift<-t(P_sift[1:k,])
  print('SIFT feature has been processed!')
  ####################################################################################################
  
  ####################################################################################################
  #2.processed advanced feature
  
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
  
  rgb_feature <- array(0, dim = c(img_num, nR*nG*nB))
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
    rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
    print(paste(as.character(i/img_num*100),'%',' completed',sep=''))
  }
  
  #Using PCA to process rgb_feature to speed up train
  Sigma_rgb=cov(rgb_feature)
  Sigma_eigen_rgb=eigen(Sigma_rgb)
  Gamma_rgb=Sigma_eigen_rgb$vectors
  P_rgb=t(Gamma_rgb)%*%t(rgb_feature)
  
  #choose the first k feature as output,we can draw the picture of the explained variance of the first k parameters and choose a good one
  k<-100
  PCA_rgb<-t(P_rgb[1:k,])
  print('Advanced feature has been processed!')
  ####################################################################################################
  
  ### output constructed features
  setwd(output_dir)
  feature_eval=list(baseline=PCA_sift,adv=PCA_rgb)
  save(feature_eval, file='feature_eval.RData')
  return(feature_eval)
}


#clear environment
#rm(list = ls())
#feature('C:/Study/Columbia/W4243_Applied_Data_Science/Project3',
#        'sift_features.csv',
#        'C:/Study/Columbia/W4243_Applied_Data_Science/Project3/images',
#        'C:/Study/Columbia/W4243_Applied_Data_Science/Project3')


