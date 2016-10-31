#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016

feature <- function(sift_feature_dir,sift_csv_name,img_dir, img_name, data_name=NULL){
  
  ### Construct process features for training/testing images
  ### Sample simple feature: Extract raw pixel values os features
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  
  #read sift feature csv
  sift_feature<-read.csv(paste0(sift_feature_dir,sift_csv_name,sep=''))
  sift_feature<-t(sift_feature)
  
  #Use the pca to process the sift feature
  Sigma=cov(sift_feature)
  Sigma_eigen=eigen(Sigma)
  Gamma=Sigma_eigen$vectors
  P=t(Gamma)%*%t(sift_feature)
  
  n_files <- length(list.files(img_dir))
  
  ### determine img dimensions
  img0 <-  readImage(paste0(img_dir, img_name, "_", 1, ".jpg"))
  mat1 <- as.matrix(img0)
  n_r <- nrow(img0)
  n_c <- ncol(img0)
  
  ### store vectorized pixel values of images
  dat <- array(dim=c(n_files, n_r*n_c)) 
  for(i in 1:n_files){
    img <- readImage(paste0(img_dir, img_name, "_", i, ".jpg"))
    dat[i,] <- as.vector(img)
  }
  
  ### output constructed features
  if(!is.null(data_name)){
    save(dat, file=paste0("./output/feature_", data_name, ".RData"))
  }
  return(dat)
}
