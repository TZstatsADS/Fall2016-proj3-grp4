#########################################################
### Train two classification models with training images ###
#########################################################

### Author: Weichuan Wu credit to Yuting Ma
### Project 3 Group 4
### ADS Fall 2016


train <- function(data_train_baseline,data_train_adv,label_train,par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using PCA processed sift features from training images
  ### Also train a svm model with kernel 'radial' using PCA processed rgb data
  
  ### Input: 
  ###  -  sift features from images and PCA processed rgb features
  ###  -  class labels for training images
  ### Output: 
  ###   - Two training models: baseline and adv
  
  
  ####################################################################################################
  #1.train baseline model
  
  
  #Use the pca to process the sift feature
  Sigma_sift=cov(data_train_baseline)
  Sigma_eigen_sift=eigen(Sigma_sift)
  Gamma_sift=Sigma_eigen_sift$vectors
  P_sift=t(Gamma_sift)%*%t(data_train_baseline)
  
  #choose the first k feature as output,we can draw the picture of the explained variance of the first k parameters and choose a good one
  k<-20
  PCA_sift<-t(P_sift[1:k,])
  
  ### load libraries
  library("gbm")
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  fit_gbm <- gbm.fit(x=PCA_sift, y=label_train,
                     n.trees=2000,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=FALSE)
  best_iter <- gbm.perf(fit_gbm, method="OOB")
  #print('Baseline model has been trained!')
  ####################################################################################################
  
  ####################################################################################################
  #2.train adv model
  
  
  #Using PCA to process rgb_feature to speed up train
  Sigma_rgb=cov(data_train_adv)
  Sigma_eigen_rgb=eigen(Sigma_rgb)
  Gamma_rgb=Sigma_eigen_rgb$vectors
  P_rgb=t(Gamma_rgb)%*%t(data_train_adv)
  
  #choose the first k feature as output,we can draw the picture of the explained variance of the first k parameters and choose a good one
  k<-100
  PCA_rgb<-t(P_rgb[1:k,])
  
  ### load libraries
  library(e1071)
  
  ### Train with svm 
  dataset_train=data.frame(PCA_rgb,train.label=as.factor(label_train))
  tune.out=tune(svm ,train.label~.,data=dataset_train ,kernel ='radial',
                ranges =list(cost=c(0.01, 0.1,1)))
  bestmod =tune.out$best.model
  #print('Advanced model has been trained!')
  ####################################################################################################
  
  #return a list of result
  return(list(baseline_fit=fit_gbm, baseline_iter=best_iter,adv_fit=bestmod,baseline_gamma=Gamma_sift,adv_gamma=Gamma_rgb))
}


###test
#clear environment
#rm(list = ls())

#set directory
#setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#create label, 1 represent for chicken and 0 for dog
#label_eval<-rep(0,2000)
#label_eval[1:1000]<-1
#save(label_eval,file='label_eval.RData')

#load data
#load('feature_eval.RData')
#load('label_eval.RData')
#sift_feature<-read.csv('sift_features.csv')
#using transpose so that sift_feature has nrow=2000
#sift_feature<-t(sift_feature)

#train will take 4.5 minutes,the first 3 minutes are pca for sift feature.
#train_model<-train(sift_feature,feature_eval,label_eval)
#save(train_model,file='train_model.RData')
