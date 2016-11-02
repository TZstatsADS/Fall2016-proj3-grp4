#########################################################
### Train two classification models with training images ###
#########################################################

### Author: Weichuan Wu credit to Yuting Ma
### Project 3 Group 4
### ADS Fall 2016


train <- function(data_train_baseline,data_train_adv,label_train,par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using PCA processed sift features from training images
  ### Also train a random forest model using PCA processed rgb data
  
  ### Input: 
  ###  -  PCA processed sift features from images and PCA processed rgb data 
  ###  -  class labels for training images
  ### Output: Two training models: baseline and adv
  
  
  ####################################################################################################
  #1.train baseline model
  
  ### load libraries
  library("gbm")
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  fit_gbm <- gbm.fit(x=data_train_baseline, y=label_train,
                     n.trees=2000,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=TRUE)
  best_iter <- gbm.perf(fit_gbm, method="OOB")
  ####################################################################################################
  
  ####################################################################################################
  #2.train adv model
  
  ### load libraries
  library(randomForest)
  
  ### Train with random forest model  
  dataset_train=data.frame(data_train_adv,train.label=as.factor(label_train))
  fit_rf <- randomForest(train.label~.,data=dataset_train,ntree=1500,type="classification",importance=TRUE)
  ####################################################################################################
  
  
  return(list(gbm_fit=fit_gbm, gbm_iter=best_iter,rf_fit=fit_rf))
}

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
#train_model<-train(feature_eval,label)
#save(train_model,file='train_model.RData')
