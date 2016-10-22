#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016

rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

feature_train<-read.csv("sift_features.csv")
label_train<-rep(1,2000)
label_train[1:1000]<-0
label_train<-as.factor(label_train)
feature<-t(feature_train)
train <- function(dat_train, label_train, par=NULL){
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 1
  } else {
    depth <- par$depth
  }
  fit_gbm <- gbm.fit(x=dat_train, y=label_train,
                     n.trees=200,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=TRUE)
  best_iter <- gbm.perf(fit_gbm, method="OOB")

  return(list(fit=fit_gbm, iter=best_iter))
}
fit_baseline<-train(feature,label_train)

library("gbm")

### Train with gradient boosting model
if(is.null(par)){
  depth <- 1
} else {
  depth <- par$depth
}

fit_gbm <- gbm.fit(x=feature, y=label_train,
                   n.trees=2000,
                   distribution="bernoulli",
                   interaction.depth=depth, 
                   bag.fraction = 0.5,
                   verbose=TRUE)

best_iter <- gbm.perf(fit_gbm, method="OOB")

pred <- predict(fit_gbm, newdata=feature[1:5,], 
                n.trees=best_iter, type="response")