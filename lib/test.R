######################################################
### Fit the classification model with testing data ###
######################################################

### Author: Weichuan Wu credit to Yuting Ma
### Project 3 Group 4
### ADS Fall 2016

test <- function(fit_train, dat_test_baseline,dat_test_adv){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ####################################################################################################
  #1.fit the baseline model
  
  #process the data
  pca_dat_test_baseline<-t(fit_train$baseline_gamma)%*%t(dat_test_baseline)
  #choose the first k feature as output,we can draw the picture of the explained variance of the first k parameters and choose a good one
  k<-20
  PCA_test_sift<-t(pca_dat_test_baseline[1:k,])
    
  ### load libraries
  library("gbm")
  
  pred_gbm <- predict(fit_train$baseline_fit, newdata=PCA_test_sift, 
                      n.trees=fit_train$baseline_iter, type="response")
  
  ####################################################################################################
  
  ####################################################################################################
  #2.fit the advanced model
  
  #process the data
  pca_dat_test_adv<-t(fit_train$adv_gamma)%*%t(dat_test_adv)
  #choose the first k feature as output,we can draw the picture of the explained variance of the first k parameters and choose a good one
  k<-100
  PCA_test_rgb<-t(pca_dat_test_adv[1:k,])
  
  ### load libraries
  library(e1071)
  
  pred_rf<-predict(fit_train$adv_fit,newdata=data.frame(PCA_test_rgb))
  
  ####################################################################################################
  
  return(list(baseline=as.numeric(pred_gbm> 0.5),adv=as.numeric(paste(pred_rf))))
}
###test
#clear environment
#rm(list = ls())

#set directory
#setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#load data
#load('train_model.RData')
#load('feature_eval.RData')
#load('label_eval.RData')
#sift_feature<-read.csv('sift_features.csv')
#using transpose so that sift_feature has nrow=2000
#sift_feature<-t(sift_feature)

#test will take 40s
#test_result<-test(train_model, sift_feature,feature_eval)
#cv.error_baseline <- mean(test_result$baseline != label_eval) 
#cv.error_adv <- mean(test_result$adv != label_eval) 
