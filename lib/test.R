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
  ### load libraries
  library("gbm")
  
  pred_gbm <- predict(fit_train$gbm_fit, newdata=dat_test_baseline, 
                      n.trees=fit_train$gbm_iter, type="response")
  
  ####################################################################################################
  
  ####################################################################################################
  #2.fit the advanced model
  ### load libraries
  library(randomForest)
  
  pred_rf<-predict(fit_train$rf_fit,newdata=data.frame(dat_test_adv))
  
  ####################################################################################################
  
  return(list(baseline=as.numeric(pred_gbm> 0.5),adv=as.numeric(paste(pred_rf))))
}

