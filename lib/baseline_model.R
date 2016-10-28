#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Weichuan Wu Credit to Yuting Ma
### Project 3 Group 4
### ADS Fall 2016

#clear environment
rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#create label, 0 represent for chicken and 1 for dog
label<-rep(1,2000)
label[1:1000]<-0

#The train process
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
    depth <- 3
  } else {
    depth <- par$depth
  }
  fit_gbm <- gbm.fit(x=dat_train, y=label_train,
                     n.trees=2000,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=TRUE)
  best_iter <- gbm.perf(fit_gbm, method="OOB")
  return(list(fit=fit_gbm, iter=best_iter))
}

#The tset process
test <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  
  pred <- predict(fit_train$fit, newdata=dat_test, 
                  n.trees=fit_train$iter, type="response")
  
  return(as.numeric(pred> 0.5))
}

#the cross validation process for pca processed sift data
#the error rate is 34.55%
k_folds<-10
k<-20
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
d<-1
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('train_',as.character(i),'.RData',sep=''))
  load(paste('test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  par <- list(depth=d)
  fit <- train(train.data, train.label, par)
  pred <- test(fit, test.data)  
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))


#The cross validation process for pixel data
#the error rate is 22.35%
k_folds<-10
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
d<-3
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('pixel_train_',as.character(i),'.RData',sep=''))
  load(paste('pixel_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  par <- list(depth=d)
  fit <- train(train.data, train.label, par)
  pred <- test(fit, test.data)  
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))
