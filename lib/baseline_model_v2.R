#Using the original feature but other classifiers

#clear environment
rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3/Fall2016-proj3-grp4/data")

#create label, 0 represent for chicken and 1 for dog
label<-rep(1,2000)
label[1:1000]<-0

################################################################################SVM classifier
#1.Using PCA processed SIFT features as feature, error rate around 33.1%.
#####train
train<-function(dataset_train){
  library(e1071)
  tune.out=tune(svm ,train.label~.,data=dataset_train ,kernel ='radial',
                ranges =list(cost=c(0.01, 0.1,1)))
  bestmod =tune.out$best.model
  return(bestmod)
}

#####test
test<-function(bestmod,dataset_test){
  pred<-predict(bestmod,newdata=dataset_test)
  return(pred)
}

#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-20
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('train_',as.character(i),'.RData',sep=''))
  load(paste('test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  bestmod =train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred=test(bestmod ,test.data )
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#2.Using 28*28*3 pixel as feature, error rate around 19.5%
#folds for cross validation,can be any interger between 1 and 10
k_folds<-5
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('pixel_train_',as.character(i),'.RData',sep=''))
  load(paste('pixel_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  bestmod =train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred=test(bestmod ,test.data )
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))


#3.Using rgb pixel as feature, error rate around 14.4%(using linear)
   #using radial error rate reduce to 12.2%
#folds for cross validation,can be any interger between 1 and 10
k_folds<-5
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('rgb_train_',as.character(i),'.RData',sep=''))
  load(paste('rgb_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  bestmod =train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred=test(bestmod ,test.data )
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

################################################################################random forest
#1.Using PCA processed SIFT features as feature, error rate around 31%.

#####train
train<-function(dataset_train){
  library(randomForest)
  fit_rf <- randomForest(train.label~.,data=dataset_train,type="classification",importance=TRUE)
  return(fit_rf)
}

#####test
test<-function(fit_rf,dataset_test){
  pred<-predict(fit_rf,newdata=dataset_test)
  return(pred)
}

#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-20
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('train_',as.character(i),'.RData',sep=''))
  load(paste('test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,test.data)  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))


#2.Using 28*28*3 pixel as feature, error rate around 18%

#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('pixel_train_',as.character(i),'.RData',sep=''))
  load(paste('pixel_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,test.data)  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#3.Using rgb pixel as feature, error rate around 14%

#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('rgb_train_',as.character(i),'.RData',sep=''))
  load(paste('rgb_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,test.data)  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))


#####################################################################################LDA
#1.Using PCA processed SIFT features as feature, error rate around 33.1%.

train<-function(dataset_train){
  library(MASS)
  fit_lda <- lda(train.label~.,data=dataset_train)
  return(fit_lda)
}

#####test
test<-function(fit_lda,dataset_test){
  pred<-predict(fit_lda,newdata=dataset_test)
  return(pred$class)
}

#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-20
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('train_',as.character(i),'.RData',sep=''))
  load(paste('test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,as.data.frame(test.data))  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#2.Using 28*28*3 pixel as feature, error rate around 21.95%
#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('pixel_train_',as.character(i),'.RData',sep=''))
  load(paste('pixel_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,as.data.frame(test.data))  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#3.Using rgb pixel as feature, error rate around 15.1%
#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('rgb_train_',as.character(i),'.RData',sep=''))
  load(paste('rgb_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,as.data.frame(test.data))  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#####################################################################################QDA
#1.Using PCA processed SIFT features as feature, error rate around 39%.
train<-function(dataset_train){
  library(MASS)
  fit_qda <- qda(train.label~.,data=dataset_train)
  return(fit_qda)
}

#####test
test<-function(fit_qda,dataset_test){
  pred<-predict(fit_qda,newdata=dataset_test)
  return(pred$class)
}

#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-20
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('train_',as.character(i),'.RData',sep=''))
  load(paste('test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,as.data.frame(test.data))  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#2.Using 28*28*3 pixel as feature, error rate around 19.15%
#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('pixel_train_',as.character(i),'.RData',sep=''))
  load(paste('pixel_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,as.data.frame(test.data))  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#3.Using rgb pixel as feature, error rate around 23.9%
#folds for cross validation,can be any interger between 1 and 10
k_folds<-10
#the pca features number
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('rgb_train_',as.character(i),'.RData',sep=''))
  load(paste('rgb_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  
  train.data=data.frame(train.data,train.label=as.factor(train.label))
  fit <- train(train.data)
  colnames(test.data)<-colnames(train.data)[1:k]
  pred <- test(fit,as.data.frame(test.data))  
  test.label=as.factor(test.label)
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

