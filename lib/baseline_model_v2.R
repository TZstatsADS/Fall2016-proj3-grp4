#Using the original feature but other classifiers

#clear environment
rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#create label, 0 represent for chicken and 1 for dog
label<-rep(1,2000)
label[1:1000]<-0

################################################################################SVM classifier
#1.Using PCA processed SIFT features as feature, error rate around 33.1%.
#####train
train<-function(dataset_train){
  library(e1071)
  tune.out=tune(svm ,train.label~.,data=dataset_train ,kernel ='linear',
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

#2.Using 28*28*3 pixel as feature, error rate around ?
iteration_num<-5
accurate_rate<-matrix(data=NA,ncol=iteration_num)
for(i in 1:iteration_num)
{
  #sample_rank<-sample(1:nrow(sift_feature),0.8*nrow(sift_feature))
  #x_train<-sift_feature[sample_rank,]
  #x_test<-sift_feature[-sample_rank,]
  sample_rank<-sample(1:nrow(pixel_feature),0.8*nrow(pixel_feature))
  x_train<-pixel_feature[sample_rank,]
  x_test<-pixel_feature[-sample_rank,]
  y_train<-label[sample_rank]
  y_test<-label[-sample_rank]
  train_data=data.frame(x_train,y_train=as.factor(y_train))
  train_data$y_train

  library(e1071)
  tune.out=tune(svm ,y_train~.,data=train_data ,kernel ='linear',
              ranges =list(cost=c(0.01, 0.1)))
  bestmod =tune.out$best.model
  ypred=predict (bestmod ,x_test )
  y_test=as.factor(y_test)
  result=table(pred=ypred,true=y_test)
  accurate_rate[i]<-(result[1,1]+result[2,2])/sum(result)
  print(paste(as.character(i/iteration_num*100),'%',' completed',sep=''))
}
mean(accurate_rate)

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


#2.Using 28*28*3 pixel as feature, error rate around 25
#####cv
data<-data.frame(cbind(pixel_feature,label))
data$label<-as.factor(data$label)
cv.function <- function(X.train, K){
  n <- length(X.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  for (i in 1:K){
    train.data <- X.train[s != i,]
    test.data <- X.train[s == i,]
    fit <- train(train.data)
    pred <- test(fit,test.data[,1:(length(X.train)-1)])  
    cv.error[i] <- mean(pred != test.data[,length(X.train)])  
    print(paste(as.character(i/K*100),'%',' completed',sep=''))
  }			
  return(c(mean(cv.error),sd(cv.error)))
}
result<-cv.function(data,5)
result
################################################################################logistic classifier
#the accurate rate is around 67%
input_data<-data.frame(cbind(pixel_feature,label))
input_data<-data.frame(cbind(sift_feature,label))
input_data$label<-as.factor(input_data$label)
glm.fit=glm(label~. ,data=input_data ,family =binomial )

#only extract the colnmn with p value less than 0.5
temp_data<-input_data[,(summary(glm.fit)$coef[,4]<0.05)[2:20]]
input_data<-data.frame(cbind(temp_data,label))
input_data$label<-as.factor(input_data$label)

accurate_rate<-matrix(data=NA,ncol=10)
for(i in 1:10)
{
#set train data and test data
  sample_rank<-sample(1:nrow(input_data),0.8*nrow(input_data))
  input_train<-input_data[sample_rank,]
  input_test<-input_data[-sample_rank,]

#fit the model and calculate the accurate rate
  glm.fit=glm(label~. ,data=input_train ,family =binomial )
  glm.probs<-predict(glm.fit,input_test,type='response')
  glm.pred<-rep(1,nrow(input_test))
  glm.pred[glm.probs<0.5]<-0
  result=table(pred=as.factor(glm.pred),true=as.factor(input_test$label))
  accurate_rate[i]<-(result[1,1]+result[2,2])/sum(result)
  print(paste(as.character(i/10*100),'%',' completed',sep=''))
}
mean(accurate_rate)

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

#2.Using 28*28*3 pixel as feature, error rate around 38.6%
#read data
pixel_feature<-readRDS('image_matrix.rds')
data<-data.frame(cbind(pixel_feature,train.label=label))
data$train.label<-as.factor(data$train.label)

#####cv
cv.function <- function(X.train, K){
  n <- nrow(X.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  for (i in 1:K){
    train.data <- X.train[s != i,]
    test.data <- X.train[s == i,]
    fit <- train(train.data)
    pred <- test(fit,test.data[,1:(ncol(X.train)-1)])  
    cv.error[i] <- mean(pred != test.data[,ncol(X.train)])  
    print(paste(as.character(i/K*100),'%',' completed',sep=''))
  }			
  return(cv.error)
}

result<-cv.function(data,5)
mean(result)

n <- nrow(data)
K<-5
n.fold <- floor(n/K)
s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
train.data <- data[s != 1,]
test.data <- data[s == 1,]
fit <- train(train.data)
pred <- test(fit,test.data[,1:(ncol(data)-1)])  
cv.error[i] <- mean(pred != test.data[,ncol(data)])

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

#2.Using 28*28*3 pixel as feature, error rate around ?
#QDA can not apply to this because the ncol is too large


################################################################################random forest
#####error rate of SVM & Kernel is around 30%
#install.packages('randomForest')
#install.packages('e1071')
library(randomForest)
library(e1071)
sample_rank<-sample(1:nrow(sift_feature),0.8*nrow(sift_feature))
x_train<-sift_feature[sample_rank,]
x_test<-sift_feature[-sample_rank,]
y_train<-label[sample_rank]
y_test<-label[-sample_rank]
train_data=data.frame(x_train,y_train=as.factor(y_train))
cost_list <- c(0.01, 0.1, 1, 2.7, 10, 100, 150, 200, 250, 300, 350)
gamma_list <- c(0.0001, 0.0005, 0.0007, 0,001, 0.01, 0.09, 0.015, 0.02, 0.025, 0.03, 0.1, 1)
for(i in cost_list) {
  for(j in gamma_list){
    set.seed(10)
    model <- svm(x_train, y_train, kernel = 'radial',gamma = j, cost = i, cross = 10)
    accuracy <- summary(model)$tot.MSE
    observation <- c(i, j, accuracy)
    print(observation)
  }
}

# model1:RandomForest
model1 <- randomForest(x_train, y_train, ntree = 1000)
feature_importance <- as.data.frame(importance(model1))
feature_name <- paste(rep('x', 20), 1:20, sep = '')
feature_importance <- cbind(feature_importance, feature_name)
feature_importance <- feature_importance[order(-feature_importance$MeanDecreaseGini),]
select_feature <- feature_importance[feature_importance$MeanDecreaseGini > 1.8,]$feature_name
select_train.data.x <- x_train[, select_feature]
# model2:Improved RF
model2 <- randomForest(select_train.data.x, train.data.y, ntree = 1000)


