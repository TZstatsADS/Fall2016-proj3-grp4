#Using the original feature but other classifiers

#clear environment
rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#read data
load('data_PCA_processed.RData')
k=20
sift_feature=t(P_train[1:k,])

#create label, 0 represent for chicken and 1 for dog
label<-rep(1,2000)
label[1:1000]<-0

################################################################################SVM classifier
#SVM have an accurate around 68%.
accurate_rate<-matrix(data=NA,ncol=10)
for(i in 1:10)
{
  sample_rank<-sample(1:nrow(sift_feature),0.8*nrow(sift_feature))
  x_train<-sift_feature[sample_rank,]
  x_test<-sift_feature[-sample_rank,]
  y_train<-label[sample_rank]
  y_test<-label[-sample_rank]
  train_data=data.frame(x_train,y_train=as.factor(y_train))
  train_data$y_train

  library(e1071)
  tune.out=tune(svm ,y_train~.,data=train_data ,kernel ='linear',
              ranges =list(cost=c(0.01, 0.1,1,10)))
  bestmod =tune.out$best.model
  ypred=predict (bestmod ,x_test )
  y_test=as.factor(y_test)
  result=table(pred=ypred,true=y_test)
  accurate_rate[i]<-(result[1,1]+result[2,2])/sum(result)
  print(paste(as.character(i/10*100),'%',' completed',sep=''))
}
mean(accurate_rate)
################################################################################random forest
#####error rate is around 30%
data<-data.frame(cbind(sift_feature,label))
data$label<-as.factor(data$label)
#fit_rf <- randomForest(label~.,data=data,type="classification",importance=TRUE)
#####train
train<-function(dataset_train){
  library(randomForest)
  fit_rf <- randomForest(label~.,data=dataset_train,type="classification",importance=TRUE)
  return(fit_rf)
}

#####test
test<-function(fit_rf,dataset_test){
  pred<-predict(fit_rf,newdata=dataset_test)
  return(pred)
}

#####cv
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

result<-cv.function(data,10)
result
################################################################################logistic classifier
#the accurate rate is around 67%
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
####error rate of LDA is around 33%
data<-data.frame(cbind(sift_feature,label))
data$label<-as.factor(data$label)
train<-function(dataset_train){
  library(MASS)
  fit_lda <- lda(label~.,data=dataset_train)
  return(fit_lda)
}

#####test
test<-function(fit_lda,dataset_test){
  pred<-predict(fit_lda,newdata=dataset_test)
  return(pred$class)
}

#####cv
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

result<-cv.function(data,10)
result

#####################################################################################QDA
####error rate of QDA is around 37.5%
data<-data.frame(cbind(sift_feature,label))
data$label<-as.factor(data$label)
train<-function(dataset_train){
  library(MASS)
  fit_qda <- qda(label~.,data=dataset_train)
  return(fit_qda)
}

#####test
test<-function(fit_qda,dataset_test){
  pred<-predict(fit_qda,newdata=dataset_test)
  return(pred$class)
}

#####cv
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

result<-cv.function(data,10)
result


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

