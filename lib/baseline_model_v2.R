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

################################################################################Using PCA to process data
Sigma=cov(sift_feature)
Sigma_eigen=eigen(Sigma)
Gamma=Sigma_eigen$vectors
P_train=t(Gamma)%*%t(sift_feature)
k=20
return_x_train=t(P_train[1:k,])
save(P_train,file='data_PCA_processed.RData')

################################################################################SVM classifier
SVM<-function(xtrain,ytrain,xtest,ytest)
{
  x_mean = apply(as.matrix(x_train),2,mean)
  x_sd = apply(as.matrix(x_train),2,sd)
  x_train_norm = matrix(data=0,nrow=nrow(x_train),ncol=ncol(x_train))
  for (i in 1:nrow(x_train_norm))
  {
    x_train_norm[i,] = (x_train[i,]-x_mean)/x_sd
  }
  x_test_norm = matrix(data=0,nrow=nrow(x_test),ncol=ncol(x_test))
  for (i in 1:nrow(x_test))
  {
    x_test_norm[i,] = (x_test[i,]-x_mean)/x_sd
  }
  y_train_factor=as.factor(y_train)
  train_data=data.frame(x_train_norm,y_train_factor)
  tune.out=tune(svm ,y_train~.,data=train_data ,kernel ='linear',
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1)))
  bestmod =tune.out$best.model
  ypred=predict (bestmod ,x_test_norm )
  y_test_factor=as.factor(y_test)
  #result=list("pred"=ypred,"true"=y_test_factor)
  result=list("pred"=ypred,"true"=y_test)
  return(result)
}

set.seed(1)
sample_rank<-sample(1:nrow(sift_feature),0.8*nrow(sift_feature))
x_train<-sift_feature[sample_rank,]
x_test<-sift_feature[-sample_rank,]
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
#result=list("pred"=ypred,"true"=y_test_factor)
result=table(pred=ypred,true=y_test)

################################################################################logistic classifier
library (ISLR)
names(Smarket )
summary (Smarket )
class(Smarket[,9])
