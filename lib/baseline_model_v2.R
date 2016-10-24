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
################################################################################

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

#####################################################################################
