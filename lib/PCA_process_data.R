################################################################################Using PCA to process data
#clear environment
rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#read data
sift_feature<-read.csv("sift_features.csv")
sift_feature<-t(sift_feature)

#in order to save time, we use PCA to reduce the dimensions
#but we have to overcome overfitting so that we decide to first seperate the data
#and do the PCA process using the training part and using these data to test the classifier
#PCA process
k<-10
n <- nrow(sift_feature)
n.fold <- floor(n/k)
set.seed(1)
s <- sample(rep(1:k, c(rep(n.fold, k-1), n-(k-1)*n.fold)))  
for(i in 1:k)
{
  train.data <- sift_feature[s != i,]
  test.data <- sift_feature[s == i,]
  Sigma=cov(train.data)
  Sigma_eigen=eigen(Sigma)
  Gamma=Sigma_eigen$vectors
  P_train=t(Gamma)%*%t(train.data)
  P_test=t(Gamma)%*%t(test.data)
  save(P_train,file=paste('train_',as.character(i),'.RData',sep=''))
  save(P_test,file=paste('test_',as.character(i),'.RData',sep=''))
  print(paste(as.character(i/k*100),'%',' completed',sep=''))
}

#################################################################################################
#process 28*28*3 pixel data using pca
pixel_feature<-readRDS('image_matrix.rds')
k<-10
n <- nrow(pixel_feature)
n.fold <- floor(n/k)
set.seed(1)
s <- sample(rep(1:k, c(rep(n.fold, k-1), n-(k-1)*n.fold))) 
i<-1
for(i in 1:k)
{
  train.data <- pixel_feature[s != i,]
  test.data <- pixel_feature[s == i,]
  Sigma=cov(train.data)
  Sigma_eigen=eigen(Sigma)
  Gamma=Sigma_eigen$vectors
  P_train=t(Gamma)%*%t(train.data)
  P_test=t(Gamma)%*%t(test.data)
  save(P_train,file=paste('pixel_train_',as.character(i),'.RData',sep=''))
  save(P_test,file=paste('pixel_test_',as.character(i),'.RData',sep=''))
  print(paste(as.character(i/k*100),'%',' completed',sep=''))
}

#to select the best num of pca dimension
#it tunrs out 100 is a good point
x<-c(1:400)
y<-matrix(data=NA,ncol=400)
for(i in x)
{
  y[i]<-sum(Sigma_eigen$values[1:x[i]])/sum(Sigma_eigen$values)
}
plot(x,y)
sum(Sigma_eigen$values[1:100])/sum(Sigma_eigen$values)
