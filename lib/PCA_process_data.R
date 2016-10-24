################################################################################Using PCA to process data
#clear environment
rm(list = ls())

#set directory
setwd("C:/Study/Columbia/W4243_Applied_Data_Science/Project3")

#read data
sift_feature<-read.csv("sift_features.csv")
sift_feature<-t(sift_feature)

#PCA process
Sigma=cov(sift_feature)
Sigma_eigen=eigen(Sigma)
Gamma=Sigma_eigen$vectors
P_train=t(Gamma)%*%t(sift_feature)

#save data
save(P_train,file='data_PCA_processed.RData')