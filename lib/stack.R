#########################################################
### Train a stacked model with training images ###
#########################################################

### Author: Yunyi Zhang Credit to Yuting Ma???Weichuan Wu
### Project 3 Group 4
### ADS Fall 2016

########construct dataset#####
load('data_PCA_processed.RData')
k=20
sift_feature=t(P_train[1:k,])
#create label, 0 represent for chicken and 1 for dog
label<-rep(1,2000)
label[1:1000]<-0
data<-data.frame(cbind(sift_feature,label))

#data$label2<-as.factor(data$label)
stack_median_test_error.vec<-rep(1,10)
stack_gbm_test_error.vec<-rep(1,10)
stack_svm_test_error.vec<-rep(1,10)
gbm_test_error.vec<-rep(1,10)
rf_test_error.vec<-rep(1,10)
lda_test_error.vec<-rep(1,10)
svm_test_error.vec<-rep(1,10)
glm_test_error.vec<-rep(1,10)

#######divide dataset into train and test dataset
n.fold <- floor(2000/10)
s <- sample(rep(1:10, c(rep(n.fold, 9), 2000-(10-1)*n.fold)))  
i=1
for (i in 1:10){
train_set<-data[s!=i,]
test_set<-data[s==i,]
########train#########
library(gbm)
fit_gbm <- gbm.fit(x=train_set[,1:20], y=train_set[,21],
                     n.trees=1800,
                     distribution="bernoulli",
                     interaction.depth=3, 
                     bag.fraction = 0.5,
                     verbose=TRUE)
best_iter <- gbm.perf(fit_gbm, method="OOB")
  
library(randomForest)
fit_rf <- randomForest(as.factor(label)~.,data=train_set,type="classification",importance=TRUE)

library(MASS)
fit_lda <- lda(as.factor(label)~.,data=train_set)

library(e1071)
tune.out=tune(svm,label~.,data=train_set,kernel ='linear',ranges =list(cost=c(0.01, 0.1,1,10)))
fit_svm =tune.out$best.model

fit_glm=glm(as.factor(label)~. ,data=train_set ,family =binomial)

##########test############
###gbm
pred_gbm <- predict(fit_gbm, newdata=test_set[,1:20], 
                  n.trees=best_iter, type="response")
gbm.pred<-rep(1,200)
gbm.pred[pred_gbm<0.5]<-0
gbm_test_error<-mean(gbm.pred!=test_set[,21])
gbm_train_pred<-predict(fit_gbm, newdata=train_set[,1:20], 
                                 n.trees=best_iter, type="response")
feature_gbm<-rep(1,1800)
feature_gbm[gbm_train_pred<0.5]<-0

###rf
pred_rf<-predict(fit_rf,test_set[,1:20])
rf_test_error<-mean(pred_rf!=test_set[,21])

feature_rf<-fit_rf$predicted

###lda  
pred_lda<-predict(fit_lda,newdata=test_set)$class
lda_test_error<-mean(pred_lda!=test_set[,21])
feature_lda<-predict(fit_lda,train_set[,1:20])$class

###svm
pred_svm=predict(fit_svm,test_set[,1:20])
svm.pred<-rep(1,200)
svm.pred[pred_svm<0.5]<-0
svm_test_error<-mean(svm.pred!=test_set[,21])

svm_train_pred<-predict(fit_svm, newdata=train_set[,1:20], 
                        n.trees=best_iter, type="response")
feature_svm<-rep(1,1800)
feature_svm[svm_train_pred<0.5]<-0

###glm
pred_glm<-predict(fit_glm,test_set[,1:20],type="response")
glm.pred<-rep(1,200)
glm.pred[pred_glm<0.5]<-0
glm_test_error<-mean(glm.pred!=test_set[,21])

glm_train_pred<-predict(fit_glm, newdata=train_set[,1:20],type="response")
feature_glm<-rep(1,1800)
feature_glm[glm_train_pred<0.5]<-0

#############stack model training set#######
model_feature<-data.frame(cbind(feature_gbm,feature_rf,feature_lda,feature_svm,feature_glm))
model_feature$feature_gbm<-as.factor(model_feature$feature_gbm)
model_feature$feature_rf<-as.factor(model_feature$feature_rf)
model_feature$feature_lda<-as.factor(model_feature$feature_lda)
model_feature$feature_svm<-as.factor(model_feature$feature_svm)
model_feature$feature_glm<-as.factor(model_feature$feature_glm)
model_feature$label<-train_set[,21]
#########stack model testing set#########
model_feature_test<-data.frame(cbind(gbm.pred,pred_rf,pred_lda,svm.pred,glm.pred))
model_feature_test$gbm.pred<-as.factor(model_feature_test$gbm.pred)
model_feature_test$svm.pred<-as.factor(model_feature_test$svm.pred)
model_feature_test$glm.pred<-as.factor(model_feature_test$glm.pred)
model_feature_test$label<-test_set[,21]
##############Train Stacked Model
###median
###gbm
fit_stack_gbm <- gbm.fit(x=model_feature[,1:5], y=model_feature[,6],
                   n.trees=1800,
                   distribution="bernoulli",
                   interaction.depth=3, 
                   bag.fraction = 0.5,
                   verbose=TRUE)
stack_best_iter <- gbm.perf(fit_stack_gbm, method="OOB")
###svm
tune.out.svm.stack<-tune(svm,as.factor(label)~.,data=model_feature,kernel ='linear',ranges =list(cost=c(0.01, 0.1,1,10)))
fit_stack_svm<-tune.out.svm.stack$best.model
##########test stack model#####
###median
numericframe<-data.frame(cbind(gbm.pred,pred_rf,pred_lda,svm.pred,glm.pred))
numericframe$pred_rf<-numericframe$pred_rf-1
numericframe$pred_lda<-numericframe$pred_lda-1
pred_stack_median<-rep(0,200)
i=0
for (i in 1:200){
  pred_stack_median[i]<-median(data.matrix(numericframe[i,]))
  i=i+1
}
stack_median_test_error<-mean(pred_stack_median!=test_set[,21])
###gbm
pred_stack_gbm <- predict(fit_stack_gbm, model_feature_test[,1:5], 
                    n.trees=stack_best_iter, type="response")
gbm.stack.pred<-rep(1,200)
gbm.stack.pred[pred_stack_gbm<0.5]<-0
stack_gbm_test_error<-mean(gbm.stack.pred!=test_set[,21])

###svm
pred_stack_svm<-predict(fit_stack_svm,model_feature_test[,1:5])
pred_stack_svm<-pred_stack_svm[1:200]
stack_svm_test_error<-mean(pred_stack_svm!=test_set[,21])

##############compare
stack_median_test_error.vec[i]<-stack_median_test_error
stack_gbm_test_error.vec[i]<-stack_gbm_test_error
stack_svm_test_error.vec[i]<-stack_svm_test_error
gbm_test_error.vec[i]<-gbm_test_error
rf_test_error.vec[i]<-rf_test_error
lda_test_error.vec[i]<-lda_test_error
svm_test_error.vec[i]<-svm_test_error
glm_test_error.vec[i]<-glm_test_error
i=i+1
}
c(mean(stack_median_test_error),mean(stack_gbm_test_error),mean(stack_svm_test_error),mean(gbm_test_error),
  mean(rf_test_error),mean(lda_test_error),mean(svm_test_error),mean(glm_test_error))
