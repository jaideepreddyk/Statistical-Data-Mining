mydata<-read.csv("diabetes.csv")

colnames(mydata)

#install.packages("gbm")
library(caret)
library(rpart)
library(gbm)
#install.packages("randomForest")
library(randomForest)
#library(geneplotter)

library(ISLR)
str(mydata)
mydata$Outcome<-as.factor(mydata$Outcome)

train<-sample(1:nrow(mydata),0.8*nrow(mydata))

traindata<-mydata[train,]
testdata<-mydata[-train,]


y_true<-as.factor(testdata$Outcome)
y_true<-as.numeric(y_true)-1

#### randomForest ####

rf.fit<-randomForest(Outcome~.,data=traindata,n.tree=10000)
x11()
varImpPlot(rf.fit)
importance(rf.fit)
y_hat<-predict(rf.fit,newdata = testdata,type = "response")
confusionMatrix(y_hat,testdata$Outcome)
## 77.27 % accuracy
y_hat<-as.numeric(y_hat)-1
misclass_rf<-sum(abs(y_hat-y_true))/length(y_true)
misclass_rf

#### bagging #####
bag.fit<-randomForest(Outcome~.,data=traindata,n.tree=10000,mtry=8)
x11()
varImpPlot(bag.fit)
importance(bag.fit)
y_hat<-predict(bag.fit,newdata = testdata,type = "response")
confusionMatrix(y_hat,testdata$Outcome)
## 74.68 % accuracy ##
y_hat<-as.numeric(y_hat)-1
misclass_bag<-sum(abs(y_hat-y_true))/length(y_true)


### boosting ####

boost.train<-traindata
boost.train$Outcome<-as.numeric(traindata$Outcome)-1
boost.test<-testdata
boost.test$Outcome<-as.numeric(testdata$Outcome)-1
boost.fit<-gbm(Outcome~.,data = boost.train,n.trees = 100,shrinkage = 0.1,interaction.depth = 6,distribution = "adaboost")
boost.fit2<-gbm(Outcome~.,data = boost.train,n.trees = 100,shrinkage = 0.6,interaction.depth = 6,distribution = "adaboost")
names(boost.fit)
summary(boost.fit)
y_hat<-predict(boost.fit,newdata = boost.test,n.trees = 100,type = "response")
y_hat1<-predict(boost.fit2,newdata = boost.test,n.trees = 100,type = "response")

misclass_boost<-sum(abs(y_hat-y_true))/length(y_true)
misclass_boost1<-sum(abs(y_hat1-y_true))/length(y_true)


## logistic Regression ##

log.fit<-glm(Outcome~.,data = traindata,family = 'binomial')
summary(log.fit)

log_pred_test<-predict(log.fit,newdata = testdata,type = 'response')
log_pred_test[which(log_pred_test>0.5)]=1
log_pred_test[which(log_pred_test<=0.5)]=0
test_err_log<-sum(abs(log_pred_test-y_true))/length(y_true)

misclass_rf
misclass_bag
misclass_boost
test_err_log









