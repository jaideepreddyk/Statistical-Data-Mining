# install.packages("ISLR")
# install.packages("MMST")
library(ISLR)
library(MASS)
# library(MMST)
mydata<-Boston
mydata_set1<-subset(mydata,select = c(zn,nox,dis,rad,ptratio,black,lstat,medv,crim))
mydata_set2<-subset(mydata,select = c(zn,rad,tax,black,lstat,crim))

sum(is.na(mydata))

str(mydata)
length(mydata$crim)/2
median_crim<-mydata$crim[length(mydata$crim)/2]

str(mydata$crim)
summary(mydata$crim)

### Pre-processing ####

mydata['crim_med']=0

mydata$crim_med[which(mydata$crim>median_crim)]=1
mydata$crim<-NULL


str(mydata$crim_med)
mydata$crim_med<-as.factor(mydata$crim_med)

## test-train split ##
library(caret)
set.seed(123)
datasplit<-createDataPartition(mydata$crim_med,p=0.75,list = F)
traindata<-mydata[datasplit,]
testdata<-mydata[-datasplit,]

summary(mydata$crim_med)

### LDA ####

lda.fit<-lda(crim_med~.,data=traindata)
names(lda.fit)
lda.fit
summary(lda.fit)

train_nopred<-subset(traindata,select =-crim_med)
lda_pred_train<-predict(lda.fit,train_nopred)

y_hat_train<-as.numeric(lda_pred_train$class)-1
y_train<-as.numeric(traindata$crim_med)-1
train_err_lda<-sum(abs(y_hat_train-y_train))/length(y_train)

test_nopred<-subset(testdata,select =-crim_med)
lda_pred_test<-predict(lda.fit,test_nopred)

y_hat_test<-as.numeric(lda_pred_test$class)-1
y_test<-as.numeric(testdata$crim_med)-1
test_err_lda<-sum(abs(y_hat_test-y_test))/length(y_test)


### Logistic Regression ####
str(traindata)
log.fit<-glm(crim_med~.,data = traindata,family = 'binomial')
summary(log.fit)
log_pred_train<-predict(log.fit,newdata = train_nopred,type = 'response')

log_pred_train[which(log_pred_train>0.5)]=1
log_pred_train[which(log_pred_train<=0.5)]=0

y_train<-as.numeric(traindata$crim_med)-1
train_err_log<-sum(abs(log_pred_train-y_train))/length(y_train)

log_pred_test<-predict(log.fit,newdata = test_nopred,type = 'response')

log_pred_test[which(log_pred_test>0.5)]=1
log_pred_test[which(log_pred_test<=0.5)]=0

y_test<-as.numeric(testdata$crim_med)-1
test_err_log<-sum(abs(log_pred_test-y_test))/length(y_test)

### KNN ####
library(class)

knn_model<-knn(train_nopred,test_nopred,traindata$crim_med,k=3)

y_test<-as.numeric(testdata$crim_med)-1
knn_pred_test<-as.numeric(knn_model)-1
test_err_knn<-sum(abs(knn_pred_test-y_test))/length(y_test)



### subset 1 ####

length(mydata_set1$crim)/2
median_crim<-mydata_set1$crim[length(mydata_set1$crim)/2]


mydata_set1['crim_med']=0

mydata_set1$crim_med[which(mydata_set1$crim>median_crim)]=1
mydata_set1$crim<-NULL


str(mydata_set1$crim_med)
mydata_set1$crim_med<-as.factor(mydata_set1$crim_med)



## test-train split ##
library(caret)
set.seed(123)
datasplit<-createDataPartition(mydata_set1$crim_med,p=0.75,list = F)
traindata_set1<-mydata_set1[datasplit,]
testdata_set1<-mydata_set1[-datasplit,]

summary(mydata_set1$crim_med)

### LDA ###

lda.fit_set1<-lda(crim_med~.,data=traindata_set1)
names(lda.fit_set1)
lda.fit_set1
summary(lda.fit_set1)

train_nopred_set1<-subset(traindata_set1,select =-crim_med)
lda_pred_train_set1<-predict(lda.fit_set1,train_nopred_set1)

y_hat_train_set1<-as.numeric(lda_pred_train_set1$class)-1
y_train_set1<-as.numeric(traindata_set1$crim_med)-1
train_err_lda_set1<-sum(abs(y_hat_train_set1-y_train_set1))/length(y_train_set1)

test_nopred_set1<-subset(testdata_set1,select =-crim_med)
lda_pred_test_set1<-predict(lda.fit_set1,test_nopred_set1)

y_hat_test_set1<-as.numeric(lda_pred_test_set1$class)-1
y_test_set1<-as.numeric(testdata_set1$crim_med)-1
test_err_lda_set1<-sum(abs(y_hat_test_set1-y_test_set1))/length(y_test_set1)


### Logistic Regression ###
str(traindata_set1)
log.fit_set1<-glm(crim_med~.,data = traindata_set1,family = 'binomial')
summary(log.fit_set1)
log_pred_train_set1<-predict(log.fit_set1,newdata = train_nopred_set1,type = 'response')

log_pred_train_set1[which(log_pred_train_set1>0.5)]=1
log_pred_train_set1[which(log_pred_train_set1<=0.5)]=0

y_train_set1<-as.numeric(traindata_set1$crim_med)-1
train_err_log_set1<-sum(abs(log_pred_train_set1-y_train_set1))/length(y_train_set1)

log_pred_test_set1<-predict(log.fit_set1,newdata = test_nopred_set1,type = 'response')

log_pred_test_set1[which(log_pred_test_set1>0.5)]=1
log_pred_test_set1[which(log_pred_test_set1<=0.5)]=0

y_test_set1<-as.numeric(testdata_set1$crim_med)-1
test_err_log_set1<-sum(abs(log_pred_test_set1-y_test_set1))/length(y_test_set1)

### KNN ###
library(class)

knn_model_set1<-knn(train_nopred_set1,test_nopred_set1,traindata_set1$crim_med,k=3)

y_test_set1<-as.numeric(testdata_set1$crim_med)-1
knn_pred_test_set1<-as.numeric(knn_model_set1)-1
test_err_knn_set1<-sum(abs(knn_pred_test_set1-y_test_set1))/length(y_test_set1)

### subset 2 ####

length(mydata_set2$crim)/2
median_crim<-mydata_set2$crim[length(mydata_set2$crim)/2]


mydata_set2['crim_med']=0

mydata_set2$crim_med[which(mydata_set2$crim>median_crim)]=1
mydata_set2$crim<-NULL


str(mydata_set2$crim_med)
mydata_set2$crim_med<-as.factor(mydata_set2$crim_med)



## test-train split ##
library(caret)
set.seed(123)
datasplit<-createDataPartition(mydata_set2$crim_med,p=0.75,list = F)
traindata_set2<-mydata_set2[datasplit,]
testdata_set2<-mydata_set2[-datasplit,]

summary(mydata_set2$crim_med)

### LDA ###

lda.fit_set2<-lda(crim_med~.,data=traindata_set2)
names(lda.fit_set2)
lda.fit_set2
summary(lda.fit_set2)

train_nopred_set2<-subset(traindata_set2,select =-crim_med)
lda_pred_train_set2<-predict(lda.fit_set2,train_nopred_set2)

y_hat_train_set2<-as.numeric(lda_pred_train_set2$class)-1
y_train_set2<-as.numeric(traindata_set2$crim_med)-1
train_err_lda_set2<-sum(abs(y_hat_train_set2-y_train_set2))/length(y_train_set2)

test_nopred_set2<-subset(testdata_set2,select =-crim_med)
lda_pred_test_set2<-predict(lda.fit_set2,test_nopred_set2)

y_hat_test_set2<-as.numeric(lda_pred_test_set2$class)-1
y_test_set2<-as.numeric(testdata_set2$crim_med)-1
test_err_lda_set2<-sum(abs(y_hat_test_set2-y_test_set2))/length(y_test_set2)


### Logistic Regression ###
str(traindata_set2)
log.fit_set2<-glm(crim_med~.,data = traindata_set2,family = 'binomial')
summary(log.fit_set2)
log_pred_train_set2<-predict(log.fit_set2,newdata = train_nopred_set2,type = 'response')

log_pred_train_set2[which(log_pred_train_set2>0.5)]=1
log_pred_train_set2[which(log_pred_train_set2<=0.5)]=0

y_train_set2<-as.numeric(traindata_set2$crim_med)-1
train_err_log_set2<-sum(abs(log_pred_train_set2-y_train_set2))/length(y_train_set2)

log_pred_test_set2<-predict(log.fit_set2,newdata = test_nopred_set2,type = 'response')

log_pred_test_set2[which(log_pred_test_set2>0.5)]=1
log_pred_test_set2[which(log_pred_test_set2<=0.5)]=0

y_test_set2<-as.numeric(testdata_set2$crim_med)-1
test_err_log_set2<-sum(abs(log_pred_test_set2-y_test_set2))/length(y_test_set2)

### KNN ###
library(class)

knn_model_set2<-knn(train_nopred_set2,test_nopred_set2,traindata_set2$crim_med,k=3)

y_test_set2<-as.numeric(testdata_set2$crim_med)-1
knn_pred_test_set2<-as.numeric(knn_model_set2)-1
test_err_knn_set2<-sum(abs(knn_pred_test_set2-y_test_set2))/length(y_test_set2)



