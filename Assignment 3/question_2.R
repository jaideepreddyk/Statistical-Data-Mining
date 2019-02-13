
#install.packages("corrplot")
library(ISLR)
library(MASS)

mydata <- read.table("Diabetes.txt",header = FALSE)

mydata <- subset(mydata,select = -c(V1,V2,V3,V4))
colnames(mydata) <- c("glucose.area","insulin.area","SSDG","relative.weight","fasting.plasma.glucose","class_no")

library(lattice)
library(ggplot2)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07") 
my_pch<-c(15,16,17)
pairs(mydata[,1:5], pch = my_pch[mydata$class_no],  cex = 1.5,col = my_cols[mydata$class_no],lower.panel=NULL)

########### COVARIANCE MATRIX #######
library(corrplot)

### CLASS 1 ###
class_1<-mydata[which(mydata$class_no==1),]
cov(as.matrix(class_1))
corrplot(cor(as.matrix(class_1[,-6])),title = "Class 1",mar=c(0,0,1,0))

### CLASS 2 ###
class_2<-mydata[which(mydata$class_no==2),]
cov(as.matrix(class_2))
corrplot(cor(as.matrix(class_2[,-6])),title = "Class 2",mar=c(0,0,1,0))

### CLASS 3 ###

class_3<-mydata[which(mydata$class_no==3),]
cov(as.matrix(class_3))
corrplot(cor(as.matrix(class_3[,-6])),title = "Class 3",mar=c(0,0,1,0))

##### Mutivariate normality ####

### CLASS 1 ###
par(mfrow=c(2,3))
plot((density(class_1$glucose.area)),main = "Distribution of glucose.area")
plot((density(class_1$insulin.area)),main = "Distribution of insulin.area")
plot((density(class_1$SSDG)),main = "Distribution of SSDG")
plot((density(class_1$relative.weight)),main = "Relative weight")
plot((density(class_1$fasting.plasma.glucose)),main = "Distribution of Fasting Plasma Glucose")

### CLASS 2 ###
plot((density(class_2$glucose.area)),main = "Distribution of glucose.area")
plot((density(class_2$insulin.area)),main = "Distribution of insulin.area")
plot((density(class_2$SSDG)),main = "Distribution of SSDG")
plot((density(class_2$relative.weight)),main = "Relative weight")
plot((density(class_2$fasting.plasma.glucose)),main = "Distribution of Fasting Plasma Glucose")

### CLASS 3 ###
plot((density(class_3$glucose.area)),main = "Distribution of glucose.area")
plot((density(class_3$insulin.area)),main = "Distribution of insulin.area")
plot((density(class_3$SSDG)),main = "Distribution of SSDG")
plot((density(class_3$relative.weight)),main = "Relative weight")
plot((density(class_3$fasting.plasma.glucose)),main = "Distribution of Fasting Plasma Glucose")


## test-train split ##
library(caret)
set.seed(123)
datasplit<-createDataPartition(mydata$class_no,p=0.75,list = F)
traindata<-mydata[datasplit,]
testdata<-mydata[-datasplit,]

## LDA model ####
lda.fit<-lda(class_no~.,data = traindata)
names(lda.fit)
lda.fit

train_nopred<-subset(traindata,select =-class_no)
test_nopred<-subset(testdata,select =-class_no)

## LDA train prediction ##
lda_pred_train<-predict(lda.fit,train_nopred)
y_hat_train<-as.numeric(lda_pred_train$class)
y_train<-as.numeric(traindata$class_no)

train_err_lda<-sum(abs(y_hat_train-y_train))/length(y_train)

## LDA test prediction ##
lda_pred_test<-predict(lda.fit,test_nopred)
y_hat_test<-as.numeric(lda_pred_test$class)
y_test<-as.numeric(testdata$class_no)

test_err_lda<-sum(abs(y_hat_test-y_test))/length(y_test)

## QDA ####

qda.fit<-qda(class_no~.,data = traindata)
names(qda.fit)
qda.fit

## qda test prediction ##
qda_pred_train<-predict(qda.fit,train_nopred)
y_hat_train_qda<-as.numeric(qda_pred_train$class)
y_train_qda<-as.numeric(traindata$class_no)
train_err_qda<-sum(abs(y_hat_train_qda-y_train_qda))/length(y_train_qda)


## qda test prediction ##
qda_pred_test<-predict(qda.fit,test_nopred)
y_hat_test_qda<-as.numeric(qda_pred_test$class)
y_test_qda<-as.numeric(testdata$class_no)
test_err_qda<-sum(abs(y_hat_test_qda-y_test_qda))/length(y_test_qda)


## Predicting on new data point PART c ####

newdata<-data.frame("glucose.area"=0.98,"insulin.area"=122,"SSDG"=544,"relative.weight"=186,"fasting.plasma.glucose"=184)


lda_newdata<-predict(lda.fit,newdata)
lda_newdata$class

qda_newdata<-predict(qda.fit,newdata)
qda_newdata$class










