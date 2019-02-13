

### Question 1

install.packages("ISLR")
library(ISLR)
View(College)
mydata<-College
summary(mydata)
str(mydata)
library(caret)
set.seed(1)

#### test-train split
datasplit<-createDataPartition(mydata$Apps,p=0.75,list = F)
traindata<-mydata[datasplit,]
testdata<-mydata[-datasplit,]


### building linear model
linearmodel<-lm(Apps~.,data = traindata)
summary(linearmodel)

test_nopred<-subset(testdata,select =-Apps)

#### predicting linear model
testpredictions<-predict(linearmodel,test_nopred)

### MSE
lm_error<-mean((testpredictions-testdata$Apps)^2)
lm_error
# PART b

install.packages("glmnet")
library(glmnet)


train_matrix<-model.matrix(Apps~.,data = traindata)
test_matrix<-model.matrix(Apps~.,data = testdata)

cv_ridge<-cv.glmnet(train_matrix,traindata$Apps,alpha=0,nfolds = 10)
plot(cv_ridge)

ridge_pred<-predict(cv_ridge,test_matrix,s=cv_ridge$lambda.min)

ridge_error<-mean((ridge_pred-testdata$Apps)^2)
ridge_error
# PART C
cv_lasso<-cv.glmnet(train_matrix,traindata$Apps,alpha=1,nfolds = 10)
plot(cv_lasso)

lasso_pred<-predict(cv_lasso,test_matrix,s=cv_lasso$lambda.min)

lasso_error<-mean((lasso_pred-testdata$Apps)^2)

# PART D

install.packages("pls")
library(pls)

model_pcr<-pcr(Apps~.,data=traindata,scale=T,validation= "CV")

head(model_pcr)
summary(model_pcr)
validationplot(model_pcr,val.type = 'MSE')

pcr_pred<-predict(model_pcr,test_nopred,ncomp =8 )

pcr_error<-mean((pcr_pred-testdata$Apps)^2)


# PART E

model_plsr<-plsr(Apps~.,data=traindata,scale=T,validation= "CV")

head(model_pcr)
summary(model_pcr)

validationplot(model_plsr,val.type = 'MSE')

plsr_pred<-predict(model_plsr,test_nopred,ncomp =6 )

plsr_error<-mean((plsr_pred-testdata$Apps)^2)



















