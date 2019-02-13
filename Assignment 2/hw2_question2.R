
setwd('C:/Users/Jaideep/Desktop/UB Data Science/Sem 1/R.Hageman/Homework 2')
traindata = read.table("ticdata2000.txt", sep="\t")

testdata = read.table("ticeval2000.txt", sep="\t")


test_tarvar = read.table("tictgts2000.txt", sep="\t")


lm_model<-lm(V86~.,data=traindata)

lm_predictions<-predict(lm_model,testdata)

lm_error<-mean((lm_predictions-test_tarvar$V1)^2)

colnames(test_tarvar)<-"V86"
test_withpred<-cbind(testdata,test_tarvar)

train_matrix<-model.matrix(V86~.,data = traindata)
test_matrix<-model.matrix(V86~.,data = test_withpred)

hist(traindata$V86)

## Forward subset

install.packages("leaps")
library(leaps)

fwd_subset<-regsubsets(V86~.,data = traindata,method = "forward",nvmax = 87)

fwd_summary<-summary(fwd_subset)
names(fwd_summary)

which(fwd_summary$cp==min(fwd_summary$cp))
which(fwd_summary$bic==min(fwd_summary$bic))

test_mse<-c()
for (i in seq(85)) {
  pred_y<-test_matrix[,names(coefficients(fwd_subset,i))]%*%coefficients(fwd_subset,i)
  test_mse[i]<-mean((pred_y-test_withpred$V86)^2)
}

plot(test_mse,type='b')
which(test_mse==min(test_mse))

## Backward subset selection

backward_subset<-regsubsets(V86~.,data = traindata,method = "backward",nvmax = 87)

backward_summary<-summary(backward_subset)
names(backward_summary)

which(backward_summary$cp==min(backward_summary$cp))
which(backward_summary$bic==min(backward_summary$bic))

test_mse<-c()
for (i in seq(85)) {
  pred_y<-test_matrix[,names(coefficients(backward_subset,i))]%*%coefficients(backward_subset,i)
  test_mse[i]<-mean((pred_y-test_withpred$V86)^2)
}

plot(test_mse,type='b')
which(test_mse==min(test_mse))

## Ridge regression

library(glmnet)

ridge.fit<-glmnet(train_matrix,traindata$V86,alpha=0)

cv_ridge<-cv.glmnet(train_matrix,traindata$V86,alpha=0,nfolds = 10)
plot(cv_ridge)
summary(cv_ridge)
cv_ridge$lambda.min

ridge_pred<-predict(ridge.fit,newx = test_matrix,s=cv_ridge$lambda.min,type = 'response')
ridge_error<-mean((ridge_pred-test_withpred$V86)^2)

## LASSO Regression
lasso.fit<-glmnet(train_matrix,traindata$V86,alpha=1)

cv_lasso<-cv.glmnet(train_matrix,traindata$V86,alpha=1)
plot(cv_lasso)
summary(cv_lasso)
cv_lasso$lambda.min

lasso_pred<-predict(cv_lasso,test_matrix,s=cv_lasso$lambda.min)
lasso_error<-mean((lasso_pred-test_withpred$V86)^2)






















