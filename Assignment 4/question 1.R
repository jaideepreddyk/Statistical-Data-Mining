library(ElemStatLearn)  
mydata=prostate
mydata$train<-NULL
library(caret)
set.seed(1)

train=sample(1:nrow(mydata),0.8*nrow(mydata))
Y.train<-mydata$lpsa[train]
Y.test=mydata$lpsa[-train]

X.train=mydata[train,1:2]
X.test=mydata[-train,1:2]

training = mydata[train, 1:9]
testing = mydata[-train, 1:9]

library(leaps)

best_subset<-regsubsets(lpsa~.,data=training,method = "exhaustive",nvmax=8)
best_summary<-summary(best_subset)
names(best_summary)

cp_min<-which.min(best_summary$cp)
bic_min<-which.min(best_summary$bic)

cp_set<-best_summary$outmat[cp_min,]
cp_pred <- names(which(cp_set == "*"))
cp_pred <- paste(cp_pred, collapse = "+")
cp.formula<-as.formula(paste0("lpsa", "~", cp_pred))

cp_model<-lm(cp.formula,data = training)
cp_pred_err<-predict(cp_model,testing)
difference_cp<-cp_pred_err-testing$lpsa
testerror_cp<-mean(difference_cp^2)


bic_set<-best_summary$outmat[bic_min,]
bic_pred <- names(which(bic_set == "*"))
bic_pred <- paste(bic_pred, collapse = "+")
bic.formula<-as.formula(paste0("lpsa", "~", bic_pred))

bic_model<-lm(bic.formula,data = training)
bic_pred_err<-predict(bic_model,testing)
difference_bic<-bic_pred_err-testing$lpsa
testerror_bic<-mean(difference_bic^2)


plot(best_summary$cp,type = "o", lty = 2, col = "blue",main = "Cp")
plot(best_summary$bic,type = "o", lty = 1, col = "red",main="BIC")


### cross validation ####
n_train=nrow(mydata)

library(DAAG)

best_fit<-regsubsets(lpsa~.,data=mydata,method = "exhaustive",nvmax=8)

summary(best_fit)

error_5fold<-c()

error_10fold<-c()

bestset=summary(best_fit)$outmat

for (i in 1:8){
########## selecting predictors ###########
models <- bestset[i,]       ### i variables is for the number of predictors
predictors <- names(which(models == "*"))
predictors <- paste(predictors, collapse = "+")
model.formula<-as.formula(paste0("lpsa", "~", predictors))


 ##### 5 fold cv #######
set.seed(1)
train.control_5 <- trainControl(method = "cv", number = 5)
cv5 <- train(model.formula, data = mydata, method = "lm",
            trControl = train.control_5)
cv5$results$RMSE

error_5fold<-c(error_5fold,cv5$results$RMSE)


##### 10 fold cv ######

set.seed(1)
train.control_10<- trainControl(method = "cv", number = 10)
cv10 <- train(model.formula, data = mydata, method = "lm",
            trControl = train.control_10)
cv10$results$RMSE

error_10fold<-c(error_10fold,cv10$results$RMSE)


}

which.min(error_5fold)
error_5fold
plot(error_5fold,type = "o", lty = 2,main="Prediction error using 5 fold cv")

which.min(error_10fold)
error_10fold
plot(error_10fold,type = "o", lty = 2,main="Prediction error using 10 fold cv")


#### bootstrap ##########
#install.packages("bootstrap")

library(bootstrap)
library(boot)  #install.packages("bootstrap")
best_sub = regsubsets(lpsa~.,data = mydata,method="exhaustive")
reg_summary=summary(best_sub)

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}
select = reg_summary$outmat
error_store <- c()
for (i in 1:8){
  # Pull out the model
  temp <- which(select[i,] == "*")
  
  res <- bootpred(mydata[,temp], mydata$lpsa, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error)
  res[[3]]
  error_store <- c(error_store, res[[3]])
  
}

error_store
















