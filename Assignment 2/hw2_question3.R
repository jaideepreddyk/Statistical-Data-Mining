
# generating data by creating instances of each variable from normal distribution of 1000
mydata<-data.frame(rnorm(1000))
for (i in seq(19)) {           # the for-loop creates 1000 random numbers from a normal distribution and 
                               # does it for 20 variables 
  y<-data.frame(rnorm(1000))
  mydata<-cbind(mydata,y)
  
}
colnames(mydata)<-paste("col",1:20,sep ="") # column names of the randomly created dataframe
epsilon<-rnorm(1000)     ### epsilon is not fixed constant to induce randomness in the data otherwise
                         ### train and test will behave the same


beta<-rnorm(20)    ####### generating variable coefficients Beta
beta[sample(1:20,5)]<-0

Y<-c()   ### an empty vector is created to hold the target variable

for (i in seq(1000)) {
  X <- 0
  for (j in seq(20)) {
    X<-mydata[i,j]*beta[j]+X          ##### generating the target variable 
  }
  Y[i]<-X+epsilon[i]
}
mydata_with_y<-cbind(mydata,Y)

library(caret)
datasplit<-createDataPartition(mydata_with_y$Y,p=0.9,list = F)
traindata<-mydata_with_y[datasplit,]
testdata<-mydata_with_y[-datasplit,]


best_subset<-regsubsets(Y~.,data = traindata,nvmax = 20)

best_summary<-summary(best_subset)
names(best_summary)

which(best_summary$cp==min(best_summary$cp))
which(best_summary$bic==min(best_summary$bic))

train_matrix<-model.matrix(Y~.,data = traindata)
test_matrix<-model.matrix(Y~.,data = testdata)


train_mse<-c()
  for (i in seq(20)) {
         pred_y<-train_matrix[,names(coefficients(best_subset,i))]%*%coefficients(best_subset,i)
         train_mse[i]<-mean((pred_y-traindata$Y)^2)
  }
plot(train_mse,type='b')


test_mse<-c()
for (i in seq(20)) {
  pred_y<-test_matrix[,names(coefficients(best_subset,i))]%*%coefficients(best_subset,i)
  test_mse[i]<-mean((pred_y-testdata$Y)^2)
}

plot(test_mse,type='b')
which(test_mse==min(test_mse))


train_mse
test_mse













