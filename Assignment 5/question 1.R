library(ElemStatLearn)

mydata<-spam

#install.packages("randomForest")
library(randomForest)

train=sample(1:nrow(mydata),0.75*nrow(mydata))
traindata<-mydata[train,]
testdata <- mydata[-train,]


modelName = c()
test_error_vector = c()

## trying different values of mtry

for(i in seq(1,15,2)){
  rf.fit = randomForest(spam~.,data = traindata, ntree =100, mtry = i)
  
  rf.fit.pred_test <- predict(rf.fit, newdata = testdata, type='class')

  rf_test_err <- mean(rf.fit.pred_test != testdata$spam)

  modelName = c(modelName,i)
  test_error_vector = c(test_error_vector,rf_test_err)
}

errorDF = data.frame(Model_Name = modelName,Test_Error = test_error_vector)

plot(errorDF$Model_Name,errorDF$Test_Error,type='o')


collection = rep(0, 100)
for(i in seq(1,15,2)){
  rf.fit = randomForest(spam~., data  = traindata, ntree = 100, mtry = i)
  rf.predictions = predict(rf.fit, testdata, type = "class")
  collection = cbind(collection, rf.fit$err.rate[,c(1)])
}

#out of bag errors.

collection_plot = data.frame(collection)[,-c(1)]
names(collection_plot) = c("mtry_2", "mtry_4","mtry_6","mtry_8","mtry_10")
collection_plot$NTrees = seq(1, 100)

#plotting
install.packages("ggplot2")
library(ggplot2)


ggplot(collection_plot, aes(NTrees)) + 
  geom_line(aes(y = mtry_2, colour = "mtry_2")) + geom_point(aes(y = mtry_2), size = 0.3) + 
  geom_line(aes(y = mtry_4, colour = "mtry_4")) + geom_point(aes(y = mtry_4), size = 0.3) + 
  geom_line(aes(y = mtry_6, colour = "mtry_6")) + geom_point(aes(y = mtry_6), size = 0.3) + 
  geom_line(aes(y = mtry_8, colour = "mtry_8")) + geom_point(aes(y = mtry_8), size = 0.3) + 
  geom_line(aes(y = mtry_10, colour = "mtry_10")) + geom_point(aes(y = mtry_10), size = 0.3) + 
  
  ggtitle("Out of Bag Errors.") + xlab("No of features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))


