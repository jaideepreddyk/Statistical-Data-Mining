library(neuralnet)
library(ElemStatLearn)

n<- names(spam)
formula <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))

mydata = spam

spam$spam = as.numeric((spam$spam))-1
spam = as.data.frame(scale(spam))
spam$spam = mydata$spam
spam$spam = as.numeric((mydata$spam))-1

set.seed(100)
train = sample(1:nrow(spam), nrow(spam)*0.80)
test = -train
trainData = spam[train, ]
testData = spam[test, ]

nn <- neuralnet(formula , data = trainData, hidden = 5, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)


#### Univariate outlier and shrinking

outlier_vec=c(200,100,10,1,0.1,-100,-10,-1) ### vector containing different outlier values
univariate_outlier_data = trainData
outlier_vec
err_vec=c()
i=0

for (x in outlier_vec){
univariate_outlier_data[4,4] = x   ### setting this data point as the outlier

nn <- neuralnet(formula , data = univariate_outlier_data, hidden = 5, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(nn, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
i=i+1
err_vec[i]=mean(predicted_class_new_data != testData$spam)

}
outlier_vec
err_vec
