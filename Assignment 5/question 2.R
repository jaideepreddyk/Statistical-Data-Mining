#Question 2
install.packages("neuralnet")
library(ElemStatLearn)
library(neuralnet)

data(spam)
spam$spam <- ifelse(spam$spam == "spam",1,0)
spam<- spam[1:4600,]

## cv error 
n<- names(spam)
f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))

crossvalidate <- function(spam,hidden_1)
{   
  error_cv <- NULL
  k <- 7 # any random value for splitting  
  for(j in 1:k)
  {
    ### splitting data into train and test set ###

    data.sample <- sample(1:nrow(spam),round(nrow(spam)/2))
    train.data<- spam[data.sample,]
    test.data <- spam[-data.sample,]
    
    ## neural network fit ##
    
    network <- neuralnet(f,data=train.data,hidden=hidden_1,err.fct='ce', linear.output=FALSE,threshold=0.5)
    predict.network <- compute(network,test.data[,1:57])
    
    ###### specifying and predict the class #####

    class <- train.data$spam
    predict <- round(predict.network$net.result)
    ## the cv error can be defined as follows  ##
    
    error_cv[j] <- sum((class - predict)/nrow(predict))
    
  }
  return(mean(error_cv))
}

## selecting the hidden neural networks 
error_train <- NULL
error_test <- NULL
set.seed(100)
## using cross validation

for(i in 1:5)
{
  network<- neuralnet(f,data=spam,hidden=c(i),err.fct='ce', linear.output=FALSE,
                      threshold=0.5)
  error_train[i] <- sum(((round(network$net.result[[1]])-(spam$spam))^2)/nrow(spam))
  error_test[i] <- crossvalidate(spam,hidden=c(i))    
}

## plotting errors 

error_train
error_test
#plot(error_train,main='Mean vs hidden neurons',xlab="Hidden neurons",ylab='error MSE of train',type='14',col='green',lwd=2)
plot(error_test,main='Mean vs hidden neurons',xlab="Hidden neurons",ylab='error MSE of test',type='l4',col='green',lwd=2)

which(min(error_train) == error_train)
