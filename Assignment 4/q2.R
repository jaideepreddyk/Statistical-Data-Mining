#install.packages("rpart")

library("rpart") 
library(MASS)

wine_data = read.csv('wine.data.txt',header = FALSE)

summary(wine_data)
str(wine_data)

wine_data$V1<-as.factor(wine_data$V1)
set.seed(100)

train = sample(1:nrow(wine_data), nrow(wine_data)*0.80)

wine_train_data = wine_data[train, ]
wine_test_data = wine_data[-train, ]

### FULL TREE ####
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
wine_model <- rpart(V1~., data = wine_train_data, method = "class", control = model.control)

summary(wine_model)
names(wine_model)

wine_model$variable.importance

train_pred = predict(wine_model, wine_train_data, type = "class")

test_pred = predict(wine_model, wine_test_data, type = "class")
mean(test_pred != wine_test_data$V1)

summary(train_pred)
summary(test_pred)

## dendograms
x11()
plot(wine_model,uniform = T,compress = T,margin = 0.2,main="Full Tree")
text(wine_model,use.n=T,all=T,cex=1)

summary(as.factor(wine_model$where))


#### PRUNING for better performance ####
wine_model$cptable[,4]

# 4th column of the cp table to get cross validation error
x11()
plot(wine_model$cptable[,4],main="Cp for model selection",ylab="cv error")

min_cp = which.min(wine_model$cptable[,4])  ## taking the model whose Cp corresponds to minimum cross val error


wine_model_pruned <- prune(wine_model, cp = wine_model$cptable[min_cp,1])

x11()
plot(wine_model_pruned,uniform = T,compress = T,margin = 0.2,main="Pruned Tree")
text(wine_model_pruned,use.n=T,all=T,cex=1)

wine_model_pruned$where
summary(as.factor(wine_model_pruned$where))

## train prediction ##
train_pred_prune = predict(wine_model_pruned, wine_train_data, type = "class")
mean(train_pred_prune != wine_train_data$V1)

summary(as.factor(wine_model_pruned$where))

### to get the number of train instances falling at each node
nodes_wine <- wine_model_pruned
nodes_wine$frame$yval = as.numeric(rownames(nodes_wine$frame))
trainnodes <- predict(nodes_wine, wine_train_data, type="vector")
trainnodes
summary(as.factor(trainnodes))


## test prediction ##
test_pred_prune = predict(wine_model_pruned, wine_test_data, type = "class")
mean(test_pred_prune != wine_test_data$V1)
summary(test_pred_prune)

summary(as.factor(wine_model_pruned$where))

### to get the number of test instances falling at each node
nodes_wine <- wine_model_pruned
nodes_wine$frame$yval = as.numeric(rownames(nodes_wine$frame))
testnodes <- predict(nodes_wine, wine_test_data, type="vector")
testnodes
summary(as.factor(testnodes))



