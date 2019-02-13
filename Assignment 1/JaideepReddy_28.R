
setwd("C:/Users/Jaideep/Desktop/UB Data Science/Sem 1/R.Hageman/Homework 1 material")

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

################################ QUESTION 1 ################################################
###################### PRE PROCESSING
head(d3)
dim(d3)

sum(is.na(d3)) ########## no missing values in d3

str(d3)        ######### structure of the merged dataset

summary(d3)    ########## summary of merged dataset

### plotting x and y components of variables ###
par(mfrow=c(3,2))
plot(d3$guardian.x);plot(d3$guardian.y)
hist(d3$studytime.x);hist(d3$studytime.y)
hist(d3$traveltime.x);hist(d3$traveltime.y)
plot(d3$schoolsup.x);plot(d3$schoolsup.y)
plot(d3$famsup.x);plot(d3$famsup.y)
plot(d3$activities.x);plot(d3$activities.y)
plot(d3$higher.x);plot(d3$higher.y)
plot(d3$romantic.x);plot(d3$romantic.y)
plot(d3$famrel.x);plot(d3$famrel.y)
plot(d3$freetime.x);plot(d3$freetime.y)
plot(d3$goout.x);plot(d3$goout.y)
plot(d3$Dalc.x);plot(d3$Dalc.y)
plot(d3$Walc.x);plot(d3$Walc.y)
hist(d3$health.x);hist(d3$health.y)
# from the above plots we can see that the x and y components of these variables are same,
# so we can remove duplicate columns


## let us will merge including the above variables

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet","guardian","studytime","traveltime","schoolsup","famsup","activities","higher","romantic","famrel","freetime","goout","Dalc","Walc","health"))

summary(d3)
str(d3)

#### renaming the columns
install.packages("data.table")
library(data.table)
setnames(d3,old =c("failures.x","paid.x","absences.x","G1.x","G2.x","G3.x"),new = c("failures.mat","paid.mat","absences.mat","G1.mat","G2.mat","G3.mat"))
setnames(d3,old =c("failures.y","paid.y","absences.y","G1.y","G2.y","G3.y"),new = c("failures.por","paid.por","absences.por","G1.por","G2.por","G3.por"))
names(d3)

######################### PLOTTING ##########################

names(d3)

par(mfrow=c(2,3))

##### box plots for math grades
bpm1<-boxplot(d3$G1.mat,horizontal = T)
bpm2<-boxplot(d3$G2.mat,horizontal = T)
bpm3<-boxplot(d3$G3.mat,horizontal = T)

### no outliers in first period math
which(d3$G2.mat %in% bpm2$out) ############## to find out the instances with outliers from boxplot
### no outlier in final period math

##### box plots for portuguese grades
bpp1<-boxplot(d3$G1.por,horizontal = T)
bpp2<-boxplot(d3$G2.por,horizontal = T)
bpp3<-boxplot(d3$G3.por,horizontal = T)

which(d3$G1.por %in% bpp1$out)  ############## to find out the instances with outliers from boxplot
which(d3$G2.por %in% bpp2$out)  ############## to find out the instances with outliers from boxplot
which(d3$G3.por %in% bpp3$out)  ############## to find out the instances with outliers from boxplot


par(mfrow=c(2,3))

# histograms of math scores
hist(d3$G1.mat,main = "First period math grade",xlab = "Score")
hist(d3$G2.mat,main=  "Second period math grade",xlab = "Score")
hist(d3$G3.mat,main = "Last period math grade",xlab = "Score")

#histograms of portuguese scores
hist(d3$G1.por,main = "First period portuguese grade",xlab = "Score")
hist(d3$G2.por,main=  "Second period portuguese grade",xlab = "Score")
hist(d3$G3.por,main = "Last period portuguese grade",xlab = "Score")

#### plots for failures variable

hist(d3$failures.mat)
hist(d3$failures.por)

#### plots for absences
par(mfrow=c(1,2))

boxplot(d3$absences.mat,horizontal = T)
boxplot(d3$absences.por,horizontal = T)

hist(d3$absences.mat)
hist(d3$absences.por)

#### parents education
hist(d3$Medu)
hist(d3$Fedu)

#### parents jobs
plot(d3$Mjob)
plot(d3$Fjob)

#### plotting first period grades of math against different variables
par(mfrow=c(2,2))

plot(d3$absences.mat,d3$G1.mat,ylab = "First period math scores",xlab = "Absences")

plot(d3$age,d3$G1.mat,ylab = "First period math scores",xlab = "age") ### looks like age has nothing to do with first period math scores

plot(d3$paid.mat,d3$G1.mat,ylab = "First period math scores",xlab = "extra paid classes")

plot(d3$higher,d3$G1.mat,ylab = "First period math scores",xlab = "higher education")

plot(d3$internet,d3$G1.mat,ylab = "First period math scores",xlab = "Internet")

plot(d3$activities,d3$G1.mat,ylab = "First period math scores",xlab = "activities")


############## QUESTION 2 ##############################################
names(d3)

str(d3)

#### model for predicting first period math grades
linearmodelmat<-lm(G1.mat~school+sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+nursery+internet+guardian+studytime+traveltime+freetime+goout+Dalc+Walc+health+paid.mat+absences.mat,data = d3)
summary(linearmodelmat)

# removing the variables based on significance values
bestfitmat<-lm(G1.mat~sex+famsize+Medu+Fedu+studytime+goout+Dalc+health,data = d3)
summary(bestfitmat)

#### model for fitting first period portuguese grades
linearmodelpor<-lm(G1.por~school+sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+nursery+internet+guardian+studytime+traveltime+freetime+goout+Dalc+Walc+health+paid.por+absences.por,data = d3)
summary(linearmodelpor)

bestfitpor<-lm(G1.por~school+sex+address+famsize+Medu+nursery+studytime+traveltime+goout+Dalc+health+paid.por+absences.por,data = d3)
summary(linearmodelpor)

### interaction between variables

interactmodel<-lm(G1.mat~sex+age+address+famsize+studytime+famsize:studytime+Medu+studytime*higher*activities+schoolsup+famsup+activities+health,data =d3)
summary(interactmodel)

library(jtools)
interact_plot(model = interactmodel,pred=studytime,modx = activities)

interact_plot(model = interactmodel,pred=studytime,modx = famsize)

interact_plot(model = interactmodel,pred=studytime,modx = higher)

############### QUESTION 3 ###################################

library(MASS)
data(Boston)
is.data.frame(Boston)
names(Boston)
str(Boston)
summary(Boston)

sum(is.na(Boston)) ########## no NA values

#####scatterplots
par(mfrow=c(2,3))

#### pairwise scatterplot
pairs(Boston,pch="16")

### scatterplots with different variable combinations
plot(Boston$zn,Boston$indus,xlab = "residential land zoned",ylab ="non retail business acres per town")
plot(Boston$chas,Boston$indus,xlab = "charles river",ylab ="non retail business acres per town")
plot(Boston$nox,Boston$indus,xlab = "nitrogen oxides concentration",ylab ="non retail business acres per town")
plot(Boston$rm,Boston$crim,xlab = "rooms per dwelling",ylab ="Crime rate")
plot(Boston$dis,Boston$indus,xlab = "distances to five boston employement centres",ylab ="non retail business acres per town")
plot(Boston$black,Boston$crim,xlab = "Proportion of blacks",ylab ="Crime rate")
# in log scale
plot(Boston$black,log(Boston$crim),xlab = "Proportion of blacks",ylab ="Crime rate(log scale)")

#### correlation plot
install.packages("corrplot")
library(corrplot)

corboston<-cor(Boston)
head(corboston)
corrplot(corboston) ## from this plot we can say that most variables look correlated

### 3(b)

## from linear model
crime_model<-lm(crim~zn+nox+dis+rad+ptratio+black+lstat+medv,data = Boston)
summary(crime_model)

plot(Boston$zn,Boston$crim,xlab = "Residential lands zoned",ylab ="Crime rate")
# from the above plot we can say that crime rates are higher where residential lands are not zoned

plot(Boston$nox,Boston$crim,xlab = "nitrous oxide concentration",ylab ="Crime rate")
# nitrous oxide concentraion affect crime rates

plot(Boston$dis,Boston$crim,xlab = "distance to employment centres",ylab ="Crime rate")
# crime rates are higher at areaas closer to employment centres

plot(Boston$rad,Boston$crim,xlab = "accessibilty to radial highways",ylab="Crime rate")

plot(Boston$medv,Boston$crim,xlab = "median values of owner occupied homes",ylab="Crime rate")


### 3(C)

summary(Boston$crim)
boxplot(Boston$crim,horizontal = T)
length(which(Boston$crim>35)) ## threshold taken based on boxplot

boxplot(Boston$tax)
plot(Boston$tax)
summary(Boston$tax)
length(which(Boston$tax>666))## based on 3rd quartile

boxplot(Boston$ptratio)
length(which(Boston$ptratio<14))## based on boxplot

### 3(d)
length(which(Boston$rm>7))
length(which(Boston$rm>8))
roomsgt8<-Boston[Boston$rm>8,]

hist(roomsgt8$age) #### most houses that have >8 rooms are built prior to 1940
hist(roomsgt8$medv)#### most houses that have >8 rooms have higher median value

########################### QUESTION 4 ###########################

install.packages("ElemStatLearn")
library(ElemStatLearn)

head(zip.train)

data("zip.train")
data("zip.test")

unique(zip.train[,1])

traindata<-zip.train[which(zip.train[,1]==2),]
traindata1<-zip.train[which(zip.train[,1]==3),]

finaltrain<-rbind(traindata,traindata1)
finaltrain<-as.data.frame(finaltrain)

unique(finaltrain[,1])

### testdata preprocessing
testdata<-zip.test[which(zip.test[,1]==2),]
testdata1<-zip.test[which(zip.test[,1]==3),]

finaltest<-rbind(testdata,testdata1)
unique(finaltest[,1])

finaltest<-as.data.frame(finaltest)

##### building linear model

lm_train<-lm(finaltrain$V1~.,data = finaltrain)
summary(lm_train)

# predicting on test data
finaltest_noV1<-finaltest[,-1]

lm_predictions<-predict(lm_train,finaltest_noV1)

difference<-lm_predictions-finaltest$V1
misclassificationrate_lm<-mean(difference^2)

##### building knn model
install.packages("class")
library(class)

kvalues<-c(1,3,5,7,9,11,13,15)
errorvec<-c()

for(i in kvalues){
knn_model<-knn(finaltrain[,-1],finaltest_noV1,finaltrain$V1,k=i)

difference_knn<-as.numeric(knn_model)-finaltest$V1
testerror<-mean(difference_knn^2)
errorvec<-c(errorvec,testerror)
}

errorvec

plot(errorvec, type="b")
abline(h=misclassificationrate_lm)












