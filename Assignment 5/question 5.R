#install.packages("alr3")
library(alr3)

data("banknote")
View(banknote)
bank=banknote

head(bank)
summary(bank) 

head(bank[,-7])
pairs(bank[,-7], col = bank$Y, upper.panel = NULL, pch = 16, cex = 0.5)

## PCA for all 200 notes
bank.pca <- princomp(bank[,-7]) 
summary(bank.pca) # summary givs the eigenvalues of covariance matrix 

loadings(bank.pca)# loadings shows the eigenvectors


bank.pc <- predict(bank.pca) # here predict() computes data in new coordinates
head(bank.pc)

bank.true <- factor(c(rep("t",100),rep("f",100)))

bank.pc[,1]

#distinguishing between true and false banknotes using the first two ORIGINAL components

plot(bank[,1],bank[,2],type="n",xlab="first variable", ylab= "second variable") 
text(bank, labels=as.character(bank.true),col=as.integer(bank.true))


#distinguishing between true and false banknotes using the first two PRINCIPLE components
plot(bank.pc[,1],bank.pc[,2],type='n',xlab="first princ. comp.", ylab= "second princ. comp.",main = "All 200 notes") 
text(bank.pc, labels=as.character(bank.true),col=as.integer(bank.true))  


## separating 100 genuine notes

bank_genuine<-bank[which(bank$Y==0),]   ## in the help it says 0 is genuine and 1 is counterfeit

bank_gen.pca <- princomp(bank_genuine[,-7]) 

summary(bank_gen.pca) 

loadings(bank_gen.pca)

bank_gen.pc <- predict(bank_gen.pca,bank[,-7])

plot(bank_gen.pc[,1],bank_gen.pc[,2],type='n',xlab="first princ. comp.", ylab= "second princ. comp.",main = "using genuine notes only") 
text(bank_gen.pc, labels=as.character(bank.true),col=as.integer(bank.true))  


## separating 100  counterfeit notes

bank_counterfeit<-bank[which(bank$Y==1),]   
bank_cou.pca <- princomp(bank_counterfeit[,-7]) 
summary(bank_cou.pca) 

loadings(bank_cou.pca)
bank_cou.pc <- predict(bank_cou.pca)

plot(bank_cou.pc[,1],bank_cou.pc[,2],type='n',xlab="first princ. comp.", ylab= "second princ. comp.",main = "using counterfeit notes only") 
text(bank_cou.pc, labels=as.character(bank.true),col=as.integer(bank.true))  













