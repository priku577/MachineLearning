data<-read.csv("/home/george/Documents/732A95/lab1_block2/spambase.csv", sep=";", dec=",")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.75))
train=data[id,]
test=data[-id,]
library(mboost)
set.seed(1234)
#no of trees 10,20...,100
trees=seq(10,100,by=10)
adaBoost<-c()
misclassification_ada<-c()

#loop over the lenght of tree for adaboost
for(i in 1:length(trees))
{
  adaBoost[[i]] <- blackboost(as.factor(Spam) ~ .,
                              data = train,
                              family = AdaExp(),
                              control = boost_control(trees[i]))
  ada_test <- predict(adaBoost[[i]], newdata=test, type='class')
  #confusion matrix
  confusion_mat_ada<-table(ada_test,as.factor(test$Spam))
  #finding the misclassification
  misclassification_ada[i]<- 1-(sum(diag(confusion_mat_ada)))/sum(confusion_mat_ada)
}
misclassification_ada

##Random forest
library(randomForest)
##random forest train values for different number of trees
rf_train<-c()
misclassification_rf<-c()
for(i in 1:length(trees))
{
  rf_train[[i]] <- randomForest(as.factor(Spam) ~ .,
                                data = train,ntree=trees[i])
  rf_test <- predict(rf_train[[i]], newdata=test, type='class')
  #confusion matrix
  confusion_mat_rf<-table(rf_test,as.factor(test$Spam))
  #misclassification rate
  misclassification_rf[i]<- 1-(sum(diag(confusion_mat_rf)))/sum(confusion_mat_rf)
}
misclassification_rf
#Ploting data
plot(y=misclassification_ada,x=trees,type="l",col="red",ylim=c(0,0.15),ylab="misclassification")
lines(y=misclassification_rf,x=trees,type="l",col="blue",ylim=c(0,0.15))
legend("topright", legend=c("adaboost", "randomforest"),
       col=c("red", "blue"), lty=1:2, cex=0.8)