#Read the data
credit_scoring<-read.csv("/home/george/Documents/732A95/lab2/creditscoring.csv", sep=",")
#dividing data into training,testing and validation
n=dim(credit_scoring)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
dfTraining=credit_scoring[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
dfValidation=credit_scoring[id2,]
id3=setdiff(id1,id2)
dfTest=credit_scoring[id3,]

#making tree using deviance as impurity measure
library(tree)
set.seed(12345)
tree1 <- tree(good_bad ~ .,data=dfTraining,split = "deviance")
#prediction with the test data
tree_pred<-predict(tree1,dfTest,type="class")
#miscalssification using mean method..also can be done using confusion matrix
cat("missclassification rate for deviance_test:", mean(tree_pred!=dfTest$good_bad))

#Gini Index
tree1_gini<- tree(good_bad ~ .,data=dfTraining,split = "gini")
tree_pred_gini<-predict(tree1_gini,dfTest,type="class")
conftab_train<-table(tree_pred_gini,dfTest$good_bad)
mis<-1-(sum(diag(conftab_train))/sum(conftab_train))
mis

cat("missclassification rate for gini_test:", mean(tree_pred_gini!=dfTest$good_bad))

#prediction using train data
tree_pred_train<-predict(tree1,dfTraining,type="class")
cat("missclassification rate for train_deviance:", mean(tree_pred_train!=dfTraining$good_bad))

tree_pred_train<-predict(tree1_gini,dfTraining,type="class")
cat("missclassification rate for train_gini:", mean(tree_pred_train!=dfTraining$good_bad))

set.seed(1235)
library(ggplot2)
classifier<- tree(good_bad ~ .,data = dfTraining, split = "deviance" )
y_pred<- predict(classifier , newdata = dfTest,type = "class")

text(classifier)
n <-summary(classifier)[4]$size
possible_leaves <- seq(2,n,1)
train_deviance <- c()
validation_deviance <- c()

#looping over all the n values to finally obtain the optimal tree depth
    for(i in 2:n)
      {
      prunedTree <- prune.tree(classifier, best=i)
      pred1 <- predict(prunedTree, newdata=dfValidation, type="tree",na.rm=TRUE)
      pred2 <- predict(prunedTree, newdata=dfTraining, type="tree",na.rm=TRUE)
      train_deviance[i] <- deviance(pred2,na.rm=TRUE)
      validation_deviance[i] <- deviance(pred1,na.rm=TRUE)
      }
data <- data.frame(Leaves=vector("numeric"),
                   deviance=vector("numeric"))
for(i in 2:n)
      {
        data[i,"Leaves"] <- i
        data[i,"deviance"] <- train_deviance[i]
        data[i,"set"] <- "train"
      }

for(i in (n+1):(2*n))
      {
        data[i,"Leaves"] <- i-n
        data[i,"deviance"] <- validation_deviance[i-n]
        data[i,"set"] <- "validation"
        
      }

ggplot(data = data ,aes(x = Leaves , y = deviance , col = set)) + geom_line()
#optimal tree
optimal_tree <- prune.tree(classifier, best=4)
plot(optimal_tree)
text(optimal_tree, pretty=0)

library(e1071)
model <- naiveBayes(good_bad ~ ., data = dfTraining)
f1<-predict(object=model,newdata=dfTest)
f2<-predict(object=model,newdata = dfTraining)

conf_matr_train<-table(f2,dfTraining$good_bad)
conf_matr_test<-table(f1,dfTest$good_bad)

#table(f1,model)
cat("missclassification rate for navie train data:", mean(f1!=dfTraining$good_bad))
cat("missclassification rate for navie test data:", mean(f1!=dfTest$good_bad))


# loss_matrix<-matrix(c(0,10,1,0),ncol=2)
set.seed(12345)
model2 <- naiveBayes(good_bad~.,data=dfTraining)
f1_1<-predict(model2,newdata=dfTest,type="raw")
a<-ifelse(f1_1[,2]/f1_1[,1] >10, 1, 0)
tab_test<-table(dfTest$good_bad,a)
accuracy_loss <- (sum(diag(tab_test)))/sum(tab_test)
mis_loss<-1-accuracy_loss
cat("missclassification rate for deviance_test:", mis_loss)
