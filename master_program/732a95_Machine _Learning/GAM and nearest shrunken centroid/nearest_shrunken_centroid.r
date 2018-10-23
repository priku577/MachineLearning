library(pamr)
datam<-read.csv("/home/george/Documents/732A95/lab3/data.csv",stringsAsFactors = FALSE, sep=";", fileEncoding = 'WINDOWS-1252')
data=datam
data=as.data.frame(data)
data$Conference=as.factor(datam$Conference)
rownames(data)=1:nrow(data)
#dividing the data into 70/30 (training/testing)
n=dim(data)[1]
set.seed(12345)
trainIndex=sample(1:n, floor(n*.7))
train_data=data[trainIndex,]
test=data[-trainIndex,]
x=t(train_data[,-ncol(train_data)])
x_test=t(test[,-ncol(test)])
y=train_data$Conference
#making the pamr model
mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)),genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0,4, 0.1))
##doing cross vaildation to find the threshhold
cvmodel=pamr.cv(model,mydata)
##tr is the threshold
tr <- cvmodel$threshold[which.min(cvmodel$error)]
#selecting the features
features = pamr.listgenes(model, mydata, threshold = tr, genenames=TRUE)
cat( paste( colnames(train_data)[as.numeric(features[1:10,1])], collapse='\n' ) )
#pamr.plotcv(cvmodel)


##finding the centroid plot
pamr.plotcen(model, mydata, threshold=tr)
best_model <- pamr.train(mydata, threshold = tr)

##finding the test error
pred_centroid <- pamr.predict(fit = best_model, newx = as.matrix(test),
                              threshold = tr, type = "centroid")
tabl_cenroid<-pamr.confusion(fit = model, threshold = tr, extra = TRUE)

####Elastic net
library(glmnet)
##selecting the penalty by corss validation
glmnet_cv<-cv.glmnet(x=t(x),y=y,family="binomial",alpha=0.5)
##plot(glmnet_cv)
penalt<-glmnet_cv$lambda.min
elas_model<-glmnet(x=t(x),y=y,family = "binomial",lambda = penalt,alpha = 0.5)

##Finding the test error
features_selected <- length(which(coef(elas_model)!=0))
pred<-predict(elas_model,newx=t(x_test),type="class")
tabl_elsnet<-table(pred,test$Conference)
misclas<-1-(sum(diag(tabl_elsnet))/sum(tabl_elsnet))
misclas

####using SVM

library(kernlab)
svm_model<-ksvm(Conference ~.,data=train_data,kernel="vanilladot")
features_svm <- length(coef(svm_model)[[1]])
features_svm

##Finding the test error
pred2<-predict(svm_model,t(x_test))
svm_tabl<-table(prediction=pred2,test$Conference)
misclas2<-1-(sum(diag(svm_tabl))/sum(svm_tabl))
cat("missclassifaction rate for test data using SVM",misclas2)

###Benjamini Hockberg Method

p_value <- integer(0)
names_data <- names(data)
for(i in 1:(length(names_data) - 1)){
  formula <- as.formula(paste(names_data[i], "~ Conference"))
  t <- t.test(formula = formula, data = data, alternative = "two.sided")
  p_value[i] <- t$p.value
}
ordered_pval <- p_value[order(p_value)]
#plot(ordered_pval)

#test
alpha <- 0.05
test_val <- alpha*1:(length(names_data) - 1)/(length(names_data) - 1)
val <- ifelse(ordered_pval < test_val, ordered_pval, NA)
res <- data.frame(names=names_data[order(p_value)], pvalues = ordered_pval, decision = val)
res[1:39,]






