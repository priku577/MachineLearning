library(ggplot2)
library(MASS)
data<-read.csv("/home/george/Documents/732A95/lab2/australian-crabs.csv", sep=",", dec=".")
p <- ggplot(data, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) +
  scale_color_manual (values = c('blue', 'red')) +
  labs(x="CL carspace length", y="RW rear Width", colour="Classes")
p
X<- data.frame(RW=data$RW , CL=data$CW )
Y <- data$sex
disc_fun=function(label, S)
{
  #label for Male and Female are stored in X1
  X1=X[Y==label,]
  # claculating the mean RW and CL for X1
  mean_value <- c(mean(X1$RW) ,mean(X1$CL))
  #inverse of covariance matrix
  inverse_covariance <- solve(S)
  # Calculating prior probability Nc/N
  prior_prob <- nrow(X1) / nrow(X)
  #calculating w1
  w1 <- inverse_covariance %*% mean_value
  #calculating w0
  b1 <- ((-1/2) %*% t(mean_value) %*% inverse_covariance %*% mean_value) + log(prior_prob)
  w1<- as.vector(w1)
  return(c(w1[1], w1[2], b1[1,1]))
}
X1=X[Y=="Male",]
X2=X[Y=="Female",]
#Covariance Matrix
S=cov(X1)*dim(X1)[1]+cov(X2)*dim(X2)[1]
S=S/dim(X)[1]
#discriminant function coefficients
res1=disc_fun("Male",S)
res2=disc_fun("Female",S)
#decision boundary coefficients 'res'
res <- c( -(res1[1]-res2[1]) , (res2[2]-res1[2]), (res2[3]-res1[3]))
# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)
#plot(X[,1], X[,2], col=Yfit+1, xlab="CL", ylab="RW")
#slope and intercept
slope <- (res[2] / res[1] ) * -1
intercept <- res[3] /res[1] * -1
#1.3
cat("Equation of the decision boundary:" , intercept, "+", slope, "* k\n")
#plot decision boundary
X<- cbind(X,sex=Y)
p <- ggplot(X, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) +
  scale_color_manual (values = c('blue', 'red')) +
  labs(x="CL-carspace length", y="RW-rear Width", colour="Classes") +
  geom_abline(slope = slope, intercept = intercept)
p
#1.4

##Logistic regression 
glm1 <- glm(sex ~ CL + RW,family=binomial(link="logit"), data=data)
slope1 <- -(glm1$coefficients[2] / glm1$coefficients[3] )
intercept1 <- -(glm1$coefficients[1] /glm1$coefficients[3] )
print(qplot(
  x =data$CL,
  y = data$RW,
  data = data,
  color = data$sex ,
  main="CL vs RW",
  xlab="Carapace Length", ylab = "Rear Width")
  +geom_abline(slope = slope1, intercept = intercept1,colour='purple')+ggtitle("CL Vs RW in Logistic Regression"))
cat("Decision boundary with linear regression:",slope1, "+",intercept1, "* k\n")