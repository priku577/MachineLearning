library(mgcv)
library(ggplot2)
##1
influenza<-read.csv("/home/george/Documents/732A95/lab3/Influenza.csv", sep=";",dec=",")
ggplot(influenza, aes(x=Time)) +
  geom_line(aes(y = Mortality, colour = "Mortality")) +
  geom_line(aes(y = Influenza, colour = "Influenza"))+
  scale_colour_manual(values=c("red", "blue"))+ggtitle("Mortality and Influenza vs Time")

 
##2
#Gam model
model1 <- gam(Mortality ~ Year + s(Week,k= 52), data = influenza,method="GCV.Cp")
model1$coefficients

##3
#Plot of the observed and fitted values
model1 <- gam(Mortality ~ Year + s(Week,k= 52), data = influenza,method="GCV.Cp")
plot(influenza$Time, influenza$Mortality, type='l',col="green" ,xlab="Time", ylab="Mortality",main = "Graph of Observed and Fitted values")
     points(influenza$Time, model1$fitted.values, type='l', col="red")
     legend("topright", c("observed", "fitted"), col=c("green","red"), lwd=1, cex=0.8)
     
#Summary of the model
summary(model1)
plot(model1)
     ##4
     
#Adding different penalty factor
fit.sp1 <- gam(Mortality ~ Year + s(Week,k=52, sp=100), data=influenza)
fit.sp2 <- gam(Mortality ~ Year + s(Week,k=52, sp=.01), data=influenza)
plot(x=influenza$Time, y=influenza$Mortality, type='l', xlab="Time", ylab="Mortality", lwd=1)
points(x=influenza$Time, y=fit.sp1$fitted.values, type='l', col="purple", lwd=1)
points(x=influenza$Time, y=fit.sp2$fitted.values, type='l', col="red", lwd=1)
legend("topright", c("observed","pf=100","pf=0.01"), col=c("black","purple","red"),lwd=1, cex=0.8)

summary(fit.sp2)
     
##5.

##Residual plot
plot(x=influenza$Time, y=model1$residuals, type='l', xlab="Time", col="black",main = "Plot of influen")
points(influenza$Time, influenza$Influenza, type='l', col='red')
legend("topright", c("residuals","influenza values"), col=c("black","red"), lwd=1)

##6.
#additive components
model2 <- gam(Mortality ~ s(Influenza,k =85) + s(Year,k=9) + s(Week,k=52), data = influenza)
summary(model2)
plot(x=influenza$Time, y=influenza$Mortality, type='l', xlab="Time", ylab="Mortality", main = "Plot of")
points(x=influenza$Time, y=model2$fitted.values, type='l', col="purple")
legend("topright", c("observed","fitted"), col=c("black","purple"), lwd=1)