#Linear Regression
##Required Libraries
library(quantmod)
library(tseries)
library(forecast)
library(MASS)
library(ggplot2)
start <- as.Date("2017-01-01")
end<-as.Date("2018-02-14")
t=getSymbols("AAPL", src = "yahoo", from = start ,to=end)
AAPL=AAPL[-281,]
n=dim(AAPL)[1]
set.seed(12345)
trainIndex=sample(1:n, floor(n*.5))
train_data=AAPL[trainIndex,]
test=AAPL[-trainIndex,]
##making the linear model
model=lm(AAPL.Close~AAPL.Open,train_data)
##making a prediction
prediction=predict(model,newx=test)
MSE<-mean(((test[,"AAPL.Close"])-prediction)^2)
cat("The mean square error using linear regression is: ",MSE)

#First 10 rows of data
head(AAPL)
#Time series plot for stock data
ts.plot(AAPL,main="Time Series plot for APPLE data")
#candle chart 
candleChart(AAPL, up.col = "black", dn.col = "red", 
            theme = "white",main="Candle Chart for APPLE stock data")

#adding moving average as 20 to candle chart
candleChart(AAPL, up.col = "black", dn.col = "red", 
            theme = "white", subset = "2016-01-04/",main="Moving Average for 20 days")
addSMA(n = 20)

####Arima model

#divinding data into 80 for training and 20% for testing
stock<-AAPL[-281,]
#dividing the data into 50/50 (training/testing)
n=dim(stock)[1]
set.seed(1234)
trainIndex=sample(1:n, floor(n*.80))
train_data=stock[trainIndex,]
test=stock[-trainIndex,]
lnstock=log(train_data[,"AAPL.Close"])
#Dickey Fuller Test
adf.test(lnstock)
#Difference of stack
difflnstock<-diff(lnstock,1)
difflnstock = difflnstock[!is.na(difflnstock)]
ts.plot(difflnstock,main="Stationary data for APPLE stock")
#PACF and ACF correlation
acf(difflnstock,lag.max=20,main="ACF PLOT")
pacf(difflnstock,lag.max=20,main="PACF PLOT")

# Finding Arima model using auto.arima
pricearma<-ts(exp(lnstock),start=c(2009,06), frequency = 24 )
model<-auto.arima(pricearma)
summary(model)

##Forecasting
forecast1<-forecast(model,h=56)
autoplot(forecast1)

pricearma<-ts(lnstock,start=c(2009,06), frequency = 24 )
model1<-auto.arima(pricearma)

#exponential of lnstock

lnexp<-exp(lnstock)

#forecasting
forecast2<-forecast(model1,h=56)

#mean of forecasted
m<-as.numeric(forecast2$mean)
eexpm<-exp(m)

#error

data1<-data.frame(test[,"AAPL.Close"],eexpm)
colnames(data1)<-c('ActualPrice','ForecastPrice')
data1[14:20,]

##forecast error
MSE<-(data1$Actual-data1$Forecast)/(data1$Actual)
MSE_error<-mean(MSE)
cat("The Mean Percentage error with Arima model is",MSE_error)
