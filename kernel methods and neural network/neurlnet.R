set.seed(1234567890)
library(neuralnet)

#Data seperation
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

#plot of sine curve based on original data
plot(trva,main="Main sine plot", xlab="Var", ylab="Sine wave")

# Random initializaiton of the weights in the interval [-1, 1]
winit <- runif(50,-1,1)
error = c()
for(i in 1:10) {
  nn <- neuralnet(Sin ~ Var, data=tr, hidden = 10,
                  threshold = i/1000 ,startweights = winit)
  y_k = compute(nn, va$Var)$net.result 
  error[i] = 0.5*sum((y_k - va$Sin)^2)
}

optimal = which.min(error)
nn <- neuralnet(Sin ~ Var , data=trva, hidden = 10, 
                threshold = optimal/1000, startweights = winit)
plot(nn,rep="best")

# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1,main="Comparison of prediction and 
original data", col="Black")
points(trva, col = "red")