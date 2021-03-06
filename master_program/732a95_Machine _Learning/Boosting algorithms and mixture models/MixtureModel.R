#Mixture models
set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data   #matrix 100x10
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions  #matrix 3x10
true_pi=c(1/3, 1/3,1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)

plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")

# Producing the training data   #prob = 1 and 0
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}

K=2 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}

for(it in 1:max_it) {
   px <- integer(N)
  
for(n in 1:1000){
   probSum <- 0
   for(k in 1:K){
   probSum <- probSum + pi[k]*prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))
     }
      px[n] <- probSum 
    }
    
  for(n in 1:1000){
    for(k in 1:K){
       z[n,k]<- (pi[k]*prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,])))/px[n]
    }
  }
  
  #Log likelihood computation. #llik

  logL <- matrix(0, nrow = 1000, ncol = K)
  for(n in 1:1000){
    for(k in 1:K){
      logL[n,k] <- (pi[k]*(prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))))
      
    }
  llik[it] <- sum(log(rowSums(logL)))
  }
 
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  pi<-colSums(z)/N 

  mu<-(t(z)%*%x)/colSums(z)
  }

 

plot(mu[1,], type="o", col="blue", ylim=c(0,1),xlab ="Variable",ylab = "mu")
points(mu[2,], type="o", col="red")
plot(llik[1:it], type="o",main="EM Algorithm for Bernoulli Distribution K=2",
xlab = "Iteration",ylab = "Observed Data Log Likelihood")

#-----------------------------------------------------------
K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}

for(it in 1:max_it) {
   px <- integer(N)
  
  for(n in 1:1000){
    probSum <- 0
    for(k in 1:K){
      probSum <- probSum + pi[k]*prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))
    }
    px[n] <- probSum 
  }
  
  for(n in 1:1000){
    for(k in 1:K){
      z[n,k]<- (pi[k]*prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,])))/px[n]
    }
  }
  
  #Log likelihood computation. #llik

  logL <- matrix(0, nrow = 1000, ncol = K)
  for(n in 1:1000){
    for(k in 1:K){
      logL[n,k] <- (pi[k]*(prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))))
      
    }
    llik[it] <- sum(log(rowSums(logL)))
  }
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
    pi<-colSums(z)/N 
  
  mu<-(t(z)%*%x)/colSums(z)
}



plot(mu[1,], type="o", col="blue", ylim=c(0,1),xlab ="Variable",ylab = "mu")
points(mu[2,], type="o", col="red")
points(mu[3,], type="o", col="green")
plot(llik[1:it], type="o",main="EM Algorithm for Bernoulli Distribution K=3",
     xlab = "Iteration",ylab = "Observed Data Log Likelihood")

#-------------------------------------------------
 

K=4 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}

for(it in 1:max_it) {
  px <- integer(N)
  
  for(n in 1:1000){
    probSum <- 0
    for(k in 1:K){
      probSum <- probSum + pi[k]*prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))
    }
    px[n] <- probSum 
  }
  
  for(n in 1:1000){
    for(k in 1:K){
      z[n,k]<- (pi[k]*prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,])))/px[n]
    }
  }
  
  #Log likelihood computation. #llik

  logL <- matrix(0, nrow = 1000, ncol = K)
  for(n in 1:1000){
    for(k in 1:K){
      logL[n,k] <- (pi[k]*(prod((mu[k,]^x[n,])*(1-mu[k,])^(1-x[n,]))))
      
    }
    llik[it] <- sum(log(rowSums(logL)))
  }
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
 pi<-colSums(z)/N 
  
  mu<-(t(z)%*%x)/colSums(z)
}



plot(mu[1,], type="o", col="blue", ylim=c(0,1),xlab ="Variable",ylab = "mu")
points(mu[2,], type="o", col="red")
points(mu[3,], type="o", col="green")
points(mu[4,], type="o", col="yellow")
plot(llik[1:it], type="o",main="EM Algorithm for Bernoulli Distribution K=4",
     xlab = "Iteration",ylab = "Observed Data Log Likelihood")

