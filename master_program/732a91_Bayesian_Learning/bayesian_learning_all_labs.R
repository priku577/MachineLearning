
#####################Lab1#########################

n = 20; s = 14; f = 6
alpha0 = 2; beta0 = 2
drawMean = c()
drawSd = c()

##simulation 
for (i in 1:10000){
  draw <- rbeta(i, shape1 = alpha0 + s, shape2 = beta0 + f)
  drawMean[i] <- mean(draw)
  drawSd[i] <- sd(draw)
}
#true mean and sd
trueMean <- (alpha0+s)/(alpha0+s+beta0+f)
trueSd <- sqrt(((alpha0+s)*(beta0+f))/(((alpha0+s+beta0+f)^2)*(alpha0+s+beta0+f+1)))
cat("The true mean:", trueMean, "\n")
cat("The standard deviation:", trueSd, "\n")

#Plot of posterior mean
n = length(drawMean)
plot(x = 1:n,y = drawMean , type = "l", col = "blue",
     xlab = "Number of draws", ylab = "Number of samples drawn",
     main = "True mean vs Sample means")
lines(x = 1:n, y = rep(trueMean,n), col = "red")
#Plot of posterior sd
n = length(drawSd)
plot(x = 1:n,y = drawSd , type = "l", col = "red",
     xlab = "Number of draws", ylab = "Number of samples drawn",
     main = "True standard deviation vs Sample standard deviations")
lines(x = 1:n, y = rep(trueSd,n), col = "black")
#(b)
#Draw 10000 samples
simulated_data <- rbeta(10000, shape1 = alpha0 + s, shape2 = beta0 + f)
#Calculate probability p(theta < 0.4|y) which is 0.0047
prob_lessthan_0.4 <- simulated_data[simulated_data < 0.4]
require_prob <- length(prob_lessthan_0.4)/length(simulated_data)
require_prob
7
#Exact value which is 0.003972681
exact_prob <- pbeta(0.4, shape1 = alpha0 + s, shape2 = beta0 + f)
exact_prob
#(c)
log_odds <- log(simulated_data/(1 - simulated_data))
hist(log_odds, breaks = 100, probability = TRUE, main = "Histogram and density of log-odds distribution")
lines(density(log_odds), col = "blue", lwd = 2)
#####Question 2#####
library(geoR)
#(a)
data <- c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
dataMean <- mean(data)
dataVar <- var(data)
n = length(data)
df = 10
x= seq(0, 10000, 1)
#Simulate inverse chisquare
tau_sq <- sum((log(data)-3.5)^2)/n
inv_chi_sq <- rinvchisq(10000, df = 10, scale = tau_sq) ##Simulated signma^2
#Plot simulated with theoretical inverse chisquare
hist(inv_chi_sq, breaks = 100, probability = TRUE,
     xlab = expression(paste(sigma^2)),
     main = "Simulated data and theoretical density of inverse-chi-square")
curve(dinvchisq(x, df=10, scale = tau_sq),
      add = TRUE,col = "blue", lwd = 2) ##Theoretical inverse chisquare
legend(1,3, legend = c("Simulated", "Theoretical"), col = c("black", "blue"), lty = 1:1)
###Another way to do the same


set.seed(123)
yi <- c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
mu <- 3.5
n <- 10000
df <- 10
sample_chi <- rchisq(n, df)
tau2 <- sum((log(yi) - mu)^2) / df
sigma2 <- (df * tau2) / sample_chi
hist(sigma2, breaks = 100, freq = FALSE,
     col = "grey",
     xlim = c(0, 1), ylim = c(0, 5),
     main = "Sampled inverse-chi-squared distribution")
lines(density(sigma2), lwd = 2, col = "red")

#(b)
sigma_sq <- inv_chi_sq
sigma <- sqrt(sigma_sq)
phi <- pnorm((sigma/sqrt(2)), 0, 1)
G_coef <- 2*phi-1
hist(G_coef, breaks = 100, probability = TRUE,
     main = "Posterior distribution of the Gini coefficient")
lines(density(G_coef), col = "blue", lwd = 2)
#(c)
#The HPD has the nice property that any point **within**
#the interval has a higher density than any other point outside.
#Thus, HPD interval is **the** collection of most likely values of
#the parameters.
#Ref https://www.r-bloggers.com/probable-points-and-credible-intervals-part-1/
#Compute a 95% equal tail credible interval for G.
credible_interval <- quantile(G_coef, probs = c(0.025, 0.975) )
credible_interval
8
# 2.5% 97.5%
#0.1748642 0.4220841
#compute a 95% Highest Posterior Density interval for G.
library(HDInterval)
kernel_density <- density(G_coef)
highest_density_interval <- hdi(kernel_density, credMass = 0.95)
highest_density_interval


####another way

sorted_gini <- sort(gini, decreasing = FALSE)
lowerbound <- sorted_gini[0.025*10000]
upperbound <- sorted_gini[0.975*10000]

###yet anotherway
quantile(gini, c(0.025, 0.975))



HPD<-function()
{
  gdensity<-density(G_coef)
  gcdf<-gdensity$y/sum(gdensity$y)
  ind<-order(gcdf,decreasing = TRUE)
  i<-0
  Xs<-c()
  while(i<0.95)
  {
    i<-i+max(gcdf)
    sel<-which.max(gcdf)
    gcdf<-gcdf[-sel]
    Xs<-c(Xs,ind[1])
    ind<-ind[-1]
  }
  return(Xs)
}

gdensity<-density(G_coef)
lowerbound<-min(gdensity$x[HPD()])
upperbound<-max(gdensity$x[HPD()])
#lower upper
#0.1606569 0.3963481
#Comparing two intervals
#The highest density interval slightly skewed to the lelf, the intervals
#quite similar



#####Question 3#####
#(a)
#record_degree <- c(40, 303, 326, 285, 296, 314, 20, 308, 299, 296)
record_radian <- c(-2.44, 2.14, 2.54, 1.82, 2.02, 2.33, -2.79, 2.23, 2.07, 2.02)
posterior <- function(k){
  bessel_0 <- besselI(k, nu = 0)
  loglike <- prod(exp(k*cos(record_radian - 2.39)) / (2*pi*bessel_0))
  prior <- exp(-k)
  pos <- loglike * prior
  return(pos)
}
#Apply posterior to vector of k
k = seq(0, 10, 0.1)
res <- sapply(seq(0, 10, 0.1), posterior)
plot(x=k, y=res, type = "l", col = "blue", lwd = 2,
     xlab = expression(paste(kappa)), ylab = "posterior",
     main = "Posterior distribution of k")
#(b)
k[which.max(res)]
#Approximate posterior mode of k is 2.1



##################Lab2################################################

##we can set v0 as n-1 degrees of freedom
temp<-read.table("TempLinkoping.txt",header=TRUE)
temp.lm<-lm(temp$temp ~ temp$time + I(temp$time^2))
summary(temp.lm)
plot(temp$time,temp$temp)
lines(temp$time,fitted(temp.lm),col="red")
library(mvtnorm)
library(MASS)
#a)
sigma_sq_0 = 2
X<-cbind(1,temp$time,temp$time^2)
Y<-temp$temp
beta_hat<- solve(t(X)%*%(X)) %*% (t(X)%*%(Y))
omega_0 = diag(3)
v_0 <- 25
mu_0 = c(-10,93,-85)
prior <-c()
n=366
library(geoR)
#b)
nDraws = 100
resultlis <- list()
for (i in 1:nDraws)
{
  sigma_sqr_prior <- rinvchisq(1, df=v_0, scale=sigma_sq_0)
  variance_prior = sigma_sqr_prior * solve(omega_0)
  prior = mvrnorm(1,mu = mu_0 , Sigma = variance_prior)
  resultlis[[i]] = prior
  prior_model = prior[1] + prior[2]*temp$time + prior[3]*temp$time^2
  lines(temp$time,prior_model,col="blue")
}
#c) posterior
nDraws = 100
v_n<-v_0+n
mu_n<-solve((t(X)%*%X)+omega_0)%*%(((t(X)%*%X)%*%beta_hat)+
                                     (omega_0 %*%mu_0))
omega_n<-(t(X)%*%X)+omega_0
sigma_sq_n<-(1/v_n)*((v_0*sigma_sq_0)+((t(Y)%*%Y)+(t(mu_0)
                                                   %*%omega_0%*%mu_0)-(t(mu_n)%*%omega_n%*%mu_n)))
diagnal_elements = as.numeric(diag(1/omega_n))
omega_inv = diag(diagnal_elements)
posterior <- list()
x_bar<-c()
sigma_sqr_post<-c()
plot(temp$time,temp$temp)
7
for (i in 1:nDraws)
{
  sigma_sqr_post[i] <- rinvchisq(1, df=v_n, scale=sigma_sq_n)
  variance_post = sigma_sqr_post[i] * omega_inv
  post = mvrnorm(1,mu = mu_n , Sigma =variance_post)
  post_model = post[1] + post[2]*temp$time + post[3]*temp$time^2
  x_bar[i]<- -(post[2]/(2*post[3]))
  posterior[[i]] = post_model
}
posterior_beta <- as.data.frame(do.call("rbind", posterior))
lower<-c()
upper<-c()

##90% equal tail  .05 and .95
for (i in 1:n){
  lower[i] <- quantile(as.vector(posterior_beta[,i]), probs = c(0.025, 0.975))[1]
  upper[i] <- quantile(as.vector(posterior_beta[,i]), probs = c(0.025, 0.975))[2]
}
posterior_mean = colMeans(posterior_beta)
plot(x=temp$time,y=temp$temp, main = "Posterior Credible interval" )
lines(x=temp$time,y = posterior_mean,col = "red")
lines(x=temp$time,y = upper, col = "green")
lines(x=temp$time,y = lower,col = "green")
#d)
cat("The time with the highest expected temperature is :",(max(x_bar)))
hist(x_bar,main = "Simulations of time with the highest expected temperature")
#e)
omega_0 = diag(c(1,1,1,999,999,999,999,999))
mu_0 = c(-10,93,-85,0,0,0,0,0)
#2.a
WomenWork=read.table("WomenWork.dat",header=TRUE)
library(mvtnorm)
####2.2
# y is response variable which is work
y<-as.vector(WomenWork[,1])
# other 8 variables are in X
X<-as.matrix(WomenWork[,2:9])
no_of_Para<-ncol(X)
#given that tau =10
tau<-10
mu_0<-as.vector(rep(0,no_of_Para))
sigma_0 <- tau^2*diag(no_of_Para)
##prior function
prior_prob <- function(beta_values,sigma_0) {
  mean1=matrix(rep(0,8),ncol = 1)
  prior <- dmvnorm(beta_values, mean= mean1, sigma=sigma_0, log=TRUE)
  return(prior)
}
8
##likelyhood function
like_prob <- function(beta_values, y, X) {
  Logist_Par <- X%*%beta_values
  exp_part<-1 + exp(Logist_Par)
  log_liklihood <- sum(Logist_Par*y - log(exp_part))
  if (abs(log_liklihood) == Inf) {
    log_liklihood = -20000
  }
  return(log_liklihood)
}
##posterior function
post_prob<-function(beta_values, y, X, mu_0, sigma_0) {
  param <- as.vector(beta_values)
  ##posterior is the sum of prior and likelyhood
  logposter <- prior_prob(beta_values, sigma_0) + like_prob(beta_values, y, X)
  return(logposter)
}
##Optim function
intital_value=rep(0,8)
Optim_res <- optim(intital_value, fn=post_prob,
                   y = y, X = X, mu_0 = mu_0, sigma_0 = sigma_0,
                   method="BFGS", control=list(fnscale=-1), hessian=TRUE)
##Optimal values for param are
n = dim(WomenWork)
num = n[1]
n = n[2]
cNames <- names(WomenWork)[2:n]
mu=Optim_res$par
names(mu) = cNames
cat("Optimal values for the beta vector are: \n")
print(mu)
##Posterior covariance matrix is -inv(Hessian)
cat("\n The posterior covariance matrix is:")
post_Cov <- - solve(Optim_res$hessian)
print(post_Cov)
n_SmallChild = sort(WomenWork$NSmallChild)
cred_int = c(n_SmallChild[floor(nrow(WomenWork) * 0.05)],
             n_SmallChild[floor(nrow(WomenWork) * 0.95)+1])
cat("Credible Interval for number of small children:
[", cred_int[1], ", ", cred_int[2], "]", sep="")
data <- c(1, 10, 8, 10, (10/10)^2, 40, 1, 1)
nDraws=100000
prediction<-function() {
  sigma <-post_Cov
  mu <-Optim_res$par
  betas = rmvnorm(nDraws, mu, sigma)
  probWork<-1/(1+exp(-betas %*% data))
  return(probWork)
}
m<-prediction()
h=hist(m, breaks = 100)




########lab3#################################

###1.a)
library(geoR)
library(dplyr)
rainfall<-read.table("rainfall.dat")
names(rainfall)<-c("precipitation")
rainfall_precep<-rainfall$precipitation
#the first mu parameter for conditional posterior
param_mean_draw<-function(data,mu_0,tou_0_sq,sigma_square)
{
  n<-length(data)
  tau_n_sq<-(n/sigma_square)+(1/tou_0_sq)
  inv_tau_sq<-1/tau_n_sq
  w=(n/sigma_square)/(tau_n_sq)
  mu_n=w*mean(data)+(1-w)*mu_0
  draw1<-rnorm(1,mu_n,inv_tau_sq)
  return(draw1)
}
param_var_draw<-function(data,v_0,mu,sigma_sq_zero)
{
  n=length(data)
  vn=v_0+n
  scale<-(v_0*sigma_sq_zero+sum((data-mu)^2))/(vn)
  draw2<-rinvchisq(1,df=vn,scale = scale)
}
#define the prior values
sigma_square <- 10
sigma_sq_zero <- 10
tou_0_sq <- 10
v_0 <- 10
mu_0 <- 10
max_iter<-1000
draws<-data.frame(iter=vector("numeric",length=max_iter),
                  mean=vector("numeric",length = max_iter),
                  sigma=vector("numeric",length=max_iter))
for(i in 1:max_iter)
{
  mu<-param_mean_draw(rainfall_precep,mu_0,tou_0_sq,sigma_square)
  sigma_square<-param_var_draw(data=rainfall_precep,v_0=v_0,mu=mu,
                               sigma_sq_zero=sigma_sq_zero)
  draws[i,]<-c(i,mu,sigma_square)
}
draws$cum_mean<-cummean(draws$mean)
draws$cum_var<-cummean(draws$sigma)
layout(matrix(c(1, 2), nrow = 1))
plot(draws$iter, draws$cum_mean, type = "l",
     main=expression(paste("Convergence of ", mu)),
     xlab="iterations",ylab=expression(paste("Cumulative mean of\t",mu)))
plot(draws$iter, draws$cum_var, type = "l",
     main=expression(paste("Convergence of ", sigma^2)),
     xlab="iterations",ylab=expression(paste("Cumulative mean of\t ", sigma^2)))
#1.b (used the sample code provided)
# Estimating a simple mixture of normals
########## BEGIN USER INPUT #################\
rainfall<-read.table("rainfall.dat")
names(rainfall)<-c("precipitation")
rainfall_precep<-rainfall$precipitation
# Data options
data(faithful)
rawData <- rainfall
11
x <- as.matrix(rawData['precipitation'])
# Model options
nComp <- 2 # Number of mixture components
# Prior options
alpha <- 10*rep(1,nComp) # Dirichlet(alpha)
muPrior <- rep(0,nComp) # Prior mean of mu
tau2Prior <- rep(10,nComp) # Prior std of mu
sigma2_0 <- rep(var(x),nComp) # s20 (best guess of sigma2)
nu0 <- rep(4,nComp) # degrees of freedom for prior on sigma2
# MCMC options
nIter <- 100 # Number of Gibbs sampling draws
# Plotting options
plotFit <- TRUE
lineColors <- c("blue", "green", "magenta", 'yellow')
sleepTime <- 0.1 # Adding sleep time between iterations for plotting
################ END USER INPUT ###############
###### Defining a function that simulates from the
rScaledInvChi2 <- function(n, df, scale){
  return((df*scale)/rchisq(n,df=df))
}
####### Defining a function that simulates from a Dirichlet distribution
rDirichlet <- function(param){
  nCat <- length(param)
  piDraws <- matrix(NA,nCat,1)
  for (j in 1:nCat){
    piDraws[j] <- rgamma(1,param[j],1)
  }
  piDraws = piDraws/sum(piDraws) # Diving every column of piDraws
  #by the sum of the elements in that column.
  return(piDraws)
}
# Simple function that converts between two different
# representations of the mixture allocation
S2alloc <- function(S){
  n <- dim(S)[1]
  alloc <- rep(0,n)
  for (i in 1:n){
    alloc[i] <- which(S[i,] == 1)
  }
  return(alloc)
}
# Initial value for the MCMC
nObs <- length(x)
S <- t(rmultinom(nObs, size = 1 , prob = rep(1/nComp,nComp)))
# nObs-by-nComp matrix with component allocations.
12
mu <- quantile(x, probs = seq(0,1,length = nComp))
sigma2 <- rep(var(x),nComp)
probObsInComp <- rep(NA, nComp)
# Setting up the plot
xGrid <- seq(min(x)-1*apply(x,2,sd),max(x)+1*apply(x,2,sd),length = 100)
xGridMin <- min(xGrid)
xGridMax <- max(xGrid)
mixDensMean <- rep(0,length(xGrid))
effIterCount <- 0
ylim <- c(0,2*max(hist(x)$density))
for (k in 1:nIter){
  message(paste('Iteration number:',k))
  alloc <- S2alloc(S) # Just a function that converts between
  #different representations of the group allocations
  nAlloc <- colSums(S)
  print(nAlloc)
  # Update components probabilities
  pi <- rDirichlet(alpha + nAlloc)
  # Update mu's
  for (j in 1:nComp){
    precPrior <- 1/tau2Prior[j]
    precData <- nAlloc[j]/sigma2[j]
    precPost <- precPrior + precData
    wPrior <- precPrior/precPost
    muPost <- wPrior*muPrior + (1-wPrior)*mean(x[alloc == j])
    tau2Post <- 1/precPost
    mu[j] <- rnorm(1, mean = muPost, sd = sqrt(tau2Post))
  }
  # Update sigma2's
  for (j in 1:nComp){
    sigma2[j] <- rScaledInvChi2(1, df = nu0[j] + nAlloc[j],
                                scale = (nu0[j]*sigma2_0[j] +
                                           sum((x[alloc == j] - mu[j])^2))/(nu0[j] + nAlloc[j]))
  }
  # Update allocation
  for (i in 1:nObs){
    for (j in 1:nComp){
      probObsInComp[j] <- pi[j]*dnorm(x[i], mean = mu[j], sd = sqrt(sigma2[j]))
    }
    S[i,] <- t(rmultinom(1, size = 1 , prob = probObsInComp/sum(probObsInComp)))
  }
  # Printing the fitted density against data histogram
  if (plotFit && (k%%1 ==0)){
    effIterCount <- effIterCount + 1
    hist(x, breaks = 20, freq = FALSE, xlim = c(xGridMin,xGridMax),
         main = paste("Iteration number",k), ylim = ylim)
    mixDens <- rep(0,length(xGrid))
    13
    components <- c()
    for (j in 1:nComp){
      compDens <- dnorm(xGrid,mu[j],sd = sqrt(sigma2[j]))
      mixDens <- mixDens + pi[j]*compDens
      lines(xGrid, compDens, type = "l", lwd = 2, col = lineColors[j])
      components[j] <- paste("Component ",j)
    }
    mixDensMean <- ((effIterCount-1)*mixDensMean + mixDens)/effIterCount
    lines(xGrid, mixDens, type = "l", lty = 2, lwd = 3, col = 'red')
    legend("topleft", box.lty = 1, legend = c("Data histogram",components, 'Mixture'),
           col = c("black",lineColors[1:nComp], 'red'), lwd = 2)
    Sys.sleep(sleepTime)
  }
}
hist(x, breaks = 20, freq = FALSE, xlim = c(xGridMin,xGridMax), main = "Final fitted density")
lines(xGrid, mixDensMean, type = "l", lwd = 2, lty = 4, col = "red")
lines(xGrid, dnorm(xGrid, mean = mean(x), sd = apply(x,2,sd)),
      type = "l", lwd = 2, col = "blue")
legend("topright", box.lty = 1,
       legend = c("Data histogram","Mixture density","Normal density"),
       col=c("black","red","blue"), lwd = 2)
###2.a)
rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# Prior
T = 200
mu = 10
sigma_sqr = 2
phi = 0.5
# Model
arModel = '
data{
int<lower=0> N;
vector[N] x;
}
parameters{
real mu;
real<lower=0> sigma_sqr;
real phi;
}
model{
for(t in 2:N)
x[t] ~ normal(mu + phi*(x[t-1]-mu), sqrt(sigma_sqr));

}
'
arProcess <- function(phi, init, sigma_sqr, T){
  y <- c()
  y[1] <- init
  for(t in 2:T){
    y[t] <- mu + phi*(y[t-1]-mu) + rnorm(1, 0, sqrt(sigma_sqr))
  }
  return(y)
}
phi_range = seq(-1, 1, 0.05)
samples <- matrix(0, nrow = length(phi_range), ncol = T)
for(i in 1:length(phi_range)){
  samples[i,] <- arProcess(phi_range[i], mu, sigma_sqr, T)
}
par(mfrow = c(2,2))
plot(samples[2,], type = "l", main = "Phi = -0.95 ")
plot(samples[15,], type = "l", main = "Phi = -0.30")
plot(samples[40,], type = "l", main = "Phi = 0.95")
###2.b)
phi_1 = 0.3
phi_2 = 0.95
x <- arProcess(phi_1, mu, sigma_sqr, T)
y <- arProcess(phi_2, mu, sigma_sqr, T)
# MCMC
fit1 <- stan(model_code = arModel, data = list(x = x, N = T))
fit2 <- stan(model_code = arModel, data = list(x = y, N = T))
# Posterior mean
Mean1 <- get_posterior_mean(fit1)
Mean2 <- get_posterior_mean(fit2)
cat("Posterior mean is for phi=0.3 : ", Mean1)
cat("Posterior mean is for phi=0.95 : ", Mean2)
# 95% credible intervals
postfit1 <- extract(fit1)
postfit2 <- extract(fit2)
CI_mu1 <- apply(as.matrix(postfit1$mu), 2, quantile, probs = c(0.025, 0.975))
CI_sigma1 <- apply(as.matrix(postfit1$sigma_sqr), 2, quantile, probs = c(0.025, 0.975))
CI_phi1 <- apply(as.matrix(postfit1$phi), 2, quantile, probs = c(0.025, 0.975))
cat("95% credible intervals of mu for phi = 0.3 is : ")
CI_mu1
cat("95% credible intervals of sigma_square for phi = 0.3 is : ")
CI_sigma1
cat("95% credible intervals of phi for phi = 0.3 is : ")
15
CI_phi1
CI_mu2 <- apply(as.matrix(postfit2$mu), 2, quantile, probs = c(0.025, 0.975))
CI_sigma2 <- apply(as.matrix(postfit2$sigma_sqr), 2, quantile, probs = c(0.025, 0.975))
CI_phi2 <- apply(as.matrix(postfit2$phi), 2, quantile, probs = c(0.025, 0.975))
cat("95% credible intervals of mu for phi = 0.95 is : ")
CI_mu2
cat("95% credible intervals of sigma_square for phi = 0.95 is : ")
CI_sigma2
cat("95% credible intervals of phi for phi = 0.95 is : ")
CI_phi2
# The number of effective posterior samples for the three inferred parameters
result1 <- summary(fit1)
posEff1 <- result1$summary[, "n_eff"]
cat("The number of effective posterior samples for the
    three inferred parameter when phi=0.3 are: ")
posEff1[1:3]
result2 <- summary(fit2)
posEff2 <- result2$summary[, "n_eff"]
cat("The number of effective posterior samples for the
    three inferred parameter when phi=0.9 are : " )
posEff2[1:3]
#True values
mat<- matrix(c(posMean1 [1, 5],posMean1 [2, 5],posMean1 [3, 5],
               posMean2 [1, 5],posMean2 [2, 5],posMean2 [3,5]),ncol=3,byrow=TRUE)
colnames(mat) <- c("mu","sigma_square","phi")
rownames(mat) <- c("phi=0.3","phi=0.95")
cat("Estimated true values are : ")
as.table(mat)
#### ii)
par(mfrow = c(1,2))
plot(x = postfit1$mu, y = postfit1$phi, col = "green", xlab = "mu",
     ylab = "phi",main = "Joint posterior with phi = 0.3")
plot(x = postfit2$mu, y = postfit2$phi, col = "blue",xlab = "mu",
     ylab = "phi",main = "Joint posterior with phi = 0.95")
###2.c)
rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#prior
mu = 10
sigma_sqr = 2
phi = 0.5
campy = read.delim("campy.dat")
PoissonModel = '
data {
int<lower=0> N; //length of data
16
int y[N] ; // data
}
parameters {
real mu;
real<lower=0> sigma_sqr;
real<lower=-1, upper=1> phi;
vector[N] xt;
}
model {
for(t in 2:N){
xt[t] ~ normal(mu + phi*( xt[t-1] - mu ), sqrt(sigma_sqr));
y[t] ~ poisson(exp(xt[t]));
}
}'
data = list(N=nrow(campy), y=campy$c)
burnin = 1000
niter = 2000
fit2<-stan(model_code=PoissonModel,
           data=data,
           warmup=burnin,
           iter=niter,
           chains=1,seed = 12345)
Post_fit2 <- extract(fit2)
theta_t <- exp(Post_fit2$xt)
#Posterior mean
mean_theta_t <- c()
for (i in 1:ncol(theta_t) ) {
  mean_theta_t[i] <- mean(theta_t[,i])
}
#Posterior mean with 95% CI for latent intensity
lower <- c()
upper <- c()
for (i in 1:ncol(theta_t) ) {
  lower[i] <- quantile((theta_t[,i]), probs = c(0.025, 0.975))[1]
  upper[i] <- quantile((theta_t[,i]), probs = c(0.025, 0.975))[2]
}
#plot
plot(campy$c, type = "p", pch= 19, cex = 0.5, col = 1,ylab = "Data",
     main = "Posterior mean with 95% CI for latent intensity" )
lines(mean_theta_t, type = "l", col = "red")
lines(lower, col = "green")
lines(upper, col = "blue")
legend("topright",c("Posterior mean", "5% CI", "95% CI"),lty = c(1,1,1),
       col = c("red","green","blue"),cex = 0.75)
###2.d)
rm(list=ls())
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#prior
mu = 10
sigma_sqr = 4
phi = 0.5
campy = read.delim("campy.dat")
PoissonModel = '
data {
int<lower=0> N; //length of data
int y[N] ; // data
}
parameters {
real mu;
real<lower=0> sigma_sqr;
real<lower=-1, upper=1> phi;
vector[N] xt;
}
model {
for(t in 2:N){
xt[t] ~ normal(mu + phi*( xt[t-1] - mu ), sqrt(sigma_sqr));
y[t] ~ poisson(exp(xt[t]));
}
}'
data = list(N=nrow(campy), y=campy$c)
burnin = 1000
niter = 2000
fit2<-stan(model_code=PoissonModel,
           data=data,
           warmup=burnin,
           iter=niter,
           chains=1,seed = 12345)
Post_fit2 <- extract(fit2)
theta_t <- exp(Post_fit2$xt)
#Posterior mean
mean_theta_t <- c()
for (i in 1:ncol(theta_t) ) {
  mean_theta_t[i] <- mean(theta_t[,i])
  18
}
#Posterior mean with 95% CI for latent intensity
lower <- c()
upper <- c()
for (i in 1:ncol(theta_t) ) {
  lower[i] <- quantile((theta_t[,i]), probs = c(0.025, 0.975))[1]
  upper[i] <- quantile((theta_t[,i]), probs = c(0.025, 0.975))[2]
}
#plot
plot(campy$c, type = "p", pch= 19, cex = 0.5, col = 1,ylab = "Data",
     main = "Posterior mean with 95% CI for latent intensity" )
lines(mean_theta_t, type = "l", col = "red")
lines(lower, col = "green")
lines(upper, col = "blue")
legend("topright",c("Posterior mean", "5% CI", "95% CI"),lty = c(1,1,1),
       col = c("red","green","blue"),cex = 0.75)

################lab4##########################################################

library(mvtnorm)
library(MASS)

poi <- read.table("https://raw.githubusercontent.com/STIMALiU/BayesLearnCourse/master/Labs/eBayNumberOfBidderData.dat", header = TRUE)
model <- glm(nBids ~ . -Const, data = poi)
##removing the intercept
features <- summary(model)$coef[-1, 4] <= 0.05
features


##2


y <- as.vector(poi[,1])
X <- as.matrix(poi[,2:10])
npara <- ncol(X)
mu0 <- rep(0, npara)
tau <- 10
sigma0 <- (tau^2) * (solve(t(X)%*%X))

logPostFunc<-function(betas,y,X,mu0,sigma0) {
  betas<-as.vector(betas)
  lambda<-exp(X%*%betas)
  mu <- matrix(rep(0,9),ncol = 1)
  logposter<- dmvnorm(betas, mean=mu, sigma=sigma0, log=TRUE) + sum(dpois(y,lambda,log = TRUE))
  return(logposter)
}

initVal <- rep(0, npara)
OptimResults <- optim(initVal,logPostFunc,
                      y = y, X = X, mu0 = mu0, sigma0 = sigma0,
                      method="BFGS", control=list(fnscale=-1), hessian=TRUE)
parmts<-mvrnorm(1000,OptimResults$par,-(solve(OptimResults$hessian)))
par(mfrow=c(2,4))
for(i in 2:9) {
  hist(parmts[,i],breaks = 30,freq = FALSE, xlab = "parameter",main = colnames(X)[i])
  lines(density(parmts[,i]),col="red",lwd=2)
}
hessian <- -(solve(OptimResults$hessian))
hessian


posterr <- function(first_fun, x0, iter, z, ...) {
  xval1<-x0
  thetas <- matrix(NA,nrow = iter, ncol = 9)
  thetas[1,] <- xval1
  for(i in 2:iter) {
    val_y <- mvrnorm(1,xval1,z)
    ph1 <- first_fun(val_y, ...)
    ph2 <- first_fun(xval1, ...)
    ph3 <- exp(ph1-ph2)
    alpha <- min(1,ph3)
    dist<- runif(1)
    if(dist< alpha) {
      xval1 <- val_y
    }
    thetas[i,] <- xval1
  }
  return(thetas)
}

result <- posterr(logPostFunc, x0=rep(0,9), iter=5000, z=0.5*hessian, y=y, X=X, mu0, sigma0)


par(mfrow=c(3, 3))
for (i in 1:9) {
  plot(result[, i],type = "l",
       main = paste("Convergence of", colnames(X)[i]),
       xlab = "index", ylab = "value")
}
par(mfrow=c(2,4))
for(i in 2:9) {
  hist(result[,i],breaks = 30,freq = FALSE, xlab = "parameter",main = colnames(X)[i])
  lines(density(result[,i]),col="red",lwd=2)
}


val_coeff <- colMeans(result)
val_observed <- matrix(c(1, 1, 1, 1, 0, 0, 0, 1, 0.5), nrow = 1)
lambda <- exp(val_observed %*% val_coeff)
sample <- rpois(1000, lambda)
hist(sample, breaks = 50, col = "grey")
probability <- dpois(0, lambda)
sum(sample == 0 + 1) / (length(sample) + 1)

# 
# LogPostBernBeta <- function(theta, s, f, a, b){
#   logPost <- (s+a-1)*log(theta) + (f+b-1)*log(1-theta)
#   return(logPost)
# }
# s <- 8;f <- 2;a <- 1;b <- 1
# logPost <- LogPostBernBeta(theta = 0.1, s, f, a, b)
# logPost
# MultiplyByTwo <- function(myFunction, ...){
#   x <- 0.3
#   y <- myFunction(x,...)
#   return(2*y)
# }
# #Let's try if the MultiplyByTwo function works:
# MultiplyByTwo(LogPostBernBeta,s,f,a,b)



