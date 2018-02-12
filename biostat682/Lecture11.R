#Biostat 682: Applied Bayesian Inference

## projpath = "/Users/jiankang/Dropbox/Umich/Biostat682/Fall2017/Rcode"
#If you have not installed the following pakcages you need to install them first
#install.packages(c("rjags","R2jags","runjags","MCMCpack"))
library(R2jags)
library(lattice)



#Define a model using JAGS language
linear.model.JAGS = function(){
  for(i in 1:n){
    y[i]-2 ~ dnorm(mu[i],tau2)
    mu[i]<- alpha + beta*(x[i]-x.bar) 
  }
  x.bar <- mean(x)
  alpha ~ dnorm(0.0, 1.0E-4)
  beta ~ dnorm(0.0, 1.0E-4)
  sigma2 <- 1.0/tau2
  tau2 ~ dgamma(0.1,0.1)
}

#simulate data
n = 100
x = rnorm(n,3.0)
y = 1.0 + 2.0*(x-3.0) + rnorm(n,sd=0.5)
dat.JAGS = list(y = y, x = x, n = n)


#set initial values
inits.JAGS = list(list(alpha=0.0,beta=0.0,tau2=1.0))

#set parameters to simulate
para.JAGS = c("alpha","beta","tau2","sigma2")

## fit.JAGS = jags(data=dat.JAGS,inits=inits.JAGS,
##                 parameters.to.save = para.JAGS,
##                 n.chains=1,
##                 n.iter=9000,
##                 n.burnin=1000,
##                 model.file=linear.model.JAGS)

## #Diagnostics 
## #summarizing the results
## print(fit.JAGS)
## #plot
## #as an rjags object
## plot(fit.JAGS) 
## #as an mcmc object
## fit.JAGS.mcmc = as.mcmc(fit.JAGS)
## plot(fit.JAGS.mcmc,ask=TRUE)
## #directly traceplot
## traceplot(fit.JAGS)
## traceplot(fit.JAGS,mfrow=c(2,3),ask=FALSE)


## summary(fit.JAGS.mcmc)
## #traceplot as an mcmc object
## xyplot(fit.JAGS.mcmc)
## xyplot(fit.JAGS.mcmc,layout=c(2,3))
## #densityplot as an mcmc object
## densityplot(fit.JAGS.mcmc)
## densityplot(fit.JAGS.mcmc,layout=c(3,2))
## #Autocorrelation plot
## autocorr.plot(fit.JAGS.mcmc,auto.layout = FALSE)



#multiple initial values
inits.JAGS = list(
                  list(alpha=-10.0,beta=-10.0,tau2=100.0))

#random initial values
inits.JAGS = function(){
  return(list(alpha=rnorm(1),beta=rnorm(1),tau2=rgamma(0.1,0.1)))
}


fit.JAGS = jags.parallel(data=dat.JAGS,
                inits=inits.JAGS,
                parameters.to.save = para.JAGS,
                n.chains=3,
                n.iter=9000,
                n.burnin=1000,
                model.file=linear.model.JAGS)
save(fit.JAGS, file="Lecture11.RData")
rm(list=ls())
load("Lecture11.RData")
#summarizing the results
fit.JAGS.mcmc = as.mcmc(fit.JAGS)
## summary(fit.JAGS.mcmc)
## #traceplot as an mcmc object
## xyplot(fit.JAGS.mcmc)
## #densityplot as an mcmc object
## densityplot(fit.JAGS.mcmc)
## #Autocorrelation plot
## autocorr.plot(fit.JAGS.mcmc)  


#gelman.diag
gelman.diag(fit.JAGS.mcmc)

