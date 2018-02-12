#Biostat 682: Applied Bayesian Inference
#Lecture 12: Bayesian linear regression

library(R2jags)


simulate_dat = function(n=100,q=5,p = 10,rho = 0.5,
                        R2 = 0.9, sigma2_x = 1.0,
                        beta_0 = 1, 
                        beta_range = c(1.0,2.0)){
  X0 = rnorm(n,sd=sqrt(rho)*sqrt(sigma2_x))
  X = X0 + matrix(rnorm(n*p,sd=sqrt(1-rho)*sqrt(sigma2_x)),nrow=n,ncol=p)
  beta = c(sample(c(-1,1),q,TRUE)*runif(q,beta_range[1],beta_range[2]),rep(0,length=p-q))
  sigma2 = (1-R2)/R2*var(X%*%beta)
  Y = beta_0 + X%*%beta + rnorm(n,sd=sqrt(sigma2))
  JAGS = list(Y=Y[,1],X=X,n=n,p=p)
  return(list(JAGS=JAGS,q=q,beta=beta,beta_0 = beta_0,sigma2=sigma2))
}

JAGS_BLR_flat = function(){
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(mu[i],inv_sigma2)
    mu[i] <- beta_0 + inprod(X[i,],beta) 
    # same as beta_0 + X[i,1]*beta[1] + ... + X[i,p]*beta[p]
  }
  # Prior for beta
  for(j in 1:p){
    beta[j] ~ dnorm(0,0.0001)
    #non-informative priors 
  }
  # Prior for intercept
  beta_0 ~ dnorm(0, 0.0001)
  
  # Prior for the inverse variance
  inv_sigma2 ~ dgamma(0.0001, 0.0001)
  sigma2 <- 1.0/inv_sigma2
}

JAGS_BLR_BLASSO = function(){
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(mu[i],inv_sigma2)
    mu[i] <- beta_0 + inprod(X[i,],beta) 
  }
  # Prior for beta
  for(j in 1:p){
    beta[j] ~ ddexp(0,inv_tau2)
    #non-informative priors 
  }
  # Prior for intercept
  beta_0 ~ dnorm(0, 0.0001)
  
  # Prior for the inverse variance
  inv_sigma2 ~ dgamma(0.0001, 0.0001)
  inv_tau2 ~ dgamma(0.0001,0.0001)
  
  sigma2 <- 1.0/inv_sigma2
  tau2 <- 1.0/inv_tau2
}


JAGS_BLR_SpikeSlab = function(){
  # Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(mu[i],inv_sigma2)
    mu[i] <- beta_0 + inprod(X[i,],beta) 
  }
  # Prior for beta
  for(j in 1:p){
    beta[j] ~ dnorm(0,inv_tau2[j])
    inv_tau2[j] <- (1-gamma[j])*1000+gamma[j]*0.01
    gamma[j] ~ dbern(0.5)
  }
  # Prior for intercept
  beta_0 ~ dnorm(0, 0.0001)
  
  # Prior for the inverse variance
  inv_sigma2 ~ dgamma(0.0001, 0.0001)
  sigma2 <- 1.0/inv_sigma2
  tau2 <- 1.0/inv_tau2
}



set.seed(682)
n = 100
p = 10
q = 5
simdat = simulate_dat(n=n,p=p,q=q)


fit_JAGS_flat = jags(data=simdat$JAGS,
                inits=list(list(beta = rnorm(p),
                                beta_0 = 0,
                                inv_sigma2 = 1)),
                parameters.to.save = c("beta_0","beta","sigma2"),
                n.chains=1,
                n.iter=10000,
                n.burnin=1000,
                model.file=JAGS_BLR_flat)

fit_JAGS_BLASSO = jags(data=simdat$JAGS,
                inits=list(list(beta = rnorm(p),
                                beta_0 = 0,
                                inv_sigma2 = 1,
                                inv_tau2 = 5.1)),
                parameters.to.save = c("beta_0","beta","sigma2","tau2"),
                n.chains=1,
                n.iter=10000,
                n.burnin=1000,
                model.file=JAGS_BLR_BLASSO)

fit_JAGS_SpikeSlab = jags(data=simdat$JAGS,
                       inits=list(list(beta = rnorm(p),
                                       beta_0 = 0,
                                       inv_sigma2 = 1,
                                       gamma = rep(1,length=p))),
                       parameters.to.save = c("beta","gamma"),
                       n.chains=1,
                       n.iter=10000,
                       n.burnin=1000,
                       model.file=JAGS_BLR_SpikeSlab)

print(fit_JAGS_flat)
print(fit_JAGS_BLASSO)
print(fit_JAGS_SpikeSlab)

fit_BLASSO =as.mcmc(fit_JAGS_BLASSO)
fit_flat =as.mcmc(fit_JAGS_flat)
fit_SpikeSlab =as.mcmc(fit_JAGS_SpikeSlab)

#Posterior Predictive Distribution

#split data into training and testing sets
split_data = function(dat_JAGS,train_test_ratio = 1,random=TRUE){
  n_train = floor(dat_JAGS$n*train_test_ratio/(1+train_test_ratio))
  n_test  =  dat_JAGS$n - n_train
  if(random){
    train_idx = sample(1:n,n_train,replace = FALSE)
    test_idx = setdiff(1:n,train_idx)
  }
  else{
    train_idx = 1:n_train
    test_idx = n_train+1:n_test
  }
  
  JAGS = list()
  JAGS$Y_train = dat_JAGS$Y[train_idx]
  JAGS$X_train = dat_JAGS$X[train_idx,,drop=FALSE]
  JAGS$X_test = dat_JAGS$X[test_idx,,drop=FALSE]
  JAGS$n_train = n_train
  JAGS$n_test = n_test
  JAGS$p = dat_JAGS$p
  return(list(JAGS=JAGS,Y_test=dat_JAGS$Y[test_idx]))
}


#Rewrite the Spike Slab Model 
#by changing the likelihood part
# and adding prediction for Y_test
JAGS_BLR_SpikeSlab_Pred = function(){
  # Likelihood for training data
  for(i in 1:n_train){
    Y_train[i] ~ dnorm(mu_train[i],inv_sigma2)
    mu_train[i] <- beta_0 + inprod(X_train[i,],beta) 
  }
  
  # Predictions
  for(i in 1:n_test){
    Y_test[i] ~ dnorm(mu_test[i],inv_sigma2)
    mu_test[i] <- beta_0 + inprod(X_test[i,],beta) 
  }
  
  # Prior for beta
  for(j in 1:p){
    beta[j] ~ dnorm(0,inv_tau2[j])
    inv_tau2[j] <- (1-gamma[j])*1000+gamma[j]*0.01
    gamma[j] ~ dbern(0.5)
  }
  # Prior for intercept
  beta_0 ~ dnorm(0, 0.0001)
  
  # Prior for the inverse variance
  inv_sigma2 ~ dgamma(0.0001, 0.0001)
  sigma2 <- 1.0/inv_sigma2
  tau2 <- 1.0/inv_tau2
}

set.seed(100)
pred_dat = split_data(dat_JAGS = simdat$JAGS,train_test_ratio = n-1)

fit_JAGS_SpikeSlab_Pred = jags(data=pred_dat$JAGS,
                          inits=list(list(beta = rnorm(p),
                                          beta_0 = 0,
                                          inv_sigma2 = 1,
                                          gamma = rep(1,length=p))),
                          parameters.to.save = c("beta","gamma","Y_test"),
                          n.chains=1,
                          n.iter=10000,
                          n.burnin=1000,
                          model.file=JAGS_BLR_SpikeSlab_Pred)

fit_SpikeSlab_Pred = as.mcmc(fit_JAGS_SpikeSlab_Pred)

plot(density(fit_SpikeSlab_Pred[[1]][,"Y_test"]),
     main="Posterior Predictive Density",col="blue",
     lwd=2, xlab="Y")
abline(v=pred_dat$Y_test,col="red",lwd=2)
legend("topright",c(expression(Y[test]),expression(pi(Y[test]~'|'~Y[train]))),lwd=2,
       col=c("red","blue"),lty=1)
