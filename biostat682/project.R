lm(list=ls())
library(R2jags)
library(lattice)
library(MASS)
library(plotrix)
library(plyr)

# Need: n, m, X, Y
model.JAGS <- function(){
    for(i in 1:n){
        Y[[i]] <- (1 - z[i]) * Y.one[[i]] + z[i] * Y.two[[i]]
        z[i] ~ dbin(p, 1)
        p ~ dbeta(1, 1)
        for(j in 1:m[i]){
            # Define Y.one
            Y.one[[i]][j] ~ dnorm(mu.one[i,j], tausq)
            mu.one[i,j] <- a.one[i] + ber.one[i] * X[[i]][j] # a.one and a.two should be the same
            # Define Y.two
            Y.two[[i]][j] ~ dnorm(mu.two[i,j], tausq)
            mu.two[i,j] <- a.two[i] + byi.two[i] * X[[i]][j] + ber.two[i] * max(X[[i]][j] - r[i], 0)
        }
        a.one[i] ~ dnorm(3, 1e-3)
        ber.one[i] ~ dgamma(1e3, 1e-3)
        a.two[i] ~ dnorm(3, 1e-3)
        byi.two[i] ~ (-1) * dgamma(1e3, 1e-3)
        ber.two[i] <- exp(ber.two.star[i])
        r[i] <- exp(r.star[i])
        ber.two.r.star[i] <- c(ber.two.star[i], r.star[i])
        ber.two.r.star[i] ~ dmnorm(c(0,0), Omega)
        Omega ~ dwish(R, 3)
        R <- diag(c(1,1))
    }
    tausq ~ dgamma(1e3, 1e-3)
}

dat <- read.csv("PSAdata.csv")
dat.length <- aggregate(dat$y, by = list(dat$ID), FUN = length)$x
ID.good <- which(dat.length > 2)
dat <- dat[is.element(dat$ID, ID.good),]
dat.Y <- aggregate(dat$y, by = list(dat$ID), FUN = c)$x
dat.X <- aggregate(dat$t, by = list(dat$ID), FUN = c)$x
dat.whichmin <- aggregate(dat$y, by = list(dat$ID), FUN = which.min)
dat.Y.mat <- plyr::ldply(dat.Y, rbind)
dat.Y.mat <- dat.Y.mat[,-1]
dat.Y.mat <- as.matrix(dat.Y.mat)
dat.X.mat <- plyr::ldply(dat.X, rbind)
dat.X.mat <- dat.X.mat[,-1]
dat.X.mat <- as.matrix(dat.X.mat)

z <- (dat.whichmin$x > 1)
Y <- dat.Y
X <- dat.X
n <- length(z)
m <- sapply(dat.Y, length)

dat.JAGS = list(Y = Y, X = X, n = n, m = m)
para.JAGS = c("a.one", "ber.one", "a.two", "byi.two", "ber.two", "r", "tausq")
inits.JAGS = list(list(a.one=rep(3,n) , ber.one=rep(1, n), a.two=rep(3, n), byi.two=rep(-1, n), ber.two=rep(1, n), z=z, tausq=1, p=0.5))

## fit.JAGS <- jags(
##     data=dat.JAGS,
##     inits=inits.JAGS,
##     parameters.to.save = para.JAGS,
##     n.chains=1,
##     n.iter=1e4,
##     n.burnin=2000,
##     ## model.file=model.JAGS
##     model.file=linear.model.JAGS
## )
