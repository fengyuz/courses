## rm(list=ls())

setwd("~/biostat682")

library(R2jags)
library(lattice)
library(MASS)
library(plyr)
library(ggplot2)
library(mcmcplots)

n.chains <- 2
n.iter <- 1e2


## Data: X, Y, n, m[1:n]
## Hyperpara: theta.one = zero_4, theta.zero = zero_2, R.one = identity_4, R.zero = identity_2
model.2.jags <- function(){
    for(i in 1:n){
        for(j in 1:m[i]){
            Y[i,j] ~ dnorm(mu[i,j,(z[i]+1)], tausq)
            mu[i,j,1] <-
                a[i] + b[i] * X[i,j]
            mu[i,j,2] <-
                (a[i] - s[i] * X[i,j]) *
                (1 - w[i,j]) +
                (a[i] - s[i] * r[i] - b[i] * r[i] + b[i] * X[i,j]) *
                (w[i,j])
            w[i,j] <- dinterval(X[i,j], r[i])
        }
        z[i] ~ dbern(p[g[i]+1])
        a[i] <- beta[i,1]
        b[i] <- exp(beta[i,2])
        r[i] <- exp(beta[i,3])
        s[i] <- exp(beta[i,4])
        # c[i] <- gamma[i,1]
        # d[i] <- exp(gamma[i,2])
        beta[i,1:4] ~ dmnorm(beta.mu, beta.Omega)
        # gamma[i,1:2] ~ dmnorm(gamma.mu, gamma.Omega)
    }
    tausq ~ dgamma(1e-1,1e-1)
    p[1] ~ dbeta(1,1)
    p[2] ~ dbeta(1,1)
    beta.mu[1] ~ dunif(-5, 20)
    beta.mu[2] ~ dunif(-10, 3)
    beta.mu[3] ~ dunif(-10, 3)
    beta.mu[4] ~ dunif(-10, 3)
    beta.Omega ~ dwish(eye.Four, 5)
    # gamma.mu[1] ~ dunif(-5, 20)
    # gamma.mu[2] ~ dunif(-10, 3)
    # gamma.Omega ~ dwish(eye.Two, 3)
##     beta.mu[2,1] ~ dunif(-5, 20)
##     beta.mu[2,2] ~ dunif(-10, 3)
##     beta.mu[2,3] <- 1e-3
##     beta.mu[2,4] <- 1e-3
##     Omega[2, 1:4, 1:4] ~ dwish(eye.Two.inf.Two, 5)
}

model.1.jags <- function(){
    for(i in 1:n){
        for(j in 1:m[i]){
            Y[i,j] ~ dnorm(mu[i,j], tausq)
            mu[i,j] <-
                (a[i] - s[i] * X[i,j]) *
                (1 - w[i,j]) +
                (a[i] - s[i] * r[i] - b[i] * r[i] + b[i] * X[i,j]) *
                (w[i,j])
            w[i,j] <- dinterval(X[i,j], r[i])
        }
        z[i] ~ dbern(p[g[i]+1])
        a[i] <- beta[i,1]
        b[i] <- exp(beta[i,2])
        r[i] <- exp(beta[i,3])
        s[i] <- exp(beta[i,4])
        beta[i,1:4] ~ dmnorm(
          beta.mu[1:4, (z[i]+1)],
          Omega[1:4, 1:4, (z[i]+1)]
          )
    }
    tausq ~ dgamma(1e-1,1e-1)
    p[1] ~ dbeta(1,1)
    p[2] ~ dbeta(1,1)
    beta.mu[1,2] ~ dunif(-5, 20)
    beta.mu[2,2] ~ dunif(-10, 3)
    beta.mu[3,2] ~ dunif(-10, 3)
    beta.mu[4,2] ~ dunif(-10, 3)
    Omega[1:4, 1:4, 2] ~ dwish(eye.Four, 5)
    beta.mu[1,1] ~ dunif(-5, 20)
    beta.mu[2,1] ~ dunif(-10, 3)
    beta.mu[3,1] <- 1e-3
    beta.mu[4,1] <- 1e-3
    Omega[1:4, 1:4, 1] ~ dwish(eye.Two.inf.Two, 5)
}

model.3.jags <- function(){
    for(i in 1:n){
        for(j in 1:m[i]){
            Y[i,j] ~ dnorm(mu[i,j], tausq)
            mu[i,j] <-
                (a[i] - s[i] * X[i,j]) *
                (1 - w[i,j]) +
                (a[i] - s[i] * r[i] - b[i] * r[i] + b[i] * X[i,j]) *
                (w[i,j])
            w[i,j] <- dinterval(X[i,j], r[i])
        }
        z[i] ~ dbern(p[g[i]+1])
        a[i] <- beta[i,1]
        b[i] <- exp(beta[i,2])
        r[i] <- exp(beta[i,3])
        s[i] <- exp(beta[i,4])
        beta[i,1:4] ~ dmnorm(
          beta.mu[1:4, (z[i]+1)],
          eye.Four
          )
    }
    tausq ~ dgamma(1e-1,1e-1)
    p[1] ~ dbeta(1,1)
    p[2] ~ dbeta(1,1)
    beta.mu[1,2] ~ dunif(-5, 20)
    beta.mu[2,2] ~ dunif(-10, 3)
    beta.mu[3,2] ~ dunif(-10, 3)
    beta.mu[4,2] ~ dunif(-10, 3)
    Omega[1:4, 1:4, 2] ~ dwish(eye.Four, 5)
    beta.mu[1,1] ~ dunif(-5, 20)
    beta.mu[2,1] ~ dunif(-10, 3)
    beta.mu[3,1] <- 1e-3
    beta.mu[4,1] <- 1e-3
    Omega[1:4, 1:4, 1] ~ dwish(eye.Two.inf.Two, 5)
}

##         beta[i, 1:4] ~ dmnorm(gamma[i, 1:4], omega[i, 1:4, 1:4])
##         for(k in 1:4){
##             gamma[i, k] <-
##                 (1 - z[i]) * gamma.zero[k] +
##                 z[i] * gamma.one[k]
##             for(l in 1:4){
##                 omega[i, k, l] <-
##                     (1 - z[i]) * omega.zero[k,l] +
##                     z[i] * omega.one[k,l]
##             }
##         }
##         z[i] ~ dbern(p)
##     }
##     gamma.one ~ dmnorm(theta.one, zeta.one)
##     for(k in 1:4){
##         for(l in 1:4){
##             zeta.one[k,l] <- omega.one[k,l] * ell
##         }
##     }
##     ell <- 1e-2
##     omega.one ~ dwish(R.one, kay.one)
##     kay.one <- 4 + 1
##     ## gamma.zero.onetwo ~ dmnorm(theta.zero, zeta.zero)
##     ## gamma.zero[1:2] <- gamma.zero.onetwo
##     gamma.zero[1:2] ~ dmnorm(theta.zero, zeta.zero)
##     zeta.zero[1,1] <- 1e-1
##     zeta.zero[1,2] <- 0
##     zeta.zero[2,1] <- 0
##     zeta.zero[2,2] <- 1e-1
##     gamma.zero[3] <- -1e3
##     gamma.zero[4] <- -1e3
##     ## gamma.zero[1] <- 0
##     ## gamma.zero[2] <- 0
##     ## for(k in 1:2){
##     ##     for(l in 1:2){
##     ##         zeta.zero[k,l] <- omega.zero[k,l] * ell
##     ##     }
##     ## }
##     omega.zero[1,3] <- 0
##     omega.zero[1,4] <- 0
##     omega.zero[2,3] <- 0
##     omega.zero[2,4] <- 0
##     omega.zero[3,1] <- 0
##     omega.zero[3,2] <- 0
##     omega.zero[3,3] <- 1e3
##     omega.zero[3,4] <- 0
##     omega.zero[4,1] <- 0
##     omega.zero[4,2] <- 0
##     omega.zero[4,3] <- 0
##     omega.zero[4,4] <- 1e3
##     ## omega.zero[1:2, 1:2] ~ dwish(R.zero, kay.zero)
##     ## omega.zero[1:2, 1:2] <- lala
##     omega.zero[1,1] <- h


##     omega.zero[1,2] <- 1
##     omega.zero[2,1] <- 1
##     omega.zero[2,2] <- 1
##     lala ~ dwish(R.zero, kay.zero)
##     ## R.zero[1,1] <- 1
##     ## R.zero[1,2] <- 0
##     ## R.zero[2,1] <- 0
##     ## R.zero[2,2] <- 1
##     kay.zero <- 2 + 1
##     tausq ~ dgamma(1e-3, 1e-3)
##     p ~ dbeta(1, 1)
## }


# Need: n, m, X, Y, g
model.JAGS <- function(){
    for(i in 1:n){
         z[i] ~ dbern(p[i])
         p[i] <- (1 - g[i]) * p_pool[1] + g[i] * p_pool[2]
        for(j in 1:m[i]){
            Y[i, j] ~ dnorm(Y.dumb[i,j], 1e20)
            Y.dumb[i, j] <- (1 - z[i]) * Y.one[i, j] + z[i] * Y.two[i, j]
            Y.one[i,j] ~ dnorm(mu.one[i,j], tausq)
            Y.two[i, j] ~ dnorm(mu.two[i,j], tausq)
            ## Y[i,j] ~ dnormmix(
            ##     c(mu.one[i,j], mu.two[i,j]),
            ##     c(tausq, tausq),
            ##     c(1-z[i], z[i])
            ## )
            mu.one[i,j] <- a.one[i] + ber.one[i] * X[i, j] # a.one and a.two should be the same
            mu.two[i,j] <- a.two[i] + byi.two[i] * X[i, j] + ber.two[i] * max(X[i, j] - r[i], 0)
        }
        a.one[i] ~ dnorm(3, 1e-3)
        ## ber.one[i] ~ dgamma(1e-3, 1e-3) ###shoud be dgamma(0.00001, 0.00001) to be uninformative
        ## ber.one[i] ~ dgamma(1e-3, 1e-3) ###shoud be dgamma(0.00001, 0.00001) to be uninformative
        ber.one[i] ~ dnorm(0, 1e-3);T(0,) ###shoud be dgamma(0.00001, 0.00001) to be uninformative
        a.two[i] ~ dnorm(3, 1e-3)
        byi.two[i] ~ dnorm(0, 1e-3);T(,0) ###shoud be dgamma(0.00001, 0.00001) to be uninformative
        ## byi.two[i] <- (-1) * byi.two.star[i] ## Change to truncation
        ## byi.two.star[i] ~ dgamma(1e-3, 1e-3) ###shoud be dgamma(0.00001, 0.00001) to be uninformative
        ber.two[i] <- exp(ber.two.star[i])
        ber.two.star[i] ~ dnorm(0, ber.two.star.tausq)
        r[i] <- exp(r.star[i])
        r.star[i] ~ dnorm(r.star.mu[i], r.star.tausq);T(,log(36))
        r.star.mu[i] <-rho * sqrt(ber.two.star.tausq/r.star.tausq) * ber.two.star[i]
        ## ber.two.r.star[(1:2),i] ~ dmnorm(S, Omega)
    }
    ber.two.star.tausq ~ dgamma(1e-3, 1e-3)
    r.star.tausq.raw ~ dgamma(1e-3, 1e-3)
    r.star.tausq <- r.star.tausq.raw / (1 - rho^2)
    rho ~ dunif(-1, 1)
    ## Omega ~ dwish(R, 3)
    ## Sigma <- inverse(Omega)#####Omega is the precision
    tausq ~ dgamma(1e-3, 1e-3)
    p_pool[1] ~ dbeta(1, 1)
    p_pool[2] ~ dbeta(1, 1)
}

# Process data
psa.csv <- "PSAdata.csv"
dat <- read.csv(psa.csv)
dat.length <- aggregate(dat$y, by = list(dat$ID), FUN = length)$x
ID.good <- which(dat.length > 2)
dat <- dat[is.element(dat$ID, ID.good),]
dat.Y <- aggregate(dat$y, by = list(dat$ID), FUN = c)$x
dat.X <- aggregate(dat$t, by = list(dat$ID), FUN = c)$x
dat.g <- aggregate(dat$g, by = list(dat$ID), FUN = unique)$x - 1
dat.whichmin <- aggregate(dat$y, by = list(dat$ID), FUN = which.min)
dat.Y.mat <- plyr::ldply(dat.Y, rbind)
dat.Y.mat <- dat.Y.mat[,-1]
dat.Y.mat <- as.matrix(dat.Y.mat)
dat.X.mat <- plyr::ldply(dat.X, rbind)
dat.X.mat <- dat.X.mat[,-1]
dat.X.mat <- as.matrix(dat.X.mat)
z <- as.numeric(dat.whichmin$x > 1)
Y <- dat.Y.mat
X <- dat.X.mat
g <- dat.g
n <- length(z)
m <- sapply(dat.Y, length)
dat.JAGS = list(Y = Y, X = X, g = g, n = n, m = m)
dat.jags = list(
    Y = Y,
    X = X,
    n = n,
    m = m,
    g = g,
    zero.four = rep(0,4),
    epsi.four = diag(rep(1e-3,4)),
    zero.Two = matrix(0, 2, 2),
    zero.Four = matrix(0, 4, 4),
    eye.Two.inf.Two = diag(c(1,1,1e3,1e3)),
    inf.Two = matrix(1e3, 2, 2),
    eye.Four = diag(rep(1,4)),
    eye.Two = diag(rep(1,2))
    ## theta.zero = rep(0, 2),
    ## theta.one = rep(0, 4),
    ## R.zero = diag(rep(1,2)),
    ## R.one = diag(rep(1,4))
)

para.jags <- c(
    "a", "s", "r", "b", "z",
    "p", "beta.mu"
    #, "beta.Omega"
)
para.JAGS = c("a.one", "ber.one", "a.two", "byi.two", "ber.two", "r", "tausq", "p_pool", "z", "rho", "ber.two.star.tausq", "r.star.tausq")

## inits.JAGS = list(list(a.one=rep(3,n),
##                        ber.one=rep(1, n),
##                        a.two=rep(3, n),
##                        byi.two=rep(-1, n), #initialize positive scale
##                        rho=0,
##                        z=z, tausq=1, p_pool=c(0.5,0.5)))
## inits.JAGS = rep(list(list(a.one=rep(3,n),
##                        ber.one=rep(1, n),
##                        a.two=rep(3, n),
##                        byi.two=rep(-1, n), #initialize positive scale
##                        rho=0,
##                        z=z, tausq=1, p_pool=c(0.5,0.5))
## ), n.chains)

# inits.jags <- list()
# for(i in 1:1){
#     inits.jags <- c(
#         inits.jags,
#         list(list(
#             p = rbeta(2,1,1),
#             tausq = rgamma(1, 1e-1, 1e-1),
#             z = rbinom(n, 1, rbeta(1,1,1)),
#             beta = mvrnorm(n, rep(0, 4), diag(rep(1,4))) 
#         ))
#     )
# }


zero.four = rep(0,4)
epsi.four = diag(rep(1e-3,4))
zero.Two = matrix(0, 2, 2)
zero.Four = matrix(0, 4, 4)
eye.Two.inf.Two = diag(c(1,1,1e3,1e3))
inf.Two = matrix(1e3, 2, 2)
eye.Four = diag(rep(1,4))
eye.Two = diag(rep(1,2))

inits.jags <- function(){
    list(
        "p" = rbeta(2,1,1),
        "tausq" = rgamma(1, 1e-1, 1e-1),
        "z" = rbinom(n, 1, rbeta(1,1,1)),
        "mu.beta" = runif(4, -3, 3)
    )
}



Sys.time()
fit.1.jags <- jags(
  model.file=model.2.jags,
     data=dat.jags,
     inits=inits.jags,
     parameters.to.save = para.jags,
     n.iter=1e3,
     n.burnin=1e0,
     n.chains=1
    # n.thin=1
)
## fit.2.jags <- jags.parallel(
##      data=dat.jags,
##      inits=inits.jags,
##      parameters.to.save = para.jags,
##      n.iter=1e3,
##      n.burnin=1e2,
##      n.chains=3,
##     n.thin=1,
##     model.file=model.2.jags
## )

Sys.time()
save(fit.1.jags, file=paste0("fit.1.jags.RData.", toString(Sys.time())))
# save(fit.2.jags, file=paste0("fit.2.jags.RData.", toString(Sys.time())))
fit.1.jags.mcmc <- as.mcmc(fit.1.jags)
# fit.2.jags.mcmc <- as.mcmc(fit.2.jags)
Sys.time()


## print(gelman.diag(fit.1.jags.mcmc, multivariate = FALSE))
mcmcplot(fit.1.jags.mcmc)
## mcmcplot(fit.2.jags.mcmc)

## pdf.file <- paste0("output.pdf.", toString(Sys.time()))
## pdf(pdf.file, width = 16, height = 8)
## mf.row <- 3
## mf.col <- 8
## n.plot <- mf.row * mf.col
## indx <- sample(c(1:n), n.plot)
## par(mfrow=c(mf.row,mf.col))
## out.a <- fit.jags$BUGSoutput$mean$a
## out.b <- fit.jags$BUGSoutput$mean$b
## out.r <- fit.jags$BUGSoutput$mean$r
## out.s <- fit.jags$BUGSoutput$mean$s
## out.z <- fit.jags$BUGSoutput$mean$z
## for(j in 1:n.plot){
##     i = indx[j]
##     plot(dat.X[[i]], dat.Y[[i]],
##          col = "red",
##          pch = 20,
##          xlim = c(0,10),
##          ylim = c(0,15)
##          )
##     abline(
##         out.a[i],
##         -out.s[i]
##     )
##     if(out.z[i] > 0.5){
##         col.vee <- "cyan"
##     }else{
##         col.vee <- "gray80"
##     }
##     abline(
##         out.a[i] - out.s[i] * out.r[i] - out.b[i] * out.r[i],
##         out.b[i],
##         col = col.vee
##     )
## }
## dev.off()


## plot(fit.JAGS)
## par(mfrow=c(1,2))
## z.out <- fit.JAGS$BUGSoutput$mean$z
## z.out <- as.logical(z.out)
## a.one.out <- fit.JAGS$BUGSoutput$mean$a.one
## a.two.out <- fit.JAGS$BUGSoutput$mean$a.two
## byi.two.out <- fit.JAGS$BUGSoutput$mean$byi.two
## ber.one.out <- fit.JAGS$BUGSoutput$mean$ber.one
## ber.two.out <- fit.JAGS$BUGSoutput$mean$ber.two
## r.out <- fit.JAGS$BUGSoutput$mean$r
## hist(a.one.out[z.out])
## hist(ber.one.out[z.out])
## hist(a.two.out[z.out])
## hist(byi.two.out[z.out])
## hist(ber.two.out[z.out])
## hist(r.out[z.out])
## hist(a.one.out[!z.out], breaks=100)
## hist(ber.one.out[!z.out], breaks=100)
## hist(a.two.out[!z.out], breaks=100)
## hist(byi.two.out[!z.out], breaks=100)
## hist(ber.two.out[!z.out], breaks=100)
## hist(r.out[!z.out], breaks=100)


##     bent = z.out[i]
##     if(!bent){
##         abline(
##             a.one.out[i],
##             ber.one.out[i]
##         )
##         inform.names = c(
##             "z",
##             "a.one",
##             "ber.one"
##             )
##         inform = c(z.out[i],
##                     a.one.out[i],
##                     ber.one.out[i]
##                     )
##     }
##     else{
##         abline(
##             a.two.out[i],
##             byi.two.out[i]
##         )
##         abline(
##             a.two.out[i] - ber.two.out[i] * r.out[i],
##             byi.two.out[i] + ber.two.out[i]
##         )
##         inform.names = c(
##             "z",
##             "a.two",
##             "byi.two",
##             "ber.two",
##             "r"
##             )
##         inform = c(z.out[i],
##                     a.two.out[i],
##                     byi.two.out[i],
##                     ber.two.out[i],
##                     r.out[i]
##                     )
##     }
##     inform = round(inform, 3)
##     legend("topleft", legend = inform.names)
##     legend("topright", legend = inform)
    
    
