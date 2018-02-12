library(R2jags)
library(lattice)
library(MASS)
library(plyr)
library(ggplot2)

# Need: n, m, X, Y
model.JAGS <- function(){
    for(i in 1:n){
        z[i] ~ dbern(p)###should be dbern(0.5)
        for(j in 1:m[i]){
            Y[i, j] ~ dnorm(Y.dumb[i,j], 1e20)
            Y.dumb[i, j] <- (1 - z[i]) * Y.one[i, j] + z[i] * Y.two[i, j]
            # Define Y.one
            Y.one[i,j] ~ dnorm(mu.one[i,j], tausq)
            mu.one[i,j] <- a.one[i] + ber.one[i] * X[i, j] # a.one and a.two should be the same
            # Define Y.two
            Y.two[i, j] ~ dnorm(mu.two[i,j], tausq)
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
        r.star.mu[i] <- rho * sqrt(ber.two.star.tausq/r.star.tausq) * ber.two.star[i]
        ## ber.two.r.star[(1:2),i] ~ dmnorm(S, Omega)
    }
    ber.two.star.tausq ~ dgamma(1e-3, 1e-3)
    r.star.tausq.raw ~ dgamma(1e-3, 1e-3)
    r.star.tausq <- r.star.tausq.raw / (1 - rho^2)
    rho ~ dunif(-1, 1)
    ## Omega ~ dwish(R, 3)
    ## Sigma <- inverse(Omega)#####Omega is the precision
    tausq ~ dgamma(1e-3, 1e-3)
   p ~ dbeta(1, 1)
}

psa.csv <- "PSAdata.csv"
dat <- read.csv(psa.csv)
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

z <- as.numeric(dat.whichmin$x > 1)
Y <- dat.Y.mat
X <- dat.X.mat
n <- length(z)
m <- sapply(dat.Y, length)
R <- diag(c(1,1))
S <- c(0,0)

dat.JAGS = list(Y = Y, X = X, n = n, m = m)
para.JAGS = c("a.one", "ber.one", "a.two", "byi.two", "ber.two", "r", "tausq", "z", "rho", "ber.two.star.tausq", "r.star.tausq")
inits.JAGS = list(list(a.one=rep(3,n),
                       ber.one=rep(1, n),
                       a.two=rep(3, n),
                       byi.two=rep(-1, n), #initialize positive scale
                       rho=0,
                       z=z, tausq=1, p=0.5))

## fit.JAGS <- jags(
##     data=dat.JAGS,
##     inits=inits.JAGS,
##     parameters.to.save = para.JAGS,
##     n.iter=10000,
##     n.burnin=1000,
##     n.chains=1,
##     model.file=model.JAGS
## )
save(fit.JAGS, file="fit.JAGS.RData")

pdf.file <- "output.pdf"
## as.POSIXct(Sys.time())
pdf(pdf.file, width = 16, height = 8)
plot(fit.JAGS)
par(mfrow=c(1,2))
z.out <- fit.JAGS$BUGSoutput$mean$z
z.out <- as.logical(z.out)
a.one.out <- fit.JAGS$BUGSoutput$mean$a.one
a.two.out <- fit.JAGS$BUGSoutput$mean$a.two
byi.two.out <- fit.JAGS$BUGSoutput$mean$byi.two
ber.one.out <- fit.JAGS$BUGSoutput$mean$ber.one
ber.two.out <- fit.JAGS$BUGSoutput$mean$ber.two
r.out <- fit.JAGS$BUGSoutput$mean$r
hist(a.one.out[z.out])
hist(ber.one.out[z.out])
hist(a.two.out[z.out])
hist(byi.two.out[z.out])
hist(ber.two.out[z.out])
hist(r.out[z.out])
hist(a.one.out[!z.out], breaks=100)
hist(ber.one.out[!z.out], breaks=100)
hist(a.two.out[!z.out], breaks=100)
hist(byi.two.out[!z.out], breaks=100)
hist(ber.two.out[!z.out], breaks=100)
hist(r.out[!z.out], breaks=100)

mf.row <- 3
mf.col <- 8
n.plot <- mf.row * mf.col
indx <- sample(c(1:n), n.plot)
par(mfrow=c(mf.row,mf.col))
for(j in 1:n.plot){
    i = indx[j]
    plot(dat.X[[i]], dat.Y[[i]],
         col = "red",
         pch = 16,
         cex = 2
         )
    bent = z.out[i]
    if(!bent){
        col.one <- "black"
        col.two <- "gray"
    }
    else{
        col.one <- "gray"
        col.two <- "black"
    }
    abline(
        a.one.out[i],
        ber.one.out[i],
        col = col.one,
        lwd = 2
    )
    inform.names.one = c(
        "z",
        "a.one",
        "ber.one"
        )
    inform.one = c(z.out[i],
                a.one.out[i],
                ber.one.out[i]
                )
    abline(
        a.two.out[i],
        byi.two.out[i],
        col = col.two,
        lwd = 2
    )
    abline(
        a.two.out[i] - ber.two.out[i] * r.out[i],
        byi.two.out[i] + ber.two.out[i],
        col = col.two,
        lwd = 2
    )
    inform.names.two = c(
        "a.two",
        "byi.two",
        "ber.two",
        "r"
        )
    inform.two = c(
                a.two.out[i],
                byi.two.out[i],
                ber.two.out[i],
                r.out[i]
                )
    inform.one = round(inform.one, 3)
    inform.two = round(inform.two, 3)
    legend("topleft",
           legend = inform.names.one,
           col = col.one)
    legend("topright",
           legend = inform.one,
           col = col.one)
    legend("bottomleft",
           legend = inform.names.two,
           col = col.two)
    legend("bottomright",
           legend = inform.two,
           col = col.two)
}
dev.off()
    
    
