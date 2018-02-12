rm(list=ls())
library(R2jags)
library(lattice)
library(MASS)
library(plyr)
library(ggplot2)
library(boot)

# Load dataset
library(geoR)
gb <- gambia

# Create village number as a variable
v_loc <- unique(gb[,"x"])
v <- match(gb[,"x"],v_loc)
gb$v <- v

# Find the villages that have both netuse and nonetuse
v.netuse0 <- unique(gb$v[gb$netuse==0])
v.netuse1 <- unique(gb$v[gb$netuse==1])
v.netuse01 <- intersect(v.netuse0, v.netuse1)
## gb <- gb[(gb$v %in% v.netuse01),]

# Find the pos rate in each village for netuse and nonetuse
gb.v.netuse <- aggregate(
    list(pos=gb$pos, popu=1),
    by = list(v=gb$v, netuse=gb$netuse),
    FUN = sum
)
gb.v.netuse$pos.rate <- gb.v.netuse$pos / gb.v.netuse$popu

# Put the two lines for each village in one line
gb.v <- merge(
    gb.v.netuse[gb.v.netuse$netuse==0, c("v", "pos", "popu", "pos.rate")], 
    gb.v.netuse[gb.v.netuse$netuse==1, c("v", "pos", "popu", "pos.rate")], 
    by = "v",
    all = TRUE
)
names(gb.v) <- c("v", 
                 "pos.nonet", "popu.nonet", "rate.nonet", 
                 "pos.net", "popu.net", "rate.net"
                 )
gb.v$effect <- gb.v$rate.net - gb.v$rate.nonet

linear.model.JAGS <- function(){
    for(i in 1:n){
        y[i] ~ dbern(p[i])
        p[i] <- exp(logitp[i]) / (1 + exp(logitp[i])) 
        logitp[i] <- alpha + beta * x[i]
    }
    alpha ~ dnorm(mu.alpha, tausq.alpha)
    beta ~ dnorm(mu.beta, tausq.beta)
}

linear.model.JAGS.randalpha <- function(){
    for(i in 1:n){
        y[i] ~ dbern(p[i])
        p[i] <- exp(logitp[i]) / (1 + exp(logitp[i])) 
        logitp[i] <- alpha[v[i]] + beta * x[i]
    }
    for(j in 1:m){
        alpha[j] ~ dnorm(mu.alpha, tausq.alpha)
    }
    beta ~ dnorm(mu.beta, tausq.beta)
}



linear.model.JAGS.randbeta <- function(){
    for(i in 1:n){
        y[i] ~ dbern(p[i])
        p[i] <- exp(logitp[i]) / (1 + exp(logitp[i])) 
        logitp[i] <- alpha + beta[v[i]] * x[i]
    }
    for(j in 1:m){
        beta[j] ~ dnorm(mu.beta, tausq.beta)
    }
    alpha ~ dnorm(mu.alpha, tausq.alpha)
}


# Data: y, x, v, n = length(y), m = length(v)
linear.model.JAGS.randalphabeta <- function(){
    for(i in 1:n){
        y[i] ~ dbern(p[i])
        p[i] <- exp(logitp[i]) / (1 + exp(logitp[i])) 
        logitp[i] <- alpha[v[i]] + beta[v[i]] * x[i]
    }
    for(j in 1:m){
        alpha[j] ~ dnorm(mu.alpha, tausq.alpha)
        beta[j] ~ dnorm(mu.beta, tausq.beta)
    }
}

dat.JAGS <- list(y = gb$pos,
                x = gb$netuse,
                v = gb$v,
                n = nrow(gb),
                m = length(unique(gb$v)),
                mu.alpha = 0,
                tausq.alpha = 1e-3,
                mu.beta = 0,
                tausq.beta = 1e-3
                )
para.JAGS <- c("alpha", "beta")


fit.JAGS.randalphabeta <- jags(data = dat.JAGS,
                 parameters.to.save = para.JAGS,
                 ## inits = inits.JAGS
                 n.chains = 1,
                 n.iter = 1e4,
                 n.burnin = 1e3,
                 model.file = linear.model.JAGS.randalphabeta
                 )

fit.JAGS <- jags(data = dat.JAGS,
                 parameters.to.save = para.JAGS,
                 ## inits = inits.JAGS
                 n.chains = 1,
                 n.iter = 1e4,
                 n.burnin = 1e3,
                 model.file = linear.model.JAGS
                 )

fit.JAGS.randalpha <- jags(data = dat.JAGS,
                 parameters.to.save = para.JAGS,
                 ## inits = inits.JAGS
                 n.chains = 1,
                 n.iter = 1e4,
                 n.burnin = 1e3,
                 model.file = linear.model.JAGS.randalpha
                 )

fit.JAGS.randbeta <- jags(data = dat.JAGS,
                 parameters.to.save = para.JAGS,
                 ## inits = inits.JAGS
                 n.chains = 1,
                 n.iter = 1e4,
                 n.burnin = 1e3,
                 model.file = linear.model.JAGS.randbeta
                 )

print("Fixed intercept and slope")
print(fit.JAGS$BUGSoutput$DIC)
print(fit.JAGS$BUGSoutput$pD)
print("Random intercept")
print(fit.JAGS.randalpha$BUGSoutput$DIC)
print(fit.JAGS.randalpha$BUGSoutput$pD)
print("Random slope")
print(fit.JAGS.randbeta$BUGSoutput$DIC)
print(fit.JAGS.randbeta$BUGSoutput$pD)
print("Random intercept and slope")
print(fit.JAGS.randalphabeta$BUGSoutput$DIC)
print(fit.JAGS.randalphabeta$BUGSoutput$pD)

alpha.result <- fit.JAGS.randalphabeta$BUGSoutput$mean$alpha
beta.result <- fit.JAGS.randalphabeta$BUGSoutput$mean$beta
alpha.result[-v.netuse01] <- NA
beta.result[-v.netuse01] <- NA
rate.nonet.result <- round(inv.logit(alpha.result), 3)
rate.net.result <- round(inv.logit(alpha.result + beta.result),3)
effect.result <- rate.net.result - rate.nonet.result 

print("Observed extremes")
print(c(which.min(rate.nonet.result), min(rate.nonet.result, na.rm = TRUE)))
print(c(which.max(rate.nonet.result), max(rate.nonet.result, na.rm = TRUE)))
print(c(which.min(effect.result), min(effect.result, na.rm = TRUE)))
print(c(which.max(effect.result), max(effect.result, na.rm = TRUE)))

print("Regression extremes")
gb.v[which.min(gb.v$rate.nonet), c("v", "rate.nonet")]
gb.v[which.max(gb.v$rate.nonet), c("v", "rate.nonet")]
gb.v[which.min(gb.v$effect), c("v", "effect")]
gb.v[which.max(gb.v$effect), c("v", "effect")]

par(mfrow=c(1,2))
v.num <- nrow(gb.v)
plot(gb.v$rate.nonet, rate.nonet.result, pch=".")
text(gb.v$rate.nonet, rate.nonet.result, c(1:v.num))
abline(0,1)
plot(gb.v$effect, effect.result, pch=".")
text(gb.v$effect, effect.result, c(1:v.num))
abline(0,1)


# Larger precision
dat.JAGS.largeTausq <- list(y = gb$pos,
                x = gb$netuse,
                v = gb$v,
                n = nrow(gb),
                m = length(unique(gb$v)),
                mu.alpha = 0,
                tausq.alpha = 1e0,
                mu.beta = 0,
                tausq.beta = 1e0
                )
fit.JAGS.randalphabeta.largeTausq <- jags(data = dat.JAGS.largeTausq,
                 parameters.to.save = para.JAGS,
                 ## inits = inits.JAGS
                 n.chains = 1,
                 n.iter = 1e4,
                 n.burnin = 1e3,
                 model.file = linear.model.JAGS.randalphabeta
                 )

# Nonzero means
dat.JAGS.offMu <- list(y = gb$pos,
                x = gb$netuse,
                v = gb$v,
                n = nrow(gb),
                m = length(unique(gb$v)),
                mu.alpha = logit(0.5),
                tausq.alpha = 1e-3,
                mu.beta = logit(0.4) - logit(0.6),
                tausq.beta = 1e-3
                )
fit.JAGS.randalphabeta.offMu <- jags(data = dat.JAGS.offMu,
                 parameters.to.save = para.JAGS,
                 ## inits = inits.JAGS
                 n.chains = 1,
                 n.iter = 1e4,
                 n.burnin = 1e3,
                 model.file = linear.model.JAGS.randalphabeta
                 )

## plot(fit.JAGS.randalphabeta)
## plot(fit.JAGS.randalphabeta.largeTausq)
## plot(fit.JAGS.randalphabeta.offMu)

par(mfrow=c(1,2))
plot(
    fit.JAGS.randalphabeta$BUGSoutput$mean$alpha,
    fit.JAGS.randalphabeta.largeTausq$BUGSoutput$mean$alpha
)
plot(
    fit.JAGS.randalphabeta$BUGSoutput$mean$beta,
    fit.JAGS.randalphabeta.largeTausq$BUGSoutput$mean$beta
)

plot(
    fit.JAGS.randalphabeta$BUGSoutput$mean$alpha,
    fit.JAGS.randalphabeta.offMu$BUGSoutput$mean$alpha
)
plot(
    fit.JAGS.randalphabeta$BUGSoutput$mean$beta,
    fit.JAGS.randalphabeta.offMu$BUGSoutput$mean$beta
)
