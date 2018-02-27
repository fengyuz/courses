# source("/Users/pm/s/first.s")
dir <- "~/biostat830/finalProject/phaseIsim/"
setwd(dir)

## scaled doses (=prior expected toxicities at doses 1..5)
p.tox0<- c(.05,.1,.2,.3,.5)              
dose  <- log(p.tox0/(1-p.tox0)) - 3
Fstar <- 0.3                             # target tox

## assumed scenarios
# p1    <-  c(.05,.15,.3,.45,.6)     # scenario 1 
p1    <- c(.05,.10,.2,.30,.5)
p2    <- c(.3,  .4, .52, .61, .76, .87)  # scneario 2
Th    <- 6                               # fixed horizon



##################################################################
# Simulation
##################################################################

sim.trial <- function(nmax=30,p0=p1)
  { ## simulate one realization of the trial
    
    dta <- NULL  # initial data
    d   <- 1     # initial dose
    tm  <- 0     # running calendar time in months
    
    ## initial escalation
    k <- 3
    repeat{
      uk <- sim.uk(p0,d,k=k) # next batch
      dta  <- rbind(dta, cbind(d,tm,uk,0,1.0))
      tm <- tm+6
      if (any(uk < Th) | (d==5) ) # any toxicity?
        break;               # start CRM
      d <- d+1               # escalate to next dose
    }

    ## TITE CRM
    k <- 1
    repeat{
      bbar <- post.mean(dta,tm)
      Fall <-   Ftox(1:5, bbar)
      d <-    which.min(abs(Fall-Fstar))
      n <- nrow(dta)              # sample size
      ## cat(" n=",n," d=",d," b=",bbar,"tm=",tm,"\n")
      if (n >= nmax)
        # add stop crit here
        break
      uk <-   sim.uk(p0,d,k=k)  # next batch
      dta  <- rbind(dta, cbind(d,tm,uk,1,bbar))
      tm <- tm+0.5
    }
    ## now d is the recommended dose and tm the total time

    colnames(dta) <- c("d","tm","uk","CRM","beta")
    return(list(dta=dta,tm=tm,dstar=d,bhat=bbar))
  }

k <- 3
sim.uk <- function(p0,d,k=3)
  { ## generate next batch of k patients
    ## assigned at dose d (index 1..5)
    ## using conditional uniform model
    ## batch size k
    if (!is.element(d,1:5)){
      cat("\n *** error: d is not 1...5.\n"); return(-1)
    }

    Fd <- p0[d]
    y <- as.numeric(runif(k) < Fd)
    ## generate time to event
    uk <- ifelse(y==1,runif(k,0,Th),Th)  # uniform model
    return(uk)
  }

    

##################################################################
# Model
##################################################################

Ftox <- function(d,beta)
  { ## returns (model) prob of toxicity F(d,beta)
    # return(dose[d]^beta)
    exp(3 + beta * dose[d]) / (1 + exp(3 + beta * dose[d]))
  }

Ftoxb <- function(beta)
  { ## returns list of tox prob's over dose range
    exp(3 + beta * dose) / (1 + exp(3 + beta * dose))
  ## return(dose^beta)
}

logPr <- function(b)
  { # p(b) = exp(-b)
    return( -b )
  }
    
logLik <- function(b, dta, tm)
  {## returns TITE log likelihood
   ## vectorized, for vector b of beta values
    di   <- dta[,1]
    t0i  <- dta[,2]
    uki  <- dta[,3]
    if (any(t0i > tm)){
      cat("\n *** error: patient with future recruitment time? \n")
      return(-1)
    }
    tos <- tm-t0i                # time on study
    yni <- (uki<Th) & (tm>t0i+uki) # toxicty observed by now
    wni <- tos/Th                # weights
    wni <- ifelse(wni<1, wni, 1)
    logL <- rep(0,length(b))     # log likelihood vector
    for(i in 1:length(b)){
      Fall <- Ftoxb(b[i])        # pr tox for all doses
      Gi <-   wni*Fall[di]       # (weighted) toxicities for dta
      logL[i] <- sum(log(Gi[yni]))+sum(log(1-Gi[!yni]))
      ## L = prod_{y=1} G(di,wni,b[i]) * prod_{y=0} (1-G(..))
    }
    return(logL)
  }
    
    
post.mean <- function(dta,tm)
  {# returns posterior mean based on TITE likelihood
    b <- seq(from=0.01, to= 7.0, length=50)
    llik <- logLik(b,dta,tm)
    lpr <-  logPr(b)
    lpost <- llik+lpr
    lpost <- lpost-max(lpost) # scale
    post  <- exp(lpost)
    bbar  <- sum(post*b)/sum(post)
    return(bbar)
  }


##################################################################
# example run
##################################################################

example <- function()
  {# run this as example
    trial <- sim.trial(nmax=48, p0=p1)
    dta <- data.frame( trial$dta )
    attach(dta)
    # ps("TITE-CRM1")
    plot(tm, d,    type="p",pch=18,
         xlab="MONTH", ylab="ALLOC",bty="l")
    points(trial$tm,trial$dstar,pch="*",col=2,cex=2)
    # devoff()
    # ps("TITE-CRM2")
    plot(tm[CRM==1], beta[CRM==1], type="l",pch=19,
         xlab="MONTH", ylab="BETA",bty="l",ylim=c(0,2),
         xlim=c(0,max(tm)))
    points(trial$tm,trial$bhat,pch="*",col=2,cex=2)
    # devoff()
    detach(dta)
}

nSim = 1000
result <- NULL
for(i in 1:nSim){
  trial <- sim.trial(nmax=n, p0=p1)
  dta <- data.frame(trial$dta)
  n <- nrow(dta)
  num.pat <- unname(table(factor(dta$d, lev = 1:5)))
  prop.dlt <- sum(dta$uk < Th) / n
  result <- rbind(result, c(trial$dstar, num.pat, prop.dlt, n))
}

prop.pat <- colSums(result[,2:6]) / sum(result[,2:6])
prop.mtd <- table(result[,1]) / nSim
prop.dlt <- mean(result[,7])
samp.size <- mean(result[,8])
print(rbind(prop.pat, prop.mtd))
print(prop.dlt)
print(samp.size)