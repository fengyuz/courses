#######################################################
# Beta-Binomial-Uniform model
# from BCLM text, Example 2.7
#
# This is the newer version by TL that uses "bugsData" and "BRugsFit" commands
#
# This is the BRugs code that calls three other files:
#   WinBUGS code:  betabinHM_BUGS.txt
#   data:          betabinHM_data.txt (created by this program)
#   inits:         betabinHM_inits.txt (created by this program)
#######################################################

setwd("~/biostat830/finalProject/BRugs_example")    # change this to *your* working directory
library(BRugs)

x <- c(20, 4, 11, 10, 5, 36, 9, 7, 4, NA)
n <- c(20, 10, 16, 19, 14, 46, 10, 9, 6, 1)
I <- 10
#dput(pairlist(x=x,n=n,I=I),"betabinHM_data.txt")
bugsData(c('x', 'n', 'I'),"betabinHM_data.txt")

#dput(pairlist(a=4,b=2,p=c(.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)), "betabinHM_inits.txt")
bugsInits(list(pairlist(a=4,b=2,p=c(.5,.5,.5,.5,.5,.5,.5,.5,.5,.5),x=c(rep(NA,9),0))), 1, "betabinHM_inits.txt")
BRugsFit('betabinHM_BUGS.txt', 'betabinHM_data.txt', c('betabinHM_inits.txt'), numChains = 1,
    c('a','b','p'), nBurnin = 1000, nIter = 10000)

samplesHistory("*", mfrow = c(3, 2))
plot(samplesSample("p[10]"), type='l')

samplesDensity("*")
plot(density(samplesSample("p[10]")))
par(mfrow=c(1,1))

plot(samplesSample("a"),samplesSample("b"),xlab="a",ylab="b")

p <- seq(0,1,length=401)
phat <- c(1, .4, .69, .53, .36, .783, .9, .778, .67)
n <- n[1:9]
samp <- samplesSample("p[10]")
plot(p,dbeta(p,107,45),type="l",lty=2,lwd=2,ylab="density", mfrow=c(1,1))
lines(density(samp,bw=.04,from=0,to=1),lty=1,lwd=2)
lines(phat,n/12, type = "h", col = "red", lwd=5)
legend(.1,10,c("posterior","likelihood (p_i's equal)","data"),lty=c(1,2,1), 
  lwd=c(1,1,5), col=c(1,1,2))

