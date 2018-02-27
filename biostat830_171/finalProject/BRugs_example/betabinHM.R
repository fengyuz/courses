#######################################################
# Beta-Binomial-Uniform model
# from BCLM text, Example 2.7
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
I<-10
dput(pairlist(x=x,n=n,I=I),"betabinHM_data.txt")
dput(pairlist(a=4,b=2,p=c(.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)),
  "betabinHM_inits.txt")

modelCheck("betabinHM_BUGS.txt")
modelData(paste("betabinHM_data.txt",sep=""))
modelCompile(numChains=1)
modelInits("betabinHM_inits.txt")
modelGenInits()    # to generate a starting value for the missing x

modelUpdate(1000)
samplesSet(c("a","b","p"))

modelUpdate(10000)
samplesHistory("*", mfrow = c(3, 2))
samplesDensity("*")
samplesStats(c("a","b","p"))
dput(samplesSample("p[10]"),"betabinHM_p10out.txt")

#  HERE IS THE BIVARIATE POSTERIOR FOR (a,b):
postscript("Pics/betabinHM.ps")
par(mfrow=c(1,1))
par(lwd=1.5,cex=1.5,mar=c(5,4,1,1))

plot(samplesSample("a"),samplesSample("b"),xlab="a",ylab="b",cex=1.5)
dev.off()

#  HERE IS THE UNIVARIATE POSTERIOR FOR p[10]:
postscript("Pics/beta10745post.ps")
par(lwd=1.5,cex=1.5,mar=c(5,4,1,1))

p<-seq(0,1,length=401)
phat<- c(1, .4, .69, .53, .36, .783, .9, .778, .67)
n <- n[1:9]

samp <- samplesSample("p[10]")

#
# plot the prior (beta) and the posterior (from WB)
#
plot(p,dbeta(p,107,45),type="l",lty=2,lwd=2,ylab="density",cex=1.5)
lines(density(samp,bw=.04,from=0,to=1),lty=1,lwd=2)
lines(phat,n/12, type = "h", col = "red", lwd=5)
legend("topleft",c("posterior","likelihood (all p_i's equal)"),lty=1:2)
dev.off()

