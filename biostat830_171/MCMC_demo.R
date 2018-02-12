

#### Accept-Reject for Normal from t w/3df

M = 1.1*dnorm(0)/dt(0,3)

cdraws = rt(50000,3)
udraws = runif(50000)

tmp = (udraws <= dnorm(cdraws)/(dt(cdraws,3)*M))
draws = cdraws[tmp]

y = seq(-20,20,.01)
quartz()
par(mfrow=c(2,2))
hist(draws,probability=T,nclass=50)
lines(y,dnorm(y),col=2)
qqnorm(draws)
abline(0,1,col=2)

#### Accept-Reject for t w/3df from Normal (incorrect because f/q is unbounded in the tails)

M = 1
ndraws = rnorm(50000)
udraws = runif(50000)

tmp = (udraws <= dt(ndraws,3)/(dnorm(ndraws,0,4)*M))
draws = ndraws[tmp]

rc = rt(length(draws),3)

hist(draws,probability=T,nclass=50)
lines(y,dt(y,3),col=2)
qqplot(rc,draws,xlim=c(-4,4),ylim=c(-4,4))
abline(0,1,col=2)

#### Independent MH for t w/3 df using a normal

draws = rep(0,56000)
x = 1
count = 0
sd = 4
for (i in 1:length(draws)) {
	q = rnorm(1,0,sd)   # try different variances

	alpha = (dt(q,3)/dt(x,3) * (dnorm(x,0,sd)/dnorm(q,0,sd)))  # proposal ratio = 1, hence does not appear in alpha
	if (runif(1) < alpha) {
		draws[i] = q
		x = q
		count = count + 1
	}
	else {
		draws[i] = x
	}

}
count/56000
quartz()
par(mfrow=c(2,2))
hist(draws[5001:56000],probability=T,nclass=100,xlim=c(-10,10),ylim=c(0,dt(0,3)+.01))
lines(y,dt(y,3),col=2)
qqplot(rc,draws,xlim=c(-10,10),ylim=c(-10,10),main="Q-Q Plot")
abline(0,1,col=2)
plot(draws,type="l",col=4,main="Trace Plot")
 quantile(draws,c(.01,.99))
 qt(c(0.01,.99),3)
sd(draws) # sd of t - 3 is sqrt(3) = 1.732


#### Random-walk MH for Cauchy using a N(x,4^2) 

draws = rep(0,56000)
x = 5
sd = 4
count = 0
for (i in 1:length(draws)) {
	q = rnorm(1,x,sd)
	alpha = dt(q,3)/dt(x,3)  # proposal ratio = 1, hence does not appear in alpha
	if (runif(1) < alpha) {
		draws[i] = q
		x = q
		count = count + 1
	}
	else {
		draws[i] = x
	}
}
count/56000
quartz()
par(mfrow=c(2,2))
hist(draws[5001:56000],probability=T,nclass=100,xlim=c(-10,10),ylim=c(0,dt(0,3)+.01))
lines(y,dt(y,3),col=2)
qqplot(rc,draws,xlim=c(-10,10),ylim=c(-10,10),main="Q-Q Plot")
abline(0,1,col=2)
plot(draws,type="l",col=4,main="Trace Plot")
 quantile(draws,c(.01,.99))
 qt(c(0.01,.99),3)
sd(draws) # sd of t - 3 is sqrt(3) = 1.732



#### Random-walk MH for t w/3 df using a U(x-5,x+5) 

draws = rep(0,56000)
x = 5
count = 0
sd = 5
for (i in 1:length(draws)) {
	q = runif(1,x-sd,x+sd)
	alpha = (dt(q,3)/dt(x,3))  # proposal ratio = 1, hence does not appear in alpha
	if (runif(1) < alpha) {
		draws[i] = q
		x = q
		count = count + 1
	}
	else {
		draws[i] = x
	}
}
count/56000
quartz()
par(mfrow=c(2,2))
hist(draws[5001:56000],probability=T,nclass=100,xlim=c(-10,10),ylim=c(0,dt(0,3)+.01))
lines(y,dt(y,3),col=2)
qqplot(rc,draws,xlim=c(-10,10),ylim=c(-10,10),main="Q-Q Plot")
abline(0,1,col=2)
plot(draws,type="l",col=4,main="Trace Plot")
 quantile(draws,c(.01,.99))
 qt(c(0.01,.99),3)
sd(draws) # sd of t - 3 is sqrt(3) = 1.732






x = rep(0,10001)
y = rep(0,10001)

for (i in 2:length(x)) {
	x[i] = rnorm(1,y[i-1],1)
	y[i] = rnorm(1,x[i],1)
}

quartz()
plot(x,y,pch=".")