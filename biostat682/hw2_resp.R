set.seed(10)
# Density functions
post.a <- function(mu,sd,y){
    ldens <- 0
    for (i in 1:length(y)) ldens <- ldens +
    log(dnorm(y[i],mu,sd))
    ldens
}
post.b <- function(mu,sd,y){
    ldens <- 0
    for (i in 1:length(y)) ldens <- ldens +
    log(pnorm(y[i]+0.5,mu,sd) - pnorm(y[i]-0.5,mu,sd))
    ldens
}

# Display the summary
summ <- function(x){
    result <- c(mean(x),sqrt(var(x)), quantile(x, seq(0.1, 0.9, 0.1)))
    round(result, 2)
}

# Input data
y <- c(10,10,12,11,9)

# Descriptive statistics
n <- length(y)
ybar <- mean(y)
s2 <- sum((y-mean(y))^2)/(n-1)
nsim <- 1e4
print(ybar)
print(s2)

# Create the grid for simulation
mugrid.lo <- 0
mugrid.hi <- 20
mugrid.n <- 1e3
mugrid <- seq(mugrid.lo,mugrid.hi,length=mugrid.n)
logsdgrid.lo <- -5
logsdgrid.hi <- 5
logsdgrid.n <- 1e3
logsdgrid <- seq(logsdgrid.lo,logsdgrid.hi,length=logsdgrid.n)

# Grid sim for unrounded data
logdens <- outer (mugrid, exp(logsdgrid), post.a, y)
dens <- exp(logdens - max(logdens))
sd.a <- sqrt((n-1)*s2/rchisq(nsim,n-1))
mu.a <- rnorm(nsim,ybar,sd.a/sqrt(n))
print (rbind (summ(mu.a),summ(sd.a)))

# Grid sim for rounded data
logdens <- outer (mugrid, exp(logsdgrid), post.b, y)
dens <- exp(logdens - max(logdens))
dens.mu <- apply(dens,1,sum)
muindex <- sample (1:length(mugrid), nsim, replace=T,
prob=dens.mu)
mu.b <- mugrid[muindex]
sd.b <- rep (NA,nsim)
for (i in (1:nsim)){
    sd.b[i] <- exp (sample (logsdgrid, 1, prob=dens[muindex[i],]))
}
print (rbind (summ(mu.b),summ(sd.b)))

# Compare the two posteriors
par(mfrow=c(2,1))
qqplot(mu.a, mu.b)
abline(0,1)
qqplot(sd.a, sd.b)
abline(0,1)

# Compute (z_1 - z_2)^2 given the rounded data
z <- matrix (NA, nsim, n)
for (i in 1:length(y)){
    lower <- pnorm (y[i]-.5, mu.b, sd.b)
    upper <- pnorm (y[i]+.5, mu.b, sd.b)
    z[,i] <- qnorm (lower + runif(nsim)*(upper-lower), mu.b, sd.b)
}
mean ((z[,1]-z[,2])^2)
