# Survmeth 895 Hw4 Problem 1(b)

require(MASS)
dat <- read.csv("survmeth895_hw4_dataset.csv", header = TRUE)
dat.inc <- na.omit(dat)
dat.exc <- dat[rowSums(is.na(dat)) > 0,]
n <- 10
x <- dat.inc$Total.Cultivated.Area
x <- cbind(rep(1, n), x)
x.exc <- dat.exc$Total.Cultivated.Area
x.exc.n <- length(x.exc)
y <- dat.inc$Area.Under.Wheat
b.hat <- solve(t(x) %*% x) %*% (t(x) %*% y)
b.hat <- c(b.hat)
b.var <- solve(t(x) %*% x)
s <- sd(y)

n.sim <- 10
sigma.sq <- (n-2) * s^2 / rchisq(n.sim * x.exc.n, n - 2)
sigma.sq <- matrix(sigma.sq, n.sim, x.exc.n)
total.wheat <- rep(sum(y), n.sim)
for (i in 1 : n.sim){
    for (j in 1:x.exc.n){
        b <- mvrnorm(1, b.hat, b.var * sigma.sq[i, j])
        mu <- b[1] + b[2] * x.exc[j]
        y.pred <- rnorm(1, mu, sqrt(sigma.sq[i,j]))
        total.wheat[i] = total.wheat[i] + y.pred
    }
}

print(mean(total.wheat))
print(quantile(total.wheat, c(0.025, 0.975)))
