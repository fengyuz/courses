## Biostat 651 Hw 2

## Problem 2(c)

dat <- read.csv("hw2.csv", header = TRUE)
Y <- dat$Y
n <- length(Y)
X <- cbind(rep(1,n), dat$X1, dat$X2)
nu <- 3
b <- c(-1, -1, -1)

n.it <- 100
B <- matrix(0, n.it, length(b))

for(i in 1:n.it){
    B[i,] = b
    eta <- X %*% b
    mu <- -1/eta
    v <- c(mu^2)
    V <- diag(v)
    V.inv <- diag(1/v)
    Z <- eta + V.inv %*% (Y - mu)
    b <- solve(t(X) %*% V %*% X) %*% t(X) %*% V %*% Z
}

bhat <- tail(B, 1)
print(bhat)

## Problem 2(d)

J <- nu * t(X) %*% V %*% X
bhat.se <- sqrt(diag(solve(J)))

# 95% CI for beta.hat.1
bhat[2] + c(-1,1) * 1.96 * bhat.se[2]
# 95% CI for beta.hat.2
bhat[3] + c(-1,1) * 1.96 * bhat.se[3]

## Problem 2(e)
lambda.null <- rep(mean(Y), n)
lambda.alt <- -1/eta

like <- function(lambda, y){
     -nu * sum(log(lambda)) - nu * sum(Y / lambda)
}

lr.stat <- 2 * (like(lambda.alt, Y) - like(lambda.null,Y))
lr.stat
lr.pval <- 1 - pchisq(lr.stat, df = 2)
lr.pval

