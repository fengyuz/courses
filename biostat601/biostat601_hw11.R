## Problem 3(c)
rm(list=ls())
m <- 1e2
l <- 1e5
k <- 1e1
x <- matrix(runif(m*l*k, -1, 1), m, l*k)
a <- 1/3
epsi <- 1e-3
pp <- c()
for(i in 1:k){
    n <- i*l
    t <- rowSums((x^2)[,1:n]) / n
    p <- mean(abs(t-a) > epsi)
    pp <- c(pp,p)
}
plot(pp)

## Problem 4(d)
rm(list=ls())
m <- 1e4
n <- 100
x <- matrix(runif(m*n, -0.5, 0.5), m, n)
prob <- mean(abs(rowSums(x)) > 10)
print(prob)

## Problem 6(d)
rm(list=ls())
m <- 1e4
n <- 1e2
qq <- c()
for(i in 1:m){    
    x <- runif(n, 0, 1)
    v <- min(x)
    q <- n*v
    qq <- c(qq, q)
}
p <- ppoints(100)
qt <- quantile(qq,p)
plot(qexp(p),qt)
