n <- 1e6

# Problem 3
y <- runif(n,0,1)
x <- runif(n,-y,y)
print(cor(x,y))

# Problem 6
x <- rexp(n,2)
y <- rexp(n,3)
y <- y[y > x]
mean(y)
8/15
