nSim = 1e4
theta1=rbeta(nSim, 29.5, 9.5)
yexc1=rbinom(nSim,1150,theta1)
theta2=rbeta(nSim, 17.5, 16.5)
yexc2=rbinom(nSim,488,theta2)
theta3=rbeta(nSim, 6.5, 27.5)
yexc3=rbinom(nSim,180, theta3)
theta4=rbeta(nSim, 4.5, 34.5)
yexc4=rbinom(nSim, 58,theta4)
t= 60+yexc1+yexc2+yexc3+yexc4
mean(t)
sd(t)
quantile(t, c(0.025, 0.975))

theta = rbeta(nSim, 59.5, 89.5)
yexc = rbinom(nSim, 1876, theta)
s = 60 + yexc
mean(s)
sd(s)
quantile(s, c(0.025, 0.975))
