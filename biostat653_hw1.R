require(matrixStats)

leadiq <- read.table("~/leadiq.txt")
colnames(leadiq) <- c("id", "expo.cat", "gender", "age", "iq")
leadiq$expo.curr <- as.integer(leadiq$expo.cat == 2)
leadiq$expo.past <- as.integer(leadiq$expo.cat == 3)
leadiq$expo.curr.gender <- leadiq$expo.curr * leadiq$gender
leadiq$expo.past.gender <- leadiq$expo.past * leadiq$gender
leadiq.model <- lm(iq ~ gender + expo.curr + expo.past + expo.curr.gender + expo.past.gender, data=leadiq)
summary(leadiq.model)

## lead <- read.table("~/lead.txt")
## colnames(lead) <- c("id", "week.0", "week.1", "week.4", "week.6")
## lead.time <- c(0,1,4,6)
## lead.mat <- as.matrix(lead[,-1])
## lead.mean <- colMeans(lead.mat)
## lead.sd <- colSds(lead.mat)
## lead.var <- lead.sd^2
## print(lead.mean)
## print(lead.sd)
## print(lead.var)
## ## matplot(lead.time, t(lead.mat), type="l", lty=1, lwd=1, col=1)
## ## points(lead.time, lead.mean, type="l", lty=1, lwd=5, col=2)
## lead.cov <- cov(lead.mat)
## lead.cor <- cor(lead.mat)
## print(lead.cov)
## print(lead.cor)

leadiq$expo.ever <- leadiq$expo.curr + leadiq$expo.past
leadiq$expo.ever.gender <- leadiq$expo.ever * leadiq$gender
