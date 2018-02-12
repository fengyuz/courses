require(matrixStats)
col.names = c("group", "id", "cho0", "cho6", "cho12", "cho20", "cho24")
cho <- read.table("../Downloads/cholesterol-data.txt", na.strings = ".", col.names = col.names)
means <- c()
vars <- c()
sds <- c()
for(i in 1:2){
    mean.this <- colMeans(cho[cho$group==i,-(1:2)], na.rm = TRUE)
    var.this <- colVars(as.matrix(cho[cho$group==i,-(1:2)]), na.rm = TRUE)
    sd.this <- colSds(as.matrix(cho[cho$group==i,-c(1:2)]), na.rm = TRUE)
    means <- rbind(means, mean.this)
    vars <- rbind(vars, var.this)
    sds <- rbind(sds, sd.this)
}
rownames(means) <- c(1,2)
rownames(vars) <- c(1,2)
rownames(sds) <- c(1,2)
colnames(vars) <- colnames(means)
colnames(sds) <- colnames(means)
occasions <- c(0, 6, 12, 20, 24)
matplot(occasions, t(means), type="b")

for(i in 1:5){
    cho.uni.this <- cho[,c(1,2,2+i)]
    colnames(cho.uni.this) <- c("group", "id", "cho")
    cho.uni.this$occasion <- occasions[i]
    cho.uni <- rbind(cho.uni, cho.uni.this)
}
str(cho.uni)


