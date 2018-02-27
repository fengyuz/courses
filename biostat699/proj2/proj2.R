rm(list=ls())

library(readxl)
library(randomForest)
library(rpart)
library(survival)
library(survminer)
library(ModelGood)

## cp.complete.idx <- complete.cases(liver[,c("cp_pretx", "cp_postfx3")])
## cor(liver$cp_pretx[cp.complete.idx],
##      liver$cp_postfx3[cp.complete.idx])
## icg15.complete.idx <- complete.cases(liver[,c("icg15_pretx", "icg15_postfx3")])
## cor(liver$icg15_pretx[icg15.complete.idx],
##      liver$icg15_postfx3[icg15.complete.idx])
## albi_score.complete.idx <- complete.cases(liver[,c("albi_score_pretx", "albi_score_postfx3")])
## cor(liver$albi_score_pretx[albi_score.complete.idx],
##      liver$albi_score_postfx3[albi_score.complete.idx])

## # Tree
## fit.tree <- rpart(resp ~ ., data = liver.pred, subset=train.idx, method="class")
## predict.tree <- predict(fit.tree, newdata=liver.pred[-train.idx, ], type="class")
## brier.tree.this <- mean(predict.tree != liver.pred$resp[-train.idx])
## brier.tree[i] <- brier.tree.this

## # Scale time-to-event to exponential
## temp.fit <- coxph(Surv(time, status) ~ 1, liver)
## liver$time.scaled <- predict(temp.fit, type = 'expected')
## surv.fit <- survfit(Surv(time.scaled, status) ~ 1, liver)
## plot(surv.fit)


setwd("~/courses/biostat699/proj2/")

print.complete <- function(liver){
  p <- ncol(liver)
  n <- nrow(liver)
  complete.rate <- sapply(1:p, function(i) sum(complete.cases(liver[,i]))/n)
  complete.rate <- round(complete.rate, 2)
  names(complete.rate) <- colnames(liver)
  complete.rate <- complete.rate[order(complete.rate)]
  print(data.frame(complete.rate))
  print(paste0("Complete/Total: ", sum(complete.cases(liver)), "/", n))
}

liver <- read_excel("proj2.xlsx", sheet = 1, col_names = TRUE, na = "")
n <- nrow(liver)
p <- ncol(liver)

# Remove low complete rate predictors
lowcomplete.names <- c("cp_postfx3",
                       "icg15_postfx3",
                       "albi_score_postfx3",
                       "pretx_disease")
liver <- liver[, !colnames(liver) %in% lowcomplete.names]
p <- ncol(liver)

# Use complete data points
liver <- liver[complete.cases(liver), ]
n <- nrow(liver)

# Remove duplicate individuals
duplicated.idx <- duplicated(liver$ptid)
liver <- liver[!duplicated.idx, ]
n <- nrow(liver)

# Format predictors
liver$mean_total_liver_dose <- as.numeric(liver$mean_total_liver_dose)
liver$time <- as.integer(difftime(liver$date_followup_death, liver$rt_end, units="days"))
liver$status <- as.logical(liver$status - 1)
liver$diag.to.treat <- as.integer(liver$date_rt_start - liver$date_primarydiagnosis)
liver$treat.length <- as.integer(difftime(liver$rt_end, liver$date_rt_start, units="days"))

# Generate random forest
gen.forest <- function(liver, liver.nonpred, train.prop, time.cut){
  ## Create categorical response
  time.cut.n <- length(time.cut)
  surv.cat <- matrix(0, n, time.cut.n)
  colnames(surv.cat) <- time.cut
  for(i in 1:n){
    for(j in 1:time.cut.n){
      time.this <- liver$time[i]
      status.this <- liver$status[i]
      cut.this <- time.cut[j] * 30
      if(time.this < cut.this){
        if(status.this){
          surv.cat[i,j] <- FALSE
        }
        else{
          surv.cat[i,j] <- NA
        }
      }
      else{
        surv.cat[i,j] <- TRUE
      }
    }
  }
  train.oob <- rep(-1, time.cut.n)
  test.oob <- rep(-1, time.cut.n)
  p <- ncol(liver) - length(liver.nonpred)
  var.importance <- matrix(-1, time.cut.n, p)
  colnames(var.importance) <- colnames(liver[, !colnames(liver) %in% liver.nonpred])

  for(i in 1:time.cut.n){
    liver.pred <- liver[, !colnames(liver) %in% liver.nonpred]
    liver.pred$resp <- as.factor(surv.cat[,i])
    liver.pred <- liver.pred[complete.cases(liver.pred),]
    liver.pred.n <- nrow(liver.pred)
    train.idx <- sample(1:liver.pred.n, liver.pred.n * train.prop)
    ## Forest
    fit.forest <- randomForest(
      y=liver.pred$resp[train.idx],
      x=liver.pred[train.idx, !colnames(liver.pred)=="resp"],
      ytest=liver.pred$resp[-train.idx],
      xtest=liver.pred[-train.idx, !colnames(liver.pred)=="resp"]
    )
    forest.impo <- fit.forest$importance
    ## forest.impo <- data.frame(
    ##   var=attributes(forest.impo)$dimnames[[1]],
    ##   dGini=unname(forest.impo)
    ## )
    ## forest.impo <- forest.impo[order(-forest.impo$dGini),]
    ## forest.impo <- head(forest.impo, 4)
    var.importance[i,] <- forest.impo
    train.oob[i] <- tail(fit.forest$err.rate,1)[1]
    test.oob[i] <- tail(fit.forest$test$err.rate,1)[1]
    ## print(fit.forest)
    ## plot(fit.forest)
    ## readline(prompt="Press [enter] to continue")
  }
  return(list(
    train.oob = train.oob,
    test.oob = test.oob,
    var.importance = var.importance
  ))
}


time.cut <- c(1:8) * 6
time.cut.n <- length(time.cut)

train.prop <- 2/3
liver.nonpred.all <- c("date_primarydiagnosis",
                   "ptid",
                   "date_rt_start",
                   "rt_end",
                   "pretx_types_of_therapies",
                   "date_followup_death",
                   "status",
                   "time"
                   )
forest.rslt.pred <- gen.forest(liver, liver.nonpred.all, train.prop, time.cut)


train.prop <- 1
forest.n <- 100
liver.nonpred.nobm <- c("date_primarydiagnosis",
                   "ptid",
                   "date_rt_start",
                   "rt_end",
                   "pretx_types_of_therapies",
                   "date_followup_death",
                   "status",
                   "time",
                   "icg15_pretx",
                   "cp_pretx",
                   "albi_score_pretx"
                   )
test.oob.mat.nobm <- matrix(-9, forest.n, time.cut.n)
test.oob.mat.nobm <- matrix(-9, forest.n, time.cut.n)
for(i in 1:forest.n){
  forest.rslt <- gen.forest(liver, liver.nonpred.nobm, train.prop, time.cut)
  test.oob.mat.nobm[i,] <- forest.rslt$train.oob
}
test.oob.baseline <- colMeans(test.oob.mat.nobm)
test.oob.baseline.mat <- matrix(test.oob.baseline, forest.n, time.cut.n, byrow = TRUE)



liver.nonpred.icg <- c("date_primarydiagnosis",
                   "ptid",
                   "date_rt_start",
                   "rt_end",
                   "pretx_types_of_therapies",
                   "date_followup_death",
                   "status",
                   "time",
                   "cp_pretx",
                   "albi_score_pretx"
                   )
test.oob.mat.icg <- matrix(-9, forest.n, time.cut.n)
for(i in 1:forest.n){
  print(i)
  forest.rslt <- gen.forest(liver, liver.nonpred.icg, train.prop, time.cut)
  test.oob.mat.icg[i,] <- forest.rslt$train.oob
}
test.oob.mat.icg.diff <- test.oob.mat.icg - test.oob.baseline.mat

liver.nonpred.cp <- c("date_primarydiagnosis",
                   "ptid",
                   "date_rt_start",
                   "rt_end",
                   "pretx_types_of_therapies",
                   "date_followup_death",
                   "status",
                   "time",
                   "icg15_pretx",
                   "albi_score_pretx"
                   )
test.oob.mat.cp <- matrix(-9, forest.n, time.cut.n)
for(i in 1:forest.n){
  print(i)
  forest.rslt <- gen.forest(liver, liver.nonpred.cp, train.prop, time.cut)
  test.oob.mat.cp[i,] <- forest.rslt$train.oob
}
test.oob.mat.cp.diff <- test.oob.mat.cp - test.oob.baseline.mat

liver.nonpred.albi <- c("date_primarydiagnosis",
                   "ptid",
                   "date_rt_start",
                   "rt_end",
                   "pretx_types_of_therapies",
                   "date_followup_death",
                   "status",
                   "time",
                   "icg15_pretx",
                   "cp_pretx"
                   )
test.oob.mat.albi <- matrix(-9, forest.n, time.cut.n)
for(i in 1:forest.n){
  print(i)
  forest.rslt <- gen.forest(liver, liver.nonpred.albi, train.prop, time.cut)
  test.oob.mat.albi[i,] <- forest.rslt$train.oob
}
test.oob.mat.albi.diff <- test.oob.mat.albi - test.oob.baseline.mat

png(filename="timetoevent_hist.png")
hist(liver$time/30, xlab="Time-to-event (months)", main = "Distribution of Time-to-Event")
dev.off()


png(filename="timetoevent_hist.png")
par(mfrow=c(2,2))
boxplot(test.oob.mat.nobm,
        xlab="Survival Period (Unit: 6 months)",
        ylab="OOB error rate",
        main="Reference (No biomarkers)"
        )
boxplot.matrix(test.oob.mat.icg.diff,
        xlab="Survival Period (Unit: 6 months)",
        ylab="OOB error rate",
        main="Change in OOB error rate after Adding ICG-15"
        )
abline(0,0)
boxplot.matrix(test.oob.mat.cp.diff,
        xlab="Survival Period (Unit: 6 months)",
        ylab="OOB error rate",
        main="Change in OOB error rate after Adding CP"
        )
abline(0,0)
boxplot.matrix(test.oob.mat.albi.diff,
        xlab="Survival Period (Unit: 6 months)",
        ylab="OOB error rate",
        main="Change in OOB error rate after Adding ALBI"
        )
abline(0,0)
dev.off()
