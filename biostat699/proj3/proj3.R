rm(list=ls())
library(nlme)
library(sas7bdat)

## # Individual slopes by recommendation categories
## png("rec.png", width = 1024, height = 700)
## par(mfrow = c(2, 3))
## par(mai = c(0.5,1,0.8,0.42))
## rec.cols <- c("MeetsFruitRec",
##                  "MeetsRedMeatRec",
##                  "MeetsHSFSRec",
##                  "MeetsLegumRec",
##                  "MeetsSatFatRec",
##                  "MeetsAddedSugarRec"
##                  )
## rec.main <- c("Fruit",
##               "Red Meat",
##               "HSFS",
##               "Legum",
##               "Saturated Fat",
##               "Added Sugar")
## for(i in 1:length(rec.cols)){
##   predictor <- rec.cols[i]
##   plot(factor(diet[,predictor]), slope,
##        main = rec.main[i], ylab = "",
##        cex.main = 3, cex.lab = 3, cex.axis = 3)
## }
## dev.off()

## ## Individual slopes by diet quality scores
## png("mdq.png", width = 1024, height = 768)
## par(mfrow = c(1, 1))
## par(mai = c(1,1,0.8,0.42))
## par(mfrow = c(1,1))
## plot(factor(diet$MDQ_Score), slope,
##      main = "Growth Slope vs Maternal Diet Quality Score",
##      xlab = "MDQ Score", ylab = "Growth Slope",
##      cex.main = 2, cex.axis = 2, cex.lab = 2)
## dev.off()

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

## Load, merge and clean dataset size
diet.raw <- read.sas7bdat("covariates.sas7bdat")
diet.raw$ParaMultiParous <- factor(diet.raw$Parity != "Nulliparous")
## diet <- diet.raw[complete.cases(diet.raw),]
## diet <- diet[(diet$Gender == "Male") | (diet$Gender == "Female"),]
## diet$Gender <- factor(diet$Gender)
size.raw <- read.sas7bdat("ultrasound.sas7bdat")
babies.raw <- merge(size.raw, diet.raw, by = "partid")
babies <- babies.raw[complete.cases(babies.raw),]
is.na(babies) <- babies == ""
babies <- babies[complete.cases(babies),]

## Create factors
factor.cols <- c("Gender",
                 "Married_partnered",
                 "BMI_CAT",
                 "Parity",
                 "education_cat",
                 "MeetsFruitRec",
                 "MeetsRedMeatRec",
                 "MeetsHSFSRec",
                 "MeetsLegumRec",
                 "MeetsSatFatRec",
                 "MeetsAddedSugarRec",
                 "MDQ_5_or_more",
                 "MDQ_0_2",
                 "MDQ_3_4"
                 )
for(i in 1:length(factor.cols)){
  babies[, factor.cols[i]] <- factor(babies[, factor.cols[i]])
}
babies$education_cat <- factor(babies$education_cat, levels(babies$education_cat)[c(3, 4, 5, 1, 2)])
babies$Parity <- factor(babies$Parity, levels(babies$Parity)[c(3, 1, 2)])
diet <- babies[,colnames(diet.raw)]
diet <- diet[!duplicated(diet$partid),]

## Create extra variables
babies$measure <- babies$CABD
babies$WeeksSinceLMPsq <- (babies$WeeksSinceLMP)^2
babies$WeeksSinceLMPcb <- (babies$WeeksSinceLMP)^3
write.csv(babies, "babies.csv", quote = FALSE)
partid <- unique(babies$partid)
N <- nrow(babies)
n <- length(unique(partid))
stopifnot(all(babies$partid == sort(babies$partid)))
stopifnot(all(diet$partid == sort(diet$partid)))
stopifnot(all(diet$partid == unique(babies$partid)))
stopifnot(all(partid == unique(babies$partid)))

# Correlation between body measurements
png("cor.png")
x <- babies$CCEF
y <- babies$DBIP
col <- 1
txt.x <- mean(x) * 0.8
txt.y <- mean(y) * 1.3
cor.xy <- paste("cor", round(cor(x, y),3), sep = "=")
plot(x, y, xlim = c(0,40), ylim = c(0,40), col = 1,
     xlab = "HC", ylab = "",
     main = "Correlation between body measurements",
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
text(txt.x, txt.y, labels = cor.xy, cex = 1.5)
y <- babies$CABD
col <- 2
txt.x <- mean(x) * 0.8
txt.y <- mean(y) * 1.3
cor.xy <- paste("cor", round(cor(x, y),3), sep = "=")
points(x, y, ylim = c(0,40), col = col)
text(txt.x, txt.y, labels = cor.xy, cex = 1.5, col = col)
y <- babies$LONGFE
col <- 3
txt.x <- mean(x) * 1.2
txt.y <- mean(y) * 0.6
cor.xy <- paste("cor", round(cor(x, y),3), sep = "=")
points(x, y, ylim = c(0,40), col = col)
text(txt.x, txt.y, labels = cor.xy, cex = 1.5, col = col)
legend("topleft", c("BIP", "AC", "FL"), pch = 1, col = c(1:3), cex = 1.5)
dev.off()

# Change of size over time
png("overtime.png")
par(mai = c(1,1,1,1))
plot(babies$WeeksSinceLMP, babies$measure,
     xlab = "Weeks since Last Menstral Period",
     ylab = "Fetus Size (HC + AC + FL)",
     main = "Change of Fetus Size Over Time",
     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
dev.off()

## Fit a linear model for every individual
cef <- matrix(0, n, 3)
for(i in 1:n){
  dat <- babies[babies$partid == partid[i],]
  x <- dat$WeeksSinceLMP
  y <- dat$measure
  fit <- lm(y ~  x + I(x^2))
  cef[i,] <- fit$coefficients
}
slope <- cef[,2]



## Regress individual slopes on covariates
png("cova.png", width = 1024, height = 1400)
par(mai = c(0.5,1,0.8,0.42))
par(mfrow = c(5, 5))
plot.cols <- c("BMI_pregest_kgm2",
              "Prepreg_weight",
              "mom_height_m",
              "agex",
              "PREGLIVEBIRTHS",
              "Gender"
               )
plot.main <- c("Maternal BMI",
              "Maternal Weight (Pre-Preg)",
              "Maternal Height",
              "Maternal Age",
              "# Previous Live Births",
              "Sex of Baby")
pvals <- rep(1, ncol(diet)-1)
for(i in 2:ncol(diet)){
  ## predictor <- plot.cols[i]
  ## confounder <- diet[,predictor]
  confounder <- diet[,i]
  lm.fit <- lm(slope ~ confounder)
  ## pval <- summary(lm.fit)$coefficients[2,4]
  ## pval <- round(pval, 3)
  ## pvals[i] <- pval
  ## plot.sub <- paste0("p-val: ", pval)
  ## col <- (pval < 0.05) + 1
  ci <- confint(lm.fit)[-1,]
  ci <- matrix(ci, ncol = 2)
  ci <- round(ci, 3)
  col <- (any(ci[,1] > 0) | any(ci[,2] < 0)) + 1
  plot.sub <- paste0("CI: (", ci[,1], ", ", ci[,2],")", collapse = "\n")
  plot(confounder, slope,
       main = colnames(diet)[i],
       ylab = "", xlab = "", col = col,
       cex.main = 2, cex.lab = 1, cex.axis = 1)
}
dev.off()

# Linear vs quadratic
png("lmm-lin.png", width = 1024, height = 500)
lmm.time.linear <- lme(measure ~ 1 + WeeksSinceLMP,
                  random = ~ 1 + WeeksSinceLMP | partid,
                  data = babies)
plot(lmm.time.linear, main = "LMM Residual Plot: AC vs. Weeks Since LMP")
dev.off()
png("lmm-qua.png", width = 1024, height = 500)
lmm.time.quadratic <- lme(measure ~ 1 + WeeksSinceLMP + WeeksSinceLMPsq + MDQ_Score,
           random = ~ 1 + WeeksSinceLMP | partid,
           data = babies)
plot(lmm.time.quadratic, main = "LMM Residual Plot: AC vs. Weeks Since LMP Squared")
dev.off()

## Fit linear mixed model
lmm <- lme(measure ~ 1 + WeeksSinceLMP + WeeksSinceLMPsq
           + MeetsAddedSugarRec
           + MeetsFruitRec
           + MeetsHSFSRec
           + MeetsLegumRec
           + MeetsRedMeatRec
           + MeetsSatFatRec
           + Gender
           + PREGLIVEBIRTHS
           + agex
           + BMI_pregest_kgm2,
           random = ~ 1 + WeeksSinceLMP | partid,
           data = babies)
summary(lmm)
anova(lmm)
lmm.coef <- coef(lmm)
## slope <- lmm.coef$WeeksSinceLMP
