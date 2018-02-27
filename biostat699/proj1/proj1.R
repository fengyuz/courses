library(gdata)
library(reshape2)

X <- read.xls("Project\ 1\ Data.xlsx",
              sheet = 1, header = TRUE, na.strings = ".")
X <- X[complete.cases(X),]

write.table(X, "~/SASUniversityEdition/myfolders/biostat699_sas/frail", sep = '\t', quote = FALSE)
write.csv(X, "~/SASUniversityEdition/myfolders/biostat699_sas/frail.csv")

aggregate(X[,c("depr", "frail_2010", "frail_2012", "bmi")],
          by = list(X$educ_cat), FUN = mean, na.rm = TRUE)
aggregate(X[,c("depr", "frail_2010", "frail_2012", "bmi")],
          by = list(X$race_eth), FUN = mean, na.rm = TRUE)
aggregate(X[,c("depr", "frail_2010", "frail_2012", "bmi")],
          by = list(X$male), FUN = mean, na.rm = TRUE)
aggregate(X[,c("depr", "frail_2010", "frail_2012", "bmi")],
          by = list(X$depr), FUN = mean, na.rm = TRUE)
aggregate(X[,c("depr", "frail_2010", "frail_2012", "bmi")],
          by = list(X$frail_2010), FUN = mean, na.rm = TRUE)
aggregate(X[,c("depr", "frail_2010", "frail_2012", "bmi")],
          by = list(X$sr_hlth), FUN = mean, na.rm = TRUE)
Y <- aggregate(X, by = list(X$dyad_id), FUN = c)
Y <- melt(X, na.rm = TRUE, id.vars = c("dyad_id", "male"))
Z <- dcast(Y, dyad_id ~ variable + male)

Z[,c("depr_0",
     "depr_1",
     "frail_2010_0",
     "frail_2010_1",
     "frail_2012_0",
     "frail_2012_1",
     "sr_hlth_0",
     "sr_hlth_1"
     )] <-
sapply(Z[,c("depr_0",
               "depr_1",
               "frail_2010_0",
               "frail_2010_1",
               "frail_2012_0",
               "frail_2012_1",
               "sr_hlth_0",
               "sr_hlth_1"
               )], as.integer)

Z[,c(
  "bmi_0",
  "bmi_1",
  "age_0",
  "age_1",
  "income_0",
  "income_1"
)] <-
sapply(Z[,c(
  "bmi_0",
  "bmi_1",
  "age_0",
  "age_1",
  "income_0",
  "income_1"
)],
as.numeric)

Z[,c(
  "dyad_id",
  "race_eth_0",
  "race_eth_1",
  "educ_cat_0",
  "educ_cat_1"
)] <-
  sapply(
Z[,c(
  "dyad_id",
  "race_eth_0",
  "race_eth_1",
  "educ_cat_0",
  "educ_cat_1"
)],
as.factor
  )

aggregate(
  Z$frail_2012_0,
  by = list(
    Z$frail_2012_0
  ),
  FUN = mean,
  na.rm = TRUE
)

aggregate(X[,c("frail_2012")],
          by = list(X$depr, X$frail_2010), FUN = mean, na.rm = TRUE)
hist(X$bmi)
max(X$income)
X.bmi <- aggregate(X$frail_2012, list(cut(X$bmi, breaks=c(0:8)*10)), mean)
png(filename="bmi_vs_frail_2012.png")
plot((c(0:7)+0.5)*10, X.bmi$x, type="l", xlab = "bmi.bin", ylab = "frail_2012")
dev.off()
