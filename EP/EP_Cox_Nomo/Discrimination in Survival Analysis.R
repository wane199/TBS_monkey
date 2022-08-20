# Assessment of Discrimination in Survival Analysis (C-statistics, etc)
# https://rpubs.com/kaz_yos/survival-auc
# Load the dataset and modify for later use
rm(list=ls())
library(survival)
dt <- read.csv("/media/wane/wade/EP/EPTLE_PET/TLE234-rad.csv")

# 对数据初步预处理(批量单因素分析变量保留数值型变量)
# 用for循环语句将数值型变量转为因子变量
for (i in names(dt)[c(2:4, 8:16)]) {
  dt[, i] <- as.factor(dt[, i])
}

set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt[ind == 1, ] # the training data set
# 测试集
test <- dt[ind == 2, ] # the test data set

ddist <- datadist(train)
options(datadist = "ddist")
train$Rel._in_5yrs <- as.numeric(as.character(train$Rel._in_5yrs))
S <- Surv(train$Follow_up_timemon, train$Rel._in_5yrs)

# Kaplan-Meier plots
par(mar = c(2,2,2,2))
layout(matrix(1:4, byrow = T, ncol = 2))
library(rms)
library(survival)
train$Sex <- factor(train$Sex,
                    levels = c(0, 1),
                    labels = c("F", "M")
)
survplot(npsurv(S ~ 1, data = train), pval=T)
survplot(npsurv(S ~ Durmon >= median(Durmon), data = train), label.curves = list(method = "arrow", cex = 1.2), pval=T)
survplot(npsurv(S ~ Sex, data = train), label.curves = list(method = "arrow", cex = 1.2))
survplot(npsurv(S ~ radscore >= median(radscore), data = train), label.curves = list(method = "arrow", cex = 1.2))

## Load epicalc package to calcuate AUC
library(epicalc)

## Model with age and sex
logit.age.sex <- glm(Rel._in_5yrs ~ Durmon + Sex, data = train, family = binomial)
lroc(logit.age.sex, graph = F)$auc

## Model with age, sex, and albumin
logit.age.sex.albumin <- glm(Rel._in_5yrs ~ radscore + Durmon + Sex, data = train, family = binomial)
lroc(logit.age.sex.albumin, graph = F)$auc

## Create a variable indicating 2-year event
pbc <- within(pbc, {
  outcome2yr <- NA
  outcome2yr[(event == 1) & (time <= 2 * 365)] <- 1 # event+ within two years
  outcome2yr[(event == 0) | (time  > 2 * 365)] <- 0 # otherwise
})

## 2-year outcome model with age and sex
logit.age.sex <- glm(outcome2yr ~ age + sex, data = pbc, family = binomial)
lroc(logit.age.sex, graph = F)$auc


library(risksetROC)














