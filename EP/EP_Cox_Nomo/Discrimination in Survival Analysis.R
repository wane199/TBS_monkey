# Assessment of Discrimination in Survival Analysis (C-statistics, etc)
# https://rpubs.com/kaz_yos/survival-auc
# Load the dataset and modify for later use
rm(list = ls())
library(survival)
library(rms)
getwd()
dt <- read.csv("./EP/EP_Cox_Nomo/TLE234-rad.csv")

# 对数据初步预处理(批量单因素分析变量保留数值型变量)
# 用for循环语句将数值型变量转为因子变量
for (i in names(dt)[c(2:4, 8:16)]) {
  dt[, i] <- as.factor(dt[, i])
}
dt$Rel._in_5yrs <- as.numeric(dt$Rel._in_5yrs == "1")

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
par(mar = c(3, 3, 2, 2))
layout(matrix(1:6, byrow = T, ncol = 2))
train$Sex <- factor(train$Sex,
  levels = c(0, 1),
  labels = c("F", "M")
)
survplot(npsurv(S ~ 1, data = train), pval = T)
survplot(npsurv(S ~ Durmon >= median(Durmon), data = train), label.curves = list(method = "arrow", cex = 1.2), pval = T)
survplot(npsurv(S ~ familial_epilepsy, data = train), label.curves = list(method = "arrow", cex = 1.2), pval = T)
survplot(npsurv(S ~ SGS, data = train), label.curves = list(method = "arrow", cex = 1.2))
survplot(npsurv(S ~ SE, data = train), label.curves = list(method = "arrow", cex = 1.2))
survplot(npsurv(S ~ radscore >= median(radscore), data = train), label.curves = list(method = "arrow", cex = 1.2))

# AUC by logistic regression models
## Load epicalc package to calcuate AUC
# library(epicalc)
library(epiDisplay)

## Model with age and sex
logit.clinic <- glm(Rel._in_5yrs ~ SGS + familial_epilepsy + Durmon + SE, data = train, family = binomial)
lroc(logit.clinic, graph = F)$auc

## Model with age, sex, and albumin
logit.rad.clinic <- glm(Rel._in_5yrs ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, family = binomial)
lroc(logit.rad.clinic, graph = F)$auc

## Create a variable indicating 2-year event**
train <- within(train, {
  outcome2yr <- NA
  outcome2yr[(Rel._in_5yrs == 1) & (Follow_up_timemon <= 2 * 12)] <- 1 # event+ within two years
  outcome2yr[(Rel._in_5yrs == 0) | (Follow_up_timemon > 2 * 12)] <- 0 # otherwise
})

## 2-year outcome model with age and sex
logit.clinic <- glm(outcome2yr ~ SGS + familial_epilepsy + Durmon + SE, data = train, family = binomial)
lroc(logit.clinic, graph = F)$auc

library(reportROC) # Confusion Matrix

# Fit Cox regression models for later use
## Null model
coxph.null <- coxph(S ~ 1, data = train)
coxph.null

coxph.clinic <- coxph(S ~ SGS + familial_epilepsy + Durmon + SE, data = train)
coxph.clinic

coxph.rad.clinic <- coxph(S ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train)
coxph.rad.clinic

## These models are significantly different by likelihood ratio test
anova(coxph.clinic, coxph.rad.clinic, test = "LRT")

## Put linear predictors ("lp") into pbc dataset
train$lp.null <- predict(coxph.null, type = "lp")
train$lp.clinic <- predict(coxph.clinic, type = "lp")
train$lp.rad.clinic <- predict(coxph.rad.clinic, type = "lp")

# Harrell’s C or concordance
library(Hmisc)

## Model with age and sex
rcorrcens(formula = S ~ I(-1 * lp.clinic), data = train)
rcorrcens(formula = S ~ I(-1 * lp.rad.clinic), data = train)

summary(coxph.clinic)
summary(coxph.rad.clinic)


# Gonen and Heller Concordance Index for Cox models
# library(CPE)
# ## Model with age and sex
# phcpe(coxph.rad.clinic, CPE.SE = TRUE)

library(clinfun)
## Model with age and sex
coxphCPE(coxph.clinic)
coxphCPE(coxph.rad.clinic)

# Cumulative case/dynamic control ROC/AUC using survivalROC package (Heagerty, 2000)
library(survivalROC)

## Define a function
fun.survivalROC <- function(lp, t) {
  res <- with(
    train,
    survivalROC(
      Stime = Follow_up_timemon,
      status = Rel._in_5yrs,
      marker = get(lp),
      predict.time = t,
      method = "KM"
    )
  ) # KM method without smoothing

  ## Plot ROCs
  with(res, plot(TP ~ FP, type = "l", main = sprintf("t = %.0f, AUC = %.2f", t, AUC)))
  abline(a = 0, b = 1, lty = 2)

  res
}

## 2 x 5 layout
layout(matrix(1:6, byrow = T, ncol = 3))

## Model with age and sex
res.survivalROC.clinic <- lapply(1:6 * 12, function(t) {
  fun.survivalROC(lp = "lp.clinic", t)
})

## Model with age, sex, and albumin
res.survivalROC.rad.clinic <- lapply(1:6 * 12, function(t) {
  fun.survivalROC(lp = "lp.rad.clinic", t)
})

# Incident case/dynamic control ROC/AUC using risksetROC package (Heagerty, 2005)
library(risksetROC)

## Define a function
fun.risksetROC <- function(lp, t) {
  res <- with(
    train,
    risksetROC(
      Stime = Follow_up_timemon,
      status = Rel._in_5yrs,
      marker = get(lp),
      predict.time = t,
      plot = FALSE
    )
  )

  ## Plot ROCs
  with(res, plot(TP ~ FP, type = "l", main = sprintf("t = %.0f, AUC = %.2f", t, AUC)))
  abline(a = 0, b = 1, lty = 2)

  res
}

## 2 x 5 layout
layout(matrix(1:6, byrow = T, ncol = 3))

## Model with age and sex
risksetROC.clinic <- lapply(12 * 1:6, function(t) {
  fun.risksetROC(lp = "lp.clinic", t)
})

risksetROC.rad.clinic <- lapply(12 * 1:6, function(t) {
  fun.risksetROC(lp = "lp.rad.clinic", t)
})

# Up to 10-year AUCs
## 1 x 2 layout
layout(matrix(1:2, byrow = T, ncol = 2))

## Model with age and sex
risksetAUC.clinic <- with(
  train,
  risksetAUC(
    Stime = Follow_up_timemon,
    status = Rel._in_5yrs,
    marker = lp.clinic,
    tmax = 5 * 12
  )
)
title(sprintf("t = %.0f, iAUC = %.2f", 5 * 12, risksetAUC.clinic$Cindex))

## Model with age, sex, and albumin
risksetAUC.rad.clinic <- with(
  train,
  risksetAUC(
    Stime = Follow_up_timemon,
    status = Rel._in_5yrs,
    marker = lp.rad.clinic,
    tmax = 5 * 12
  )
)
title(sprintf("t = %.0f, iAUC = %.2f", 5 * 12, risksetAUC.rad.clinic$Cindex))

# Various methods provided survAUC package (Potapov et al)
library(survAUC)

## *Cumulative case*/dynamic control AUC by Chambless and Diao (Stat Med 2006;25:3474-3486.)
# res.AUC.cd <- AUC.cd(Surv.rsp     = train$S,
#                      Surv.rsp.new = train$S,
#                      lp           = coxph.clinic$linear.predictors,
#                      lpnew        = coxph.rad.clinic$linear.predictors,
#                      times        = 1:5 * 12)
# res.AUC.cd$iauc
# plot(res.AUC.cd)

## *Cumulative case*/dynamic control AUC by Hung and Chiang (Can J Stat 2010;38:8-26)
res.AUC.hc <- AUC.hc(
  Surv.rsp = train$S,
  Surv.rsp.new = train$S,
  ## lp           = coxph.age.sex$linear.predictors,
  lpnew = coxph.clinic$linear.predictors,
  times = 1:3 * 12
)
res.AUC.hc$iauc
plot(res.AUC.hc)

## *Incident case* or *Cumulative case*/dynamic control AUC by Song and Zhou (Biometrics 2011;67:906-16)
# res.AUC.sh <- AUC.sh(Surv.rsp     = train$S,
#                      Surv.rsp.new = train$S,
#                      lp           = coxph.clinic$linear.predictors,
#                      lpnew        = coxph.clinic$linear.predictors,
#                      times        = 1:5 * 12)
# res.AUC.sh$iauc
# plot(res.AUC.sh)

## *Cumulative case*/dynamic control AUC by Uno et al.
## (http://biostats.bepress.com/cgi/viewcontent.cgi?article=1041&context=harvardbiostat)
# res.AUC.uno <- AUC.uno(Surv.rsp     = train$S,
#                        Surv.rsp.new = train$S,
#                        ## lp           = coxph.age.sex$linear.predictors,
#                        lpnew        = coxph.clinic$linear.predictors,
#                        times        = 1:5 * 12)
# res.AUC.uno$iauc
# plot(res.AUC.uno)

## C-statistic by Begg et al. (Stat Med 2000;19:1997-2014)
res.BeggC <- BeggC(
  Surv.rsp = train$S,
  Surv.rsp.new = train$S,
  lp = coxph.clinic$linear.predictors,
  lpnew = coxph.rad.clinic$linear.predictors
)
res.BeggC

## Gonen and Heller’s Concordance Index for Cox PH models (Biometrika 2005;92:965-970)
res.GHCI <- GHCI( ## Surv.rsp     = pbc$Surv,
  ## Surv.rsp.new = pbc$Surv,
  ## lp           = coxph.age.sex$linear.predictors,
  lpnew = coxph.rad.clinic$linear.predictors
)
res.GHCI

## O'Quigley et al. (Stat Med 2005;24:479-489)
res.OXS <- OXS(
  Surv.rsp = train$S,
  lp = coxph.rad.clinic$linear.predictors,
  lp0 = coxph.clinic$linear.predictors
)
res.OXS

## Nagelkerke (Biometrika 1991;78:691-692)
res.Nagelk <- Nagelk(
  Surv.rsp = train$S,
  lp = coxph.rad.clinic$linear.predictors,
  lp0 = coxph.clinic$linear.predictors
)
res.Nagelk


## Xu et al. (Journal of Nonparametric Statistics 1999;12:83-107)
res.XO <- XO(
  Surv.rsp = train$S,
  lp = coxph.rad.clinic$linear.predictors,
  lp0 = coxph.clinic$linear.predictors
)
res.XO

# Uno methods for C-statistics and IDI/NRI implemented in survC1 and survIDINRI
## Create numeric variable for sex
train$female <- as.numeric(train$Sex == "F")

# C-statistics (10-years follow up) using survC1 package
library(survC1)
## C-statistic for age sex model
unoC.clinic <- Est.Cval(mydata = train[, c(
  "Rel._in_5yrs", "Follow_up_timemon", "radscore", "SGS", "familial_epilepsy",
  "Durmon", "SE"
)], tau = 5 * 12)
unoC.clinic$Dhat

## Comaprison of C-statistics
uno.C.delta <- Inf.Cval.Delta(
  mydata = train[, c("Follow_up_timemon", "Rel._in_5yrs")],
  covs0 = train[, c("SGS", "familial_epilepsy", "Durmon", "SE")], # age sex model
  covs1 = train[, c("radscore", "SGS", "familial_epilepsy", "Durmon", "SE")], # age sex albumin model
  tau = 5 * 12, # Trucation time (max time to consider)
  itr = 100
) # Iteration of perturbation-resampling
uno.C.delta

# IDI, continous NRI, and median improvement (10-years follow up) using survIDINRI
library(survIDINRI)
train$Rel._in_5yrs <- as.numeric(train$Rel._in_5yrs == "1")
train$Follow_up_timemon <- as.numeric(train$Follow_up_timemon)

res.IDI.INF <- IDI.INF(
  indata = train[, c("Follow_up_timemon", "Rel._in_5yrs")],
  covs0 = train[, c("SGS", "familial_epilepsy", "Durmon", "SE")], # age sex model
  covs1 = train[, c("radscore", "SGS", "familial_epilepsy", "Durmon", "SE")], # age sex albumin model
  t0 = 5 * 12,
  npert = 300, npert.rand = NULL, seed1 = NULL, alpha = 0.05
)

## M1 IDI; M2 continuous NRI; M3 median improvement
IDI.INF.OUT(res.IDI.INF)

## M1 red area; M2 distance between black points; M3 distance between gray points
IDI.INF.GRAPH(res.IDI.INF)

# 批量单因素Cox
# https://www.jianshu.com/p/617db057df37
coxm0 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS, data = train)
coxm1 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ Durmon, data = train)
coxm2 <- survival::coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train)

cox.zph(coxm2) # 等比例风险假定
print(coxm2)
summary(coxm0)$concordance # 未校准的时间C-index


library(corrplot)
dfc <- train[, names(train) %in% c("radscore", "SGS", "familial_epilepsy", "Durmon", "SE")]
corrplor <- cor(as.matrix(dfc))
corrplot.mixed(corrplor)
data.frame(corrplor)
library(GGally)
ggpairs(dfc)

vif <- rms::vif(coxm2) # 检测共线性
sqrt(vif) < 2


# R语言 ggplot 循环画图 与多图合并 https://blog.csdn.net/weixin_46623488/article/details/120385106
# 循环作图   在 { }之间加入要画的图，循环的变量为R
p <- apply(data3, 2, function(R) {
  ggplot(data) +
    aes(x = year, fill = R) +
    geom_bar(position = "fill")
})

# S3提取其中一幅ggplot图
p["age"]$age # 填入你循环画图的一个变量
# 加标签
p2 <- p["age"]$age + labs(fill = "年龄")
p3 <- p["张口受限"]$张口受限 + labs(fill = "张口受限")
p4 <- p["弹响"]$弹响 + labs(fill = "弹响")
# 合并图片
# 组合成一幅图，按照两行两列排列，标签分别为ABCD（LETTERS[1:4]
pic <- cowplot::plot_grid(p2, p3, p4, p5, p6, p7, p8, p9, p10, p11,
  ncol = 4, nrow = 3, labels = LETTERS[1:10]
)
pic

# 将表格插入ggplot图中
# Density plot of "Sepal.Length"
train$Rel._in_5yrs <- as.factor(train$Rel._in_5yrs)
density.p <- ggdensity(train, x = "radscore", add = "mean", rug = TRUE,
                       color = "Rel._in_5yrs", fill = "Rel._in_5yrs", palette = "R3")
# Draw the summary table of Sepal.Length
# Compute descriptive statistics by groups
stable <- desc_statby(train, measure.var = "radscore",
                      grps = "Rel._in_5yrs")
stable <- stable[, c("Rel._in_5yrs", "length", "mean", "sd")]
# Summary table plot, medium orange theme
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("light"))
# Draw text
text <- paste("Epilepsy dataset of FDG-PET radiomics.", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")
# Arrange the plots on the same page
ggarrange(density.p, stable.p, text.p, 
          ncol = 1, nrow = 3,
          heights = c(1, 0.5, 0.3))
density.p + annotation_custom(ggplotGrob(stable.p),
                              xmin = 0.3, ymin = 1.4,
                              xmax = 1.0)

