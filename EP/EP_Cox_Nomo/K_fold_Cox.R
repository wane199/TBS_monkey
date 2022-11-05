# 预测模型 | 10. K折交叉验证可视化 (Cox回归)
# https://mp.weixin.qq.com/s?__biz=Mzg2MjU2NDQwMg==&mid=2247497310&idx=1&sn=d0aade5c787e85e1689579679e8a6ba7&chksm=ce074f03f970c6157372da63431520b5009c590824f2b087f6ccc67c7d08b7e83ec08c296424&mpshare=1&scene=1&srcid=0904ZBKuwGpRm9OC14OkTKHm&sharer_sharetime=1662264266271&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# 清理工作环境
rm(list = ls())
library(survival) # cox回归用
library(caret) # K折交叉验证用
library(riskRegression) # cox回归时间AUC用
library(pec) # cox回归时间c指数用

# 读入数据
dt0 <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/Task2/TLE234group.csv")
dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
table(dt$Freq)
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")

# 查看变量性质
str(dt)
# 批量数值转因子
for (i in names(dt)[c(4:7)]) {
  dt[, i] <- as.factor(dt[, i])
}
# 再次查看变量性质
str(dt)

# 多次K折交叉验证
# 建个空表res备用，盛放结果
res <- as.numeric()
# 设置随机种子，使数据分割可重复
set.seed(123)
# 设定K和N,预测3年AUC的5折200次验证
# status是结局1为复发
folds <- createMultiFolds(
  y = dt$Rel._in_5yrs,
  k = 5, times = 100
)

# 3年AUC的2000次验证
# for循环将上述1000种新数据均运行一次，
# 求1000个AUC
# 1-建立空表，放每次的AUC
auc_value36 <- as.numeric()
# 2-做成循环，3年
for (i in 1:1000) { # 1000次循环（1000组新数据）
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- Score(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, metrics = "auc", times = 36
  ) # 3年或5年
  auc_value36 <- append(auc_value36, as.numeric(mod$AUC$score$AUC))
}
# 3-将1000个结果放入res表中
res <- data.frame(auc_value36)

# 2.计算5年AUC的1000次交叉验证值
# 1-建立空表，放每次的AUC
auc_value60 <- as.numeric()
# 2-for循环
for (i in 1:1000) { # 1000次循环（1000组新数据）
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- Score(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, metrics = "auc", times = 60
  ) # 3年或5年
  auc_value60 <- append(auc_value60, as.numeric(mod$AUC$score$AUC))
}
# 3-将结果放入res表
res$auc_value60 <- with(res, auc_value60)

# 3年C-index的2000次验证
### 建立空值
c_value36 <- as.numeric()
# 做成循环
for (i in 1:1000) {
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- cindex(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, eval.times = 36
  )
  c_value36 <- append(c_value36, as.numeric(mod$AppCindex))
}
# 将36个月的c指数放入res
res$c_value36 <- with(res, c_value36)

# 5年C-index的2000次验证
### 建空表
c_value60 <- as.numeric()
# 做成循环
for (i in 1:1000) {
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- cindex(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, eval.times = 60
  )
  c_value60 <- append(c_value60, as.numeric(mod$AppCindex))
}
res$c_value60 <- with(res, c_value60)

# res表宽转长
library(reshape2)
res <- data.frame(res)
res1 <- melt(res,
  measure.vars = c(
    "auc_value36", "auc_value60",
    "c_value36", "c_value60"
  ),
  variable.name = "Sample",
  value.name = "value"
)
# 需要转换的列名：measure.vars()
# 新数据标签列名：variable.name()
# 新数数据数值名：value.name()

# 交叉验证结果可视化
# 1. 小提琴图
library(ggplot2)
library(ggprism)
ggplot(res1, aes(Sample, value)) +
  geom_violin(aes(fill = Sample)) +
  geom_boxplot(width = 0.1) +
  theme_prism(base_size = 15, border = T) +
  theme(legend.position = "none")

# 2. 箱线图+散点图
ggplot(data = res1) +
  geom_boxplot(
    mapping = aes(x = Sample, y = value, colour = Sample),
    alpha = 0.5, size = 1.5, width = 0.6
  ) +
  geom_jitter(
    mapping = aes(x = Sample, y = value, colour = Sample),
    alpha = 0.3, size = 2
  ) +
  scale_color_manual(
    limits = c(
      "auc_value36", "auc_value60",
      "c_value36", "c_value60"
    ),
    values = c("#c1121f", "#0466c8", "#2e7542", "#fca311")
  ) +
  theme_prism(base_size = 15, border = T) +
  theme(legend.position = "none")

# 云雨图（小提琴+蜂图）
# 4.云雨图
library(see)
ggplot(res1, aes(x = Sample, y = value, fill = Sample)) +
  geom_violindot(binwidth = 0.0025, dots_size = 1, color = NA) +
  coord_flip() +
  scale_fill_discrete() +
  theme_prism(border = T) +
  theme(legend.position = "none")

# 5. 云雨图（小提琴+散点）
library(gghalves)
# 小提琴+散点
ggplot(res1, aes(x = Sample, y = value, fill = Sample)) +
  geom_half_violin(aes(fill = Sample),
    position = position_nudge(x = .15, y = 0), side = "r"
  ) +
  geom_point(aes(x = as.numeric(Sample) + 0.05, y = value, color = Sample),
    position = position_jitter(width = 0.05), size = 0.25, shape = 20
  ) +
  coord_flip() + # 转90度
  theme_prism(border = T) +
  theme(legend.position = "none")

# 6. 云雨图-3（小提琴+箱图+散点）
library(tidyverse)
library(dplyr)
library(ggsci)
# 先计算均值等,报错重装dplyr
summ_res1 <- res1 %>%
  group_by(Sample) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n()
  ) %>%
  mutate(
    se = sd / sqrt(n),
    Species = factor(Sample, levels = c("auc_value36", "auc_value60", "c_value36", "c_value60"))
  )
summ_res1

## 小提琴+箱线图+散点+误差
ggplot(res1, aes(x = Sample, y = value, fill = Sample)) +
  geom_half_violin(aes(fill = Sample),
    position = position_nudge(x = .15, y = 0),
    adjust = 1.5, trim = T, colour = NA, side = "r"
  ) +
  geom_point(aes(x = as.numeric(Sample) - 0.1, y = value, color = Sample),
    position = position_jitter(width = 0.05), size = .25, shape = 20
  ) +
  geom_half_boxplot(aes(x = as.numeric(Sample) + 0.15, y = value, fill = Sample),
    width = 0.25, color = "black"
  ) +
  geom_errorbar(
    data = summ_res1, aes(
      x = Sample, y = mean, group = Sample,
      colour = Sample, ymin = mean - se,
      ymax = mean + se
    ), width = 0.05,
    position = position_nudge(x = -0.01, y = 0)
  ) +
  scale_color_jco() +
  scale_fill_jco() +
  coord_flip() +
  theme_prism(border = T) +
  theme(legend.position = "none")


###############################
# 生存分析任务转化为分类任务
rm(list = ls())
library(survival)
library(rms)
library(epiDisplay)
getwd()
# dt <- read.csv("./EP/EP_Cox_Nomo/TLE234-rad.csv")
dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/Task2/TLE234group.csv")
dt <- read.csv("C:/Users/wane199/Desktop/EP/Structured_Data/Task2/TLE234group.csv")

dt1 <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
DT::datatable(dt)
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")
table(dt$Rel._in_5yrs)
## Create a variable indicating 1/2-year event**
dt <- within(dt, {
  outcome1yr <- NA
  outcome1yr[(Rel._in_5yrs == 1) & (Follow_up_timemon <= 1 * 12)] <- 1 # event+ within two years
  outcome1yr[(Rel._in_5yrs == 0) | (Follow_up_timemon > 1 * 12)] <- 0 # otherwise
})

table(dt$outcome1yr)
table(dt1$oneyr)

## 1-year outcome model with age and sex
logit.clinic <- glm(outcome1yr ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = dt, family = binomial)
lroc(logit.clinic, graph = F)$auc
library(Rmisc)
CI(lroc(logit.clinic, graph = F)$auc,ci=0.95)

# ROC曲线及PR曲线
pre <- predict(logit.clinic, type = "response") # 预测概率，预测分类(class)
pre
library(pROC)
plot.roc(dt$outcome1yr, pre,
         main = "ROC curve in Training set", percent = TRUE,
         print.auc = TRUE,
         ci = TRUE, of = "thresholds",
         thresholds = "best",
         print.thres = "best"
)
rocplot1 <- roc(dt$outcome1yr, pre)
auc(rocplot1)
ci.auc(rocplot1)
# ROC详细结果
roc.result <- coords(rocplot1, "best", ret = "all", transpose = F)
as.matrix(roc.result)
# PR curve
library(modEvA)
aupr <- AUC(
  obs = dt$outcome1yr, pred = pre, interval = 0.001,
  curve = "PR", method = "trapezoid", simplif = F, main = "PR curve"
)
# 绘制ROC需要实际值和预测值，DCA需要实际值和预测概率(阳性结局)
library(reportROC) # Confusion Matrix
reportROC(gold = dt$outcome1yr, predictor = pre, 
          plot = T, important = "se", exact = FALSE)
reportROC(gold = dt$outcome1yr, predictor = c(dt$SGS, dt$radscore, dt$familial_epilepsy, dt$Durmon, dt$SE),
          plot = T, important = "se", exact = FALSE)

# [利用timeROC包绘制多分类多条ROC曲线](https://mp.weixin.qq.com/s?__biz=MzkyODIyOTY5Ng==&mid=2247485325&idx=2&sn=9e87a03c95f6d6d7733221f943e59441&chksm=c21ab7a2f56d3eb439d5c6e3821ca7101255021f7b49166f8bf32d186485bb146c5a8d89b1ba&mpshare=1&scene=1&srcid=1019d64aFsP83P9MWOGnNUpN&sharer_sharetime=1666136506398&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# [多个时间点及多指标ROC曲线](https://zhuanlan.zhihu.com/p/453214424)
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")
dt <- read.csv('/Users/mac/Downloads/glioma1.csv')
summary(dt)

table(dt$oneyr)
train <- train[4:19]
library(survminer)
library(timeROC)
# 绘制ROC图：
#颜色
bioCol=rainbow(ncol(train)-2,0.4)
#绘制
aucText=c()
# outFile="ROC.pdf"   
# pdf(file=outFile,width=6,height=6)
i=2
ROC_train=timeROC(T=train$Follow_up_timemon,delta=train$oneyr,marker=train[,i],cause=1,weighting='aalen',times=c(12),ROC=TRUE)
print(ROC_train)
plot(ROC_train,time=12,col=bioCol[i-2],title=FALSE,lwd=2)
aucText=c(paste0(colnames(train)[i],", AUC=",sprintf("%.3f",ROC_train$AUC[2])))
abline(0,1)

for(i in 4:ncol(train)){
  ROC_train=timeROC(T=train$Follow_up_timemon,delta=train$Rel._in_5yrs,marker=train[,i],cause=1,weighting='aalen',times=c(12),ROC=TRUE)
  plot(ROC_train,time=12,col=bioCol[i-2],title=FALSE,lwd=2,add=TRUE)
  aucText=c(aucText,paste0(colnames(train)[i],", AUC=",sprintf("%.3f",ROC_train$AUC[2])))
}
legend("bottomright", cex=0.6, aucText, lwd=1, bty="n", col=bioCol[1:(ncol(train)-2)])
# dev.off()

# This is equivalent to using roc.formula:
library(pROC) # 单一因素ROC绘制，
library(glmnet) # 多指标联合预测ROC曲线分析
dt <- dt[-1]
roc.list <- roc(grade ~  ., data = dt)
ggroc(roc.list, size = 1.2, alpha=.6, legacy.axes = TRUE) + theme_bw() + xlab('1-Specificity(FPR)') + ylab('Sensitivity(TPR)') +
  scale_y_continuous(expand = c(0, 0),breaks = seq(0,1.0,0.2)) + scale_x_continuous(expand = c(0, 0),breaks = seq(0,1.0,0.2)) +
  theme(legend.background=element_rect(fill = alpha("white", 0)), legend.title=element_blank(), legend.justification=c(1,0), legend.position=c(1,0))

  

g3 + ggsci::scale_color_lancet()

# Also without ROC objects.
# For instance what AUC would be significantly different from 0.5?
power.roc.test(ncases=41, ncontrols=72, sig.level=0.05, power=0.95)


# R语言pec包深度验证Cox模型
# https://mp.weixin.qq.com/s?__biz=MzI1NjM3NTE1NQ==&mid=2247486232&idx=1&sn=190efb3a76991a6463807add2d9ef899&chksm=ea26eb04dd516212eb40e2bdfc4fa600bfd568c50f27def90647cdef10b04a2a3257d2328f72&mpshare=1&scene=1&srcid=0701wLVI3cuGSyw1YZCW9UsQ&sharer_sharetime=1666330600019&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd





# 绘制预测模型的校准曲线
# https://mp.weixin.qq.com/s?__biz=MzU4OTc0OTg2MA==&mid=2247494081&idx=1&sn=18a2cf98d09ae4d73d1bbd9719f4d239&chksm=fdca62cacabdebdc593c44459933f17480ac5e11a3bf71a5c1b6b4e529b569d0dadc6673aef1&mpshare=1&scene=1&srcid=10214qI37ajpAIJcmDyKbaxA&sharer_sharetime=1666330370105&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# multiple regression
library(riskRegression) # 可同时绘制ROC曲线和校正曲线
library(prodlim)
# absolute risk model
multi.arr <- ARR(Hist(time,status)~logthick+sex+age+ulcer,data=Melanoma,cause=1)

# stratified model allowing different baseline risk for the two gender
multi.arr <- ARR(Hist(time,status)~thick+strata(sex)+age+ulcer,data=Melanoma,cause=1)

# stratify by a continuous variable: strata(age)
multi.arr <- ARR(Hist(time,status)~tp(thick,power=0)+strata(age)+sex+ulcer,
                 data=Melanoma,
                 cause=1)

fit.arr2a <- ARR(Hist(time,status)~tp(thick,power=1),data=Melanoma,cause=1)
summary(fit.arr2a)
fit.arr2b <- ARR(Hist(time,status)~timevar(thick),data=Melanoma,cause=1)
summary(fit.arr2b)

## logistic risk model
fit.lrr <- LRR(Hist(time,status)~thick,data=Melanoma,cause=1)
summary(fit.lrr)

## nearest neighbor non-parametric Aalen-Johansen estimate
library(prodlim)
fit.aj <- prodlim(Hist(time,status)~thick,data=Melanoma)
plot(fit.aj,conf.int=FALSE)

# prediction performance
x <- Score(list(fit.arr2a,fit.arr2b,fit.lrr),
           data=Melanoma,
           formula=Hist(time,status)~1,
           cause=1,
           split.method="none")

