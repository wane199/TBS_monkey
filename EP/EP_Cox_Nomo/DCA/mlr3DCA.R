# https://www.bilibili.com/video/BV1Yu411U7KD/?spm_id_from=333.788&vd_source=23f183f0c5968777e138f31842bde0a0
# [机器学习方法建立和验证预测模型 | 疯狂统计学2.0 ](https://mp.weixin.qq.com/s?__biz=MzU1ODY5MzUyMQ==&mid=2247488469&idx=3&sn=d979ce87bc7a35253496dc14f105b7e3&chksm=fc23fe2bcb54773d5ed519640bc13234f4ec29c049bd813cf1d0096b4ad6ff6f4ae5d2b7388e&mpshare=1&scene=1&srcid=0824eo1m20LtK8cjp3v0EQF2&sharer_sharetime=1661301700925&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# [机器学习模型如何添加DCA](https://mp.weixin.qq.com/s?__biz=MzAwMjY4MDE2Mg==&mid=2247602598&idx=1&sn=c899876f696a5ea4bf0a82bb32b55edd&chksm=9ac5a5abadb22cbd0a54635d4e5996d37e579982756630204bc9c2b2f277dbce9e01efb8a6e6&mpshare=1&scene=1&srcid=0803GrawYQzTYSmjlwiVrVEO&sharer_sharetime=1666835331276&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
rm(list = ls())
library(mlr3) # 主体包
library(mlr3viz) # 执行可视化功能
library(mlr3learners) # 提供额外学习器
library(mlr3verse) # 扩展包
library(mlr3tuning)
library(data.table)
library("magrittr")
library(randomForest) # 执行随机森林算法
library(randomForestSRC)
library(varSelRF) # 挑选变量
library(reshape2)
library(readxl)
library(Boruta)
library(mlbench)
library(caret)
library(pROC)
library(ggplot2)
getwd()
source("./EP/EP_Cox_Nomo/DCA/dca.r") # 执行DCA的脚本


dt1 <- read.csv("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.csv")
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/PT_radiomic_features_temporal_ind2.csv")
dt0 <- read.csv("/home/wane/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
dt0 <- dt0[c(-1, -2)]
# dt1 <- read_excel("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.xlsx")

train <- subset(dt0, dt0$Group == "Training")
test <- subset(dt0, dt0$Group == "Test")
normal_para <- preProcess(x = train[, 3:16], method = c("center", "scale")) # 提取训练集的标准化参数
train_normal <- predict(object = normal_para, newdata = train[, 3:16])
test_normal <- predict(object = normal_para, newdata = test[, 3:16])
library(dplyr)
train <- mutate(train[, 2:2], train_normal)
train <- cbind(train[, 2], train_normal)
test <- mutate(test[, 2], test_normal)
test <- cbind(test[, 2], test_normal)
# 重命名
colnames(train)[1] <- "oneyr"
colnames(test)[1] <- "oneyr"

# 利用Boruta函数进行特征选取
Boruta(oneyr ~ ., data = train, doTrace = 2) -> Bor.train
# random forest with selected variables
getConfirmedFormula(Bor.train) # 确认为重要的变量（绿色）
getNonRejectedFormula(Bor.train) # 没有被拒绝的变量（绿色+黄色）
print(Bor.train)
plot(Bor.train) # Shows important bands
plot(Bor.train, sort = FALSE)
plotImpHistory(Bor.train)

## Prediction & Confusion Matrix,模型验证
# 运用训练集构建随机森林模型
df_rf <- randomForest(factor(oneyr) ~ .,
  data = train,
  ntree = 400, important = TRUE, proximity = TRUE
)
df_rf

df_rf$importance
df_rf$p
# 随机森林分析与可视化
library(linkET)
correlate(train) %>% 
    qcorrplot(type = "lower") +
    geom_square() +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"))
qpairs(train) + geom_pairs(base_size = 2) # pairs plot
correlate(train[-1]) %>% 
  as.igraph() %>% 
  plot(layout = layout_with_circular) # network

p <- correlate(train[1], train[-1]) %>% 
  qcorrplot(extra_mat = list(importance = df_rf$importance),
            fixed = FALSE) +
  geom_tile(colour = "grey80", size = 0.25) +
  geom_point(aes(size = importance), fill = NA, shape = 21,
             data = function(data) data[data$pvalue < 0.05, , drop = FALSE]) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"))

df_rf$explained

p2 <- ggplot(df_rf$explained, aes(df_rf$explained, df_rf$name)) +
  geom_col(fill = "steelblue") +
  scale_y_discrete(limits = rev(names(train[1]))) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
p2
# 训练集的C-index,敏感度，特异度，阴性预测值，阳性预测值，准确度等
p1 <- predict(df_rf, train)
confusionMatrix(p1, factor(train$oneyr))
# 测试集的C-index,敏感度，特异度，阴性预测值，阳性预测值，准确度等
p2 <- predict(df_rf, test)
confusionMatrix(p2, factor(test$oneyr))

library(reportROC)
reportROC(gold=factor(test$oneyr), predictor.binary=p2)
library(DescTools)
BrierScore(df_rf)

predic_ <- predict(df_rf, type = "response", newdata = test) # 预测分类(class)
predict <- ifelse(prob > 0.5, 1, 0)
test$predict1 <- predict
head(train)
# 测试集预测概率
testpredprob <- predict(df_rf, newdata = test, type = "prob") # 预测概率，预测分类(class)
testroc <- roc(response = test$oneyr, predict = testpredprob[, 2])
# ROC曲线
plot(testroc,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grid = T,
  max.auc.polygon = T,
  auc.polygon.col = "skyblue",
  print.thres = T,
  legacy.axes = T,
  bty = "l"
)
# 约登法则
bestp <- testroc$thresholds[
  which.max(testroc$sensitivities + testroc$specificities - 1)
]
bestp
# 测试集预测分类
testpredlab <- as.factor(ifelse(testpredprob[, 2] > 0.11, 1, 0))
# 测试集混淆矩阵
confusionMatrix(
  data = testpredlab, # 预测类别
  reference = factor(test$oneyr), # 实际类别
  positive = "1",
  mode = "everything"
)
# 测试集ROC
testroc <- roc(
  response = test$oneyr, # 实际类别
  predict = testpredprob[, 2]
) # 预测概率
# 测试集、训练集ROC曲线叠加
plot(testroc,
  print.auc = TRUE,
  grid = c(0.1, 0.2),
  auc.polygon = T, print.thres = T,
  legacy.axes = T,
  max.auc.polygon = T,
  main = "ROC",
  grid.col = c("green", "red")
)

model <- riskRegression::Score(list("full" = df_rf), formula = oneyr ~ 1,  data = test, plots = "roc",
                               metrics = "auc")
plotROC(model)

# 获取预测数据并整理数据, DCA & CIC 曲线绘制
prob <- data.frame("outcome" = test$oneyr, "rf_prob" = testpredprob[, 2])
head(prob)
dcaoutput <- dca(
  data = prob, outcome = "outcome",
  predictors = c("rf_prob"),
  xstart = 0, xstop = 1, ymin = 0
)

# 当然我们也可以通过导入到ggplot2来进行DIY
dcadf <- data.frame(dcaoutput$net.benefit)
# 宽数据转长数据
temp <- melt(dcadf, id = "threshold", measure = c("rf_prob", "all", "none"))
ggplot(temp, aes(x = threshold, y = value, colour = variable)) +
  geom_line() +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.05, 0.15)) +
  scale_color_discrete(name = "Model", labels = c("ranger", "all", "none")) +
  labs(x = "Threshold probability (%)") +
  labs(y = "Net benefit") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank())

# DCA & CIC
library(rmda)
library(ggDCA)
library(rms)
d <- dca(df_rf)
ggplot(d)
ggplot(d,color=FALSE)
ggplot(d,linetype = FALSE)
rfp=rFP.p100(x=d) # Calculate reduction in false positive count
ggplot(rfp)
AUDC(d) # Area under Decision Curve
range(d) # Ranges for net benefit



## 尽管文章中没有展示，我们也可以探索一下calibration plot
p2=as.numeric(p2)
cal1 <- calibration(as.factor(oneyr) ~ p2, data = test)
# cal2 <- calibration(as.factor(c) ~ d, data = r2)
xyplot(cal1)
# xyplot(cal2)
# 绘制预测模型的校准曲线
library(riskRegression)
xb <- Score(list(model1 = df_rf), oneyr ~ 1, 
            data = test,
            plots="cal")
# 绘制校准曲线使用riskRegression包的plotCalibration()函数。
plotCalibration(xb, brier.in.legend=TRUE) # 显示AUC和Brier score
# 上面的模型的校准曲线使用的是曲线形式，还可以使用条形图的形式来表示。
plotCalibration(xb, bars=TRUE, model="model1")
plotCalibration(xb, 
                model="model1", # 绘制模型1的校准曲线
                bars=TRUE,  # 校准曲线为条形图形式
                show.frequencies = TRUE, # 条形图上显示频率
                xlab = "", # x轴标签
                ylab = "", # y轴标签
                col = c("#ca3e47","#1ee3cf"), # 设置条形图的颜色
                names.cex = 1.0, # X轴数字的缩放倍数
                cex = 1.5) # 设置图例和标签的缩放倍数

## 其实前面的步骤基本上都是基本建模的流程，我们重点其实是需要知道，我们想要绘制DCA只需要两类数据，一个是结局的编码（这里我们需要我们的编码是二分类并且是0和1的形式），然后我们需要提供我们机器学习模型预测阳性结局的概率
## 注意，由于随机森林等机器学习算法给出来的预测是0或者1，而非连续性的概率，所以calibration，DCA等分析方法并不十分适合(需要概率)


####################################################
####################################################
# RSF
dt0 <- na.omit(dt0) # 按行删除缺失值
attach(dt0)
set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt0[ind == 1, ] # the training data set
# 测试集
test <- dt0[ind == 2, ] # the test data set

## analysis, 生存随机森林因变量不设置分类变量
library(rms)
ddist <- datadist(train)
options(datadist = "ddist")
vars <- paste0(colnames(train[c(7:22)]), collapse = "+")
rad.obj <- rfsrc(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + side + Sex + Surgmon + Onsetmon + Durmon + Freq + SE + SGS + early_brain_injury + familial_epilepsy + brain_hypoxia + Central_Nervous_System_Infections + traumatic_brain_injury + history_of_previous_surgery + MRI,
  data = train, nsplit = 10, block.size = 10,
  mtry = 50, nodesize = 15, ntree = 400, importance = TRUE, samptype = "swor", splitrule = "logrank"
)

print(rad.obj)
print(rad.obj$importance)
print(sort(rad.obj$importance, decreasing = T)[1:6])
plot(rad.obj)
top <- sort(rad.obj$importance, decreasing = T)[1:6]
plot(top)

vs.rad <- var.select(object = rad.obj) # 变量筛选
topvars <- vs.rad$topvars
topvars

max.subtree(rad.obj, conservative = T)$topvars
vimp.obj <- vimp(rad.obj, importance = "random", block.size = 10)
print(sort(vimp.obj$importance, decreasing = T))

# 绘制变量与mortality的关系
plot.variable(rad.obj, plots.per.page = 6)
plot.variable(rad.obj, xvar.names = c("radscore"), surv.type = "mort")
plot.variable(rad.obj, xvar.names = c("radscore"), surv.type = "rel.freq")

# 绘制生存图形
plot.variable(rad.obj, xvar.names = c("radscore"), surv.type = "surv", time = 36)
plot.survival(rad.obj, subset = c(80))
plot.survival(rad.obj, plots.one.page = F)
which.min(rad.obj$err.rate)
rad.pred <- predict(rad.obj, test)
print(rad.pred)


