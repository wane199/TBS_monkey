# binomial classification nomogram-LR
rm(list = ls())

dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/process_PT-22.csv")
str(dt) ## 查看每个变量结构
summary(dt)
colnames(dt)

set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt[ind == 1, ] # the training data set
# 测试集
test <- dt[ind == 2, ] # the test data set

library(rms)
dd <- datadist(train) ## 设置数据环境
options(datadist = "dd")

# 拟合模型
fit1 <- glm(Y ~ .,  data = train, family = "binomial")
fit2 <- step(fit1)
summary(fit2)
1 / exp(coef(fit2))
# logit P 计算
pred.logit2 <- predict(fit2)
# 预测概率P
P2 <- predict(fit2, type = "response")

m <- NROW(train) / 5
val.prob(P2, train$Y, m = m, cex = 0.8) # 预测概率与真实值进行矫正

fit <- lrm(Y ~ .,  data = train)
fit
fit$stats # Brier score

# Alignment Diagram/Nomogram Plot
nomogram <- nomogram(fit, # 模型名称
                     lp = T, # 显示线性概率
                     fun.at = c(0.1, 0.3, 0.5, 0.7, 0.9), # 坐标轴刻度
                     conf.int = c(0.1, 0.7), # 显示置信区间
                     funlabel = "Risk", # 设置坐标轴名称
                     fun = function(x) 1 / (1 + exp(-x))
) # 逻辑回归计算公式
# 绘制列线图
plot(nomogram)
# 设置因子的水平标签
train$sex <- factor(train$sex,
                          levels = c(1, 0),
                          labels = c("Male", "Female")
)
# 设置变量的名称
label(trainingset$sex) <- "Gender"
label(trainingset$age) <- "Age"
label(Affairs$religiousness) <- "宗教信仰"
label(Affairs$rating) <- "婚姻自我评分"

nom <- nomogram(fit,
                fun = plogis, conf.int = c(0.1, 0.7),
                fun.at = c(.001, .01, .05, seq(.1, .9, by = .1), .95, .99, .999),
                lp = F, funlabel = "Risk of Fracture"
)
plot(nom,
     lplabel = "Linear Predictor", # 设置线性概率坐标轴名称
     # fun.side = c(1,3,1,3,1,3), # 坐标轴刻度位置
     col.grid = c("blue", "yellow"), # 垂直参考线的颜色
     col.conf = c("red", "green"), # 设置置信区间的颜色
     conf.space = c(0.1, 0.5) # 设置置信区间条位置
)

library(shinyPredict)
getwd()
shinyPredict(
  models = list("model" = fit1),
  path = "./EP/BLS_EP_files/EML/shinyapp/", # 需更改为自己的工作路经
  data = train,
  title = "Predicting TLE probability",
  shinytheme = "journal"
)

library(regplot)
regplot(fit, # 模型名称
        odds = T, # 设置OR显示
        title = "Nomogram for Fracture Risk at CKD",
        observation = trainingset[1, ], # 指定观测值
        interval = "confidence", points = TRUE
) # 最大刻度100

# 常见列线图的绘制及自定义美化详细教程
# https://mp.weixin.qq.com/s?__biz=MzU4OTc0OTg2MA==&mid=2247497910&idx=1&sn=350a4d6c689462d7337e04455912c8ce&chksm=fdca73bdcabdfaab401fab2a00a9a24a60e7e706675b774bd3d866f6c884cfc80c7a478e7403&mpshare=1&scene=1&srcid=0607mDKXD346ABXXHRc5Sn6I&sharer_sharetime=1654698032379&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# ROC曲线及PR曲线
pre <- predict(fit1, type = "response") # 预测概率，预测分类(class)
pre
library(pROC)
plot.roc(trainingset$Y, pre,
         main = "ROC curve in Training set", percent = TRUE,
         print.auc = TRUE,
         ci = TRUE, of = "thresholds",
         thresholds = "best",
         print.thres = "best"
)
rocplot1 <- roc(trainingset$Y, pre)
auc(rocplot1)
ci.auc(rocplot1)
# ROC详细结果
roc.result <- coords(rocplot1, "best", ret = "all", transpose = F)
as.matrix(roc.result)
# PR curve
library(modEvA)
aupr <- AUC(
  obs = trainingset$Y, pred = pre, interval = 0.001,
  curve = "PR", method = "trapezoid", simplif = F, main = "PR curve"
)
