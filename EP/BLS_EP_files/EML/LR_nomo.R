# binomial classification nomogram-LR
rm(list = ls())
options(digits=3) # 限定输出小数点后数字的位数为3位
dt <- read.csv("C:/Users/wane199/Desktop/EP/REFER/BLS/KAI/process_rad_lat_7.csv")
dt <- read.csv("/home/wane/Desktop/EP/REFER/BLS/KAI/process_rad_lat_7.csv")
str(dt) ## 查看每个变量结构
summary(dt)
colnames(dt)

set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt[ind == 1, ] # the training data set
# 测试集
test <- dt[ind == 2, ] # the test data set

train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")
train <- train[c(-1:-2,-3)]
test <- test[c(-1:-2,-3)]

library(rms)
dd <- datadist(train) ## 设置数据环境
options(datadist = "dd")
train$Y <- factor(train$Y)
# 拟合模型
fit1 <- glm(Y ~ original_gldm_DependenceEntropy+log.sigma.5.0.mm.3D_firstorder_Energy+wavelet.LHL_glrlm_GrayLevelNonUniformity, # original_gldm_DependenceEntropy+log.sigma.5.0.mm.3D_firstorder_Energy+log.sigma.5.0.mm.3D_firstorder_Mean+wavelet.LHL_glrlm_GrayLevelNonUniformity
            data = train, family = "binomial")
fit2 <- step(fit1)
summary(fit2)
1 / exp(coef(fit2))
# logit P 计算
pred.logit2 <- predict(fit2)
# 预测概率P
P2 <- predict(fit2, type = "response")

m <- NROW(train) / 5
m
Ca1 <- val.prob(P2, train$Y, m = m, cex = 0.8) # 预测概率与真实值进行矫正

# 验证集
# logit P 计算
pred.logit3 <- predict(fit2, newdata = test)
# 预测概率P
P3 <- predict(fit1, type = "response", newdata = test)
m <- NROW(test) / 5
Ca1 <- val.prob(P3, test$Y, m = m, cex = 0.8) 
# 预测概率与真实值进行矫正

paste0(colnames(train),collapse = "+")
str(train)
library(cutoff)
logit(data=train,
      y='Y',
      x='.',
      cut.numb=1,
      n.per=0.25,
      y.per=0.25)

# 采用cut函数, 连续变量处理成分类变量(训练集的cutoff应用于全数据(验证集))
str(dt)
dt$original_firstorder_Mean <- cut(dt$original_firstorder_Mean, breaks = c(-Inf, -0.037, Inf), labels = c("1", "2"), right = FALSE)
dt$original_gldm_DependenceEntropy <- cut(dt$original_gldm_DependenceEntropy, breaks = c(-Inf, -0.079, Inf), labels = c("1", "2"), right = FALSE)
dt$log.sigma.5.0.mm.3D_firstorder_Energy <- cut(dt$log.sigma.5.0.mm.3D_firstorder_Energy, breaks = c(-Inf, -0.373, Inf), labels = c("1", "2"), right = FALSE)
dt$log.sigma.5.0.mm.3D_firstorder_Mean <- cut(dt$log.sigma.5.0.mm.3D_firstorder_Mean, breaks = c(-Inf, 0.01, Inf), labels = c("1", "2"), right = FALSE)
dt$log.sigma.5.0.mm.3D_firstorder_TotalEnergy <- cut(dt$log.sigma.5.0.mm.3D_firstorder_TotalEnergy, breaks = c(-Inf, -0.373, Inf), labels = c("1", "2"), right = FALSE)
dt$wavelet.LHL_glrlm_GrayLevelNonUniformity <- cut(dt$wavelet.LHL_glrlm_GrayLevelNonUniformity, breaks = c(-Inf, -0.06, Inf), labels = c("1", "2"), right = FALSE)
dt$wavelet.LHH_gldm_GrayLevelNonUniformity <- cut(dt$wavelet.LHH_gldm_GrayLevelNonUniformity, breaks = c(-Inf, 0.08, Inf), labels = c("1", "2"), right = FALSE)
write.csv(dt, "C:/Users/wane199/Desktop/EP/REFER/BLS/KAI/process_rad_lat_7_factor.csv", row.names = FALSE)

fit <- rms::lrm(Y ~ original_gldm_DependenceEntropy+log.sigma.5.0.mm.3D_firstorder_Energy+wavelet.LHL_glrlm_GrayLevelNonUniformity, # original_gldm_DependenceEntropy+log.sigma.5.0.mm.3D_firstorder_Energy+wavelet.LHL_glrlm_GrayLevelNonUniformity 
                data = train) # , x=T, y=T
fit
fit$stats # Brier score: 衡量了预测概率与实际概率之间的差异，取值范围在0-1之间，数值越小表示校准度越好。

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

# 彩色条带式静态诺莫图
library(VRPM)
col <- glm(Y ~ .,  data = train, family = "binomial")
colplot(col)
# visualize the model: more than one plot is generated in the current directory
outnames=colnames(fitted(col))
labels=c(paste("Linear predictor for",outnames[-1]),paste
         ("Predicted chance of being",outnames))
colplot(col,coloroptions=3,risklabel=labels,filename="div")

library(shinyPredict)
getwd()
shinyPredict(
  models = list("model" = fit1),
  path = "./EP/BLS_EP_files/EML/shinyapp/", # 需更改为自己的工作路经
  data = train,
  title = "Predicting TLE probability",
  shinytheme = "journal"
)

# 示例展示nomogram
library(regplot)
regplot(fit, # 模型名称
        odds = T, # 设置OR显示
        title = "Nomogram for TLE Classification",
        observation = train[1, ], # 指定观测值
        interval = "confidence", points = TRUE
) # 最大刻度100

# 常见列线图的绘制及自定义美化详细教程
# https://mp.weixin.qq.com/s?__biz=MzU4OTc0OTg2MA==&mid=2247497910&idx=1&sn=350a4d6c689462d7337e04455912c8ce&chksm=fdca73bdcabdfaab401fab2a00a9a24a60e7e706675b774bd3d866f6c884cfc80c7a478e7403&mpshare=1&scene=1&srcid=0607mDKXD346ABXXHRc5Sn6I&sharer_sharetime=1654698032379&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# ROC曲线及PR曲线
pre <- predict(fit1, type = "response") # 预测概率，预测分类(class)
pro <- predict(fit1, type = "terms") # 预测概率，预测分类(class)
pre
library(pROC)
plot.roc(train$Y, pre,
         main = "ROC curve in Training set", percent = TRUE,
         print.auc = TRUE,
         ci = TRUE, of = "thresholds",
         thresholds = "best",
         print.thres = "best"
)
rocplot1 <- roc(train$Y, pre)
auc(rocplot1)
ci.auc(rocplot1)
# ROC详细结果
roc.result <- coords(rocplot1, "best", ret = "all", transpose = F)
as.matrix(roc.result)

# ROC曲线及PR曲线
pre1 <- predict(fit1, type = "response", newdata = test) # 预测概率，预测分类(class)
plot.roc(test$Y, pre1,
         main = "ROC curve in Test set", percent = TRUE,
         print.auc = TRUE,
         ci = TRUE, of = "thresholds",
         thresholds = "best",
         print.thres = "best"
)

# PR curve
library(modEvA)
aupr <- AUC(obs = train$Y, pred = pre, interval = 0.001,
  curve = "PR", method = "trapezoid", simplif = F, main = "PR curve")

# DCA & CIC
library(rmda)
library(ggDCA)
library(rms)
d <- dca(fit)
ggplot(d)
ggplot(d,color=FALSE)
ggplot(d,linetype = FALSE)
rfp=rFP.p100(x=d) # Calculate reduction in false positive count
ggplot(rfp)
AUDC(d) # Area under Decision Curve
range(d) # Ranges for net benefit

library(rmda)
set.seed(123)
baseline.model <- decision_curve(Y ~ .,  data = train,
                                 thresholds = seq(0, .4, by = .001),
                                 bootstraps = 25) #should use more bootstrap replicates in practice!

#plot using the defaults
plot_decision_curve(baseline.model,  curve.names = "baseline model")

#plot the clinical impact
plot_clinical_impact(baseline.model, xlim = c(0, .4),
                     col = c("black", "blue"))

# [predictiveness curve](https://mp.weixin.qq.com/s?__biz=MjM5NDM3NjczOA==&mid=2247485894&idx=1&sn=829d7b464a84a2cb83de3a2f83a94ef4&chksm=a689f48b91fe7d9d7e0fe5d8cfce6af7ea4cadc39859edcd0ef35fafe3e4c0ccdffac015ec2f&mpshare=1&scene=1&srcid=10275oDAiISr7KXj5S6szHzp&sharer_sharetime=1666845032744&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# specify dataset with outcome and predictor variables 
library(PredictABEL)
data(ExampleData)

# fit logistic regression models
# all steps needed to construct a logistic regression model are written in a function
# called 'ExampleModels', which is described on page 4-5
riskmodel1 <- ExampleModels()$riskModel1
riskmodel2 <- ExampleModels()$riskModel2

# obtain predicted risks
predRisk1 <- predRisk(riskmodel1)
predRisk2 <- predRisk(riskmodel2)

# specify range of y-axis
rangeyaxis <- c(0,1) 
# specify labels of the predictiveness curves
labels <- c("without genetic factors", "with genetic factors")

# produce predictiveness curves
plotPredictivenessCurve(predrisk=cbind(predRisk1,predRisk2),
                        rangeyaxis=rangeyaxis, labels=labels)
