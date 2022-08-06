# https://blog.csdn.net/qq_42696043/article/details/125134962?spm=1001.2014.3001.5502
library(ggplot2)
dt <- read.csv("C:\\Users\\wane199\\Desktop\\TBS&Mon\\BIAO\\PTH1\\CKD1-2.csv", header = T)
str(dt)
summary(dt)

# 因子化
VarsC <- names(dt[c(1:2, 4:12, 17:21)]) # 分类变量
for (i in VarsC) {
  dt[, i] <- as.factor(dt[, i])
} # 利用循环因子化
# 批量数值转因子
for (i in names(dt)[c(3:5, 7:16)]) {
  dt[, i] <- as.factor(dt[, i])
}

plot(density(dt$TBS), main = "Distribution of TBS")
polygon(density(dt$TBS), col = "grey") # polygon()用于曲线内填充颜色
abline(v = 1.34, lwd = 2, col = "skyblue") # abline（a, b）表示在图上添加一条y=a+bx的直线

dt$TBS <- dt$TBS > 1.34
summary(dt)

library(caret)
# 待筛选特征标准化
dtx <- scale(dt[, c(3:20)])
dtx <- as.data.frame(dtx)
dt <- dplyr::mutate(dt[1], dtx)
dt2 <- cbind(dt[1], dtx)
set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt[ind == 1, ] # the training data set
# 测试集
test <- dt[ind == 2, ] # the test data set
# 看一下，不要让临床信息差的太多，输出table1
prop.table(table(train$Y))
prop.table(table(test$Y))

fit1 <- glm(Y ~ TBS, data = train, family = binomial())
prob1 <- predict(fit1, newdata = test, type = "response")
# type = "link", 缺省值，给出线性函数预测值
# type = "response", 给出概率预测值
# type = "terms"，给出各个变量的预测值
library(ROCR) # ROCR包提供多种评估分类执行效果的方法及可视化
pred1 <- prediction(prob1, test$Y) # 转换prob1的格式
performance(pred1, "auc")@y.values[[1]]

fit2 <- glm(Y ~ TscoreL1L4, data = train, family = binomial())
prob2 <- predict(fit2, newdata = test, type = "response")
pred2 <- prediction(prob2, test$Y)
performance(pred2, "auc")@y.values[[1]]

fit3 <- glm(Y ~ BMD, data = train, family = binomial())
prob3 <- predict(fit3, newdata = test, type = "response")
pred3 <- prediction(prob3, test$Y)
performance(pred3, "auc")@y.values[[1]]

fit4 <- glm(Y ~ age + sex + P + DM + Ca + TscoreL1L4 + Dialysis_duration, data = train, family = binomial())
prob4 <- predict(fit4, newdata = test, type = "response")
pred4 <- prediction(prob4, test$Y)
performance(pred4, "auc")@y.values[[1]]

plot(performance(pred1, "tpr", "fpr"), colorize = T, lwd = 3, main = "ROC Curves")
legend(
  x = "bottomright",
  legend = "TBS",
  lty = 1,
  col = c("coral", "skyblue"),
  bty = "n",
  horiz = T
)
plot(performance(pred2, "tpr", "fpr"), add = T, colorize = T, lwd = 3)
plot(performance(pred3, "tpr", "fpr"), add = T, colorize = T, lwd = 3)
abline(a = 0, b = 1, lty = 2, lwd = 3, col = "black")

varsU <- names(dt[, 2:21]) # 自变量
Result <- c()
for (i in 1:length(varsU)) {
  fit <- glm(substitute(Y ~ x, list(x = as.name(varsU[i]))), data = dt, family = binomial())
  fitSum <- summary(fit)
  result1 <- c()
  result1 <- rbind(result1, fitSum$coef)
  OR <- exp(fitSum$coef[, "Estimate"])
  result1 <- data.frame(cbind(result1, cbind(OR, exp(confint(fit)))))
  result1$Characteristics <- varsU[i] # 添加变量名
  Result <- rbind(Result, result1[-1, ]) # [-1,],删除常数项
}

Uni_log <- data.frame(Result[, c(1, 4:8)]) # 提取"P","OR","CIlower","CIupper"和变量名
colnames(Uni_log)[2:5] <- c("P", "OR", "CIlower", "CIupper") # 变量重命名
ExtractVar <- unique(Uni_log$Characteristics[Uni_log$"P" < 0.05]) # 提取有意义的变量
write.csv(Uni_log, file = "Uni_log.csv") # 输出文档
Uni_log

# https://mp.weixin.qq.com/s?__biz=Mzg2MjU2NDQwMg==&mid=100008208&idx=1&sn=f725f48085617f41e02aa83e13fd2aae&chksm=4e07584d7970d15b4b2d1a274f186c0f1adde7f24bb8d295e2eba3e590c7c6635fa779af0166#rd
# 结果合并需要的包
library(plyr)
# 可进行logistic回归的包
library(rms) # 可实现逻辑回归模型（lrm）
library(epiDisplay) # 快速输出OR、95%CI、P
library(gtsummary) # 精美三线表（但，95%CI有误）
Uni_glm_model <-
  function(x) {
    # 拟合结局和变量
    FML <- as.formula(paste0("Y==1~", x))
    # glm()逻辑回归
    glm1 <- glm(FML, data = train, family = binomial)
    # 提取所有回归结果放入glm2中
    glm2 <- summary(glm1)
    # 1-计算OR值保留两位小数
    OR <- round(exp(coef(glm1)), 2)
    # 2-提取SE
    SE <- glm2$coefficients[, 2]
    # 3-计算CI保留两位小数并合并
    CI5 <- round(exp(coef(glm1) - 1.96 * SE), 2)
    CI95 <- round(exp(coef(glm1) + 1.96 * SE), 2)
    CI <- paste0(CI5, "-", CI95)
    # 4-提取P值
    P <- round(glm2$coefficients[, 4], 2)
    # 5-将变量名、OR、CI、P合并为一个表，删去第一行
    Uni_glm_model <- data.frame(
      "Characteristics" = x,
      "OR" = OR,
      "CI" = CI,
      "P" = P
    )[-1, ]
    # 返回循环函数继续上述操作
    return(Uni_glm_model)
  }

# 把它们放入variable.names中
variable.names <- colnames(train)[c(2:21)]
variable.names

# 变量带入循环函数
Uni_glm <- lapply(variable.names, Uni_glm_model)
# 批量输出结果并合并在一起
Uni_glm <- ldply(Uni_glm, data.frame)
Uni_glm

# 注意：这一步只是为了提取变量仅此而已
variable.names
paste0(variable.names, collapse = "+")
names <- glm(Y == 1 ~ sex + age + Cre + eGFR + Urea + CysC + ALP + VD + PTH + Ca + P +
  BMI + BMD + TBS + TscoreL1L4 + Dialysis_duration + Smoking + Drinking + DM + Drugs,
data = train,
family = binomial
)
name <- data.frame(summary(names)$aliased)
# 将提取的数据表的行名删除第一行并给三线表
rownames(Uni_glm) <- rownames(name)[-1]
# 原三线表的行名不再需要，删去
Uni_glm <- Uni_glm[, -1]
# 最后，将P值=0的变为p<0.0001
Uni_glm$P[Uni_glm$P == 0] <- "<0.001"
Uni_glm
# 保存为Excel
write.csv(Uni_glm, "批量单因素回归三线表结果.csv")

# 多因素logistic回归
varsMul <- c("age", "sex", "P", "DM", "Ca", "TBS", "Dialysis_duration") # 需要进行多因素分析的变量，随机生成的数据单因素无意义，故强制纳入
dataAM <- data.frame(subset(train, select = c("Y", varsMul[1:length(varsMul)]))) # 将因变量和要分析的自变量单独建库
fitMul <- glm(Y ~ ., data = dataAM, family = binomial()) # 行多因素logistic回归分析
fitSum <- summary(fitMul)
ResultMul <- c() # 准备空向量，用来储存结果
ResultMul <- rbind(ResultMul, fitSum$coef)
OR <- exp(fitSum$coef[, "Estimate"])
ResultMul <- cbind(ResultMul, cbind(OR, exp(confint(fitMul))))
ResultMul
write.csv(ResultMul, file = "Mul_log.csv")

# https://zhuanlan.zhihu.com/p/369933231
# 全子集回归 | 最优子集筛选
lmfit <- lm(Y == 1 ~ sex + age + Cre + eGFR + Urea + CysC + ALP + VD + PTH + Ca + P + BMI + TBS
  + Dialysis_duration + Smoking + Drinking + DM + Drugs, data = train)


library(olsrr)
# 全子集回归
ols_step_all_possible(lmfit)
plot(ols_step_all_possible(lmfit))

# 最优子集回归
ols_step_best_subset(lmfit)
plot(ols_step_best_subset(lmfit))

lmfitbm <- lm(Y ~ age + sex + P + DM + Ca + TBS + Dialysis_duration, data = train)
summary(lmfitbm)


library(bestglm)
library(leaps)

lgtdata <- train[c("age", "sex", "P", "DM", "Ca", "TBS", "Dialysis_duration", "Y")]
lgtdata <- as.data.frame(as.matrix(lgtdata))
str(lgtdata)
bestglm(lgtdata, IC = "CV", family = binomial) ## Information criteria to use: "AIC", "BIC", "BICg", "BICq", "LOOCV", "CV". Family to use: binomial(link = "logit"),gaussian(link = "identity"),
# Gamma(link = "inverse"),inverse.gaussian(link = "1/mu^2"),poisson(link = "log"),quasi(link = "identity", variance = "constant"),quasibinomial(link = "logit"),quasipoisson(link = "log")

# https://mp.weixin.qq.com/s?__biz=MzIzNjk2NDg4NA==&mid=2247486273&idx=1&sn=a098f0f274cd72577fb8e077dfc797db&chksm=e8ce963adfb91f2cc557915fefd78b265b12172ae987d467c44e6403d6c8aed94460fd00a3e1&mpshare=1&scene=1&srcid=0805D2CX1m2beuBxiL39Q7LC&sharer_sharetime=1659707936766&sharer_shareid=e01bedee3072575c19cc0d90b2125a40#rd
# 数据准备，切分数据集
# 按7:3将数据集分割为训练集和测试集合
dt <- read.csv("/home/wane/Desktop/TBS&Mon/BIAO/PTH1/CKD1-2.csv", header = T)
dt <- dt[-1]
set.seed(123)
ss <- sample(nrow(dt), nrow(dt) * 0.7)
trainingset <- dt[ss, ]
testingset <- dt[-ss, ]
# Deep EDA
library(DataExplorer)
plot_bar(dt)
# Plot bar charts by `cut`
plot_bar(dt, by = "Y")
plot_histogram(dt)
plot_boxplot(dt)
plot_density(dt)

library(SmartEDA)
ExpCatViz(dt, Page = c(3, 5))
library(tidyverse)
ExpCatViz(
  trainingset %>%
    select(Y, TBS),
  target = "Y"
)

par(mfrow = c(4, 5))
for (i in 2:21) {
  hist(dt[, i], border = "pink", col = "lightblue", main = "", label = T,xlab = "")
}

library(flextable) # beautifying tables
dlookr::describe(trainingset) %>% flextable()

# library(caTools)
# set.seed(123)
# split<-sample.split(biopsy$class, SplitRatio = 0.70)
# trainingset<-subset(biopsy, split==TRUE)
# testingset<-subset(biopsy, split == FALSE)
# library(caret)
# library(ggplot2);library(lattice)
# set.seed(123)
# split<-createDataPartition(biopsy$class,p=0.7,list=FALSE)
# trainingset3<-biopsy[split,]
# testingset3<-biopsy[-split,]
## 生成解释变量和结局变量的矩阵格式，glmnet数据格式是矩阵
Xtrain <- as.matrix(trainingset[, 2:21])
Ytrain <- as.matrix(trainingset[, 1])
Xtest <- as.matrix(testingset[, 2:21])
Ytest <- as.matrix(testingset[, 1])
# Lasso回归筛选变量与模型评估
library(glmnet)
library(Matrix)
## lasso回归
lsofit <- glmnet(Xtrain, Ytrain, family = "binomial", alpha = 1)
# 解释变量须是矩阵形式；family=c("gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian")，gaussian适用于连续型因变量, mgaussian适用于连续型的多元因变量，poisson适用于计数因变量,binomial适用于二分类因变量, multinomial适用于多分类的因变量, cox适用于生存资料；弹性网络alpha取值在0和1之间，0≤alpha≤1，取值1时拟合lasso回归，取值0时拟合领回归；nlambda为λ值的数量，默认等于100；dfmax和pmax可以分别限定模型中的变量的数量；relax=TRUE表示将进行Relaxed lasso
print(lsofit)
# 结果会产生三列结果，分别是Df (非0系数的变量数量), %dev (模型解释的偏差的百分数) 和 Lambda (相应的λ值)。偏差（deviance）即-2倍的Log-likelihood
## lasso回归系数
coef.apprx <- coef(lsofit, s = 0.2)
# 参数s指定具体的λ值。上述命令是提取λ=0.2时的lasso回归系数，此时模型会有4个非0系数变量。精确系数估算可用coef.exact<-coef(lsofit, s=0.2,exact=TRUE, x=Xtrain, y=Ytrain)
coef.apprx
predict(lsofit, type = "coefficients", s = 0.2)
# predict也可以用来提取λ某一个取值时的lasso回归系数
# type=c("link","response","coeffificients","nonzero");当想用新的取值做预测时，可用参数newx来指定预测变量的矩阵，可用于type=c("link","response")时；exact参数同可参见coef函数。link给出的是线性预测值，即进行logit变化前的值；response给出的是概率预测值，即进行logit变换之后的值；coefficients给出的是指定λ值的模型系数；nonzero给出指定的定λ值时系数不为零的模型变量
## 系数路径图
plot(lsofit, xvar = "lambda", label = TRUE)
# 参数xvar=c("norm", "lambda", "dev")，用于指定X轴的变量，norm表示横坐标使用系数的L1范数，lambda表示横坐标使用lnλ，而 "dev"表示横坐标采用解释偏差的百分比

set.seed(123) # 设置随机种子，保证K折验证的可重复性
lsocv <- cv.glmnet(Xtrain, Ytrain, family = "binomial", nfolds = 100)
# family同glmnet函数的family；type.measure用来指定交叉验证选取模型的标准，可取值"default", "mse", "deviance", "class", "auc", "mae", "C"。type.measure的默认值是"deviance"，线性模型是squared-error for gaussian models (type.measure="mse" ), logistic和poisson回归是deviance， Cox模型则是偏似然值（partial-likelihood）。deviance即-2倍的对数似然值，mse是实际值与拟合值的mean squred error，mae即mean absolute error，class是模型分类的错误率(missclassification error)，auc即area under the ROC curve。nfolds表示进行几折验证，默认是10
lsocv ## print(lsocv) ，glmnet模型交叉验证的结果

lsocv$lambda.min
lsocv$lambda.1se

plot(lsocv) # 绘制交叉验证曲线

coef(lsocv, s = "lambda.min") # 获取使模型偏差最小时λ值的模型系数
coef(lsocv, s = "lambda.1se") # 获取使模型偏差最小时λ值+一个标准误时的模型系数
cbind2(coef(lsocv, s = "lambda.min"), coef(lsocv, s = "lambda.1se")) # 合并显示