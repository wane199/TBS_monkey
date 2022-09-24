# https://blog.csdn.net/qq_42696043/article/details/125134962?spm=1001.2014.3001.5502
rm(list = ls())
library(ggplot2)
library(readxl)
library(scorecard) # 利用scorecard R包开发评分卡
dt <- read.csv("C:\\Users\\wane199\\Desktop\\TBS&Mon\\BIAO\\PTH1\\CKD2.csv", header = T)
dt <- read.csv("/home/wane/Desktop/TBS&Mon/BIAO/PTH1/CKD2.csv", header = T)
dt <- dt[-1]
str(dt)
summary(dt)

# 因子化
VarsC <- names(dt[c(1:2, 4:13, 15, 17:21)]) # 分类变量
for (i in VarsC) {
  dt[, i] <- as.factor(dt[, i])
} # 利用循环因子化

plot(density(dt$TBS), main = "Distribution of TBS")
polygon(density(dt$TBS), col = "grey") # polygon()用于曲线内填充颜色
abline(v = 1.34, lwd = 2, col = "skyblue") # abline（a, b）表示在图上添加一条y=a+bx的直线

library(caret)
set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt[ind == 1, ] # the training data set
# 测试集
test <- dt[ind == 2, ] # the test data set
# 看一下，不要让临床信息差的太多，输出table1
prop.table(table(train$Y))
prop.table(table(test$Y))
# 待筛选特征标准化(防止数据泄漏，先分组后标化)
dtx <- scale(dt[, c(3:20)])
dtx <- as.data.frame(dtx)
dt <- dplyr::mutate(dt[1], dtx)
dt2 <- cbind(dt[1], dtx)

dt <- read_excel("/home/wane/Desktop/EP/Structured_Data/Physician.xlsx")
table(dt$Phy1)
fit1 <- glm(Label ~ Phy1, data = dt, family = binomial())
# 训练集预测概率
prob1 <- predict(fit1, newdata = dt, type = "response")
prob2 <- predict(fit1, newdata = dt, type = "response")
Phy1prob <- as.data.frame(prob1) # Converting list to dataframe in R
dt1 <- dplyr::mutate(dt[c(1, 2, 4, 5)], Phy1prob, Phy2prob)
write.csv(dt1, "/home/wane/Desktop/EP/Structured_Data/Physician1.csv")
# type = "link", 缺省值，给出线性函数预测值
# type = "response", 给出概率预测值
# type = "terms"，给出各个变量的预测值
library(ROCR) # ROCR包提供多种评估分类执行效果的方法及可视化
pred1 <- ROCR::prediction(prob1, dt$Label) # 转换prob1的格式
performance(pred1, "auc")@y.values[[1]]

# 混淆矩阵绘制
require(caret)
library(pROC)
# 训练集ROC
roc <- roc(response = dt$Label, predict = prob1)
# 训练集ROC曲线
plot(roc, col = "red")
# 约登法则，最佳cutoff值
bestp <- roc$thresholds[
  which.max(roc$sensitivities + roc$specificities - 1)
]
bestp
# 训练集预测分类
predlab <- as.factor(ifelse(prob1 > bestp, 1, 0))
Actual <- factor(dt$Label, levels = c(1, 0), labels = c("True", "False"))
# 训练集混淆矩阵
caret::confusionMatrix(
  data = predlab, # 预测类别
  reference = factor(dt$Label), # 实际类别
  positive = "1",
  mode = "everything"
)

fit2 <- glm(Y ~ sex, data = train, family = binomial())
prob2 <- predict(fit2, newdata = train, type = "response")
pred2 <- prediction(prob2, train$Y)
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

varsU <- names(trainingset[, 2:21]) # 自变量
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
write.csv(Uni_log, file = "./TBS/Uni_log.csv") # 输出文档
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
    glm1 <- glm(FML, data = trainingset, family = binomial)
    # 提取所有回归结果放入glm2中
    glm2 <- summary(glm1)
    # 1-计算OR值保留两位小数
    OR <- round(exp(coef(glm1)), 4)
    # 2-提取SE
    SE <- glm2$coefficients[, 4]
    # 3-计算CI保留四位小数并合并
    CI5 <- round(exp(coef(glm1) - 1.96 * SE), 4)
    CI95 <- round(exp(coef(glm1) + 1.96 * SE), 4)
    CI <- paste0(CI5, "-", CI95)
    # 4-提取P值
    P <- round(glm2$coefficients[, 4], 4)
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
variable.names <- colnames(trainingset)[c(2:21)]
variable.names

# 变量带入循环函数
Uni_glm <- lapply(variable.names, Uni_glm_model)
# 批量输出结果并合并在一起
Uni_glm <- ldply(Uni_glm, data.frame)
Uni_glm

# 注意：这一步只是为了提取变量仅此而已
variable.names
paste0(variable.names, collapse = "+")

names <- glm(Y == 1 ~ sex + age + Cre + eGFR + Urea + CysC + ALP + VD + PTH + Ca + P + BMI + BMD +
  TBS + TscoreL1L4 + Dialysis_duration + Smoking + Drinking + DM + Drugs,
data = trainingset,
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
write.csv(Uni_glm, "./TBS/单因素回归三线表结果.csv")

# 多因素logistic回归
varsMul <- c("age", "sex", "P", "Ca", "eGFR", "ALP", "TBS") # 需要进行多因素分析的变量，随机生成的数据单因素无意义，故强制纳入
dataAM <- data.frame(subset(trainingset, select = c("Y", varsMul[1:length(varsMul)]))) # 将因变量和要分析的自变量单独建库
fitMul <- glm(Y ~ ., data = dataAM, family = binomial()) # 行多因素logistic回归分析
fitSum <- summary(fitMul)
ResultMul <- c() # 准备空向量，用来储存结果
ResultMul <- rbind(ResultMul, fitSum$coef)
OR <- exp(fitSum$coef[, "Estimate"])
ResultMul <- cbind(ResultMul, cbind(OR, exp(confint(fitMul))))
ResultMul
write.csv(ResultMul, file = "./TBS/Mul_log.csv")

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

lmfitbm <- lm(Y ~ age + sex + P + eGFR + Ca + TBS, data = train)
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
rm(list = ls())
# write.csv(dt, "/home/wane/Desktop/TBS&Mon/BIAO/PTH1/CKD1-3.csv")
dt <- read.csv("/home/wane/Desktop/TBS&Mon/BIAO/PTH1/CKD1-3.csv", header = T)
dt <- dt[-1]
str(dt)
summary(dt)
# 多分类变量进行哑变量编码(Dummy Encoding)/独热编码(One-Hot Encoding)
# https://blog.csdn.net/qq_22253901/article/details/124203745#:~:text=R%E8%AF%AD%E8%A8%80%E4%B8%AD%E5%93%91%E5%8F%98%E9%87%8F%E7%9A%84%E8%AE%BE%E7%BD%AE%20%E5%9C%A8R%E8%AF%AD%E8%A8%80%E4%B8%AD%E5%AF%B9%E5%8C%85%E6%8B%AC%E5%88%86%E7%B1%BB%E5%8F%98%E9%87%8F,(factor)%E7%9A%84%E6%95%B0%E6%8D%AE%E5%BB%BA%E6%A8%A1%E6%97%B6%EF%BC%8C%E4%B8%80%E8%88%AC%E4%BC%9A%E5%B0%86%E5%85%B6%E8%87%AA%E5%8A%A8%E5%A4%84%E7%90%86%E4%B8%BA%E8%99%9A%E6%8B%9F%E5%8F%98%E9%87%8F%E6%88%96%E5%93%91%E5%8F%98%E9%87%8F%20(dummy%20variable)%E3%80%82
# 构造分类变量
dt$TBS <- ifelse(dt$TBS < 1.34, 0, 1)
dt$BMI <- ifelse(dt$BMI < 24, 0, dt$BMI)
dt$BMI <- ifelse(dt$BMI >= 24 & dt$BMI < 30, 1, dt$BMI)
# dt$BMIc<-ifelse (dt$BMI>=30 & dt$BMI<35,2,dt$BMIc)
# dt$BMIc<-ifelse (dt$BMI>=35 & dt$BMI<40,3,dt$BMIc)
dt$BMI <- ifelse(dt$BMI >= 30, 2, dt$BMI)
dt$Dialysis_duration <- ifelse(dt$Dialysis_duration > 0, 1, dt$Dialysis_duration)
dt$TBS <- factor(dt$TBS)
dt$BMI <- factor(dt$BMI)
dt$Dialysis_duration <- factor(dt$Dialysis_duration)

a <- caret::dummyVars(~., trainingset)
b <- predict(a, trainingset)
head(b)
library(cattonum)
dt.BMIc.onehot <- catto_onehot(dt, c(10, 22)) # 独热编码，相当于在x2的基础上增加参照水平的0/1赋值。cattonum包中的函数catto_dummy也可以进行哑变量编码
trainingset.onehot <- catto_onehot(trainingset)
dt$Y <- ifelse(dt$Y == "0", "No Fracture", "Fracture")
# dt$Y <- as.factor(as.character(dt$Y))
# dt$Y <- as.factor(dt$Y,levels = c(0, 1),
#                   labels = c("No Fracture", "Fracture"))
# 批量数值转因子
for (i in names(dt)[c(1:2, 4:13, 15, 17:19)]) {
  dt[, i] <- as.factor(dt[, i])
}
set.seed(123)
ss <- sample(nrow(dt), nrow(dt) * 0.7)
trainingset <- dt[ss, ]
testingset <- dt[-ss, ]
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

# Deep EDA(https://yuzar-blog.netlify.app/posts/2021-01-09-exploratory-data-analysis-and-beyond-in-r-in-progress/)
# Explore numeric variables with descriptive statistics
library(flextable) # beautifying tables
library(dplyr)
library(dlookr)
dlookr::describe(train) %>% flextable()

dt[c(-1, -2, -4)] %>%
  group_by("Group") %>%
  univar_numeric() %>%
  knitr::kable(format = "pipe")

trainingset %>%
  diagnose_numeric() %>%
  flextable()

SmartEDA::ExpNumStat(dt[c(-1, -2,-4)], by = "GA", gp = "Group", Outlier = TRUE, Qnt = c(.25, .75), round = 2) %>% flextable()

library(summarytools)
dt[c(-1, -2, -4)] %>%
  group_by("Group") %>%
  descr()

library(psych)
describeBy(
  dt,
  dt$Group
)

# Summary tools
library(gtsummary)
tra <- trainingset %>%
  # select(mpg, hp, am, gear, cyl) %>%
  tbl_summary(by = Y) %>%
  add_p()
tes <- testingset %>%
  # select(mpg, hp, am, gear, cyl) %>%
  tbl_summary(by = Y) %>%
  add_p()

tra %>% # build gtsummary table
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "./TBS/train.png"
  )
# 保存为.html .tex .ltx .rtf
tra %>%
  as_gt() %>%
  gt::gtsave(filename = "./TBS/train_ex1.html") # use extensions .html .tex .ltx .rtf
# 保存为word
library(gdtools)
library(officer)
tf <- tempfile(fileext = ".docx")
tra %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = tf)

# using the knitr::kable function
as_kable(tra, format = "latex")
# using the {kableExtra} package
as_kable_extra(tra, format = "latex")
# 基线特征描述统计
library(autoReg)
ft <- gaze(Group ~ ., data = dt[c(-1, -2, -4)]) %>% myft()
ft
library(rrtable)
table2pptx(ft) # Exported table as Report.pptx
table2docx(ft) # Exported table as Report.docx
table2docx(tra, title = "Train", append = TRUE, vanilla = TRUE)
# 二元LR回归，多分类变量必须处理成factor
trainingset$Y <- factor(trainingset$Y, labels = c("No Fracture", "Fracture"))
## setLabel()函数给变量名添加标签
trainingset$Y <- setLabel(trainingset$Y, "Fracture Risk")
fit <- glm(Y ~ age + eGFR + Ca + P + TBS + sex,
  data = trainingset, family = "binomial"
)
summary(fit)
autoReg(fit) %>% myft()
# 如果不想在表中显示参考值，可以缩短表。
shorten(autoReg(fit, uni = T, threshold = 1)) %>% myft()
tab2 <- autoReg(fit, uni = TRUE, threshold = 1, final = TRUE) %>% myft()
table2pptx(tab2) # Exported table as Report.pptx
table2docx(tab2)

x <- modelPlot(fit)
x
plot2pptx(print(x)) ## Exported plot as Report.pptx
plot2docx(print(x))
modelPlot(fit, uni = TRUE, threshold = 1, show.ref = FALSE)

# Explore distribution of numeric variables
library(DataExplorer)
library(ggplot2)
plot_bar(dt)
# Plot bar charts by `cut`
plot_bar(dt, by = "Y")
plot_histogram(trainingset, ggtheme = theme_classic())
plot_density(dt)

library(SmartEDA)
ExpCatViz(dt[c(-1, -2, -4)], Page = c(3, 5))
library(tidyverse)
ExpCatViz(
  trainingset %>%
    select(Y, TBS),
  target = "Y"
)

op <- par(mfrow = c(4, 5))
hist(trainingset, col = "lightblue", border = "pink")
utils::str(hist(trainingset, col = "gray", labels = TRUE))
r <- hist(sqrt(trainingset), breaks = 12, col = "lightblue", border = "pink")
text(r$mids, r$density, r$counts, adj = c(.5, -.5), col = "blue3")
sapply(r[2:3], sum)
sum(r$density * diff(r$breaks)) # == 1
lines(r, lty = 3, border = "purple") # -> lines.histogram(*)
par(op)

par(mfrow = c(4, 5))
for (i in 2:21) {
  hist(dt[, i], border = "pink", col = "lightblue", main = "", label = T, xlab = "")
}

# Explore categorical and numeric variables with Box-Plots
library(ggstatsplot)
ggbetweenstats(
  data = trainingset,
  x    = Y,
  y    = TBS,
  type = "np"
)

ExpNumViz(trainingset, target = "Y", Page = c(1, 3))

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
lsocv1 <- cv.glmnet(Xtrain, Ytrain, family = "binomial", nfolds = 100)
lsocv <- cv.glmnet(Xtrain, Ytrain,
  alpha = 1, family = "binomial",
  nfolds = 10, type.measure = "deviance"
)
# family同glmnet函数的family；type.measure用来指定交叉验证选取模型的标准，可取值"default", "mse", "deviance", "class", "auc", "mae", "C"。type.measure的默认值是"deviance"，线性模型是squared-error for gaussian models (type.measure="mse" ), logistic和poisson回归是deviance， Cox模型则是偏似然值（partial-likelihood）。deviance即-2倍的对数似然值，mse是实际值与拟合值的mean squred error，mae即mean absolute error，class是模型分类的错误率(missclassification error)，auc即area under the ROC curve。nfolds表示进行几折验证，默认是10
lsocv ## print(lsocv) ，glmnet模型交叉验证的结果

lsocv$lambda.min
lsocv$lambda.1se

plot(lsocv) # 绘制交叉验证曲线

coef(lsocv, s = "lambda.min") # 获取使模型偏差最小时λ值的模型系数
coef(lsocv, s = "lambda.1se") # 获取使模型偏差最小时λ值+一个标准误时的模型系数
cbind2(coef(lsocv, s = "lambda.min"), coef(lsocv, s = "lambda.1se")) # 合并显示


## 构建响应变量和解释变量矩阵
y <- as.matrix(trainingset[, 1])
x1 <- as.matrix(trainingset[, c("age", "sex", "P", "DM", "Ca", "TBS", "Dialysis_duration")])
# 解释变量全部为连续变量的多因素矩阵
head(x1)

x2 <- model.matrix(~ age + sex + P + DM + Ca + TBS + Dialysis_duration,
  data = trainingset
)[, -1]
# 哑变量编码。构建多因素矩阵，model.matrix(~A+B+…)[,-1] 将为因子编码生成变量0/1哑变量，并保持连续变量不变。最后的[,-1]从 model.matrix 的输出中删除常数项
head(x2)

library(rms)
dd <- datadist(trainingset) ## 设置数据环境
options(datadist = "dd")

# 拟合模型
fit1 <- glm(Y ~ age + P + Ca + TBS + sex + eGFR,
  data = trainingset, family = "binomial"
)
fit2 <- step(fit1)
summary(fit2)
1 / exp(coef(fit2))
# logit P 计算
pred.logit2 <- predict(fit2)
# 预测概率P
P2 <- predict(fit2, type = "response")

m <- NROW(trainingset) / 5
val.prob(P2, trainingset$Y, m = m, cex = 0.8) # 预测概率与真实值进行矫正

fit <- lrm(Y ~ age + P + Ca + TBS + sex + eGFR,
  data = trainingset
)
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
trainingset$sex <- factor(trainingset$sex,
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

# 混淆矩阵绘制
require(caret)
# 训练集预测概率
trainpredprob <- predict(fit1, newdata = trainingset, type = "response")
# 训练集ROC
trainroc <- roc(response = trainingset$Y, predict = trainpredprob)
# 训练集ROC曲线
plot(smooth(trainroc), col = "red")
plot(trainroc, col = "red", legacy.axes = T) ## 更改Y轴格式

plot(trainroc,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grid = T,
  max.auc.polygon = T,
  auc.polygon.col = "skyblue",
  print.thres = T,
  legacy.axes = T,
  bty = "l"
)
# 约登法则，最佳cutoff值
bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities + trainroc$specificities - 1)
]
bestp
# 训练集预测分类
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, 1, 0))
Actual <- factor(trainingset$Y, levels = c(1, 0), labels = c("True", "False"))

# 训练集混淆矩阵
confusionMatrix(
  data = trainpredlab, # 预测类别
  reference = factor(trainingset$Y), # 实际类别
  positive = "1",
  mode = "everything"
)
# 校准度/Calibration curve
formula1 <- as.formula(Y ~ age + P + Ca + TBS + sex + eGFR)
formula2 <- as.formula(Y ~ age + P + Ca + sex + eGFR)
fitcal <- lrm(formula1,
  data = trainingset, x = TRUE, y = TRUE
)
cal <- calibrate(fitcal, method = "boot", B = 1000)
plot(cal,
  subtitles = F,
  # xlim = c(0,1.0),
  xlab = "Nomogram Predicted Fracture",
  ylab = "Actual Fracture", main = "Calibration Curve in Training set"
)
# C-index计算
v <- validate(fitcal, method = "boot", B = 1000, dxy = T)
Dxy <- v[rownames(v) == "Dxy", colnames(v) == "index.corrected"]
orig_Dxy <- v[rownames(v) == "Dxy", colnames(v) == "index.orig"]
bias_corrected_c_index <- abs(Dxy) / 2 + 0.5
orig_c_index <- abs(orig_Dxy) / 2 + 0.5

c <- rcorrcens(Y ~ predict(fitcal), data = trainingset)
lower <- c[1, 1] - 1.96 * c[1, 4] / 2
upper <- c[1, 1] + 1.96 * c[1, 4] / 2
cindex <- rbind(orig_c_index, lower, upper, bias_corrected_c_index)
cindex

# Hosmer-Lemeshow Goodness of Fit(GOF) Test/H-L拟合优度检验
library(ResourceSelection)
model <- glm(formula1,
  data = trainingset, family = binomial(link = "logit")
)
hl <- hoslem.test(model$y, fitted(model), g = 4)
hl

# 内部验证集Internal validation Discrimination/Test set
ddist <- datadist(testingset)
options(datadist = "ddist")
testingset$Y <- as.numeric(as.character(testingset$Y))
pre1 <- predict(fit, newdata = testingset)
plot.roc(testingset$Y, pre1,
  main = "ROC curve in Test set", percent = TRUE,
  print.auc = TRUE,
  ci = TRUE, of = "thresholds",
  thresholds = "best",
  print.thres = "best"
)
rocplot2 <- roc(testingset$Y, pre1)
auc(rocplot2)
ci.auc(rocplot2)

# 混淆矩阵绘制
require(caret)
# 测试集预测概率
testpredprob <- predict(fit1, newdata = testingset, type = "response")
# 测试集ROC
testroc <- roc(response = testingset$Y, predict = testpredprob)
plot(trainroc, col = "red", legacy.axes = T)
plot(testroc, add = TRUE, col = "blue")
legend("bottomright", legend = c("Training set", "Test set"), col = c("red", "blue"), lty = 2)

roc.test(trainroc, testroc)

# 测试集ROC曲线
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
testpredlab <- as.factor(ifelse(testpredprob > bestp, 1, 0))
# 测试集混淆矩阵
confusionMatrix(
  data = testpredlab, # 预测类别
  reference = factor(testingset$Y), # 实际类别
  positive = "1",
  mode = "everything"
)
# Calibration curve
fitcal2 <- lrm(Y ~ age + P + Ca + TBS + sex + eGFR,
  data = testingset, x = TRUE, y = TRUE
)
cal2 <- calibrate(fitcal2, method = "boot", B = 1000)
plot(cal2,
  xlab = "Nomogram Predicted Fracture",
  ylab = "Actual Fracture", main = "Calibration Curve in Test set"
)

# R2/AIC/BIC
R2
AIC(fit1)
BIC(fit1)

# NRI&IDI，模型比较
# 分类变量不处理成factor类型，多分类变量需要处理成哑变量(stats::model.matrix())
stats::model.matrix()
library(nricens)
mnew <- glm(formula1, family = binomial(), data = trainingset, x = T)
mstd <- glm(formula2, family = binomial(), data = trainingset, x = T)
# 截断值的选择
set.seed(123)
nribin(mdl.std = mstd, mdl.new = mnew, cut = c(0.4, 0, 6), niter = 1000, updown = "category")
nribin(mdl.std = mstd, mdl.new = mnew, cut = 0, niter = 1000, updown = "diff")

# 计算p value
z <- abs(0.02222222 / 0.13617120)
pvalue <- (1 - pnorm(z)) * 2
pvalue

library(PredictABEL)
pnew <- mnew$fitted.values
pstd <- mstd$fitted.values
demo <- as.matrix(trainingset)
reclassification(data = trainingset, cOutcome = 1, predrisk1 = pstd, predrisk2 = pnew, cutoff = c(0, 0.2, 0.4, 1))

# DCA&CIC
library(rmda)
full.model <- decision_curve(formula1,
  data = trainingset, family = binomial(link = "logit"),
  thresholds = seq(0, .4, by = .001), confidence.intervals = 0.95,
  bootstraps = 100
) # should use more bootstrap replicates in practice!

# plot the DCA curve
plot_decision_curve(full.model,
  curve.names = c("Full model"), xlim = c(0, 0.4),
  col = c("red"), confidence.intervals = FALSE, # remove confidence intervals
  cost.benefit.axis = T, # remove cost benefit axis
  standardize = TRUE, legend.position = "topright"
)

# plot the clinical impact
plot_clinical_impact(full.model,
  xlim = c(0, .4),
  col = c("black", "blue")
)
plot_clinical_impact(full.model,
  population.size = 1000, cost.benefit.axis = T,
  n.cost.benefits = 8, col = c("red", "blue"),
  confidence.intervals = F
)
# Net benefit
full.model$derived.data[, c("thresholds", "NB", "sNB")]

# Hold-out/K-fold/LOO-CV/Bootstrap模型泛化能力评估
names(getModelInfo())
# 交叉验证LGOCV
train.Control_1 <- trainControl(method = "LGOCV", p = 0.7, number = 50)
set.seed(123)
my_model_1 <- train(formula1, data = trainingset, trControl = train.Control_1, method = "glm")
my_model_1
# 10折交叉验证
train.Control_2 <- trainControl(method = "CV", number = 10)
set.seed(123)
my_model_2 <- train(formula1, data = trainingset, trControl = train.Control_2, method = "glm")
my_model_2
# 留一法LOOCV
train.Control_3 <- trainControl(method = "LOOCV")
set.seed(123)
my_model_3 <- train(formula1, data = trainingset, trControl = train.Control_3, method = "glm")
my_model_3
# Boot
train.Control_4 <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 100)
train.Control_5 <- trainControl(
  method = "repeatedcv", number = 10, repeats = 100,
  classProbs = T, summaryFunction = twoClassSummary
)
set.seed(123)
my_model_4 <- train(formula1, data = trainingset, trControl = train.Control_4, method = "glm")
my_model_4
my_model_5 <- train(formula1, data = dt, trControl = train.Control_5, method = "glm")
as.matrix(my_model_5$results[2])

# Total points
library(nomogramEx)
nom1 <- nomogram(fit,
  fun = function(x) 1 / (1 + exp(-x)),
  lp = T,
  fun.at = c(0.1, 0.3, 0.5, 0.7, 0.9),
  funlabel = "Risk"
)
nomogramEx(nom1)

# 二次验证，最终LR模型，cutoff风险分层，T-test箱线图



# Heatmap of ML-radiomics
heat <- read.csv("/home/wane/Desktop/EP/REFER/BLS/KAI/radiomics-heat.csv")
library(data.table)
transpose(heat)
t(heat)
hea <- as.data.frame(t(heat[-1]))
heatmap(as.matrix(hea))

# Heat map & Radiomics 
library(data.table)
mydata <- read.csv("./TBS/app/data/heat.csv")
data <- mydata[-1]
dt <- transpose(data)
colnames(dt) <- c("RF","ETC","GBC","EGB","KNN","DTC")
rownames(dt) <- c("Acc","AUC","Recall","Prec.","F1","Kappa")

heatmap(as.matrix(mydata[-1]),Colv = NA, symm = F,
        Rowv = NA)
heatmap(as.matrix(dt),symm = F, add.expr,
        Colv = NA,
        Rowv = NA, scale = "column",
        xlab = "Performance",
        ylab = "Classifier",
        main = "Heatmap",
        col = cm.colors(256)) # 颜色  
write.csv(dt,"./TBS/app/data/heat-ML.csv")

