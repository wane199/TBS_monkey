# multiclass.roc(), Examples for a univariate decision value
# https://www.bilibili.com/video/BV1Nm4y1D7N2?spm_id_from=333.880.my_history.page.click&vd_source=23f183f0c5968777e138f31842bde0a0
rm(list = ls())
library(pROC) # 三分类
library(readxl)
# dt <- read_excel("/home/wane/Desktop/EP/Structured_Data/Physician.xlsx")
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/Physician1.csv")
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/process_PT-22-lr.csv")
table(dt$Label)
str(dt)
# 批量数值转因子
for (i in names(dt)[c(2:5)]) {
  dt[, i] <- as.factor(dt[, i])
}
dt$Label <- as.factor(dt$Label)
train[16:18] <- lapply(train[16:18], FUN = function(y) {
  as.numeric(y)
}) # int类型转num类型
# 有序多分类LR
library(MASS)
fit2 <- polr(Label ~ lr, data = dt)
summary(fit2)
fit2
fit2$coefficients
multiclass.roc(dt$Label, dt$lr)

# 训练集预测概率
pred2 <- predict(fit2, newdata = dt, type = "class")
prob2 <- predict(fit2, newdata = dt, type = "probs")
head(prob2)
LRprob <- as.data.frame(prob2) # Converting list to dataframe in R
dt <- dplyr::mutate(dt, LRprob)
names(dt)[4] <- "LR1_0"
names(dt)[5] <- "LR1_1"
names(dt)[6] <- "LR1_2"
write.csv(dt, "/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/LR-T1.csv")

# Basic example
roc1 <- multiclass.roc(dt$Label, prob2[, 3])
multiclass.roc(dt$Label, prob2[, 1])
Phy1prob <- as.data.frame(roc1[3])
auc(roc1)
plot.roc(roc1$rocs[[1]], col = "blue", print.auc = TRUE, print.auc.adj = c(0, 1))
plot.roc(roc1$rocs[[2]], add = T, col = "red", print.auc = TRUE, print.auc.adj = c(0, 0))
plot.roc(roc1$rocs[[3]], add = T, col = "brown", print.auc = TRUE, print.auc.adj = c(0, 2))
plot.roc(roc1$rocs[[1]], col = "red", print.auc = TRUE, print.auc.col = "darkgreen", auc.polygon = TRUE, auc.polygon.col = "pink")
# 三分类混淆矩阵
library(caret)
cf <- caret::confusionMatrix(as.factor(pred2), as.factor(dt$Label))
cf
# Produces an innocuous warning because one level has no observation
# Select only 3 of the aSAH$gos6 levels:‘
multiclass.roc(dt$Label, prob1[, 3])
multiclass.roc(dt$Label, dt$Phy1, levels = c(0, 1, 2))
multiclass.roc(dt$Label, dt$Phy2, levels = c(0, 1, 2))
# Give the result in percent
multiclass.roc(dt$Label, dt$Phy3, percent = TRUE)

# 仅限二分类任务
library(reportROC)
reportROC(gold = dt$Label, predictor = dt$lr, plot = T)
reportROC(
  gold = dt$Label, predictor.binary = dt$Phy3,
  plot = T, important = "se", exact = FALSE
)


# Examples for multivariate decision values (e.g. class probabilities)
# Example with a multinomial log-linear model from nnet
# We use the iris dataset and split into a training and test set
requireNamespace("nnet") # 无序多分类
mn.net <- nnet::multinom(Label ~ Phy1, dt)
mn.net
summary(mn.net)$coefficients
exp(summary(mn.net)$coefficients) # 计算OR值
exp(confint(mn.net)) # 计算OR值95%CI
# Use predict with type="prob" to get class probabilities
predictions <- predict(mn.net, newdata = dt, type = "prob")
head(predictions)
# This can be used directly in multiclass.roc:
multiclass.roc(dt$Label, predictions)
multiclass.roc(dt$Label, predictions, direction = ">")
multiclass.roc(dt$Label, predictions,
  percent = TRUE,
  partial.auc = c(100, 90), partial.auc.focus = "se"
)



# Let's see an other example with an artificial dataset
n <- c(100, 80, 150)
responses <- factor(c(rep("X1", n[1]), rep("X2", n[2]), rep("X3", n[3])))
# construct prediction matrix: one column per class

preds <- lapply(n, function(x) runif(x, 0.4, 0.6))
predictor <- as.matrix(data.frame(
  "X1" = c(preds[[1]], runif(n[2] + n[3], 0, 0.7)),
  "X2" = c(runif(n[1], 0.1, 0.4), preds[[2]], runif(n[3], 0.2, 0.8)),
  "X3" = c(runif(n[1] + n[2], 0.3, 0.7), preds[[3]])
))
multiclass.roc(responses, predictor)

# One can change direction , partial.auc, percent, etc:
multiclass.roc(responses, predictor, direction = ">")
multiclass.roc(responses, predictor,
  percent = TRUE,
  partial.auc = c(100, 90), partial.auc.focus = "se"
)

# Limit set of levels
multiclass.roc(responses, predictor, levels = c("X1", "X2"))
# Use with formula. Here we need a data.frame to store the responses as characters
data <- cbind(as.data.frame(predictor), "response" = responses)
multiclass.roc(Label ~ Phy1 + Phy2, dt)

# CBCgrps中文版教程 https://zhuanlan.zhihu.com/p/110238149
library(CBCgrps)
tab2 <- multigrps(dt, gvar = "type")

print(tab2, quote = T)

## 分组柱状图展示多个模型三分类AUROC及AUPRC
# 多模型效能比较，除了ROC还有别的选择吗？
library(ggplot2)
library(dplyr)

auc <- read.csv("/home/wane/Desktop/EP/Structured_Data/AUC.csv")
auc$AUC <- as.numeric(auc$AUC)
fix(auc)
# Grouped
auc %>%
  mutate(Legend = factor(Model, levels = c("B-Sia Net", "Sia Net", "RF", "KNN", "LR", "Physian2", "Physian1"))) %>%
ggplot(aes(fill = Legend, y = reorder(AUC, Image), x = reorder(Image,AUC))) +
  geom_bar(position = "dodge", stat = "identity") + theme_classic() + 
  labs(x = "Images", y = "AUC") + scale_y_continuous(breaks=seq(0, 1.0, 0.05)) 
  geom_text(aes(label = auc$AUC),
    position = position_dodge2(width = 0.9, preserve = "single"),
    vjust = -0.2, hjust = 0.5) + scale_fill_grey(start = 0.0, end = 1.0)





