# multiclass.roc
# Examples for a univariate decision value
# https://www.bilibili.com/video/BV1Nm4y1D7N2?spm_id_from=333.880.my_history.page.click&vd_source=23f183f0c5968777e138f31842bde0a0
rm(list = ls())
library(pROC)
dt <- read_excel("/home/wane/Desktop/EP/Structured_Data/Physician.xlsx")
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/Physician1.csv")
table(dt$Label)
str(dt)
# 批量数值转因子
for (i in names(dt)[c(2:5)]) {
  dt[, i] <- as.factor(dt[, i])
}
dt$Label <- as.factor(dt$Label)
# 有序多分类LR
library(MASS)
fit2 <- polr(Label ~ Phy2, data = dt)
summary(fit1)
# 训练集预测概率
pred2 <- predict(fit2, newdata = dt, type = "class")
prob2 <- predict(fit2, newdata = dt, type = "probs")
head(prob1)
Phy2prob <- as.data.frame(prob2)  # Converting list to dataframe in R
dt1 <- dplyr::mutate(dt1,Phy2prob)
names(dt1)[6] <-"Phy1_0"
names(dt1)[7] <-"Phy1_1"
names(dt1)[8] <-"Phy1_2"
write.csv(dt1,"/home/wane/Desktop/EP/Structured_Data/Physician2.csv")

# Basic example
roc1 <- multiclass.roc(dt$Label, prob1[,3])
multiclass.roc(dt$Label, prob1[,2])
Phy1prob <- as.data.frame(roc1[2])
auc(roc1)
plot.roc(roc1$rocs[[1]],col='blue')
plot.roc(roc1$rocs[[2]],add=T,col='red')
plot.roc(roc1$rocs[[3]],add=T,col='yellow')
plot.roc(roc1$rocs[[1]],col="red",print.auc =TRUE,print.auc.col = "darkgreen",auc.polygon = TRUE,auc.polygon.col = "pink")
# 三分类混淆矩阵
library(caret)
cf <- caret::confusionMatrix(as.factor(pred1), as.factor(dt$Label))
cf
# Produces an innocuous warning because one level has no observation
# Select only 3 of the aSAH$gos6 levels:‘
multiclass.roc(dt$Label, prob1[,3])
multiclass.roc(dt$Label, dt$Phy1, levels=c(0, 1, 2))
multiclass.roc(dt$Label, dt$Phy2, levels=c(0, 1, 2))
# Give the result in percent
multiclass.roc(dt$Label, dt$Phy3, percent=TRUE)

# Examples for multivariate decision values (e.g. class probabilities)
# Example with a multinomial log-linear model from nnet
# We use the iris dataset and split into a training and test set
requireNamespace("nnet")
mn.net <- nnet::multinom(Label ~ Phy1, dt)

# Use predict with type="prob" to get class probabilities
predictions <- predict(mn.net, newdata=dt, type="prob")
head(predictions)

# This can be used directly in multiclass.roc:
multiclass.roc(dt$Label, predictions)

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
multiclass.roc(responses, predictor, percent = TRUE, 
               partial.auc = c(100, 90), partial.auc.focus = "se")

# Limit set of levels
multiclass.roc(responses, predictor, levels = c("X1", "X2"))
# Use with formula. Here we need a data.frame to store the responses as characters
data <- cbind(as.data.frame(predictor), "response" = responses)
multiclass.roc(Label ~ Phy1+Phy2, dt)

