# multiclass.roc(), Examples for a univariate decision value
# https://www.bilibili.com/video/BV1Nm4y1D7N2?spm_id_from=333.880.my_history.page.click&vd_source=23f183f0c5968777e138f31842bde0a0
rm(list = ls())
library(pROC) # 三分类
library(readxl)
# dt <- read_excel("/home/wane/Desktop/EP/Structured_Data/Physician.xlsx")
dt <- read.csv("/home/wane/Desktop/EP/REFER/BLS/phy.csv")
dt <- read.csv("C:/Users/wane199/Desktop/EP/Structured_Data/LR.csv")
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
multiclass.roc(dt$Label, prob2[, 3])
Phy1prob <- as.data.frame(roc1[2])
auc(roc1)
plot.roc(roc1$rocs[[1]], col = "blue", print.auc = TRUE, print.auc.adj = c(0, 1))
plot.roc(roc1$rocs[[2]], add = T, col = "red", print.auc = TRUE, print.auc.adj = c(0, 0))
plot.roc(roc1$rocs[[3]], add = T, col = "brown", print.auc = TRUE, print.auc.adj = c(0, 2))
plot.roc(roc1$rocs[[1]], col = "red", print.auc = TRUE, print.auc.col = "darkgreen", auc.polygon = TRUE, auc.polygon.col = "pink")

plot.roc(dt$Phy1_0, col = "red", print.auc = TRUE, print.auc.col = "darkgreen", auc.polygon = TRUE, auc.polygon.col = "pink")

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

auc <- read.csv("/home/wane/Desktop/EP/REFER/BLS/ACC.csv")
str(auc)
auc$ACC <- as.numeric(auc$ACC)
# fix(auc)
# Grouped
auc %>%
  mutate(Methods = factor(Model, levels = c("Physian1", "Physian2", "LR", "KNN", "RF", "Sia Net", "B-Sia Net"))) %>%
  ggplot(mapping = aes(x = reorder(Image, Methods), y = ACC, fill = Methods)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.6) +
  coord_cartesian(ylim = c(0.5, 1)) +
  scale_y_continuous(expand = c(0, 0)) + # 消除x轴与绘图区的间隙
  # scale_fill_grey(start = 0.2, end = 1.0) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Images", y = "ACC") +
  theme_classic() +
  geom_text(aes(label = auc$ACC),
    size = 2.2,
    position = position_dodge2(width = 0.75, preserve = "single"),
    vjust = -0.2, hjust = 0.5
  )

# 拼接pdf文件
library(qpdf) 
# Merge multiple PDF files into one
## 一行代码搞定
setwd('/home/wane/Desktop/EP/REFER/BLS/roc/')
pdf_combine(c("rocforleft_mri.pdf","rocforright_mri.pdf"),
            output = "./joined.pdf")

################# -----------------
# [multiROC](https://github.com/WandeRum/multiROC)
require(multiROC)
# 70% training data and 30% testing data
set.seed(123)
total_number <- nrow(dt)
train_idx <- sample(total_number, round(total_number * 0.6))
train_df <- dt[train_idx, ]
test_df <- dt[-train_idx, ]

# Random forest
rf_res <- randomForest::randomForest(Species ~ ., data = train_df, ntree = 100)
rf_pred <- predict(rf_res, test_df, type = "prob")
rf_pred <- data.frame(rf_pred)
colnames(rf_pred) <- paste(colnames(rf_pred), "_pred_RF")

# Multinomial logistic regression
mn_res <- nnet::multinom(Species ~ ., data = train_df)
mn_pred <- predict(mn_res, test_df, type = "prob")
mn_pred <- data.frame(mn_pred)
colnames(mn_pred) <- paste(colnames(mn_pred), "_pred_MN")

# Merge true labels and predicted values
true_label <- dummies::dummy(test_df$Species, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
final_df <- cbind(true_label, rf_pred, mn_pred)

# multiROC and multiPR
roc_res <- multi_roc(final_df, force_diag = T)
pr_res <- multi_pr(final_df, force_diag = T)

Plot
plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

require(ggplot2)
ggplot(plot_roc_df, aes(x = 1 - Specificity, y = Sensitivity)) +
  geom_path(aes(color = Group, linetype = Method), size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
    colour = "grey", linetype = "dotdash"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.justification = c(1, 0), legend.position = c(.95, .05),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = NULL, size = 0.5,
      linetype = "solid", colour = "black"
    )
  )

ggplot(plot_pr_df, aes(x = Recall, y = Precision)) +
  geom_path(aes(color = Group, linetype = Method), size = 1.5) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.justification = c(1, 0), legend.position = c(.95, .05),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = NULL, size = 0.5,
      linetype = "solid", colour = "black"
    )
  )


#### 三分类哑变量处理
library(fastDummies)
true_label <- fastDummies::dummy_cols(dt$Label)
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
final_df <- cbind(true_label, rf_pred, mn_pred)

# 生成随机数转为datafrmae
x2 <- round(runif(244, 0.0, 0.31),3)
x2 <- as.data.frame(x2)
names(x2) <- "Phy"
write.csv(x2,'/home/wane/Desktop/EP/REFER/BLS/phy.csv',row.names = F)
x2[,1]




