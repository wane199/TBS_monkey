# https://www.jianshu.com/p/33425451f1f2TCGA生存模型的构建以及模型预测和评估
# https://zhuanlan.zhihu.com/p/161836228基于Lasso回归筛选变量构建Cox模型并绘制Nomogram
# http://blog.fens.me/r-na-mice/
# https://www.bilibili.com/video/BV1bq4y1P7uN?spm_id_from=333.999.0.0
# https://www.bilibili.com/video/BV1b3411y7bJ?spm_id_from=333.337.search-card.all.click
# https://www.jianshu.com/p/0512bc3e3be9
setwd("/home/wane/Documents/RDocu") ## 设置工作目录
getwd()
rm(list = ls())
list.files() ## 列出工作目录下的文件
library(glmnet) ## Lasso回归、岭回归、弹性网络模型
library(caret) ## 标准化及混淆矩阵
library(survival) ##  生存分析包
library(survminer) # ggforest
library(rms) ## 画列线图
library(randomForest)
library(dplyr)
library(survIDINRI)
library(My.stepwise)

# 读取数据集
# write.csv(dt,"/home/wane/Desktop/EP/结构化数据/TableS1-2.csv", row.names = FALSE)
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/Task2/PT_radiomic_features_temporal_ind2.csv")
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.csv")
dt <- dt[-3]
dt <- as.data.frame(dt)
as.matrix(head(dt))

str(dt) ## 查看每个变量结构
summary(dt)

corrlate <- cor(as.matrix(dt))
corrplot.mixed(corrlate)
dt <- na.omit(dt) # 按行删除缺失值
attach(dt)

set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt[ind == 1, ] # the training data set
# 测试集
test <- dt[ind == 2, ] # the test data set
# 待筛选组学特征标准化(Standardization或Normalization)
dtx <- scale(dt[, c(6:1138)])
# x=scale(x,center=T,scale=T)  #Z-score标准化方法
normal_para <- preProcess(x = train[, 4:1135], method = c("center", "scale")) # 提取训练集的标准化参数
train_normal <- predict(object = normal_para, newdata = train[, 4:1135])
test_normal <- predict(object = normal_para, newdata = test[, 4:1135])
train_normal <- mutate(train[, 1:3], train_normal)
train1 <- transform(train_normal, Group = "Training")
test1 <- transform(test_normal, Group = "Test")
nor <- rbind(train1,test1)
# 列名数组
cols <- colnames(nor)
# 最后一列移到第二列
n_cols <- c(cols[1], cols[length(cols)], cols[2:(length(cols) - 1)])
# 最后一列移到第一列
n_cols <- c(cols[length(cols)], cols[1:(length(cols) - 1)])
# dataframe排序
nor1 <- nor[, n_cols]
write.csv(nor1, "/home/wane/Desktop/EP/Structured_Data/Task2/process_PT_radiomic_features_temporal_ind2.csv", row.names = F)

train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")
# 看一下，不要让临床信息差的太多，输出table1
prop.table(table(train$Follow_up_timemon))
prop.table(table(test$Follow_up_timemon))
prop.table(table(test$Rel._in_5yrs))
prop.table(table(train$Rel._in_5yrs))

## 影像组学导论R语言实现冗余性分析（含代码）
## 影像组学导论 冗余性分析 你懂她嘛?(pearson OR spearman)
# 9.feature selection: reduce redundancy
# 9.1 calculate p of normality test
trainx <- train[5:1136]
norm_result <- apply(trainx, 2, function(x) shapiro.test(x)$p.value)
norm_feature <- trainx[which(norm_result >= 0.05)] # 获取满足正态性特征
# 9.2 calculate r
cor_nor <- cor(norm_feature, method = "pearson") # 针对正态特征使用Pearson
cor_all <- cor(trainx, method = "spearman") # 针对非正态特征使用Spearman
# 9.3 change matrix
num_nor <- dim(cor_nor)[1]
cor_all[1:num_nor, 1:num_nor] <- cor_nor # 将正态部分覆盖所有特征的Spearman结果上
# 9.4 set 0
cor_all[upper.tri(cor_all)] <- 0
diag(cor_all) <- 0 # 上、主对角线值为0
# 9.5 get training_set after reduce redundancy
data_reduce <- trainx[, !apply(cor_all, 2, function(x) any(abs(x) > 0.9))] # 将系数大于0.9的特征删除
# 9.6 check new data
dim(data_reduce) # 初步降维

# lasso process
str(train) # 变量均设置为num数值类型(double)，非factor|character类型
train[16] <- lapply(train[16], FUN = function(y) {
  as.numeric(y)
}) # int类型转num类型
cv_x <- as.matrix(data_reduce) # 数据矩阵化
# cv_y <- train[1:2]
cv_y <- data.matrix(Surv(train$Follow_up_timemon, train$Rel._in_5yrs))
set.seed(123)
# tow pictures for lasso
nocv_lasso <- glmnet(
  x = cv_x, y = cv_y,
  family = "cox", alpha = 1,
)
par(font.lab = 2, mfrow = c(2, 1), mar = c(4.5, 5, 3, 2))
## 设置画图参数: mar 以数值向量表示的边界大小，顺序为“下、左、上、右”，单位为英分*。默认值为c(5, 4, 4, 2) + 0.1 ,mgp 设定标题、坐标轴名称、坐标轴距图形边框的距离。默认值为c(3,1,0)，其中第一个值影响的是标题
## cex.axis 坐标轴刻度放大倍数,cex.main 标题的放大倍数,legend.x，legend.y 图例位置的横坐标和纵坐标,legend.cex 图例文字大小
# layout(matrix(c(1,2,3,3),2,2,byrow=F))

plot(nocv_lasso, xvar = "lambda", las = 1, lwd = 2, xlab = "log(lambda)") # Fig1
abline(v = log(nocv_lasso$lambda.min), lwd = 1, lty = 3, col = "black")

lasso_selection <- cv.glmnet(
  x = cv_x,
  y = cv_y, type.measure = "deviance",
  family = "cox", alpha = 1, nfolds = 1000
) # cross validation
# fitcv1 <- cv.glmnet(x, y, alpha = 1, family = "cox", type.measure = "C")
lasso_selection
plot(x = lasso_selection, las = 1, xlab = "log(lambda)") # Fig2
# 给每一副子图加上序号，tag_level选a，表示用小写字母来标注
library(cowplot)
plot_grid(p1, p2, labels = c("a", "b"))

# lasso回归结果美化
library(tidyverse)
tmp <- as_tibble(as.matrix(coef(nocv_lasso)), rownames = "coef") %>%
  pivot_longer(
    cols = -coef,
    names_to = "variable",
    names_transform = list(variable = parse_number),
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    lambda = lasso_selection$lambda[variable + 1],
    norm = sum(if_else(coef == "(Intercept)", 0, abs(value)))
  )

ggplot(tmp, aes(log(lambda), value, color = coef)) +
  geom_vline(xintercept = log(cvfit$lambda.min), size = 0.8, color = "grey60", alpha = 0.8, linetype = 2) +
  geom_line(size = 1) +
  xlab("Lambda (log scale)") +
  # xlab("L1 norm")+
  ylab("Coefficients") +
  theme_bw(base_rect_size = 2) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    legend.position = "right"
  ) +
  # annotate('text',x = -3.3,y=0.26,label='Optimal Lambda = 0.012',color='black')+
  guides(col = guide_legend(ncol = 1))

ggplot(tmp, aes(norm, value, color = coef, group = coef)) +
  geom_line(size = 1.2) +
  labs(x = "Log Lambda", y = "Coefficients") +
  theme_bw()

library(broom) # 模型统计结果输出
library(textreg)
# 提取数据，就是这么简单！
tidy_df <- broom::tidy(lasso_selection)
tidy_cvdf <- broom::tidy(fitcv1)
head(tidy_df)
tidy_df
head(tidy_cvdf)
library(ggplot2)
library(RColorBrewer)
mypalette <- c(brewer.pal(11, "BrBG"), brewer.pal(11, "Spectral"), brewer.pal(5, "Accent"))

ggplot(tidy_df, aes(lambda, estimate, group = nzero, color = nzero)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0) +
  ylab("Coefficients") +
  scale_color_manual(name = "variable", values = mypalette) +
  theme_bw()
p2 <- ggplot(tidy_df, aes(lambda, estimate, group = term, color = term)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0) +
  scale_x_log10(name = "Log Lambda") +
  ylab("Coefficients") +
  scale_color_manual(name = "variable", values = mypalette) +
  theme_bw()

## 准备数据, 10折交叉验证的图
cvfit <- lasso_selection
xx <- data.frame(
  lambda = cvfit[["lambda"]], cvm = cvfit[["cvm"]], cvsd = cvfit[["cvsd"]],
  cvup = cvfit[["cvup"]], cvlo = cvfit[["cvlo"]], nozezo = cvfit[["nzero"]]
)
xx$ll <- log(xx$lambda)
xx$NZERO <- paste0(xx$nozezo, " vars")
ggplot(xx, aes(ll, cvm, color = NZERO)) +
  geom_errorbar(aes(x = ll, ymin = cvlo, ymax = cvup), width = 0.05, size = 1) +
  geom_vline(xintercept = xx$ll[which.min(xx$cvm)], size = 0.8, color = "grey60", alpha = 0.8, linetype = 2) +
  geom_point(size = 2) +
  xlab("Log Lambda") +
  ylab("Partial Likelihood Deviance") +
  theme_bw(base_rect_size = 1.5) +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    legend.position = "bottom"
  ) +
  annotate("text", x = -5.3, y = 12.4, label = "Optimal Lambda = 0.008", color = "black") +
  guides(col = guide_legend(ncol = 3))

# get coefficient and waterfalls plot,正负显示的条形图
# https://zhuanlan.zhihu.com/p/431060791
coefPara <- coef(object = lasso_selection, s = "lambda.min")
lasso_values <- as.data.frame(which(coefPara != 0, arr.ind = T))
lasso_names <- rownames(lasso_selection[-1])
lasso_coef <- data.frame(
  Feature = rownames(lasso_values),
  Coef = round(coefPara[which(coefPara != 0, arr.ind = T)], 4)
)
lasso_coef
# lasso_coef %>%
#   mutate_if(is.numeric, ~ scales::number(., accuracy = 0.0001))
# https://cloud.tencent.com/developer/article/1089081
ggplot(lasso_coef, aes(x = reorder(Feature, Coef), y = Coef, fill = Coef)) +
  xlab("") +
  ylab("Coefficients") +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", width = 0.78, size = 0.25, position = position_dodge(0.7)) +
  # ylim(-0.30, 0.20) +
  geom_text(aes(label = Coef), vjust = -0.2) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.title.x = element_text(face = "bold")) +
  theme(axis.text.y = element_blank()) + # hjust=1调整横轴距离
  geom_text(aes(y = ifelse(Coef > 0, -0.01, 0.01), label = Feature, fontface = 4, hjust = ifelse(Coef > 0, 1, 0))) +
  scale_fill_gradient2(low = "#366488", high = "red", mid = "white", midpoint = 0)
write.csv(lasso_coef, file = "/home/wane/Desktop/EP/Structured_Data/Task2/coef.minPTcox9.csv", quote = T, row.names = F)
# coef <- read_csv("/media/wane/wade/EP/EPTLE_PET/TLE_pet_ind/coef.minPTcox9.csv", show_col_types = FALSE)
# 提取特征名
var <- unlist(lasso_coef[, 1])
# vars <- paste0(coef[,1],collapse = '+')
var
# Radiomics Score
# information preparation, 将Lasso降维后的训练集、验证集进行整理
train_lasso <- data.frame(cv_x)[var]
test_lasso <- test[names(train_lasso)]
Data_all <- as.matrix(rbind(train_lasso, test_lasso))
xn <- nrow(Data_all)
yn <- ncol(Data_all)
# get beta and calculate，系数矩阵化，进行矩阵运算
beta <- as.matrix(coefPara[which(coefPara != 0), ]) # get beta = Coefficients
beta
betai_matrix <- as.matrix(beta) # get beta_i
beta0_matrix <- matrix(beta[1], xn, 1) # get beta_0
Radscore_Matrix <- Data_all %*% betai_matrix + beta0_matrix # get Rad-score
radscore_all <- as.numeric(Radscore_Matrix)

# get radiomics score
Radscore_train <- radscore_all[1:nrow(train_lasso)]
Radscore_test <- radscore_all[(nrow(train_lasso) + 1):xn]

# show Rad-score
Rad_train <- as.data.frame(Radscore_train)
Rad_test <- as.data.frame(Radscore_test)
train1 <- mutate(Rad_train, train)
test1 <- mutate(Rad_test, test)
train1 %>%rename(radscore = Radscore_train) -> train1
test1 %>%rename(radscore = Radscore_test) -> test1
rad <- rbind(train1,test1)
write.csv(rad, file = "/home/wane/Desktop/EP/Structured_Data/Task2/process_ind2_rad.csv", quote = T, row.names = F)

## Lasso筛选变量后进一步逐步回归筛选(训练集)stepwise
ddist <- datadist(train)
options(datadist = "ddist")
vars <- c(
  "log.sigma.5.0.mm.3D_glcm_ClusterShade", "original_glszm_GrayLevelNonUniformity", "wavelet.HHH_glszm_ZoneEntropy",
  "wavelet.LHL_firstorder_RootMeanSquared", "wavelet.HHL_glszm_SizeZoneNonUniformityNormalized", "wavelet.LHL_firstorder_Median",
  "log.sigma.5.0.mm.3D_glszm_SmallAreaLowGrayLevelEmphasis", "wavelet.LHL_glcm_Imc2"
)
var <- unlist(lasso_coef[, 1])
fe <- dt %>%
  select(var)
dts <- mutate(dt[, 1:3], fe)
write_csv(dts, "/media/wane/wade/EP/EPTLE_PET/TLE_pet_ind/process_PT9.csv")

# options(scipen = 100)
My.stepwise.coxph(
  Time = "Follow_up_timemon",
  Status = "Rel._in_5yrs",
  variable.list = var,
  data = train
)

# 计算radscore并z-score标准化，添加至临床资料表
dt <- dt %>%
  as_tibble(dt) %>%
  mutate(radscore = 0.1675 * log.sigma.5.0.mm.3D_glcm_ClusterShade
    - 0.0648 * log.sigma.5.0.mm.3D_glszm_SmallAreaLowGrayLevelEmphasis
    - 0.0012 * wavelet.LLH_gldm_LowGrayLevelEmphasis
    + 0.0503 * wavelet.LHL_firstorder_Median
    + 0.048 * wavelet.LHL_firstorder_RootMeanSquared
    - 0.2359 * wavelet.LHL_glcm_Imc2
    + 0.0407 * wavelet.LHL_gldm_DependenceNonUniformity
    + 0.0662 * wavelet.HHH_glszm_ZoneEntropy
    - 0.0841 * wavelet.LLL_glcm_Imc2)
radscore <- as.data.frame(dt["radscore"])

### 临床资料汇总、最佳cutoff值及标准化
dt <- read.csv("/media/wane/wade/EP/EPTLE_PET/TLE234-rad.csv")

# 字符型变量转化为数值型
# datexpr2=as.data.frame(lapply(datexpr,as.numeric))
dt$side <- as.numeric(as.character(dt$side))
dt$Freq.permon <- as.numeric(as.character(dt$Freq.permon))
# 用for循环语句将数值型变量转为因子变量
for (i in names(dt)[c(12:22)]) {
  dt[, i] <- as.factor(dt[, i])
}
# 批量分类变量转化为因子
dt$Rel._in_5yrs <- as.numeric(as.character(dt$Rel._in_5yrs))
vars <- c("Sex", "SE")
dt <- dt %>% mutate(across(one_of(vars), as.factor))
dt$Rel._in_5yrs <- as.factor(dt$Rel._in_5yrs)

# 拆分数据集，临床资料单因素及多因素Cox回归筛选
dt$Rel._in_5yrs <- ifelse(dt$Rel._in_5yrs == "0", "Seizure-free", "Relapse")
colnames(dt)[6] <- "Seizure Outcome"

dt1 <- read.csv("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.csv")
str(dt1)
for (i in names(dt1)[c(6,8,9,13:22)]) {
  dt1[, i] <- as.factor(dt1[, i])
}
train <- subset(dt1, dt1$Group == "Training")
test <- subset(dt1, dt1$Group == "Test")
# Train vs. Test set
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)
library(patchwork)
p1 <- ggbetweenstats(
  data = train,
  x = Rel._in_5yrs,
  y = radscore,
  xlab = "",
  ylab = "Radiomics Score",
  plot.type = "violin",
  package = "ggsci",
  palette = "uniform_startrek"
) +
  ggtitle("Training Set") +
  ggeasy::easy_center_title()
p2 <- ggbetweenstats(
  data = test,
  x = Rel._in_5yrs,
  y = radscore,
  ylab = "", xlab = "",
  plot.type = "violin",
  title = "Test Set",
  package = "ggsci",
  palette = "uniform_startrek"
) +
  ggtitle("Test Set") +
  ggeasy::easy_center_title()

library(ggpubr)
library(ggsci)
# 根据变量分面绘制箱线图
dt$Group = factor(dt$Group,levels = c('Training','Test')) # 调整分面顺序
p <- ggboxplot(dt,
  x = "Seizure Outcome", y = "radscore", ylab = "Radiomics Score",
  color = "Seizure Outcome", palette = "jco",
  add = "jitter", xlab = "", legend = "right",
  facet.by = "Group", short.panel.labs = FALSE
) + labs(color = "Seizure Outcome", shape = "Seizure Outcome")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", method = "t.test")
# Or use significance symbol as label
p + stat_compare_means(label = "p.signif", label.x = 1.5)

p1 <- ggboxplot(train,
  x = "Rel._in_5yrs",
  y = "radscore", xlab = "", ylab = "Radiomics Score",
  color = "Rel._in_5yrs",
  palette = c("#00AFBB", "#E7B800"),
  add = "jitter",
  shape = "Rel._in_5yrs"
) +
  # stat_compare_means(method = "wilcox.test") +
  ggtitle("Training Set") + labs(color = "Seizure Outcome", shape = "Seizure Outcome") +
  ggeasy::easy_center_title()
p2 <- ggboxplot(test,
  x = "Rel._in_5yrs",
  y = "radscore", ylab = "", xlab = "",
  color = "Rel._in_5yrs",
  palette = c("#00AFBB", "#E7B800"),
  add = "jitter",
  shape = "Rel._in_5yrs"
) +
  # stat_compare_means(method = "wilcox.test") +
  ggtitle("Test Set") + labs(color = "Seizure Outcome", shape = "Seizure Outcome") +
  ggeasy::easy_center_title()
ggarrange(p1, p2,
  ncol = 2, labels = c("a", "b"),
  common.legend = TRUE, legend = "right"
)
# tableone
library(CBCgrps)
tab1 <- twogrps(dt[c(-1,-2,-4)], gvar = "Group", skewvar = c("radscore"))
print(tab1, quote = T)
write.csv(tab1[1], "/home/wane/Desktop/EP/Structured_Data/Task2/traintesttable1.csv", row.names = F)
# 基线资料汇总，tableone
library(tableone)
## 需要转为分类变量的变量
catVars <- c("Sex")
paste0(unlist(names(train)),collapse = "+")
## Create a TableOne object
tab <- CreateTableOne(data = dt, strata = "Group", factorVars = catVars, addOverall = TRUE)
print(tab, showAllLevels = TRUE)
print(CreateTableOne(data = testset), showAllLevels = TRUE)
tabMat <- print(tab, staquote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE)
## 保存为 CSV 格式文件
write.csv(tabMat, file = "/media/wane/Data/CN/t1myTable.csv")

# 最佳cutoff值
library(cutoff)
str(train)
train$Rel._in_5yrs <- as.numeric(as.character(train$Rel._in_5yrs))
train$Rel._in_5yrs <- as.factor(train$Rel._in_5yrs)

logresult <- cutoff::logrank(
  data = train, # 数据集
  time = "Follow_up_timemon", # 生存时间
  y = "Rel._in_5yrs", # 生存状态
  x = "radscore", # 连续自变量
  cut.numb = 1, # 截断值选择1个分界点
  n.per = 0.10, # 分组后自变量组最少比例
  y.per = 0.10, # 分组后因变量组最少比例
  p.cut = 0.005, # 检验水准，结果只显示pvalue小于0.05的截断值
  round = 3
) # 保留5位有效小数
logresult
ggline(logresult,
  x = "cut1", y = "pvalue", color = "blue", axis.title.x = 0.01,
  palette = "jco", xlab = "截断值取值", ylab = "p value"
)

res.cut <- surv_cutpoint(
  data = train, time = "Follow_up_timemon",
  event = "Rel._in_5yrs", variables = c("radscore")
)
summary(res.cut) # 最佳截断值为0.528+-5.898
plot(res.cut, "radscore", palette = "npg")
res.cat <- surv_categorize(res.cut)
fit <- survfit(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore, data = res.cat)
summary(fit)
survdiff(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore, data = res.cat)

ggsurvplot(fit,
  data = res.cat,
  risk.table = TRUE, conf.int = TRUE,
  surv.median.line = "hv", # 同时显示垂直和水平参考线
  pval = T, xlab = "months", ylab = "Free of Relapse(%)"
)

ggsurvplot(fit,
  data = train, risk.table = TRUE, conf.int = TRUE,
  pval = T, xlab = "Time in months", ylab = "Free of Relapse",
  palette = c("#E7B800", "#2E9FDF"), braek.time.by = 12, ggtheme = theme_classic(), risk.table.y.text.col = T,
  risk.table.y.text = F, risk.table.height = 0.25,
  ncensor.plot = T, ncencer.plot.height = 0.25, # conf.int.style="step",
  surv.median.line = "hv", legend.labs = c("Radscore-low", "Radscore-high")
)
# 绘制累积风险曲线
ggsurvplot(fit,
  plaette = "#2E9FDF", data = train,
  fun = "cumhaz", # 绘制累积风险曲线
  conf.int = T, pval = T,
  palette = c("#E7B800", "#2E9FDF"),
  legend.labs = c("Radscore-low", "Radscore-high"),
  xlab = "Time in months", ylab = "Cum Relapse"
)

# radscore及测试集最佳截断值添加至临床资料总表
write.csv(res.cat, "/media/wane/wade/EP/EPTLE_PET/res.cat.csv", row.names = FALSE)
res.cat <- read.csv("/media/wane/wade/EP/EPTLE_PET/res.cat.csv")
res.cat <- as.data.frame(res.cat)

# 采用cut函数
dt$rad <- cut(dt$radscore, breaks = c(-Inf, 0.3346, Inf), labels = c("low", "high"), right = FALSE)
write.csv(dt, "/media/wane/wade/EP/EPTLE_PET/TLE234-rad.csv", row.names = FALSE)

dt$Rad[dt$rad == "low"] <- "0"
dt$Rad[dt$rad == "high"] <- "1"
dtx <- scale(dt[, c(3:16, 18)]) # z标准化
dtx <- as.data.frame(dtx)
head(dt[, 1:2])
dt <- mutate(dt[, 1:2], dtx)
write.csv(dt, "/media/wane/wade/EP/EPTLE_PET/process_TLE234-rad.csv", row.names = F)
dt <- read.csv("/media/wane/wade/EP/EPTLE_PET/TLE234-rad.csv")
dt <- read.csv("./EP/EP_Cox_Nomo/TLE234-rad.csv")

create_report(dt)
# 对数据初步预处理(批量单因素分析变量保留数值型变量)
# 用for循环语句将数值型变量转为因子变量
for (i in names(train)[c(-1:-6, -7, -10:-12)]) {
  train[, i] <- as.factor(train[, i])
}

ddist <- datadist(train) # 数据打包
options(datadist = "ddist")

# 输出单因素和多因素结果
library(finalfit)
str(train)
train['Follow_up_timemon'] <- lapply(train['Follow_up_timemon'], FUN = function(y) {
  as.numeric(y)
}) # int类型转num类型

paste0(colnames(dt[c(-1, -2)]), collapse = "\",\"")
# Crosstable
explanatory <- unlist(colnames(train)[c(-1:-6)])
# explanatory <- c(
#   "side", "Sex", "Surgmon", "Onsetmon", "Durmon", "SE", "SGS",
#   "early_brain_injury", "familial_epilepsy", "brain_hypoxia", "Central_Nervous_System_Infections", "traumatic_brain_injury", "history_of_previous_surgery", "MRI", "radscore"
# )
dependent <- 'Rel._in_5yrs'
train %>%
  summary_factorlist(dependent, explanatory,
    cont = "mean", # 连续性定量变量计算均数(标准差)，cont="median"计算中位数（四分位数间距）
    cont_nonpara = 1, # 指定变量序号，采用非参数检验和统计指标
    cont_range = T, # 四分位数间距用P25，P75表示
    column = T, # 按列统计，column=F则按行统计
    total_col = T, # 添加列的合计结果
    p_cont_para = "aov", # 指定定量变量的参数统计方法，包括"aov"和"t.test"，非参数方法统一为kruskal.test
    p_cat = "chisq", # 指定分类变量的统计方法，包括"chisq" or "fisher"
    digits = c(1, 1, 3, 2), # 依次设定小数位数：（1）平均值/中位数（2）标准差/四分位数范围（3）P值（4）数百分比。
    p = TRUE
  ) -> t1
knitr::kable(t1, align = c("l", "l", "r", "r", "r"), "simple")
write.csv(t1, "/media/wane/wade/EP/EPTLE_PET/final_train.csv", row.names = F)

# 指定自变量
explanatory <- unlist(colnames(train)[c(-1:-6)])
# 指定因变量
train$Rel._in_5yrs <- as.numeric(train$Rel._in_5yrs) # 拟合cox回归需要转为数值变量
dependent <- "Surv(Follow_up_timemon, Rel._in_5yrs)"
# 拟合和输出结果
train %>%
  finalfit(dependent, explanatory,
    metrics = T, # metrics=T表示输出模型检验的指标
    add_dependent_label = F
  ) -> t2 # add_dependent_label=F表示不在表的左上角添加因变量标签。
knitr::kable(t2, "simple")
write.csv(t2, "/home/wane/Desktop/EP/Structured_Data/Task2/finalfit.csv", row.names = F)

# hr_plot()生成Cox比例风险模型的风险比表和图, or_plot用于从glm()或lme4::glmer()模型中生成一个OR值表和图。
# https://mp.weixin.qq.com/s?__biz=MzIzMzc1ODc4OA==&mid=2247485564&idx=1&sn=db5b0e544afa6f09e89d8c4606e21659&chksm=e8818157dff60841de4e60cc25785defca31241f2342d6b6a1b705151b6cbe7e023bfbd44aaa&mpshare=1&scene=1&srcid=0205aGisjDzv3Rrn1Raq1gUL&sharer_sharetime=1660926034730&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
explanatory <- unlist(colnames(dt)[c(-1:-6)])
train %>%
  hr_plot(dependent, explanatory)

# 整体数据集的生存曲线
# https://cloud.tencent.com/developer/article/1966729
fit0 <- survfit(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ 1, data = train)
ggsurvplot(fit0,
  plaette = "#2E9FDF", data = train, risk.table = TRUE, surv.median.line = "hv",
  pval = T, xlab = "Time in months", ylab = "Free of Relapse"
)

# 绘制累积风险曲线
ggsurvplot(fit0,
  plaette = "#2E9FDF", data = train, risk.table = TRUE,
  fun = "cumhaz", # 定义生存曲线变换的任意函数，"event"累积事件f(y) = 1-y, "cumhaz"累积风险函数f(y) = -log(y),"pct"生存概率(百分比)。
  conf.int = T, # legend.labs = c("Radscore-low", "Radscore-high"),
  xlab = "Time in months", ylab = "Cum Relapse"
)

fit1 <- survfit(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ Sex, data = train)
ggsurvplot(fit1,
  plaette = "#2E9FDF", risk.table = TRUE, conf.int = TRUE, surv.median.line = "hv",
  data = train, pval = T, xlab = "Time in months", ylab = "Free of Relapse"
)

# 批量完成单因素Cox回归分析，变量筛选，多种methods
library(ezcox)
str(train)
ddist <- datadist(train)
options(datadist = "ddist")
paste0(colnames(dt)[3:17], collapse = '","')
var <- unlist(colnames(train)[7:22])

results <- ezcox(train,
  time = "Follow_up_timemon", status = "Rel._in_5yrs",
  covariates = var
)
results
knitr::kable(results, "latex")
write.csv(results, "/home/wane/Desktop/EP/Structured_Data/Task2/results.csv", row.names = FALSE)

show_forest(train,
  time = "Follow_up_timemon", status = "Rel._in_5yrs",
  covariates = unlist(colnames(train)[c(-1:-4)]), controls = "radscore"
)

# Stepwise筛选变量
# options(scipen = 100, digits=5)
library(MASS) # 逐步回归，stepAIC()函数 https://www.bilibili.com/video/BV18F411q7v3/?vd_source=23f183f0c5968777e138f31842bde0a0

library(My.stepwise)
My.stepwise.coxph(
  Time = "Follow_up_timemon",
  Status = "Rel._in_5yrs",
  variable.list = var,
  data = train
)

library(StepReg)
Surv <- formula("Surv(Follow_up_timemon,Rel._in_5yrs==1) ~ .")
stepwiseCox(Surv,
  data = train,
  include = NULL,
  selection = c("bidirection"),
  select = "HQ",
  method = c("efron"),
  sle = 0.15,
  sls = 0.15,
  weights = NULL,
  best = NULL
)

# 用for循环语句将数值型变量转为因子变量
for (i in names(train)[c(1, 21, 7:18)]) {
  train[, i] <- as.factor(train[, i])
}
str(train)
train$rad <- cut(train$radscore, breaks = c(-Inf, 0.3346, Inf), labels = c("0", "1"), right = FALSE)
train$rad <- factor(train$rad,
  levels = c(0, 1),
  labels = c("Low", "High")
)
# 拟合cox回归
# coxm1 <- cph(Surv(Follow_up_timemon,Rel._in_5yrs==1) ~ Radscore, x=T,y=T,data=train,surv=T)
coxm0 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ SGS + familial_epilepsy + Durmon + SE + Surgmon , data = test)
coxm1 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore, data = test)
coxm2 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE , data = test)

cox.zph(coxm2) # 等比例风险假定
print(coxm2)
summary(coxm2)
## These models are significantly different by likelihood ratio test
anova(coxm0, coxm1, coxm2, test = "LRT")
summary(coxm2)$concordance # 未校准的时间C-index

## Put linear predictors ("lp") into EP train dataset
test$lp.clinic <- predict(coxm0, type = "lp")
test$lp.rad <- predict(coxm1, type = "lp")
test$lp.rad_clinic <- predict(coxm2, type = "lp")

library(Hmisc)
## Model with clinic(Hmisc::rcorrcens)
rcorrcens(formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train)

model1 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train)
print(model1, data = train)

library(eoffice)
p <- ggforest(model1, data = train) # https://cache.one/read/16896085
topptx(figure = p, filename = "./EP/EP_Cox_Nomo/forest.pptx")

### 开始cox-nomo graph
# 设置因子的水平标签(常见列线图的绘制及自定义美化详细教程)
train$SGS <- factor(train$SGS,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
train$SE <- factor(train$SE,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
train$familial_epilepsy <- factor(train$familial_epilepsy,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
# 设置生存函数
model <- cph(
  formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  surv = T, data = train
)
surv <- Survival(model) # 建立生存函数
surv1 <- function(x) surv(1 * 12, lp = x) # 定义time.inc,1年OS
surv3 <- function(x) surv(1 * 36, lp = x) # 定义time.inc,3年OS
surv5 <- function(x) surv(1 * 60, lp = x) # 定义time.inc,5年OS
plot(nomogram(model,
  fun = list(surv1, surv3, surv5),
  lp = F, # naxes=13,
  # force.label=F,
  # col.grid=c("Tomato2","DodgerBlue"),
  funlabel = c("1-Year Relapse", "3-Year Relapse", "5-Year Relapse"),
  maxscale = 100,
  fun.at = c("0.90", "0.85", "0.80", "0.70", "0.60", "0.50", "0.40", "0.30", "0.20", "0.10")
),
xfrac = .45
)
# 画上参考线
plot(nomogram(model,
  fun = list(surv1, surv3, surv5),
  funlabel = c("1-Year Relapse", "3-Year Relapse", "5-Year Relapse"),
  maxscale = 100, lp = F,
  fun.at = c("0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1")
),
col.grid = c("pink", "cyan")
)
# 常见列线图的绘制及自定义美化详细教程 https://mp.weixin.qq.com/s?__biz=MzU4OTc0OTg2MA==&mid=2247497910&idx=1&sn=350a4d6c689462d7337e04455912c8ce&chksm=fdca73bdcabdfaab401fab2a00a9a24a60e7e706675b774bd3d866f6c884cfc80c7a478e7403&mpshare=1&scene=1&srcid=0607mDKXD346ABXXHRc5Sn6I&sharer_sharetime=1660988218815&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# maxscale 参数指定最高分数，一般设置为100或者10分
# fun.at 设置生存率的刻度
# xfrac 设置数值轴与最左边标签的距离，可以调节下数值观察下图片变化情况

library(regplot)
regplot(model1,
  observation = train[6, ], # 指定某一患者，4即是选择数据集中第四位患者
  interval = "confidence", title = "Nomogram",
  plots = c("violin", "boxes"), clickable = T,
  failtime = c(12, 36, 60)
) # 设置随访时间1年、3年和5年

regplot(model1,
  observation = train[2, ],
  failtime = c(12, 36, 60), prfail = TRUE, droplines = T
)

library(VRPM)
fit <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  data = train,
  model = TRUE
)
colplot(fit)
colplot(fit,
  coloroptions = 2, # 颜色方案(1-5)
  time = 60, # 设置随访时间为8年
  risklabel = "Probability of Relapse",
  filename = "Color Nomogram_5-Yr"
)

# 制作在线交互式动态列线图
# https://www.bilibili.com/video/BV1Jb4y1h7iw/?spm_id_from=pageDriver
library(DynNom)
library(shiny)
library(plotly)
library(compare)
library(stargazer)

DynNom(fit, train)
# covariate = c("slider", "numeric")
# 设置参数covariate = "numeric"，可以将动态列线图中变量的调整方式从滑块改为输入
# 生成本地DynNomapp脚本文件
DNbuilder(fit) ## 生成下图文件于工作目录处

library(shinyPredict)
train$Rel._in_5yrs <- factor(train$Rel._in_5yrs)

mod1 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ SGS + familial_epilepsy + Durmon + SE,
  data = train, model = FALSE, y = FALSE
)
mod2 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  data = train, model = FALSE, y = FALSE
)
shinyPredict(
  models = list("model" = mod1, mod2),
  path = "./EP/EP_Cox_Nomo/shinyPredict/", # 需更改为自己的工作路经
  data = train[c("Follow_up_timemon", "Rel._in_5yrs", "radscore", "SGS", "familial_epilepsy", "Durmon", "SE")],
  title = "Predicting TLE ralapse probability",
  shinytheme = "journal"
)

## 模型区分度对比和验证
# Concordance index(未校准的时间C-index)
f0 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore, data = train)
f01 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  x = T, data = train
)
print(f01)
sum.surv <- summary(f01)
print(sum.surv)
c_index <- sum.surv$concordance
c_index

# C-index计算及ROC曲线绘制(校准后的时间C-index)
require(pec) # 计算时间C-index验证模型
t <- c(1 * 12, 3 * 12, 5 * 12) # 设置预测生存概率的时间点，根据模型预测患者1年，3年和5年的生存概率。
survprob <- predictSurvProb(f01, newd = test, times = t)
head(survprob)
as.matrix(head(train))

cli <- cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ SGS + familial_epilepsy + Durmon + SE,
  x = T, y = T, surv = T, data = train, time.inc = 60
)
full <- cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  x = T, y = T, surv = T, data = train, time.inc = 60
)
c_index <- cindex(list("Clinic" = cli, "Rad-clinic" = full),
  formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ .,
  data = train,
  eval.times = seq(12, 5 * 12, 6)
)
## 设置画图参数: mar以数值向量表示的边界大小，顺序为“下、左、上、右”，单位为英分*。默认值为c(5, 4, 4, 2) + 0.1 ,mgp 设定标题、坐标轴名称、坐标轴距图形边框的距离。默认值为c(3,1,0)，其中第一个值影响的是标题
## cex.axis 坐标轴刻度放大倍数,cex.main 标题的放大倍数,legend.x，legend.y 图例位置的横坐标和纵坐标,legend.cex 图例文字大小
par(mgp = c(3.1, 0.8, 0), mar = c(5, 5, 3, 1), cex.axis = 0.8, cex.main = 0.8, las = 1)
plot(c_index, xlim = c(0, 60), legend.x = 1, legend.y = 1, legend.cex = 0.8)
## splitMethod 拆分方法 ="bootcv"表示采用重抽样方法, B表示重抽样次数
c_index1 <- cindex(list("Clinic" = cli, "Rad-clinic" = full),
  formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ .,
  data = train,
  eval.times = seq(0, 5 * 12, 6),
  splitMethod = "bootcv",
  B = 1000
)
plot(c_index1, xlim = c(0, 60), legend.x = 1, legend.y = 1, legend.cex = 0.8)
set.seed(123)
c_index1 <- cindex(list("Clinic" = cli, "Rad-clinic" = full),
  # formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ .,
  data = train,
  eval.times = seq(0, 60, 12), # 各时间点的c-index
  cens.model = "cox", # 指定截尾数据的逆概率加权方法
  keep.pvalues = T,
  confInt = T,
  confLevel = 0.95,
  splitMethod = "bootcv", # 重抽样行交叉验证
  B = 1000
)
c_index1
plot(c_index1,
  xlim = c(0, 60), legend.x = 1, legend.y = 1,
  legend.cex = 0.8
)

# 绘制Time-dependent ROC curve, Assessment of Discrimination in Survival Analysis (C-statistics, etc), https://rpubs.com/kaz_yos/survival-auc
library(survivalROC)
## Put linear predictors ("lp") into pbc dataset
train$lp.Radscore_clinic <- predict(full, type = "lp")
## Define a function
fun.survivalROC <- function(lp, t) {
  res <- with(
    test,
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

## Model with Radscore_clinc/Coxph/cph??
res.survivalROC.Radscore_clinc <- lapply(1:6 * 12, function(t) {
  fun.survivalROC(lp = "lp.Radscore_clinic", t)
})
res.survivalROC.Radscore_clinc <- lapply(1:6 * 12, function(t) {
  fun.survivalROC(lp = "lp.rad_clinic", t)
})

## Model with Radscore
res.survivalROC.Radscore <- lapply(1:6 * 12, function(t) {
  fun.survivalROC(lp = "lp.rad", t)
})

## Model with clinic
res.survivalROC.clinic <- lapply(1:6 * 12, function(t) {
  fun.survivalROC(lp = "lp.clinic", t)
})

# 多个多因素模型ROC曲线的比较及多个时间AUC曲线
library(riskRegression)
str(train)
train$Follow_up_timemon <- as.numeric(as.character(train$Follow_up_timemon))
# 拟合cox回归
f1 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
f2 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore, data = train, x = T)
f3 <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
### 例如评估两年的ROC及AUC值
model <- Score(list("Cox(radscore + SGS + familial_epilepsy + Durmon + SE)" = f3),
  formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ 1,
  data = train,
  times = 12,
  plots = "roc",
  metrics = "auc"
)
# 3.画图
plotROC(model,
  xlab = "1-Specificity",
  ylab = "Sensitivity",
  lty = 1, # 线型，2=虚线
  cex = 1.1, # 字体大小
  pch = 2, # 文字格式
  lwd = 2, # 线粗
  col = "red",
  legend = "Radscore_clinc model"
)

# 2.3个模型同时进行ROC评估，
pk1 <- Score(list(
  model.clinic = f1,
  model.rad = f2,
  model.rad.clinic = f3
),
Hist(Follow_up_timemon, Rel._in_5yrs == 1) ~ 1,
data = train,
times = 12, # 比较三者5年ROC
plots = "roc",
metrics = "auc"
)
# 3.画图
plotROC(pk1,
  xlab = "1-Specificity",
  ylab = "Sensitivity",
  lty = 1, # 线型
  cex = 1, # 字体大小
  pch = 2, # 文字格式
  lwd = 2, # 线粗
  col = c("red", "blue", "darkgreen"),
  legend = c("clinc model", "Radscore model", "Radscore_clinc model")
)
# 时间AUC
pk <- Score(list(
  model1 = f1,
  model2 = f2,
  model3 = f3
),
formula = Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ 1,
data = train,
metrics = "auc",
null.model = F,
times = seq(1, 36, 6)
)
# 2. 画图+展示每个模型每个时间点的auc值
auc <- plotAUC(pk)
auc
# 3.美化，加载两个包
library(ggplot2)
library(ggprism)

# 设置配色
cols <- c("red", "blue", "darkgreen")
# 画图，注意这里的数据来源是刚刚做好的auc数据
p <- ggplot(data = auc, aes(x = times, y = AUC, color = model)) + # 指定数据集，x，y轴来源颜色分组
  geom_line(size = 1.0) + # 指定图形为线条图
  theme_classic() + # 使用经典主题
  theme_prism(base_size = 15) + # 使用prism主题
  theme(legend.position = "top") + # 正下方为"bottom"，
  ylim(0.4, 0.9) + # y轴范围
  labs(x = "Follow-up months", y = "AUC value") + # x，y命名
  scale_colour_manual(values = cols)
p # 颜色

# 不同时间点的ROC
pk2 <- Score(list(model1 = f1),
  Hist(Follow_up_timemon, Rel._in_5yrs == 1) ~ 1,
  data = train,
  times = 12,
  plots = "roc",
  metrics = "auc"
)
plotROC(pk2,
  col = "red",
  legend = "1 year ROC"
)

pk2 <- Score(list(model1 = f1),
  Hist(Follow_up_timemon, Rel._in_5yrs == 1) ~ 1,
  data = train,
  times = 36,
  plots = "roc",
  metrics = "auc"
)
plotROC(pk2,
  col = "blue", # 修改颜色以区分
  legend = "3 years ROC", # 修改标签
  add = T
)

# Calibration Curve绘制，校准曲线/图，评估模型的拟合优度(Hosmer-Lemeshow),一致性
## pec包函数和rms包中的calibrate()函数原理一致。
calPolt1 <- pec::calPlot(list("Clinic" = cli, "Rad-clinic" = full),
  time = 3 * 12, # 设置想要观察的时间点，同理可以绘制其他时间点的曲线
  data = test, legend.x = 0.5,
  legend.y = 0.3, legend.cex = 0.8
)
print(calPolt1) ## 查看内容

calPolt2 <- pec::calPlot(list("Clinic" = cli, "Rad-clinic" = full),
  time = 3 * 12, # 设置想要观察的时间点
  data = test, legend.x = 0.5,
  legend.y = 0.3, legend.cex = 0.8,
  splitMethod = "BootCv",
  B = 1000
)

units(train$Follow_up_timemon) <- "Months"
for (i in names(train)[c(1, 21, 7:18)]) {
  train[, i] <- as.factor(train[, i])
}
## 2 x 5 layout
layout(matrix(1:6, byrow = T, ncol = 3))
set.seed(123)

cal1 <- rms::calibrate(full,
  cmethod = "KM",
  method = "boot",
  u = 12, # u与time.inc一致
  m = 28, # m约等于样本量的1/3
  B = 1000
) # bootstrap重复次数
plot(cal1,
  lwd = 2, # 线段宽度
  lty = 1, # 线段类型
  errbar.col = "blue",
  xlim = c(0, 1), ylim = c(0, 1),
  xlab = "Nomogram-Predicted Probability of 12-month relapse",
  ylab = "Actual 12-month relapse(proportion)",
  col = "red",
  subtitles = F
)
# lines(cal1[,c("mean.predicted","KM")],type="b",lwd=2,col="red",pch=16)
abline(0, 1, lty = 3, lwd = 2, col = "black")

full3 <- cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ rad + SGS + familial_epilepsy + Durmon + SE,
  x = T, y = T, surv = T, data = test, time.inc = 36
)
cal2 <- rms::calibrate(coxmodel2,
  cmethod = "KM",
  method = "boot",
  u = 36, # u与time.inc一致
  m = 28,
  B = 200
)
plot(cal2,
  lwd = 2,
  lty = 1,
  errbar.col = "blue",
  xlim = c(0, 1), ylim = c(0, 1),
  xlab = "Nomogram-Predicted Probability of 36-month relapse",
  ylab = "Actual 36-month relapse(proportion)",
  col = "red",
  subtitles = F
)
# lines(cal1[,c("mean.predicted","KM")],type="b",lwd=2,col="red",pch=16)
abline(0, 1, lty = 3, lwd = 2, col = "black")

full5 <- rms::cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  x = T, y = T, surv = T, data = test, time.inc = 60
)
cal3 <- rms::calibrate(full5,
  cmethod = "KM",
  method = "boot",
  u = 60, # u与time.inc一致
  m = 28,
  B = 200
)
plot(cal3,
  lwd = 2,
  lty = 1,
  errbar.col = "blue",
  xlim = c(0, 1), ylim = c(0, 1),
  xlab = "Nomogram-Predicted Probability of 60-month relapse",
  ylab = "Actual 60-month relapse(proportion)",
  col = "red",
  subtitles = F
)
abline(0, 1, lty = 3, lwd = 2, col = "black")


# 临床决策曲线分析(DCA&CIC)
# https://blog.csdn.net/dege857/article/details/115061901
# https://blog.csdn.net/dege857/article/details/119373671?spm=1001.2014.3001.5501
library(ggDCA)
library(dcurves)
library(rmda)

d_full <- ggDCA::dca(full, full3, full5,
  new.data = test,
  times = c(12, 36, 60)
) #### 多个模型3年和5年后生存率
ggplot(d_full, linetype = 1, size = 5) ### 不设时间的话默认中位数时间

rmda::plot_clinical_impact(full)

##### 生成2个模型
for (i in names(train)[c(1, 21, 7:18)]) {
  train[, i] <- as.numeric(as.character(train[, i]))
}
for (i in names(train)[c(1, 21, 7:18)]) {
  train[, i] <- as.factor(train[, i])
}

f1 <- coxph(
  Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  test
)
test$Free_of_Relapse_12_mon <- c(1 - (summary(survfit(f1, newdata = test), times = 12)$surv))
test$Free_of_Relapse_36_mon <- c(1 - (summary(survfit(f1, newdata = test), times = 36)$surv))
test$Free_of_Relapse_60_mon <- c(1 - (summary(survfit(f1, newdata = test), times = 60)$surv))

dcurves::dca(Surv(Follow_up_timemon, Rel._in_5yrs) ~ Free_of_Relapse_12_mon + Free_of_Relapse_36_mon + Free_of_Relapse_60_mon,
  data = test,
  time = 36,
  thresholds = 1:50 / 100
) %>%
  plot(smooth = F) + theme_classic()

dcurves::dca(Surv(Follow_up_timemon, Rel._in_5yrs) ~ Free_of_Relapse_12_mon + Free_of_Relapse_36_mon + Free_of_Relapse_60_mon,
  data = train,
  time = 60,
  thresholds = 1:50 / 100
) %>%
  plot(smooth = F) + theme_classic()

# ggDCA包需要配合cph()函数使用
f11 <- cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE, surv = T, train)
fig1 <- ggDCA::dca(f11,
  new.data = train,
  times = c(12, 36, 40)
)
ggplot(fig1)
library(ggprism)
ggplot(fig1,
  linetype = F,
  lwd = 1.2
) +
  theme_classic() +
  theme_prism(base_size = 15) +
  theme(legend.position = c(0.7, 0.7)) +
  scale_x_continuous(
    limits = c(0, 1),
    guide = "prism_minor"
  ) +
  scale_y_continuous(
    limits = c(-0.01, 0.15),
    guide = "prism_minor"
  ) +
  scale_colour_prism(
    palette = "candy_bright",
    name = "Cylinders",
    label = c(
      "1 year DCA", "3 years DCA", "5 years DCA",
      "ALL-1 year", "ALL-3 years", "ALL-5 years",
      "None"
    )
  ) +
  labs(title = "1-5 Years DCA")

AUDC(fig1)
rFP.p100(fig1)
fig1
fig2 <- dca(f2, times = c(12, 36, 60))
ggplot(fig1, linetype = 1)

ggplot(fig1, linetype = F, lwd = 1.2) +
  theme_classic() +
  theme_prism(base_size = 17) +
  theme(legend.position = "top") +
  scale_x_continuous(
    limits = c(0, 1),
    guide = "prism_minor"
  ) +
  scale_y_continuous(
    limits = c(-0.01, 0.2),
    guide = "prism_minor"
  ) +
  scale_colour_prism(
    palette = "candy_bright",
    name = "Cylinders",
    label = c("模型1", "ALL", "None")
  ) +
  labs(title = "3 years DCA")

# riskplot绘制
# https://cloud.tencent.com/developer/article/1765625
library(ggrisk)
as.matrix(head(train))
str(train)
fit <- rms::cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  data = train
)
fit1 <- rms::cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  data = test
)
ggrisk(fit1,
  heatmap.genes = c("radscore", "SE", "Durmon", "SGS", "familial_epilepsy"),
  cutoff.value = "roc", # 可选‘median’, ’roc’ or ’cutoff’
  cutoff.x = 50, # “cutoff”文本的水平位置
  cutoff.y = -1
) # “cutoff”文本的垂直位置
two_scatter(fit1,
  cutoff.value = "roc",
  # cutoff.x = -3,#cutoff标签位置
  # cutoff.y = -2.8,
  code.0 = "Free of Relapse",
  code.1 = "Relapse",
  code.highrisk = "High Risk",
  code.lowrisk = "Low Risk",
  title.A.ylab = "Risk Score",
  title.B.ylab = "Free of Relapse(month)",
)
two_scatter(fit, # cutoff.x = -3,cutoff.y = -2.8,
  cutoff.value = "cutoff"
)
ggrisk(fit,
  heatmap.genes = c("radscore", "SE", "Durmon", "SGS", "familial_epilepsy"),
  cutoff.value = "roc"
)
# wald统计量有些区别(进一步进行anova分析时可以看出区别).cph来自rms包,coxph来自survival包

# NRI计算与绘制
library(nricens)
as.matrix(head(train))
m.old <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ SGS + familial_epilepsy + Durmon + SE,
  data = train, x = TRUE
)
m.new <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  data = train, x = TRUE
)
summary(m.new)$concordance # 未校准的时间C-index
predict(m.new, type = "lp", newdata = train) # "lp", "risk", "expected", "terms", "survival"
predict(m.new, type = "survival")
predict(m.new, type = "risk", se.fit = TRUE)
predict(m.new, type = "terms", se.fit = TRUE)

set.seed(123)
nricens(
  mdl.std = m.old,
  mdl.new = m.new,
  t0 = 5 * 12, # 设置时间点
  cut = c(0.2, 0.4), # 设置cutoff
  updown = "category", # 指定计算category NRI
  niter = 100
) # 设置结果显示标准误差

nricens(
  mdl.std = m.old,
  mdl.new = m.new,
  t0 = 5 * 12,
  cut = 0, updown = "diff", # 指定计算连续性NRI
  niter = 100
)
# 明确的划分切点适用分类NRI

# IDI计算与绘制
library(randomForestSRC)
library(survey)
library(survIDINRI)
for (i in names(train)[c(1, 21, 7:18)]) {
  train[, i] <- as.numeric(as.character(train[, i]))
}
Y <- train[, c("Follow_up_timemon", "Rel._in_5yrs")]

z.old <- train[, c("familial_epilepsy", "Durmon", "SGS", "SE")]
z.new <- train[, c("familial_epilepsy", "Durmon", "SE", "SGS", "radscore")]
set.seed(123)
x <- IDI.INF(Y,
  z.old,
  z.new,
  t0 = 1 * 12,
  npert = 200
)
IDI.INF.OUT(x)
IDI.INF.GRAPH(x)

# 混淆矩阵及F1-score(https://www.geeksforgeeks.org/how-to-calculate-f1-score-in-r/)
library("caret")
library(MLmetrics)
## Load epicalc package to calcuate AUC
library(epicalc)
library(reportROC)

## Model with age and sex
logit.Rad.cli <- glm(Rel._in_5yrs ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, family = binomial)
lroc(logit.Rad.cli, graph = F)$auc
print(lroc(logit.Rad.cli, graph = F))
## Model with Rad and clinic separately
lroc(logit.Rad, graph = F)$auc
logit.cli <- glm(Rel._in_5yrs ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, family = binomial)
lroc(logit.cli, graph = F)$auc

## Create a variable indicating N-year event
train <- within(train, {
  outcome1yr <- NA
  outcome1yr[(Rel._in_5yrs == 1) & (Follow_up_timemon <= 1 * 12)] <- 1 # event+ within two years
  outcome1yr[(Rel._in_5yrs == 0) | (Follow_up_timemon > 1 * 12)] <- 0 # otherwise
})

## 2-year outcome model with radscore and clinic
logit.Rad.cli <- glm(outcome1yr ~ radscore + familial_epilepsy + SE + SGS + Durmon, data = train, family = binomial)
lroc(logit.Rad.cli, graph = F)$auc
print(lroc(logit.Rad.cli))
print(lroc(logit.Rad.cli, graph = F))
# real=train$outcome1yr

predict_ <- predict.glm(logit.Rad.cli, type = "response", newdata = train)
predict <- ifelse(predict_ > 0.5, 1, 0)
train$predict1 <- predict
head(train)
# write.csv(train,"3ytrain.csv")
str(train)
train$predict1 <- as.factor(train$predict)
train$outcome1yr <- as.factor(train$outcome1yr)
# create confusion matrix
train1 <- caret::confusionMatrix(train$predict1, train$outcome1yr)
# 结果提取
train1$byClass
train1

## 2-year outcome model with Rad
logit.Rad <- glm(outcome2yr ~ Rad, data = train, family = binomial)
lroc(logit.age.sex.albumin, graph = F)$auc

reportROC(
  gold = dt$Label, predictor.binary = dt$Phy3,
  plot = T, important = "se", exact = FALSE
)

# install.packages("party")
library(party)
tree <- ctree(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = test)
tree <- ctree(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore, data = train)
plot(tree)

# 简易评分系统, WALD-1(变量重要性),
# Total points
library(nomogramEx)
fita <- rms::cph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ radscore + SGS + familial_epilepsy + Durmon + SE,
  data = train, x = TRUE, y = TRUE, surv = TRUE, time.inc = 12
)
surv <- Survival(fita)
nomo <- nomogram(fita,
  fun = list(function(x) surv(12, x), function(x) surv(6, x)),
  lp = TRUE, funlabel = c("12-Month Survival Prob", "6-Month Survival Prob")
)
nomogramEx(nomo = nomo, np = 2, digit = 9)

# 二次验证，最终Cox模型，cutoff风险分层(12,36,60 months)，K-M曲线绘制**
summary(fita)
summary(m.new)
train$lp.rad_clinic <- predict(m.new, type = "lp")
logresult <- cutoff::logrank(
  data = train, # 数据集
  time = "Follow_up_timemon", # 生存时间
  y = "Rel._in_5yrs", # 生存状态
  x = "lp.rad_clinic", # 连续自变量
  cut.numb = 1, # 截断值选择1个分界点
  n.per = 0.10, # 分组后自变量组最少比例
  y.per = 0.10, # 分组后因变量组最少比例
  p.cut = 0.005, # 检验水准，结果只显示pvalue小于0.05的截断值
  round = 3
) # 保留5位有效小数
logresult
ggpubr::ggline(logresult,
  x = "cut1", y = "pvalue", color = "blue", axis.title.x = 0.01,
  palette = "jco", xlab = "截断值取值", ylab = "p value"
)

res.cut <- surv_cutpoint(
  data = train, time = "Follow_up_timemon",
  event = "Rel._in_5yrs", variables = c("lp.rad_clinic")
)
summary(res.cut) # 最佳截断值为1.877
plot(res.cut, "lp.rad_clinic", palette = "npg")
res.cat <- surv_categorize(res.cut)
fit <- survfit(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ lp.rad_clinic, data = res.cat)
summary(fit)
survdiff(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ lp.rad_clinic, data = res.cat)

ggsurvplot(fit,
  data = res.cat,
  risk.table = TRUE, conf.int = TRUE,
  surv.median.line = "hv", # 同时显示垂直和水平参考线
  pval = T, xlab = "months", ylab = "Free of Relapse(%)"
)

# 交叉验证与重抽样, 重复论证
library(CoxBoost)
str(train)
train$Rel._in_5yrs <- as.numeric(as.character(train$Rel._in_5yrs))
train$Rel._in_5yrs <- as.factor(train$Rel._in_5yrs)

months <- train$Follow_up_timemon
relapse <- train$Rel._in_5yrs

x <- as.matrix(train[c("radscore", "SGS", "SE", "familial_epilepsy", "Durmon")])
x <- as.matrix(train[c(7:22)])
cbfit <- CoxBoost(
  time = months,
  status = relapse,
  x = x,
  stepno = 10,
  penalty = 50
)
summary(cbfit)

optim.res <- optimCoxBoostPenalty(
  time = months,
  status = relapse,
  x = x,
  trace = FALSE,
  start.penalty = 10
)
optim.res$penalty

set.seed(123)
cv.res <- cv.CoxBoost(
  time = months,
  status = relapse,
  x = x,
  maxstepno = 500,
  K = 10,
  type = "verweij",
  penalty = optim.res$penalty
)
plot(cv.res$mean.logplik)
cv.res$optimal.step # 最优次数

cbfit <- CoxBoost(
  time = months,
  status = relapse,
  x = x,
  stepno = cv.res$optimal.step,
  penalty = optim.res$penalty
)
summary(cbfit)

names <- cbfit$xnames[which(cbfit$coefficients[111, ] != 0)]
coef <- cbfit$coefficients[111, ][which(cbfit$coefficients[111, ] != 0)]
cbind(names, coef)
