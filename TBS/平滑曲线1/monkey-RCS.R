# 如何建立非线性回归预测模型https://zhuanlan.zhihu.com/p/101906049
# https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0
# https://mp.weixin.qq.com/s?__biz=MzI1NjM3NTE1NQ==&mid=2247484446&idx=1&sn=487c68752949698fea9102b15fc5d2c0&chksm=ea26e402dd516d14667cc1171151d3c1527e9a32fed6f390813d31ddb87dac815a50b4194735&mpshare=1&scene=1&srcid=0612zm7B4uWhTaFCwvNQLrEs&sharer_sharetime=1655014649840&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
rm(list = ls())
library(ggplot2)
library(segmented)
library(splines)
library(Hmisc)
library(rms)
library(mgcv)
library(caret)
library(readxl)
library(dlookr)
library(DataExplorer)

# 读取数据
TLM <- read.csv("/home/wane/Desktop/TBS&Mon/Monkey/Ziqing/66-PVM.csv")
TLM <- read.csv("/home/wane/Desktop/TBS&Mon/Monkey/QIANG/PartⅡ猴脑代谢发育数据分析/PET_refWhole_SUVr.csv")
# TLM <- read_excel("/home/wane/Desktop/TBS/TLMey/BMC.xlsx")
# 数据探索
TLM <- TLM[c(-1, -2, -5)]
summary(TLM)
glimpse(TLM)
sum(!is.na(TLM))
fix(TLM)
TLM <- base::transform(TLM, LM_L3 = LM_L3 / 12)
TLM <- TLM[, 2:8]
N <- names(TLM)
f <- as.formula(paste(" ~", paste(N[!N %in% "TBVBW"], collapse = " + ")))
f
TLM$Gender <- factor(TLM$Gender)
eda_pLM_L3d_report(TLM, output_file = "/home/wane/Desktop/TBS&Mon/TLMey/Ziqing/ZZQ.pdf")
cover <- file.path(system.file(packLM_L3 = "dlookr"), "report", "/home/wane/Desktop/TBS&Mon/TLMey/Ziqing/hist-B.png")
eda_pLM_L3d_report(TLM,
  cover_img = cover, title_color = "gray", target = "Gender",
  output_file = "/home/wane/Desktop/TBS/TLMey/Voxel_EDA.pdf"
)
eda_pLM_L3d_report(TLM,
  target = "Gender",
  output_file = "/home/wane/Desktop/TBS/TLMey/Voxel_EDA.pdf"
)
TLM <- TLM[, 10:15]
create_report(TLM)

# 雨云图(Raincloud)
library(ggdist)
TLM <- TLM[c(-1, -3)]
# pdf("/media/wane/wade/EP/EPTLE_PET/CN_PET_csv/raincloud.pdf",width=20, height=10)
ggplot(data = TLM, aes(y = LM_L3, x = factor(Group), fill = factor(Group))) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.2, outlier.color = NA) +
  ggdist::stat_dots(side = "left", justification = 1.1)
# dev.off()
con <- subset(TLM, TLM$Group == "con")
sub <- subset(TLM, TLM$Group == "sub")
summary(con)
summary(sub)
psych::describe(sub)

# 分层分析
F <- subset(TLM, TLM$Sex == 0)
M <- subset(TLM, TLM$Sex == 1)

dd <- datadist(TLM) # 为后续程序设定数据环境
options(datadist = "dd")
ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() # 绘制散点图
p <- ggplot() +
  geom_point(data = TLM, mapping = aes(x = LM_L3, y = TLM)) +
  theme_classic()
p

# 建立线性回归模型
model.lm <- lm(TLM ~ LM_L3, data = TLM) # 构建线性回归模型
summary(model.lm) # 查看回归模型结果
p1 <- ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ x)
p1
# 建立曲线方程(log,exp)
model.log <- lm(TLM ~ log(LM_L3), data = TLM) # 建立对数曲线方程
summary(model.log) # 查看模型概况
# 拟合曲线
p2 <- ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ log(x))
p2
model.log10 <- lm(TLM ~ log10(LM_L3), data = TLM) # 建立指数曲线方程
summary(model.log10) # 查看模型概况
# 拟合曲线
ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log10(x))

# 建立分段回归模型
# https://blog.csdn.net/weixin_40575651/article/details/107575012
model.segmented <- segmented(model.lm) # 构建分段回归模型
summary(model.segmented) # 查看模型概况
# 查看拟合效果
plot(TLM$LM_L3, TLM$TLM, pch = 1, cex = 1.5)
abline(a = coef(model.lm)[1], b = coef(model.lm)[2], col = "red", lwd = 2.5)
plot(model.segmented, col = "blue", lwd = 2.5, add = T)

p3 <- p + theme_classic() +
  geom_smooth(
    data = TLM, mapping = aes(x = LM_L3, y = TLM),
    method = "gam", formula = y ~ x + I((x - 11.3) * (x > 11.3))
  ) +
  geom_vline(xintercept = 11.3, linetype = 2, color = "red")
p3

# 手动设置拐点，分三段回归
model.segmented2 <- segmented(model.lm, psi = c(4, 12)) # 构建分段回归模型
summary(model.segmented2) # 查看模型概况
# 查看拟合效果
plot(TLM$LM_L3, TLM$TLM, pch = 1, cex = 1.5)
abline(a = coef(model.lm)[1], b = coef(model.lm)[2], col = "red", lwd = 2.5)
plot(model.segmented2, col = "blue", lwd = 2.5, add = T)

# 样条回归
model.spline <- lm(TLM$TLM ~ rcs(TLM$LM_L3, 3)) # 建立样条回归，设置3个节点
summary(model.spline) # 查看模型概况
# 样条回归拟合效果
p4 <- ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3))
p4
# Lowess函数建立局部加权回归
model.lowess <- lowess(TLM$TLM ~ TLM$LM_L3) # 建立局部加权回归
summary(model.lowess) # 查看概况
# 查看拟合
p5 <- ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  theme_classic() +
  stat_smooth(method = loess, formula = y ~ x)
p5
# 广义可加模型gam**
# https://mp.weixin.qq.com/mp/appmsgalbum?__biz=MzI1NjM3NTE1NQ==&action=getalbum&album_id=2077935014574374912&scene=173&from_msgid=2247485011&from_itemidx=1&count=3&nolastread=1#wechat_redirect
model.gam <- gam(TLM ~ s(LM_L3), data = TLM) # 建立gam模型
summary(model.gam) # 查看模型概况
pr.gam <- predict(model.gam, TLM) # 生成预测值
# 计算RSME和R方
data.frame(
  RMSE = RMSE(pr.gam, TLM$TLM),
  R2 = R2(pr.gam, TLM$TLM)
)
# 查看模型拟合情况
p6 <- ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  theme_classic() +
  stat_smooth(method = gam, formula = y ~ s(x))
p6
# gam
coef(model.gam)[1] # Intercept
par(mfrow = c(1, 2)) # 2*2画布
plot(model.gam, shade = T, shade.col = "lightblue")
plot(model.gam,
  rug = T, residuals = T, seWithMean = T,
  pch = 2, cex = 1
)
# vis.gam(model.gam, type = "link", plot.type = "contour")
vis.gam(model.gam, type = "link", plot.type = "contour")
vis.gam(model.gam, type = "response", plot.type = "persp", border = NA, phi = 30, theta = 30)

gam.check(model.gam)
concurvity(model.gam, full = F)
anova(model.log, model.gam)

# https://cloud.tencent.com/developer/article/1972411
library(patchwork)
p4 + p1 + p6 + plot_layout(nrow = 2, byrow = FALSE) #  从上到下
p4 + p5 + p6 +
  plot_layout(ncol = 2) # 从左到右
p4 / p5 | (p6)
# https://zhuanlan.zhihu.com/p/384189537
library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6,
  label_size = 12,
  hjust = -0.2, vjust = 1.4,
  labels = c("LM", "LOG", "SEGMENTED", "RCS", "LOWESS", "GAM")
)
cowplot::plot_grid(p1, p2, p3, p4, p5, p6,
  ncol = 3, labels = "AUTO"
)

# R语言绘制限制立方条图2（基于logistic回归和线性回归）
data1 <- read.csv("/home/wane/Desktop/TBS/TLMey/VoxelNumbers_InMachin_atlas_whole.csv")
library(rms) # 限制性立方样条需要的包
library(survminer) # 曲线
library(ggplot2) # 画图
library(ggsci) # 调色板 作者：data小白 https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0
library(splines)

names(TLM)
# 选取出来icu_patients_per_million, new_deaths_per_million 这两组数据
data2 <- data.frame(TLM, LM_L3)
# 查看选取出来的数据
head(data2, 20)
data2 <- TLM
summary(data2)
dd <- datadist(data2) # 为后续程序设定数据环境
options(datadist = "dd") # 为后续程序设定数据环境
# fit <-ols(new_deaths_per_million ~ rcs(icu_patients_per_million, 4),data=data2) #做变量的回归
fit <- ols(TscoreL1L4 ~ rcs(LM_L3, nk = 5) + rcs(BMI, nk = 5) + Gender,
  data = RCS
)
fit <- ols(TLM ~ rcs(LM_L3, nk = 5),
  data = TLM, x = TRUE, y = TRUE
)
summary(fit)
an <- anova(fit)

Predict(fit, LM_L3)
# fun=exp
plot(Predict(fit, LM_L3), anova = an, pval = T)
OLS1 <- Predict(fit, LM_L3, ref.zero = F)
OLS1

ggplot() +
  geom_line(data = OLS1, aes(LM_L3, yhat), linetype = 1, size = 1, alpha = 0.9, colour = "red") +
  geom_ribbon(data = OLS1, aes(LM_L3, ymin = lower, ymax = upper), alpha = 0.3, fill = "red") +
  theme_classic() +
  labs(title = "RCS", x = "LM_L3", y = "TLM")

# 不分组/全人群HR与协变量变化关系
Pre0 <- rms::Predict(fit, LM_L3,
  type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2
)
## 其中fun是转化函数,fun=exp
ggplot(Pre0)
View(Pre0)

ggplot() +
  geom_line(
    data = Pre0,
    aes(LM_L3, yhat, colour = Gender), alpha = 0.7
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre0,
    aes(LM_L3, ymin = lower, ymax = upper, fill = Gender), alpha = 0.1
  ) +
  scale_fill_nejm() +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75)

# 不同分组(男女组)之间HR与协变量变化关系
Pre1 <- rms::Predict(fit, LM_L3, Gender = c("1", "0"), type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1) +
  geom_line(
    data = Pre1,
    aes(LM_L3, yhat), alpha = 0.7
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre1,
    aes(LM_L3, ymin = lower, ymax = upper), alpha = 0.1
  ) +
  scale_fill_nejm() +
  scale_colour_discrete(
    name = "Gender", breaks = c("1", "0"),
    labels = c("M", "F")
  ) +
  scale_shape_discrete(
    name = "Gender", breaks = c("1", "0"),
    labels = c("M", "F")
  ) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75)
View(Pre1)


# 线性回归 作者：男二号关陆陆 https://www.bilibili.com/read/cv13828406/ 出处：bilibili
# 样条回归并绘制限制立方条图 https://blog.csdn.net/dege857/article/details/113842472
ggplot(TLM, aes(x = LM_L3 + weight, y = TLM, colour = Gender)) +
  geom_point() +
  geom_smooth(col = "red", method = "lm")

ggplot(data = TLM, aes(x = LM_L3 + weight, y = TLM, colour = Gender)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3))


model.spline <- lm(TLM$TLM ~ rcs(TLM$LM_L3, 4)) # 建立样条回归，设置3个节点
summary(model.spline) # 查看模型概况
# P值，小于0.05表明非线性关系
anova(model.spline)
names(TLM)
ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3)) ## 绘制样条回归拟合效果图

model.spline1 <- lm(TLM$TLM ~ rcs(TLM$LM_L3, c(5, 10, 20))) # 建立样条回归，设置4个节点
ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, c(5, 10, 20))) ## 绘制样条回归拟合效果图

ggplot(TLM, aes(LM_L3, TLM, fill = weight, size = weight)) +
  geom_point(shape = 21, size = 4, col = "black") +
  stat_smooth(method = lm, formula = y ~ rcs(x, 4)) ## 美化一下图形

ggplot(TLM, aes(LM_L3, TLM, fill = Gender, group = Gender)) +
  geom_point(shape = 21, size = 4, col = "black") +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3)) ## 分组表示

ggplot(TLM, aes(LM_L3, TLM, fill = Gender, color = Gender, Group = Gender)) +
  geom_point(shape = 21, size = 4, col = "black") +
  stat_smooth(method = lm, aes(color = Gender), formula = y ~ rcs(x, 3)) ## 分组表示

library(ggpubr)
library(ggsci)
library(cowplot)
str(TLM$Gender)
ggplot(TLM, aes(x = LM_L3, y = TLM, color = Gender)) +
  geom_point(aes(color = Gender), size = 5) +
  scale_fill_nejm() +
  scale_colour_nejm() +
  geom_smooth(method = "lm", formula = y ~ rcs(x, 3), se = T)
ggplot(TLM, aes(x = LM_L3, y = TLM, color = Gender)) +
  geom_point(aes(color = Gender), size = 5) +
  geom_smooth(method = "lm", formula = y ~ rcs(x, 3), se = T) +
  stat_cor(data = TLM, method = "spearman")
# 显著性检验stat_cor(data=data, method = "pearson")意为用pearson相关进行相关性分析，可编辑更改。


# 平滑曲线与阈值效应分析(一)gam
rm(list = ls())
dt <- read.csv("/home/wane/Desktop/TBS/TLMey/VoxelNumbers_InMachin_atlas_whole.csv")
str(dt)
# 2.3拟合平滑曲线
# 2.3.1 构建模型，使用mgcv::gam()函数拟合平滑曲线。
fml <- "TLM ~ s(LM_L3,fx=FALSE) + Gender"
gam <- mgcv::gam(formula(fml),
  weights = dt$weights,
  data = dt, family = gaussian(link = "identity")
)
gam1 <- gam(TLM ~ s(LM_L3, k = 4, bs = "fs") + Gender,
  data = dt, method = "REML"
)
gam2 <- gam(TLM ~ s(LM_L3, k = 3) + Gender,
  weights = dt$weights,
  data = dt, family = gaussian(link = "identity")
)
summary(gam)
coef(gam)[1] # Intercept
par(mfrow = c(2, 2)) # 2*2画布
plot(gam, shade = T, shade.col = "lightblue")
plot(gam,
  rug = T, residuals = T, seWithMean = T,
  pch = 2, cex = 1
)
# vis.gam(gam1, type = "link", plot.type = "contour")
vis.gam(gam, type = "response", plot.type = "contour")
vis.gam(gam, type = "response", plot.type = "persp", border = NA, phi = 30, theta = 30)

gam.check(gam)
gam.check(gam1)
concurvity(gam, full = F)

# 2.3.2 计算拟合值
pred <- predict.gam(gam, type = "terms", se.fit = TRUE)
mfit <- pred$fit[, "s(LM_L3)"]
sfit <- pred$se.fit[, "s(LM_L3)"]
# mfit <- mfit+(mean(mdl$fitted.values)-mean(mfit))  ###考虑是随机截距
dt <- cbind(dt, mfit, sfit)
# 2.3.3 计算95%CI
y.low <- dt$mfit - 1.96 * dt$sfit
y.upp <- dt$mfit + 1.96 * dt$sfit
dt <- cbind(dt, y.low, y.upp)
# 2.3.4 绘制曲线
summary(dt)
dt <- arrange(dt, LM_L3)
co <- c(0, 30, -10, 15) ## 定义坐标
col <- c("blue", "purple")
# ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),
plot(dt$mfit ~ dt$LM_L3, col = col[1], ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), type = "l", lty = 1, lwd = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$LM_L3, col = col[2], ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), type = "l", lty = 3, lwd = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$LM_L3,
  col = col[2], ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), type = "l", lty = 3, lwd = 1,
  ylab = "TLM", xlab = "LM_L3"
)
rug(dt$LM_L3, col = "blue")
# 2.4 阈值效应分析
# 2.4.1 计算拐点
fml <- "TLM ~ LM_L3 + Gender"
source("/home/wane/Documents/RDocu/平滑曲线1/get_cutoff_lm.R")
cut_off <- get_cutoff_lm("LM_L3", dt, fml)
print(cut_off)
# 2.4.2 生成分段变量
x <- dt[, "LM_L3"]
X1 <- (x <= cut_off) * (x - cut_off)
X2 <- (x > cut_off) * (x - cut_off)
dt1 <- cbind(dt, x, X1, X2)
# 2.4.3 构建分段模型
mdl0 <- glm(TLM ~ x + X2 + Gender, family = "gaussian", weights = dt1$weights, data = dt1)
mdl1 <- glm(TLM ~ X1 + X2 + Gender, family = "gaussian", weights = dt1$weights, data = dt1)
mdl2 <- glm(TLM ~ x + Gender, family = "gaussian", weights = dt1$weights, data = dt1)
# 直线拟合数据
summary(mdl2)
summary(mdl1)
summary(mdl0)
### 似然比检验
round(1 - pchisq(2 * (logLik(mdl0)[1] - logLik(mdl2)[1]), 1), 3)

#### 折点作图到图上
co <- c(0, 30, -15, 15) ## 定义坐标
col <- c("blue", "purple")

plot(dt$mfit ~ dt$LM_L3, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[1], type = "l", lty = 1, lwd = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$LM_L3, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$LM_L3,
  ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1,
  ylab = "TLM", xlab = "LM_L3"
)
rug(dt$LM_L3, col = "blue")
abline(v = cut_off, col = "black", lty = 2)
