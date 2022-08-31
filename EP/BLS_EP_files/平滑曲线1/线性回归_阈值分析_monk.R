# R语言|平滑曲线与阈值效应分析(一)
# R语言绘图|2.基于GAMM：分层的平滑曲线怎么画？https://zhuanlan.zhihu.com/p/489149912
rm(list=ls())##清空当前环境
library(mgcv) ##GAMM
library(ggplot2)#画图
library(ggthemes)##ggplot主题
library(writexl)
library(dplyr)

dt <- read.csv("jixian.csv")
dt <- read.csv("/home/wane/Desktop/TBS&Mon/Monkey/QIANG/PartⅡ猴脑代谢发育数据分析/SUVr_whole.csv")
# TLM <- read_excel("/home/wane/Desktop/TBS/TLMey/BMC.xlsx")
# 数据探索
dt <- dt[c(-1,-2,-5)]
dt$Sex <- as.factor(dt$Sex)
summary(dt)
# colnames(dt) <- toupper(colnames(dt))
str(dt)
fml <- "SUVr_whole_refPons~s(Age,k=5,fx=FALSE)+factor(Sex)"
gam1 <- mgcv::gam(formula(fml), weights=dt$weights, data = dt, family = gaussian(link = "identity"))
summary(gam1)
vis.gam(gam1,color="heat",theta=30,phi=30)
gam1$weights
plot(gam1, pages = 1, col = "blue", las = 1, se = T, rug = T)

m <- mgcv::gam(SUVr_whole_refPons ~ s(Age, k=5, by = Sex) + factor(Sex), data = dt)
plot(modelbased::estimate_relation(m, length = 100, preserve_range = FALSE))

plot(gam1, scale=0, page=1, shade = TRUE, las = 1, all.terms=TRUE, cex.axis=1.2, cex.lab=1.5, main="Univariable Model")
AIC(gam1)
abline(v = 0, col = "blue")

### predict
pred <- predict.gam(gam1, type = "terms", se.fit = TRUE)
mfit <- pred$fit[, "s(Age)"]
sfit <- pred$se.fit[, "s(Age)"]
mfit <- mfit + (mean(gam1$fitted.values) - mean(mfit)) ### 考虑是随机截距
dt <- cbind(dt, mfit, sfit)

### 95%CI
y.low <- dt$mfit - 1.96 * dt$sfit
y.upp <- dt$mfit + 1.96 * dt$sfit
dt <- cbind(dt, y.low, y.upp)

### order
dt <- arrange(dt, Age)
co <- c(0, 25, 0.2, 0.8) ## 定义坐标
col <- c("blue", "purple")

plot(dt$mfit ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[1], type = "l", lty = 1, lwd = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, ylab = "HB", xlab = "AGE")
rug(dt$AGE, col = "blue")

# 广义可加模型gam**
# https://mp.weixin.qq.com/mp/appmsgalbum?__biz=MzI1NjM3NTE1NQ==&action=getalbum&album_id=2077935014574374912&scene=173&from_msgid=2247485011&from_itemidx=1&count=3&nolastread=1#wechat_redirect
pr.gam <- predict(gam1, dt) # 生成预测值
# 计算RSME和R方
data.frame(
  RMSE = RMSE(pr.gam, dt$Frontal_Cortex),
  R2 = R2(pr.gam, dt$Frontal_Cortex)
)
# 查看模型拟合情况
p1 <- ggplot(dt, aes(Age, whole)) + scale_x_continuous(breaks = seq(0,26,2)) +
  geom_point() + geom_vline(aes(xintercept=8.0),linetype=4,col="red") +
  theme_classic() +
  stat_smooth(method = mgcv::gam, formula = y ~ s(x, k=5))
p1
# 分类gam曲线拟合
library(ggsci)
?formula.gam
ggplot(dt, aes(x = Age, y = SUVr_whole_refPons, color = Sex)) +
  geom_point(aes(color = Sex), size = 1) + scale_x_continuous(breaks = seq(0,26,2)) +
  # scale_fill_nejm() + scale_colour_nejm() + 
  theme_classic() + 
  # geom_vline(aes(xintercept=8.0),linetype=4,col="red") +
  geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 3), se = T)

# gam
coef(gam1)[1] # Intercept
par(mfrow = c(1, 2)) # 2*2画布
plot(gam1, shade = T, shade.col = "lightblue")
plot(gam1,
     rug = T, residuals = T, seWithMean = T,
     pch = 2, cex = 1
)
# vis.gam(model.gam, type = "link", plot.type = "contour")
vis.gam(gam1, type = "link", plot.type = "contour")
vis.gam(gam1, type = "response", plot.type = "persp", border = NA, phi = 30, theta = 30)

gam.check(gam1)
concurvity(gam1, full = F)
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

#### 阈值效应分析
## model I 一条直线效应
fit1 <- lm(Frontal_Cortex ~ Age, data = dt)
summary(fit1)

## model II 分段模型
fml <- "Frontal_Cortex ~ Age"
source("/home/wane/Documents/RDocu/RCS/平滑曲线1/get_cutoff_lm.R")
cut_off <- get_cutoff_lm("Age", dt, fml)
cut_off

x <- dt[, "Age"]
X1 <- (x <= cut_off) * (x - cut_off)
X2 <- (x > cut_off) * (x - cut_off)
dt1 <- cbind(dt, x, X1, X2)

mdl0 <- glm(Frontal_Cortex ~ x + X2, family = "gaussian", data = dt1)
mdl1 <- glm(Frontal_Cortex ~ X1 + X2, family = "gaussian", data = dt1)
mdl2 <- glm(Frontal_Cortex ~ x, family = "gaussian", data = dt1)
#### model I 一条直线效应(线性回归)
summary(mdl2)
#### 折点
print(cut_off)
#### < K 段效应(B+se)/P
#### > K 段效应(B+se)/P
summary(mdl1)
### > K 段与< K的效应(B+se)/P
summary(mdl0)

### 考虑分段后模型和不分段对数似然比检验
round(1 - pchisq(2 * (logLik(mdl0)[1] - logLik(mdl2)[1]), 1), 3)

#### 折点作图到图上
plot(dt$mfit ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[1], type = "l", lty = 1, lwd = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, ylab = "Frontal_Cortex", xlab = "Age")
rug(dt$AGE, col = "blue")
abline(v = cut_off, col = "black", lty = 2)


