# R语言|平滑曲线与阈值效应分析(一)
# R语言绘图|2.基于GAMM：分层的平滑曲线怎么画？https://zhuanlan.zhihu.com/p/489149912
# https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0
rm(list = ls()) ## 清空当前环境
options(digits = 3) # 限定输出小数点后数字的位数为3位
library(mgcv) ## GAMM
library(gamm4)
library(ggplot2) # 画图
library(ggthemes) ## ggplot主题
library(writexl)
library(dplyr)
theme_set(theme_classic() + theme(legend.position = "bottom"))

# dt <- read.csv("jixian.csv")
dt <- read.csv("C:\\Users\\wane199\\Desktop\\TBS&Mon\\Monkey\\QIANG\\1030\\T1_TBV.csv")
dt <- read.csv("/home/wane/Desktop/TBS&Mon/Monkey/QIANG/1030/T1_TBV.csv", fileEncoding = "GBK")
# TLM <- read_excel("/home/wane/Desktop/TBS/TLMey/BMC.xlsx")
# 数据探索EDA
dt <- dt[c(-1, -2)]
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
summary(dt)
str(dt)
# colnames(dt) <- toupper(colnames(dt))
# rcssci(linear models with RCS splines were performed to explore the shape linear or nonlinear(U, inverted U,J,S,L,log,-log,temporary plateau shape)
library(rcssci)
rcssci_linear(
  data = dt, y = "TBV_mm3", x = "Age", covs = c("Sex"), ref.zero = F,
  prob = 0.1, filepath = "C:\\Users\\wane199\\Desktop\\TBS&Mon\\Monkey\\QIANG\\1030\\"
) + # 默认prob = 0.5
  ggplot2::theme_classic()

fml <- "SUVr_whole_refPons ~ s(Age,k=4,fx=FALSE)+factor(Sex)"
gam1 <- mgcv::gam(formula(fml), weights = dt$weights, data = dt, family = gaussian(link = "identity"))
summary(gam1) # 检验自变量的显著性以及评估回归整体的方差解释率
vis.gam(gam1, color = "gray", theta = 30, phi = 30) # "topo", "heat", "cm", "terrain", "gray" or "bw"
gam1$weights
plot(gam1, pages = 1, col = "blue", las = 1, se = T, rug = T)

mgam <- gam(TBV ~ s(Age) + factor(Sex),
  data = dt, family = gaussian(link = "identity"), #  poisson(), gaussian(link = "identity")，binomial(link = "logit"),
  model = T
)
summary(mgam)
vis.gam(mgam, ticktype = "detailed", color = "bw", theta = 40, phi = 40)
vis.gam(mgam, color = "gray", theta = 30, phi = 30)
vis.gam(mgam, theta = 30, phi = 30, plot.type = "contour", color = "cm")
plot(mgam, pages = 1, col = "blue", las = 1, se = T, rug = T)

m <- mgcv::gam(TBV ~ s(Age, k = 4) + factor(Sex), data = dt) # , by = Sex
summary(m)
anova(m)
plot(modelbased::estimate_relation(m, length = 100, preserve_range = FALSE))

plot(gam1, scale = 0, page = 1, shade = TRUE, las = 1, all.terms = TRUE, cex.axis = 1.2, cex.lab = 1.5, main = "Univariable Model")
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
library(caret)
data.frame(
  RMSE = RMSE(pr.gam, dt$TBV),
  R2 = R2(pr.gam, dt$TBV)
)
# 查看模型拟合情况
ggplot(dt, aes(Age, TBV)) +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  geom_point() +
  geom_vline(aes(xintercept = 8.0), linetype = 4, col = "red") +
  theme_classic() +
  stat_smooth(method = mgcv::gam, formula = y ~ s(x, k = 5))

library(ggpmisc)
library(ggpubr)
library(scales) # 构建一个 squash_axis 函数来实现坐标轴压缩功能，这个函数需要使用scales包(https://zhuanlan.zhihu.com/p/358781655)
squash_axis <- function(from, to, factor) {
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
  trans <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    # apply transformation
    x[isq] <- from + (x[isq] - from) / factor
    x[ito] <- from + (to - from) / factor + (x[ito] - to)
    return(x)
  }
  inv <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from) / factor
    ito <- x >= from + (to - from) / factor
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from) / factor))
    return(x)
  }
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}

my.formula <- y ~ s(x, k = 4, bs = "cs")
ggplot(dt, aes(Age, TBV.BW, colour = Sex)) +
  geom_point() +
  stat_cor(aes(), label.x = 3) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 32, 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  stat_smooth(method = mgcv::gam, se = TRUE, formula = my.formula) +
  coord_trans(x = squash_axis(0, 5, 0.50)) + 
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

####################################
# 分类gam曲线拟合
paste0(colnames(dt[4:16]), collapse = "+")
Frontal_Cortex + Temporal_Cortex + Parietal_Cortex + Occipital_Cortex + Insula_Cortex +
  Striatum + Hippocampus +
  Thalamus + Amygdala + Cingulate + Globus_pallidus + corpus_callosum + Cerebellum

# ?formula.gam
library(ggsci)
library(ggpmisc)
library(ggpubr)
theme_set(theme_classic() + theme(legend.position = "bottom"))
my.formula <- y ~ s(x, bs = "cs")
my.formula <- y ~ s(x, k = 4)

my.formula <- y ~ x + I(x^2)
# 散点图
ggplot(data = dt, mapping = aes(x = Age, y = TBV, color = Sex, shape = Sex)) +
  geom_point(size = 2) +
  theme_classic() +
  scale_colour_nejm() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
# 置信区间虚线
ggplot(data = dt, mapping = aes(x = Age, y = SUVr_whole_refPons, colour = Sex)) +
  scale_colour_nejm() +
  # geom_point(size = 2) + stat_cor(aes(), label.x = 6) # 显示p值和R值
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_smooth(
    method = "gam", formula = my.formula, size = 3,
    se = FALSE
  ) +
  stat_cor(aes(), label.x = 6) +
  geom_ribbon(
    stat = "smooth", formula = my.formula, size = 1,
    method = "gam", se = TRUE, alpha = 0, # or, use fill = NA
    linetype = "dotted"
  ) +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  ) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = my.formula, parse = TRUE
  )
# 置信区间带
p2 <- ggplot(dt, mapping = aes(x = Age, y = volume_ratio, colour = Side, fill = Side, linetype = Side)) +
  # ylab(bquote(TBV/BW(cm^3/kg)))  + # 上下标 xlab("") + scale_fill_nejm() + scale_colour_nejm() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, 1)) + # scale_y_continuous(expand = c(0,0)) +
  # stat_cor(aes(), label.x = 3) + geom_vline(aes(xintercept=8.0),linetype=4,col="red") +
  geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 4), se = T) + # ylab(bquote(TBV/BW(cm^3/kg))) +
  geom_point(aes(colour = Side, shape = Side, fill = Side), size = 2) +
  coord_trans(x = squash_axis(1, 99, 30)) # 局部压缩坐标轴
theme(
  axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
  axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
)
# stat_poly_eq(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula =  y ~ s(x, k = 4), parse = TRUE
# )
p2
# for循环
theme_set(theme_classic() + theme(legend.position = "bottom"))
plot_list <- list()
for (i in 4:ncol(dt)) {
  print(p <- ggplot(dt, aes_string(x = "Age", y = colnames(dt)[i], colour = "Side", fill = "Side", linetype = "Side")) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, 1)) +
    geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 4), se = T) +
    geom_point(aes(colour = Side, shape = Side, fill = Side), size = 2) +
    theme(
      axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
      axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
    ))
  plot_list[[i - 3]] <- p
}
# 拼图
# library(cowplot)
# combined_plot <- cowplot::plot_grid(plotlist = plot_list, align = "h", nrow = 4)
# legend <- get_legend(plot_list[[13]] + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
# plot_grid(combined_plot, legend,ncol=1,rel_heights = c(1, .1))

library("patchwork")
wrap_plots(plot_list, byrow = T, nrow = 4) + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

# Fit polynomial equation:
ggplot(data = dt, mapping = aes(x = Age, y = TBV, colour = Sex)) +
  geom_point() +
  stat_cor(aes(), label.x = 3) +
  geom_smooth(aes(fill = Sex), method = "lm", formula = y ~ poly(x, 3, raw = T), se = T)
# Polynomial regression. Sow equation and adjusted R2
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(data = dt, mapping = aes(x = Age, y = TBV, colour = Sex)) +
  # geom_point() +
  geom_smooth(aes(fill = Sex), method = "lm", formula = formula) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula, parse = TRUE
  ) -> p
ggpar(p, palette = "jco")

# [combine into single plot](https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/ggplot2.html)
library("patchwork")
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
  p11 + p12 + p13 + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

p1 + p2 + p3 + p4 + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")
ggsave("p.tiff", p, dpi = 600) # 保存为精度为600 dpi的tiff文件
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

# 组间比较
library(ggpubr)
library(ggsci)
# 根据变量分面绘制箱线图
dt$Group <- factor(dt$Side, levels = c("Left", "Right")) # 调整分面顺序
ggviolin(dt,
  x = "Side", y = "Frontal_Cortex", ylab = "Frontal_Cortex",
  color = "Side", palette = "jco", add = "jitter", xlab = "", legend = "right"
)

library(ggstatsplot)
ggbetweenstats(
  data = dt,
  x = Side,
  y = Frontal_Cortex,
  xlab = "",
  ylab = "Frontal_Cortex",
  plot.type = "violin",
  package = "ggsci",
  palette = "uniform_startrek"
) +
  ggtitle("") +
  ggeasy::easy_center_title()
p <- ggboxplot(dt,
  x = "Age", y = "Frontal_Cortex", ylab = "Frontal_Cortex",
  color = "Side", palette = "jco",
  add = "jitter", xlab = "", legend = "right",
  facet.by = "Side", short.panel.labs = FALSE
) + labs(color = "Side", shape = "Side")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", method = "t.test")
# Or use significance symbol as label
p + stat_compare_means(label = "p.signif", label.x = 1.5)
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
fml <- "Age ~ TBV"
my.formula <- y ~ s(x, k = 3)
my.formula <- y ~ x + I(x^2) + I(x^3)
dt$SUVr_whole_refPons
source("/home/wane/Documents/EP_code/git/Presentation/TBS/Monkey_fitting/get_cutoff_lm.R")
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

#######################################
# 限制性立方样条RCS-R语言代码实战
library(rms) # 限制性立方样条需要的包
library(survminer) # 曲线
library(ggplot2) # 画图
library(ggsci) # 调色板 作者：data小白 https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0 出处：bilibili
str(dt)
dt$Sex <- factor(dt$Sex)
dt$Side <- as.numeric(as.character(dt$Side, levels = c("Left", "Right"), labels = c("1", "2")))


dd <- datadist(dt) # 为后续程序设定数据环境
options(datadist = "dd") # 为后续程序设定数据环境
fit <- ols(TBV ~ rcs(Age, 5) + Sex, data = dt) # 做变量的回归
summary(fit)
an <- anova(fit)
an

Predict(fit, Age)
plot(Predict(fit, Age), anova = an, pval = T)
OLS1 <- Predict(fit, Age)

# RCS绘图，不分组
ggplot() +
  geom_line(data = OLS1, aes(Age, yhat), linetype = 1, size = 1, alpha = 0.9, colour = "grey") +
  geom_ribbon(data = OLS1, aes(Age, ymin = lower, ymax = upper), alpha = 0.3, fill = "grey") +
  theme_classic() +
  labs(title = "RCS", x = "Age", y = "Frontal_Cortex")

# 不同分组(男女组)之间HR与协变量变化关系
Pre1 <- rms::Predict(fit, Age, Sex = c("F", "M"), type = "predictions", ref.zero = F, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1) +
  geom_line(
    data = Pre1,
    aes(Age, yhat), alpha = 0.7
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre1,
    aes(Age, ymin = lower, ymax = upper), alpha = 0.1
  ) +
  scale_fill_nejm() +
  scale_colour_discrete(
    name = "Sex", breaks = c("F", "M"),
    labels = c("F", "M")
  ) +
  scale_shape_discrete(
    name = "Sex", breaks = c("F", "M"),
    labels = c("F", "M")
  ) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75)
View(Pre1)

# [非线性拟合及寻找数据拐点](https://www.jianshu.com/p/e8f8f7ec10d2)
