# RCS限制性立方样条(https://mp.weixin.qq.com/s?__biz=MzAxNTUzNDMyNQ==&mid=2650951130&idx=1&sn=a92eba32ee72b34b710f212fd8d1d4d5&chksm=80747c40b703f556c2f2272cdee38d090811fb4457bac8f07e2cfba41ffb2b80909fb3d0215a&mpshare=1&scene=1&srcid=0517CBgTAnM2HVCc8TyXNwwv&sharer_sharetime=1684290022685&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
rm(list = ls()) # 清除内存
pacman::p_load(rms, survival)
getwd()
library(haven)

dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\T1_TBV.csv", sep = ";", fileEncoding = "GBK")
dt <- dt[c(-1, -2, -3)]
str(dt)
dt$Sex <- factor(dt$Sex)
dt$Side <- as.numeric(as.character(dt$Side, levels = c("Left", "Right"), labels = c("1", "2")))
covs <- c("Sex", "Side")
# 设定数据环境
dd <- datadist(dt)
options(datadist = "dd")

# ========测试二分类 logistic为主分析，AIC最小 选择knot=3;lrm 测试logistic
## 获取 knot minAIC 方法1,=3
aics <- NULL
for (knot in 3:10) {
  formula <- paste0(
    "TBV ~ rcs(Age, ", knot, ")" # , " + ", paste0(covs, collapse=" + ")
  )
  model <- ols(as.formula(formula), data = dt, x = TRUE)
  summary(model)
  aics <- c(aics, AIC(model))
}
aics
seq(3, 10)[which.min(aics)]

# 方法2, 需要输入covs,nk=3
for (knot in 3:10) {
  fit <- lm(TBV ~ rcs(Age, knot), data = dt)
  tmp <- extractAIC(fit)
  if (knot == 3) {
    AIC <- tmp[2]
    nk <- 3
  }
  if (tmp[2] < AIC) {
    AIC <- tmp[2]
    nk <- knot
  }
}
nk # 5

# 获取非线性检验p， x=LnAl,knot=3
# y为二分类，lrm函数拟合：fit<- lrm(y ~  rcs(x1),data=data)
# y为连续变量，ols函数拟合模型：fit<-ols(y~ rcs(x1)
# y为 time相关随访结局，cph函数拟合

# 采用默认 knot=3，P10 P50 P90为位置参数
fit <- ols(TBV ~ rcs(Age, 3), data = dt, x = T, y = T)
anova(fit)

## 获得x的5%分位间隔的分位数，以此获得P5，P35，P50，P65，P95等等..分位
quantile(dt$Age, probs = seq(0, 1, 0.05))
# 采用自定义 knot=3，P20 P50 P80为位置参数
fit.LnAl <- ols(TBV ~ rcs(Age, c(6.50)), data = dt, x = T, y = T)
anova(fit.LnAl)
# p-non-linear  0.0107 或 0.0079 p<0.05, 非线性趋势
p <- round(anova(fit.LnAl)[, 3], 3)
p
Pre_OR.LnAl <- rms::Predict(fit.LnAl, Age, fun = exp, type = "predictions", ref.zero = F, conf.int = 0.95, digits = 2)
ggplot(Pre_OR.LnAl)
# y-hat =3.23 是OR=1切点
View(Pre_OR.LnAl)

# 对数据进行打包，并指定参考值默认median value=3.17; 本例value=3.23，OR=1，
# refvalue <- quantile(dt[, "x"], prob=0.5)
refvalue <- 3.23
ddist <- datadist(dt)
## 这里是设置参考点，也就是HR为1的点，常见的为中位数或者临床有意义的点
ddist$limits$LnAl[2] <- refvalue
options(datadist = "ddist")
# OR计算，fun是转化函数
Pre_OR.LnAl <- rms::Predict(fit.LnAl, LnAl, fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
ggplot(Pre_OR.LnAl)
# ========测试time数据  cox为主分析，AIC最小 选择knot=3; cph 测试cox
pacman::p_load(haven, smoothHR, survival, rms, Hmisc)
# 测试cox
# dtcox <- read_sav("D:/合作者/黄素丽/高血压重金属/rcscox.sav")
load(file = "dt.Rdata")
y <- "hypertension"
x <- "LnAl"
covs <- c("age", "BMI", "gender", "smoking", "drinking", "UA", "dm", "hyperlipidemia", "eGFR")
# dt 新增 xy
dt[, "y"] <- dt[, y]
dt[, "x"] <- dt[, x]
# 删除自变量x covs中，有缺失值的95行， 统计RCS用数据集 dt
sum(!complete.cases(dt[, c(y, x, covs)]))
dt <- dt[complete.cases(dt[, c(y, x, covs)]), ]
# test 数据集 dt需带time status

quantile(dt$LnAl, probs = seq(0, 1, 0.05))
# 获得x的5%分位间隔的分位数，以此获得5，35，50，65，95等等分位

# 对数据进行打包，寻找y-hat=1
ddist <- datadist(dt)
options(datadist = "ddist")
S <- Surv(dt$time, dt$status == 1)

# 获取多因素 cox分析 AIC 最小 knot, 单变量LnAl
for (knot in 3:10) {
  fit <- cph(S ~ rcs(LnAl, knot) + gender, data = dt, x = TRUE, y = TRUE, surv = TRUE)
  tmp <- extractAIC(fit)
  if (knot == 3) {
    AIC <- tmp[2]
    nk <- 3
  }
  if (tmp[2] < AIC) {
    AIC <- tmp[2]
    nk <- knot
  }
}
nk # 4

#---------基于cox绘制 单因素LnAl 的 RCS
pacman::p_load(rms, survminer, ggplot2, ggsci)
# 构建cph 函数获取rcs, 单指标LnAl；也可校正其他因素 S ~ rcs(LnAl,4)+ BMI 等
fit.LnAl <- cph(S ~ rcs(LnAl, 4) + gender, x = TRUE, y = TRUE, data = dt)
# PH 检验,P>0.05 符合PH假设
cox.zph(fit.LnAl, "rank")
# 残差图的横轴是时间，纵轴是残差，残差均匀分布则表示残差与时间相互独立，满足ph假设
ggcoxzph(cox.zph(fit.LnAl, "rank"))
# 非线性检验p-non-linear，0.9289
anova(fit.LnAl)
p <- round(anova(fit.LnAl)[, 3], 3)
## HR计算，fun是转化函数
Pre_HR.LnAl <- rms::Predict(fit.LnAl, LnAl, fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
ggplot(Pre_HR.LnAl)
# y-hat=1.00000, 对应LnAl= 3.16
View(Pre_HR.LnAl)

# 根据y-hat =1 调整ref，指定参考值对数据再次进行打包，OR=1
# refvalue <- 3.16
# ddist <- datadist(dt)
# ddist$limits$LnAl[2] <- refvalue
# options(datadist="ddist")
# S <- Surv(dt$time,dt$status==1)

# 全人群的COX模型 RCS绘图
rcs_all <- ggplot() +
  geom_line(
    data = Pre_HR.LnAl,
    aes(LnAl, yhat), # x y轴距
    linetype = "solid", # 曲线加粗
    size = 1,
    alpha = 0.7,
    colour = "purple"
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre_HR.LnAl,
    aes(LnAl, ymin = lower, ymax = upper, fill = "purple"), alpha = 0.1
  ) +
  theme_classic() +
  scale_fill_nejm() +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75) # y=1水平线
labs(
  title = "风险随LnAl变化曲RCS",
  x = "LnAl",
  y = "HR (95%CI)"
)
rcs_all


# 分性别的，分层人群的COX模型 RCS绘图；gender=1 男性，2女性
# 男性无拐点，女性拐点3.16
Pre1 <- rms::Predict(fit.LnAl, LnAl, gender = c("1", "2"), fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1)
View(Pre1)
### ---直方图+平滑曲线----
## HR计算，fun是转化函数；承接上述fit.LnAl
Pre0 <- rms::Predict(fit.LnAl, LnAl, fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)

par(mar = c(3, 4, 1, 5), new = T)
# mar是设置图形边界空白宽度，四个数值分别代表，下左上右的边界宽度，取值单位是线条宽度lwd 。new=T表示可以添加新的图层。
##### -----------------------------
col <- c("#5BA5DA", "#F06955", "#FDC059", "skyblue", "blue") ## 蓝 红 黄
hist(dt$LnAl,
  axes = F, xlab = "", ylab = "",
  xlim = c(1.5, 4.5),
  col = col[4], main = "", freq = T
)
axis(4) ## 加右边Y轴,1，2，3，4分别代表下左上右
par(new = T) ## 创建新的画布，类似于ggplot2中的图层叠加
plot(Pre0[, 1],
  axes = T, Pre0$yhat, type = "l", lty = 1, lwd = 2,
  # Plot中type绘制散点图的类型常用的6种类型，其中“p”表示绘制散点；“b”表示点连线；
  # “l”表示绘制线图；“s”表示绘制阶梯；“o”表示绘制点连线，此处线条穿过每一个点，与“b”类型稍有不同
  # “n”表示不绘制任何图形元素。其中s就是绘制生存曲线KM的阶梯线图
  col = col[2], xlim = c(1.5, 4.5),
  ylim = c(0, 3), xlab = "LnAl", ylab = "HR(95%CI)"
)
lines(Pre0[, 1], Pre0$lower, type = "l", lty = 2, lwd = 2, col = col[2])
lines(Pre0[, 1], Pre0$upper, type = "l", lty = 2, lwd = 2, col = col[2])
mtext("freq", side = 4, line = 3, outer = F)

## 图形添加散点
points(
  x = ddist$limits$LnAl[2], ## 散点X轴坐标
  y = 1, #### 散点y轴坐标
  pch = 19, ## pch是点的形状，数字从1-25代表不同的点形状。
  col = "red"
)
Text <- paste("Ref=", round(ddist$limits$LnAl[2], 3), sep = "")
## 图像添加文字，这里添加参考点
text(Text, x = ddist$limits$LnAl[2] + 5, y = 0.5, cex = 1, font = 3)
# abline(h=1,col='black')

outfile <- "fig_rcs.pdf"
pdf(outfile, onefile = FALSE, width = 15 / 2.54, height = 15 / 2.54)
plot


##### 直方图+平滑曲线+置信区间填充
col <- c("#E0E7F2", "#EE947B")
par(mar = c(4, 4, 1, 5))
##### -----------------------------
hist(dt$LnAl,
  axes = F, xlab = "", ylab = "",
  # ylim=c(0,0.035),
  xlim = c(1.5, 4.5),
  # 两个plot的X轴范围要保持一致，保持对齐
  col = col[1], main = "", freq = T, border = "gray"
)
axis(4) ## 加右边Y轴
par(new = T) ## 创建新的画布，类似于ggplot2中的图层叠加
plot(Pre0[, 1],
  axes = T, Pre0$yhat, type = "l",
  lty = 1, lwd = 2,
  col = col[2],
  xlim = c(1.5, 4.5),
  ylim = c(0, 5), xlab = "LnAl", ylab = "HR(95%CI)"
)
c <- rgb(1, 0, 0, alpha = 0.15)
polygon(c(Pre0[, 1], rev(Pre0[, 1])),
  c(Pre0$lower, rev(Pre0$upper)),
  col = c, border = c
)
mtext("freq", side = 4, line = 3, outer = F)

## 图形添加散点
points(
  x = ddist$limits$LnAl[2], ## 散点X轴坐标
  y = 1, #### 散点y轴坐标
  pch = 19, ## pch是点的形状，数字从1-25代表不同的点形状。
  col = "red"
)
abline(h = 1, col = "black", lty = 2)


outfile <- "fig_rcs.pdf"
pdf(outfile, onefile = FALSE, width = 15 / 2.54, height = 15 / 2.54)
plot
dev.off()

outfile <- "fig_rcs.tiff"
tiff(outfile, width = 15, height = 15, unit = "cm", res = 600, compression = "lzw+p")
plot
dev.off()

### 1. https://www.nature.com/articles/s41409-019-0679-x.











