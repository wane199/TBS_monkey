# [Generalized additive model for Location, Shape and Scale](https://rpubs.com/Saravanaraj_K/gamlss_explanation)
# plotSimpleGamlss函数 https://mp.weixin.qq.com/s?__biz=Mzg3MDgyMTgxMg==&mid=2247483688&idx=1&sn=6d805f16d094b5465aafe56e9ec32d58&chksm=ce86bbf3f9f132e50bb4b68a8edef8d8e283d6e230ca5e8f610dd683604be41ee07265090aa7&mpshare=1&scene=1&srcid=0912Hyxqs78INjOgSU3KxHIU&sharer_sharetime=1662997861161&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
library(gamlss)
library(gamlss.util)

dt <-
  read.csv("/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/QIANG/PartⅠ猴脑体积发育数据分析/Gender.csv")
dt <-
  read.csv("/home/wane/Desktop/TBS&Mon/Monkey/QIANG/PartⅠ猴脑体积发育数据分析/L&R.csv")
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
summary(dt)
str(dt)

# 构建LMS模型（该模型为GAMLSS方法下的一个特例）
m0a <- lms(
  Frontal_Cortex,
  Age,
  data = dt,
  trans.x = TRUE,
  k = 3,
  families = "BCTo"
) # 自由度选择2，方法为"BCCGo", "BCPEo", "BCTo"
m0a
m1 <- lms(
  y = Frontal_Cortex,
  x = Age,
  data = dt,
  trans.x = TRUE
)
m1 <- lms(
  y = Frontal_Cortex,
  x = Age,
  data = dt,
  n.cyc = 30
)
m2 <-
  lms(
    y = Frontal_Cortex,
    x = Age,
    data = dt,
    method.pb = "GAIC",
    k = log(610)
  )
mDEG_zip <- gamlss(
  formula = deg ~ pb(SE_score) + TL + species + sex + season +
    year + re(random = ~ 1 | code) + re(random = ~ 1 | station),
  family = ZIP(),
  data = node_dat,
  control = gamlss.control(n.cyc = 200)
)
m <-
  gamlss(Frontal_Cortex ~ cs(Age, df = 3),
    family = BCT,
    data = dt
  )
plot(m)
# 分位数曲线，没有给出图例，因为没找到更改图例位置的参数，legend=TRUE有图例，ylim限定了y轴范围
centiles(
  m,
  dt$Age,
  cent = c(1, 3, 5, 10, 25, 50, 75, 90, 95, 97, 99),
  legend = FALSE,
  ylab = "Frontal_Cortex",
  xlab = "Age",
  main = "",
  ylim = c(0.08, 0.12)
)

gamlss(
  Frontal_Cortex ~ cs(Age, df = 3),
  sigma.fo = log(Age),
  nu.fo = ~1,
  tau.fo = ~1,
  family = BCT,
  data = dt
)


Output1 <- gamlss(
  Frontal_Cortex ~ Age,
  sigma.fo = ~Age,
  nu.fo = ~1,
  family = BCCGo,
  data = dt
)
Output2 <- gamlss(
  Frontal_Cortex ~ Age,
  sigma.fo = ~Age,
  nu.fo = ~Age,
  family = BCCGo,
  data = dt
)
AIC(Output1, Output2)
plot(Output1)
op <- par(mfrow = c(1, 2))
wp(Output1)
title(" (c) BCCG(mu, sigma)")
wp(Output2)
title(" (d) BCCG(mu, sigma, nu)")
par(op)

# 绘制平滑曲线，并添加数据分布热图
plot <- plotSimpleGamlss(
  Frontal_Cortex,
  Age,
  m1,
  data = dt,
  x.val = seq(0, 30, 2),
  # 定义数据分组
  xlim = c(0, 30),
  # 横轴的起止点为-2，23
  ylim = c(0, 0.15),
  val = 200
) # 纵轴的起止点为0，500

# 图像另存为
tiff(
  file = "Frontal_Cortex.tiff",
  res = 1400,
  width = 8800,
  height = 8800,
  compression = "lzw"
)
plot
dev.off()

mod <- gamlss(
  Frontal_Cortex ~ pb(Age),
  sigma.fo = ~ pb(Age),
  family = BCT,
  data = dt,
  method = mixed(1, 20)
)
plot(mod)
# rm(mod)
plot <- plotSimpleGamlss(
  Frontal_Cortex,
  Age,
  mod,
  data = dt,
  x.val = seq(0, 30, 2),
  # 定义数据分组
  xlim = c(0, 30),
  # 横轴的起止点为-2，23
  ylim = c(0.075, 0.115),
  val = 50
) # 纵轴的起止点为0，500

z.scores(mod, x = c(2, 15, 30, 40), y = c(45, 50, 56, 63))



# Quantile Generalized Additive Models: moving beyond Gaussianity - Part 2
# https://www.youtube.com/watch?v=44UUBuF9RFM
library(mgcViz)
library(gamlss.data)
data(grip)

plot(grip ~ log(age), data = grip)
log(exp(3))
log10(1e7) # = 7

x <- 10^-(1 + 2 * 1:9)
cbind(x, log(1 + x), log1p(x), exp(x) - 1, expm1(x))

# fit a standard Gaussian GAM
fit1 <- gamV(grip ~ s(age),
  data = grip,
  aViz = list(nsim = 50)
)

check1D(fit1, "age") + l_gridCheck1D(sd) # stan dev
check1D(fit1, "age") + l_densCheck()


# using a gaulss model
fit2 <- gamV(
  list(grip ~ s(age), ~ (age)),
  data = grip,
  family = gaulss,
  aViz = list(nsim = 50)
)

check1D(fit2, "age") + l_gridCheck1D(sd) # stan dev

# residual skewness(asymmetry)
library(e1071)
check1D(fit2, "age") + l_gridCheck1D(skewness) # stan dev

# shash model
library(mgcFam)
fit3 <- gamV(
  list(grip ~ s(age), ~ (age), ~ (age), ~1),
  data = grip,
  family = shash,
  aViz = list(nsim = 50)
)

check1D(fit3, "age") + l_gridCheck1D(skewness) # stan dev

AIC(fit1, fit2, fit3)


# Quantile modeling:
library(mgcViz)
library(SemiPar)
data(age.income)
age.income$income <- exp(age.income$log.income)

fitQ <- qgamV(income ~ s(age), data = age.income, qu = 0.5)

# predict and plot
ft <- predict(fitQ, se = T)

ord <- order(age.income$age)
plot(
  age.income$age,
  age.income$income,
  xlab = "Age",
  las = 1.0,
  ylab = "Income (CAD)",
  col = "grey"
)
lines(age.income$age[ord], ft$fit[ord], col = 2)
lines(age.income$age[ord],
  (ft$fit + 2 * ft$se.fit)[ord],
  col = 2,
  lty = 2
)
lines(age.income$age[ord],
  (ft$fit - 2 * ft$se.fit)[ord],
  col = 2,
  lty = 2
)


fitQ <-
  mqgamV(income ~ s(age),
    data = age.income,
    qu = c(0.1, 0.25, 0.5, 0.75, 0.9)
  )
plot(fitQ)
plotRGL(fitQ)
plot(
  age.income$age,
  age.income$income,
  xlab = "Age",
  las = 1.0,
  ylab = "Income (CAD)",
  col = "grey"
)

for (ii in 1:5) {
  ft <- predict(fitQ[[ii]], se = TRUE)
  lines(age.income$age[ord], ft$fit[ord], col = 1)
}

summary(fitQ[[1]])







library(gamlss)
data(abdom)
h1 <-
  gamlss(y ~ cs(x), family = LOGNO, data = abdom) # fits the log-Normal distribution
h2 <-
  gamlss(y ~ cs(x), family = LNO, data = abdom) # should be identical to the one above
# to change to square root transformation, i.e. fix nu=0.5
h3 <-
  gamlss(
    y ~ cs(x),
    family = LNO,
    data = abdom,
    nu.fix = TRUE,
    nu.start = 0.5
  )
