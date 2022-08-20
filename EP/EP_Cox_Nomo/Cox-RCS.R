# 医学统计与R语言：绘制Cox回归模型中的样条曲线（spline）https://mp.weixin.qq.com/s?__biz=MzIzMzc1ODc4OA==&mid=2247485639&idx=1&sn=0b8f2e37e2b705b6fe2457270c660d2b&chksm=e88181ecdff608fa662afd60f61377be66534fdd6506d9b9db80dbb3419cb6c57df75d27cb11&scene=132#wechat_redirect
# 生存分析之限制性立方样条(RCS)
rm(list = ls())
dt <- read.csv("data.csv")
library(rms) # RCS
library(survminer) # 曲线
library(ggplot2) # 画图
library(ggsci) # 调色板

# 设定数据环境
dd <- datadist(train)
options(datadist = "dd")
str(train)
S <- Surv(train$Follow_up_timemon, train$Rel._in_5yrs == 1)

## rcs
fit <- cph(S ~ rcs(Durmon, 3) + SE, x = TRUE, y = TRUE, data = train)
cox.zph(fit, "rank") # tests of PH
ggcoxzph(cox.zph(fit, "rank")) # 可视化等比例假定
anova(fit) # 非线性检验
## 注意：这里的R命令是“cph”，而不是常见的生存分析中用到的“coxph"
## Tips：若因变量为二分类变量，改用lrm函数拟合模型：fit<- lrm(y ~  rcs(x1),data=data)；若因变量为连续变量，改用ols函数拟合模型：fit<-ols(y~ rcs(x1)
## 2.检验比例风险假设-PH假设
## Cox比例风险模型构建的一个前提条件是比例风险(PH)的假定，可以通过假设检验和Schoenfeld残差图检验，前者P>0.05,表示满足假设检验，后者残差图的横轴是时间，纵轴是残差，如果残差与时间有相关趋势，则违反PH假定，如果残差均匀分布则表示残差与时间相互独立，满足假设。

## 不分组/总体
Pre0 <- rms::Predict(fit, Durmon, fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
ggplot(Pre0,ylab="logRR(95%CI)") + theme_classic()
View(Pre0)

## 分组展示
Pre1 <- rms::Predict(fit, Durmon, SE = c("0", "1"), fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1,ylab="logRR(95%CI)") + theme_classic()
View(Pre1)

## 自定义ggpot作图
## 不分组
ggplot() +
  geom_line(
    data = Pre0,
    aes(Durmon, yhat, colour = SE), # xy轴的数据
    linetype = 1,
    alpha = 0.7,
    size = 1
  ) +
  # scale_colour_manual(values=col)+
  scale_color_nejm() +
  geom_ribbon(
    data = Pre0,
    aes(Durmon,
      ymin = lower, ymax = upper,
      fill = SE
    ),
    alpha = 0.1
  ) +
  # scale_fill_manual(values=col)
  scale_fill_nejm() +
  # theme_classic()+
  geom_hline(yintercept = 1, linetype = 2, size = 0.75) +
  labs(
    title = "",
    x = "radcore",
    y = "HR(95%CI)"
  )
col <- c("darkcyan", "tomato", "purple")
### 分组做
ggplot() +
  geom_line(
    data = Pre1,
    aes(Durmon, yhat, colour = SE, group = SE), # xy轴的数据
    linetype = 1,
    alpha = 0.7,
    size = 1
  ) +
  # scale_colour_manual(values=col)+
  scale_color_nejm() +
  geom_ribbon(
    data = Pre1,
    aes(Durmon,
      ymin = lower,
      ymax = upper,
      fill = SE,
      group = SE
    ),
    alpha = 0.1
  ) +
  # scale_fill_manual(values=col)
  scale_fill_nejm() +
  # theme_classic()+
  geom_hline(yintercept = 1, linetype = 2, size = 0.75) +
  labs(
    title = "",
    x = "Durmon",
    y = "HR(95%CI)"
  )

## 绘制男性组生存率随时间的变化曲线
survplot(fit,
  SE = "1",
  col = "red",
  conf.int = .95,
  conf = "bars",
  xlab = "months"
)
#### 分别绘制男性组和女性组生存率随时间的变化曲线
survplot(fit,
  SE = c("1", "0"),
  col = 2:3,
  conf.int = .95,
  conf = "bars",
  xlab = "months"
)

###########------
# Cox模型:连续变量计算最佳阈值
# 计算拟合值logRR
fit <- coxph(S ~ rcs(Durmon, 3) + SE, data = train)

pred<-predict(fit,type="terms",se.fit=TRUE)
mfit <- pred$fit[,"rcs(Durmon, 3)"]
sfit <- pred$se.fit[,"rcs(Durmon, 3)"]
train<-cbind(train,'yhat'=mfit,'se'= sfit)

# 计算95%CI
train$lower <- train[,'yhat']-1.96*train[,'se']
train$upper <- train[,'yhat']+1.96*train[,'se']
train<-train[order(train[,"Durmon"]),]

# plot作图
co <- c(65,162,-2,4)
col = c("darkcyan","tomato","purple")  ##制作一个颜色变量
plot(train$Durmon,train$yhat,col=col[1],type="l",lty=1,pch=2, ylab="", xlab="")
par(new=TRUE); 
plot(train$Durmon,train$lower,col=col[2],type="l",lty=2,ylab="", xlab="")
par(new=TRUE); 
plot(train$Durmon,train$upper,col=col[2],type="l",lty=2, ylab='Log RR', xlab='Durmon')

# 
before.cox.curve.1 <- coxph(S ~ rcs(radscore, 3), data = train, x = T)
# pspline()拟合惩罚性样条曲线
before.cox.curve.1
library(smoothHR)
hr1 <- smoothHR(data=train, coxfit= before.cox.curve.1)
print(hr1)
plot(hr1,predictor="radscore",prob=0,conf.level=0.95,ref.label="radscore")

Pre0 <- rms::Predict(before.cox.curve.1, radscore, fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
plot(Pre0, predictor="radscore",prob=0,conf.level=0.95,ref.label="radscore",
     col=c("red","blue","green"), 
     #分别设置拟合曲线、95%置信线和区域范围的颜色
     xlab="radscore",ylab="调整的HR自然对数",main="")

hist(train$radscore,  
     breaks=30,
     col="peachpuff",  
     border="black",
     prob = T,ylab="",yaxt = "n")#隐去X、Y轴标签和刻度

library(Greg)
par(new=TRUE)     #继续在原来图上加图形
plotHR(before.cox.curve.1, term = "radscore",  #指定目标变量
       xlim = c(2,10), 
       xlab = "K", #X轴标签
       ylab="log(Hazard Ratio)", #Y轴标签
       rug = "ticks", #目标变量密度(density)或抖动图(ticks)
       ylog=T, # Y轴对数值（建议）
       col.term =  "red", #曲线颜色
       lwd.term = 3, #曲线粗细
       lwd.se=2,    #置信线粗细
       lty.term="solid",#曲线样式
       lty.se="dashed",#置信线样式
       polygon_ci = F,#不显示置信区间多边形
       col.se ="blue", #置信线颜色
       alpha=0.05,#95%CI
       cex = 1.2)#字体大小

##################----------------
# 连续变量阈值效应分析
library(survival)
library(rms)
dt <- read.csv("jixian.csv")
## ---阈值效应分析--
## 计算拐点
source("get_cutoff_cox.R")
fml <- "Surv(time,status)~Hb+gender+age"
cut_off <- get_cutoff_cox("Durmon", dt, fml)
cut_off # 定义临界点

### 计算拐点95%置信区间
source("get_cutoff_ci.R")
get_cutoff_ci("Hb", dt, fml, cut_off)
## 生成分段变量
x <- dt[, "Hb"]

X1 <- (x <= cut_off) * (x - cut_off)
X2 <- (x > cut_off) * (x - cut_off)
dt <- cbind(dt, X1, X2)

fit0 <- coxph(Surv(time, status) ~ Hb + age + factor(gender), data = dt)
fit1 <- coxph(Surv(time, status) ~ Hb + X2 + age + factor(gender), data = dt)
fit2 <- coxph(Surv(time, status) ~ X1 + X2 + age + factor(gender), data = dt)
Coef(fit0)
Coef(fit1)
Coef(fit2)
###

Coef <- function(Obj) {
  coef <- summary(Obj)
  coef <- data.frame(coef$coefficients, coef$conf.int[, 3:4])
  colnames(coef) <- c("coef", "exp(coef)", "se(coef)", "z", "P", "exp(B)95%CIlower", "exp(B)95%CIUpper")
  coef <- round(coef, 3)
  return(coef)
}

### 对数似然比检验，下面两种方法都可以做。
##  method1
plrt <- 1 - pchisq(2 * (logLik(fit2)[1] - logLik(fit0)[1]), 1)
plrt
##  method2
anova(fit0, fit2)$"P(>|Chi|)"[2]
