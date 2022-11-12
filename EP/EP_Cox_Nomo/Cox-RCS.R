# 医学统计与R语言：绘制Cox回归模型中的样条曲线（spline）https://mp.weixin.qq.com/s?__biz=MzIzMzc1ODc4OA==&mid=2247485639&idx=1&sn=0b8f2e37e2b705b6fe2457270c660d2b&chksm=e88181ecdff608fa662afd60f61377be66534fdd6506d9b9db80dbb3419cb6c57df75d27cb11&scene=132#wechat_redirect
# 生存分析之限制性立方样条(RCS)，剂量反应关系图
# [Cox比例风险模型的假设检验条件](https://zhuanlan.zhihu.com/p/164668320)
rm(list = ls())
options(digits=3) # 限定输出小数点后数字的位数为3位
dt <- read.csv("data.csv")
library(rms) # RCS
library(survminer) # 曲线
library(ggplot2) # 画图
library(ggsci) # 调色板
library(ggrcs)
library(scales)

# 设定数据环境
dd <- datadist(train)
options(datadist = "dd")

attr(train, "tcount")
str(train)
train$Rel._in_5yrs <- as.factor(train$Rel._in_5yrs)
train$Lat_radscore <- as.numeric(as.character(train$Lat_radscore))

# Cox比例风险模型的假设检验条件(https://mengte.online/archives/3900)
# 共线性诊断
# library(car)
lm.reg<-lm(Rel._in_5yrs ~ AI_radscore + Lat_radscore + SGS + Durmon, data=train) #线性回归分析
vif(lm.reg) #计算vif

fit <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ AI_radscore + Lat_radscore + SGS + Durmon, 
             data = train) # , tt = function(x, t, ...) x*log(t+18)
model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ AI_radscore + Lat_radscore + SGS + Durmon + tt(Lat_radscore), 
              tt=function(x,t,...) x*log(t+18), data = train) # , tt = function(x, t, ...) x*log(t+18)
summary(fit1) 
cox.zph(fit, "rank") # tests of PH
cox.zph(fit, transform = function(time) log(time)) # tests of PH
zph <- cox.zph(fit, "rank")[[1]] # 检验结果导出(https://blog.csdn.net/yijiaobani/article/details/83116578)
DT::datatable(zph)
library(xtable)
library(flextable)
set_flextable_defaults(digits = 3)
m1 = xtable_to_flextable(xtable(zph))
m1
library(officer)
doc = read_docx()
doc = body_add_flextable(doc,m1)
print(doc,"./EP/EP_Cox_Nomo/supple_zph.docx")
write.table(zph,"./EP/EP_Cox_Nomo/supple_zph.csv",sep=",") #result是想保存的变量

ggcoxzph(cox.zph(fit, "rank")) # 可视化等比例假定
# plot(cox.zph(fit)) # 分图展示
res_martingale <- residuals(fit, type = "martingale") # dfbetas, score, deviance, partial, schoenfeld
scatter.smooth(Lat_radscore, res_martingale) # https://www.youtube.com/watch?v=4Edu6Ij7jEM

ggcoxdiagnostics(fit, type = "scaledsch", ox.scale = "linear.predictions")
ggcoxdiagnostics(fit,
  type = "dfbeta", # type = "deviance",“ martingale”，“ score”，“ schoenfeld”，“ dfbeta”，“ dfbetas”，“ scaledsch”，“ partial”
  linear.predictions = FALSE, ggtheme = theme_bw()
)  # 检查异常值, Anomaly Detection
ano <- anova(fit) # 非线性关系P-Nonlinear<0.05为存在非线性关系
library(broom)
tidy(zph)
library(texreg)
screenreg(fit, custom.model.names = "coxphmodel", digits = 3, single.row = T, ci.force = T)
htmlreg(fit, file = "texreg.doc",
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE)

## 注意：这里的R命令是“cph”(rms包)，而不是常见的生存分析中用到的“coxph"(survival包)。
## Tips：若因变量为二分类变量，改用lrm函数拟合模型：fit<- lrm(y ~  rcs(x1),data=data)；若因变量为连续变量，改用ols函数拟合模型：fit<-ols(y~ rcs(x1)
## 2.检验比例风险假设-PH假设
## Cox比例风险模型构建的一个前提条件是比例风险(PH)的假定，可以通过假设检验和Schoenfeld残差图检验，前者P>0.05,表示满足假设检验，后者残差图的横轴是时间，纵轴是残差，如果残差与时间有相关趋势，则违反PH假定，如果残差均匀分布则表示残差与时间相互独立，满足假设。
res.cox <- coxph(S ~ radscore + I(radscore^2), data = train)
ggcoxfunctional(res.cox, data = train, point.col = "blue", point.alpha = 0.5) # 检测对数风险值与协变量之间关系的非线性情况,仅针对连续变量绘制martingale残差图和部分残差图。
ggcoxfunctional(S ~ radscore + sqrt(radscore), data = train)

# 非比例风险的Cox回归模型--时依系数法(https://zhuanlan.zhihu.com/p/420063750)







# chest包自动计算效应改变量change-in-estimate(https://mp.weixin.qq.com/s?__biz=MzIzMzc1ODc4OA==&mid=2247485666&idx=1&sn=f28aff82d66af79d099f237e8e302f89&chksm=e88181c9dff608df3a55a831f6d7dbcce32a29cb1f52ae16bf994c4585b06a52d1013084af29&mpshare=1&scene=24&srcid=1108Gz1ANkBpbNLK03j9kR93&sharer_sharetime=1667838586772&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(chest)
? chest_cox
vlist <- c("Lat_radscore", "SGS" ,"Durmon")
results <- chest_cox(crude = "Surv(Follow_up_timemon, Rel._in_5yrs == 1) ~ AI_radscore", xlist = vlist, data = train)
chest_plot(results)
chest_forest(results)  

# rcssci()
library(rcssci)
rcssci_cox(data=sbpdata, y = "status",x = "sbp",covs = c("age", "gender"), time = "time", 
           prob=0.1, filepath = "/Users/mac/Desktop/BLS-ep-pre/EP/sci/cph/")
## rcs, ggrcs包，一个用于绘制直方图+限制立方样条+双坐标轴图的R包
S <- Surv(train$Follow_up_timemon, train$Rel._in_5yrs == 1)
fit <- cph(S ~ AI_radscore + Lat_radscore + SGS + Durmon, x = TRUE, y = TRUE, data = train)
fit
ggrcs(data=train,fit=fit,x="Lat_radscore", histbinwidth = 0.05, histcol="blue", ribcol="green",
      histlimit=c(0,50),leftaxislimit=c(0,1),lift = T)

## 不分组/总体
Pre0 <- rms::Predict(fit, radscore, fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
ggplot(Pre0, ylab = "logRR(95%CI)") +
  theme_classic() +
  geom_hline(aes(yintercept = 1), linetype = "dashed", colour = "red")
View(Pre0)

## 分组展示
Pre1 <- rms::Predict(fit, radscore, SGS = c("0", "1"), fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1, ylab = "logRR(95%CI)") +
  theme_classic() +
  geom_hline(aes(yintercept = 1), linetype = "dashed", colour = "red")
View(Pre1)

## 自定义ggpot作图
## 不分组
str(train)
ggplot() +
  geom_line(
    data = Pre0,
    aes(radscore, yhat, colour = factor(SGS)), # xy轴的数据
    linetype = 1,
    alpha = 0.7,
    size = 1
  ) +
  # scale_colour_manual(values=col)+
  scale_color_nejm() +
  geom_ribbon(
    data = Pre0,
    aes(radscore,
      ymin = lower, ymax = upper,
      fill = factor(SGS)
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

# 基于Cox+限制性立方样条：密度曲线+平滑曲线双坐标轴超详细绘图 https://zhuanlan.zhihu.com/p/444672731
# https://blog.csdn.net/Tuo1uo/article/details/121267836
fit <- cph(S ~ rcs(radscore,3), x=TRUE, y=TRUE,data=train)
Pre0<-rms::Predict(fit,radscore,fun=exp,type="predictions",ref.zero=T,conf.int = 0.95,digits=2)
par(mar = c(5, 4, 4, 5)) # 说明见下,BLTR
col <- c("#5BA5DA", "#F06955", "#FDC059", "skyblue", "blue") ## 颜色变量 col <- c("darkcyan", "tomato", "purple")
plot(density(train$radscore), ### 密度估计曲线
  axes = F, ## 不展示坐标轴，这是下面添加坐标轴的前提
  xlab = "", ylab = "", ## X轴和Y轴标签
  las = 1, ylim = c(0, 1.5), ## Y轴刻度范围
  xlim = c(-1.0, 1.5),
  type = "l", # 说明见下
  lty = 2, ## 线条样式 取值0，1，2，分别是不划线；实线；虚线；
  lwd = 2, ## 线条宽度 默认是1
  col = col[4], # 颜色
  main = ""
) ## 标题
# axis(side=2,col='red')
## 绘制图形填充颜色
polygon(density(train$radscore),angle = c(-45, 45), lty = c("dashed"),##### 密度估计曲线
  col = col[4], ## 填充颜色
  border = col[1]
) ## 边界颜色
## 添加坐标轴
axis(4, las = 1) ## 加右边Y轴；说明见下
par(new = T) ## 创建新的画布，类似于ggplot2中的图层叠加
### 叠加平滑曲线
plot(Pre0[, 1], Pre0$yhat, las = 1,
  axes = T, type = "l", lty = 1, lwd = 2, col = col[2],
  ylim = c(0, 5.0), ## Y轴刻度范围
  xlim = c(-1.0, 1.5), ## 注意刻度范围要和上面保持一致
  xlab = "radscore", ylab = "HR(95%CI)"
)
## 叠加平滑曲线的95%直线区间
lines(Pre0[, 1], Pre0$lower, type = "l", lty = 2, lwd = 2, col = col[2])
lines(Pre0[, 1], Pre0$upper, type = "l", lty = 2, lwd = 2, col = col[2])
abline(h = 1.0, lty = 3, lwd = 2, col = 'lightgray')
## 图形周边添加文本
mtext("density", side = 4) ## 取值1，2，3，4分别代表下左上右，本例右边添加文本
## 图形添加散点
points(
  x = dd$limits$radscore[2], ## 散点X轴坐标
  y = 1, #### 散点y轴坐标
  pch = 19, ## pch是点的形状，数字从1-25代表不同的点形状。
  col = "red"
)
Text <- paste("Ref=", round(dd$limits$radscore[2], 3), sep = "")
## 图像添加文字，这里添加参考点及箭头 ggannotate包
text(Text, x = dd$limits$radscore[2], y = 0.5, cex = 1)
arrows(0.183, 0.70, 0.183, 0.95, col = 'black', length = 0.14, angle = 30,
       code = 2)


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

############# --------------
# Cox模型:连续变量计算最佳阈值
# 计算拟合值logRR
fit <- coxph(S ~ rcs(Durmon, 3) + SE, data = train)

pred <- predict(fit, type = "terms", se.fit = TRUE)
mfit <- pred$fit[, "rcs(Durmon, 3)"]
sfit <- pred$se.fit[, "rcs(Durmon, 3)"]
train <- cbind(train, "yhat" = mfit, "se" = sfit)

# 计算95%CI
train$lower <- train[, "yhat"] - 1.96 * train[, "se"]
train$upper <- train[, "yhat"] + 1.96 * train[, "se"]
train <- train[order(train[, "Durmon"]), ]

# plot作图
co <- c(65, 162, -2, 4)
col <- c("darkcyan", "tomato", "purple") ## 制作一个颜色变量
plot(train$Durmon, train$yhat, col = col[1], type = "l", lty = 1, pch = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(train$Durmon, train$lower, col = col[2], type = "l", lty = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(train$Durmon, train$upper, col = col[2], type = "l", lty = 2, ylab = "Log RR", xlab = "Durmon")


# 绘制Cox回归模型中的样条曲线(spline)
before.cox.curve.1 <- coxph(S ~ pspline(radscore), data = train, x = T)
# pspline()拟合惩罚性样条曲线
before.cox.curve.1
library(smoothHR)
hr1 <- smoothHR(data = train, coxfit = before.cox.curve.1)
print(hr1)
plot(hr1, predictor = train$radscore, prob=0,conf.level=0.95,ref.label="radscore",
     col=c("red","blue","green"), 
     #分别设置拟合曲线、95%置信线和区域范围的颜色
     xlab="radscore",ylab="调整的HR自然对数",main="")

Pre0 <- rms::Predict(before.cox.curve.1, radscore, fun = exp, type = "predictions", ref.zero = TRUE, conf.int = 0.95, digits = 2)
plot(Pre0,
  predictor = "radscore", prob = 0, conf.level = 0.95, ref.label = "radscore",
  col = c("blue", "green"),
  # 分别设置拟合曲线、95%置信线和区域范围的颜色
  xlab = "radscore", ylab = "调整的HR自然对数", main = ""
)

hist(train$radscore,
  breaks = 30,
  col = "peachpuff",
  border = "black",
  prob = T, ylab = "", yaxt = "n"
) # 隐去X、Y轴标签和刻度

library(Greg)
par(new = TRUE) # 继续在原来图上加图形
plotHR(before.cox.curve.1,
  term = "radscore", # 指定目标变量
  xlim = c(2, 10),
  xlab = "K", # X轴标签
  ylab = "log(Hazard Ratio)", # Y轴标签
  rug = "ticks", # 目标变量密度(density)或抖动图(ticks)
  ylog = T, # Y轴对数值（建议）
  col.term = "red", # 曲线颜色
  lwd.term = 3, # 曲线粗细
  lwd.se = 2, # 置信线粗细
  lty.term = "solid", # 曲线样式
  lty.se = "dashed", # 置信线样式
  polygon_ci = F, # 不显示置信区间多边形
  col.se = "blue", # 置信线颜色
  alpha = 0.05, # 95%CI
  cex = 1.2
) # 字体大小

####################################################
# 连续变量阈值效应分析
library(survival)
library(rms)
dt <- read.csv("jixian.csv")
## ---阈值效应分析--
## 计算拐点
source("/home/wane/Documents/EP_code/git/Presentation/EP/EP_Cox_Nomo/get_cutoff_cox.R")
fml <- "S~radscore"
cut_off <- get_cutoff_cox("radscore", train, fml)
cut_off # 定义临界点

### 计算拐点95%置信区间
source("get_cutoff_ci.R")
get_cutoff_ci("radscore", train, fml, cut_off)
## 生成分段变量
x <- train[, "radscore"]

X1 <- (x <= cut_off) * (x - cut_off)
X2 <- (x > cut_off) * (x - cut_off)
dt <- cbind(dt, X1, X2)

fit0 <- coxph(Surv(time, status) ~ Hb + age + factor(gender), data = dt)
fit1 <- coxph(Surv(time, status) ~ Hb + X2 + age + factor(gender), data = dt)
fit2 <- coxph(Surv(time, status) ~ X1 + X2 + age + factor(gender), data = dt)
Coef(fit0)
Coef(fit1)
Coef(fit2)

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


##################################################
library(ggrcs)
library(rms)
library(ggplot2)
library(scales)
dt<-smoke
dd<-datadist(dt)
options(datadist='dd')
fit<- cph(Surv(time,status==1) ~ rcs(age,4)+gender, x=TRUE, y=TRUE,data=dt)
###single group
ggrcs(data=dt,fit=fit,x="age")
##two groups
ggrcs(data=dt,fit=fit,x="age",group="gender") + xlab('Age(years)') + 
    theme(axis.text = element_text(size = 10, face = "bold"), axis.ticks.length=unit(-0.25, "cm"), 
                                                        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                                                        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))



