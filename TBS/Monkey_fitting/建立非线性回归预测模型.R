# https://zhuanlan.zhihu.com/p/101906049
rm(list = ls())
library(ggplot2)
library(segmented)
library(splines)
library(Hmisc)
library(rms)
library(mgcv)
library(caret)
library(readxl)

setwd("/Users/mac/RDocu") # 设置路径
setwd("C:\\Users\\wane\\Desktop\\R&Py\\RDocu")
getwd()
list.files() # 查看当前工作目录下的文件
dir() # 查看当前工作目录下的文件

data <- read_excel("Normal_female.xlsx") # 导入excel格式数据
# data=read.csv("female_df.csv")
# 理解特征、数据类型
ncol(data) # 字段数量 列数
nrow(data) # 样本数量 行数
colnames(data) # 字段名 列名
cbind(apply(data, 2, function(x) length(unique(x))), sapply(data, class)) # 获取每列的数据种类数，和 数据类型
# integer | numeric 为连续变量/数值型特征; factor 为分类变量/离散型的特征，一般需要额外处理才能训练; character为字符串特征，需要转为数值型才能进行后续计算。
data$Age <- as.numeric(data$Age)
data$BMI <- as.numeric(data$BMI)
data$Height <- as.numeric(data$Height)
data$Weight <- as.numeric(data$Weight)
data$TBSL1L4 <- as.numeric(data$TBSL1L4)
data$BMDL1L4 <- as.numeric(data$BMDL1L4)
data
str(data)
class(data$Age)
summary(data$Age)
describe(data$Age)
sd(data$Age)
# shapiro.test(data$Age)##SPSS 规定:当样本含量3 ≤n ≤5000 时,结果以Shapiro - Wilk (W 检验) 为准,当样本含量n > 5000 结果以Kolmogorov - Smirnov 为准。而SAS 规定:当样本含量n ≤2000 时,结果以Shapiro - Wilk (W 检验) 为准,规定当样本含量n >2000 时,结果以Kolmogorov - Smirnov (D 检验) 为准，所以我在这里也提一下 R语言中的 Kolmogorov-Smirnov 检验

# 数据清洗，去除离群值
data_NA <- na.omit(data) # 删除包含空值的行
# data_NA
# str(data_NA)
# outlier_values <- boxplot.stats(data$Age)$out
# boxplot(data$Age, main="Age", boxwex=0.1)
# mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
#
# ggplot(data = data) +
#   scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100)) +
#   geom_histogram(mapping = aes(x = Age), binwidth = 0.5)
#
# data %>% filter(data$Age < 18) %>%
#   ggplot(mapping = aes(x = Age)) +
#   geom_histogram(binwidth = 0.1)

outliers <- boxplot(total$Age, total$BMI, plot = FALSE)$out
# 定义离群值
x <- total
x <- x[-which(x$Age & x$BMI %in% outliers), ]

data2 <- data[data$Age > 20 & data$Age < 90, ]
data2 <- data[data$Age > 20 & data$Age < 90 & data$BMI > 18 & data$BMI < 30, ]

data2
str(data2)
ncol(data2) # 字段数量 列数
nrow(data2) # 样本数量 行数
colnames(data2)
cbind(apply(data2, 2, function(x) length(unique(x))), sapply(data2, class))
summary(data2$Age)
describe(data2$Age)
sd(data2$Age)

# 数据探索
ggplot(data2, aes(Age, TBSL1L4)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05) # 绘制散点图

ggplot(data2, aes(Age, BMDL1L4)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05) # 绘制散点图

# 建立线性回归模型
model.lm <- lm(TBSL1L4 ~ Age, data = data2) # 构建线性回归模型
summary(model.lm) # 查看回归模型结果

model.lm2 <- lm(BMDL1L4 ~ Age, data = data2)
summary(model.lm2) # Residual standard error为残差标准误，是模型用自变量预测因变量的平均误差，该值越小说明模型拟合越好；Adjusted R-squared为调整R2，可理解为模型对数据集的解释程度，该值越大模型拟合程度越好。
# 线性回归的拟合效果
ggplot(data2, aes(Age, TBSL1L4)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05)

# 建立曲线方程
model.log <- lm(TBSL1L4 ~ log(Age), data = data2) # 建立对数曲线方程
summary(model.log) # 查看模型概况
# 拟合曲线
ggplot(data2, aes(Age, TBSL1L4)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x)) +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05)

View(data2)

# 建立分段回归模型
model.segmented <- segmented(model.lm) # 构建分段回归模型
summary(model.segmented) # 查看模型概况
# 查看拟合效果
plot(TBSL1L4 ~ Age, data2, pch = 10, cex = 0.5, bty = "l")
axis(1, seq(10, 100, 10))
abline(a = coef(model.lm)[1], b = coef(model.lm)[2], col = "red", lwd = 2.5)
plot(model.segmented, col = "blue", lwd = 2.5, add = T, bty = c("l"), )
# ggplot(data, aes(Age,TBSL1L4)) + geom_point() + stat_smooth(method = lm, formula = y ~ log(x))+ theme_classic() +  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))

# 手动设置拐点，分三段回归
model.segmented2 <- segmented(model.lm, psi = c(40, 70)) # 构建分段回归模型
summary(model.segmented2) # 查看模型概况
# 查看拟合效果
plot(TBSL1L4 ~ Age, data2, pch = 10, cex = 0.5, bty = "l")
axis(1, seq(10, 100, 10))
abline(a = coef(model.lm)[1], b = coef(model.lm)[2], col = "red", lwd = 2.5)
plot(model.segmented2, col = "blue", lwd = 2.5, add = T, bty = c("l"), )

# 限制性立方样条回归(RCS)
model.spline <- lm(data2$TBSL1L4 ~ rcs(data2$Age, c(20, 30, 35))) # 建立样条回归，设置3个节点
summary(model.spline) # 查看模型概况
# 样条回归拟合效果
ggplot(data2, aes(Age, TBSL1L4)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, c(20, 30, 35))) +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05)

# Lowess函数建立局部加权回归
model.lowess <- lowess(data2$TBSL1L4 ~ data2$Age) # 建立局部加权回归
summary(model.lowess) # 查看概况
# 查看拟合
ggplot(data2, aes(Age, TBSL1L4)) +
  geom_point() +
  stat_smooth() +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05)
# 局部加权给一般只做数据探索，stat_smooth()就是默认用lowess画拟合图

# 广义可加模型: 和lowess函数一样，广义可加模型也无法给出明确的系数，但它的适用范围更广，可以执行因变量与多个自变量之间的各种非参数拟合。
model.gam <- gam(TBSL1L4 ~ s(Age), data = data2) # 建立gam模型
summary(model.gam) # 查看模型概况
pr.gam <- predict(model.gam, data) # 生成预测值

# 计算RSME和R方
data.frame(RMSE = RMSE(pr.gam, data$TBSL1L4), R2 = R2(pr.gam, data$TBSL1L4))
# 查看模型拟合情况
ggplot(data2, aes(Age, TBSL1L4)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_point(size = 0.05)
# 从图形可以看出，广义可加模型的曲线拟合效果非常好。虽然模型在本数据集中表现良好，但仍需要注意过拟合的情况。

#########################################
# 进行nls模型分析(https://mp.weixin.qq.com/s?__biz=Mzg3MzQzNTYzMw==&mid=2247500276&idx=1&sn=7ecc432b5bc3c9d0f22651618f816d1b&chksm=cee2996af995107ce08d0b4cc0b6ed72fb5500a92cdd08c9a8bd8c0c83624b35546ee21c80a7&mpshare=1&scene=1&srcid=12070XF4be44p0nkerDGMFWP&sharer_sharetime=1670430178629&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# 指数拟合，加载R包
library(tidyverse)
library(ggpmisc)
library(gginnards)
library(ggtext)
str(dt)

# nls分析即可视化
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))

ggplot(dt,aes(Age,TBV.BW)) +
  geom_point() +
  stat_fit_augment(method = "nls",method.args = args) +
  stat_fit_tidy(method = "nls",method.args = args,label.x = "right",
                label.y = "top",
                aes(label = sprintf("\"y\"~`=`~%.3g %%*%% %.3g^{\"x\"}",
                                    after_stat(k_estimate),
                                    after_stat(e_estimate))),parse = TRUE )
# 分步进行nls分析
nlsFit <- nls(formula=`SUVr_whole_refPons` ~ k*e^`Age`,
              start = list(k=1.67,e=0.998),
              data=dt,
              control=nls.control(maxiter=200))
summary(nlsFit)
# 构建标签
nlsParams <- nlsFit$m$getAllPars()

nlsEqn <- substitute(italic(y) == k %.% e^ italic(x),
                     list(k=format(nlsParams['k'],digits=3),
                          e=format(nlsParams["e"],digits=3)))
dlabel <- tibble(label="y = 49.7*0.746<sup>x</sup>",x=4,y=35)

# 自定义添加标签
ggplot(dt,aes(Age,SUVr_whole_refPons)) +
  geom_point()+
  stat_smooth(method = 'nls',
              method.args = list(start = c(a=1, b=1)), 
              formula = y~a*exp(b*x), se = FALSE)+
  geom_richtext(data=dlabel,aes(x=x,y=y,label=label),
                fill=NA,label.color=NA,show.legend = F)+
  theme_bw()

## [基于R语言的指数拟合和密度图组合复现](https://mp.weixin.qq.com/s?__biz=Mzg5MTc0NTAyNg==&mid=2247484144&idx=1&sn=4caecf65588379600f0b3daea114d312&chksm=cfc9e49df8be6d8b4ca10b91288ff9f75989e10c9f3dc4081988fa98e593624006971e2e1263&mpshare=1&scene=1&srcid=1208qyuV2yIIUqp13bt7po1f&sharer_sharetime=1670464975013&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(tidyverse)
library(readxl)
# dt <- read_xlsx('相关性曲线图.xlsx', sheet = 'Sheet1')
dt <- read.csv("C:\\Users\\wane199\\Desktop\\TBS&Mon\\Monkey\\QIANG\\1030\\T1_TBV_1204.csv", fileEncoding = "GBK")
dt <- dt[c(-1, -2, -3)]
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
str(dt)

# 构建指数函数 
# 注意a和b的取值尽可能地接近结果，有时候会报错
modle <- nls(TBV.BW ~ a*exp(b*Age), data = dt, start = list(a=1, b=0))
# 查看模型结果
summary(modle)

ggplot(dt, aes(Age, SUVr_whole_refPons, size = SUVr_whole_refPons)) +
  geom_point(shape = 21, alpha = 0.5, 
             color = 'black', fill = 'blue') +
  labs(y =expression('Spikelets number ('~panicle^-1~')'), 
       x = expression('Panicle number ('~m^-2~')'), title = "")+
  annotate('text', x= 400, y= 10, parse = TRUE, size =6, family = 'serif', label = 'y == 377.8*e^{-0.00397*x}') +
  geom_line(aes(SUVr_whole_refPons = 26.65*exp(-0.05*Age)), size = 2) 
  
p <- ggplot(dt, aes(Age, SUVr_whole_refPons, size= SUVr_whole_refPons)) +
  geom_point(shape = 21, alpha = 0.5, 
             color = 'black', fill = 'blue') +
  labs(y =expression('Spikelets number ('~panicle^-1~')'), 
       x = expression('Panicle number ('~m^-2~')'), title = "")+
  annotate('text', x= 400, y= 10, parse = TRUE, size =6, family = 'serif', label = 'y == 377.8*e^{-0.00397*x}') +
  geom_line(aes(SUVr_whole_refPons = a*exp(b*Age)), size = 2) +
  guides(size = guide_legend(title = 'Spikelets number ('~panicle^-1~')'), direction = 'horizontal') +
  theme_test() +
  theme(#legend.position = c(0.6, 0.85),
        #legend.justification = c(0.0, 0.7),
        legend.background = element_blank()) +
  theme(panel.border = element_rect(color = 'black', size=1),
        axis.ticks = element_line(color = 'black', size= 1),
        axis.text = element_text(color = 'black', size= 10))

# 边缘图的包
library(ggExtra)
ggMarginal(p, type="density",  yparams = list(fill = "#0855C9", size = 1), xparams = list(fill ="#EB4E11", size = 1))
ggsave(filename = 'Fig2.jpg',
       dpi = 300,
       width = 8,
       height = 8)


