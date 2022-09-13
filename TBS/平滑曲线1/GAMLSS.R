# plotSimpleGamlss函数 https://mp.weixin.qq.com/s?__biz=Mzg3MDgyMTgxMg==&mid=2247483688&idx=1&sn=6d805f16d094b5465aafe56e9ec32d58&chksm=ce86bbf3f9f132e50bb4b68a8edef8d8e283d6e230ca5e8f610dd683604be41ee07265090aa7&mpshare=1&scene=1&srcid=0912Hyxqs78INjOgSU3KxHIU&sharer_sharetime=1662997861161&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
library(gamlss)
library(gamlss.util)

dt <- read.csv("/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/QIANG/PartⅠ猴脑体积发育数据分析/L&R.csv")
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
summary(dt)
str(dt)

# 构建LMS模型（该模型为GAMLSS方法下的一个特例）
m0a<-lms(Frontal_Cortex,Age,data=dt,
         trans.x=TRUE,k=3,families ="BCCG") #自由度选择2，方法为BCCG
m1 <- lms(y=Frontal_Cortex, x=Age, data=dt, trans.x=TRUE)

# 绘制平滑曲线，并添加数据分布热图
plot<-plotSimpleGamlss(Frontal_Cortex,Age,m0a,data=dt,
                       x.val=seq(0,30,2), #定义数据分组
                       xlim=c(0,30), #横轴的起止点为-2，23
                       ylim=c(0,0.15),val=200) #纵轴的起止点为0，500

# 图像另存为
tiff(file = "Frontal_Cortex.tiff", res = 1400, width = 8800, 
     height = 8800, compression = "lzw")
plot
dev.off()

mod<-gamlss(Frontal_Cortex~pb(Age),sigma.fo=~pb(Age),family=BCT, data=dt, method=mixed(1,20))
plot(mod)
# rm(mod)
plot<-plotSimpleGamlss(Frontal_Cortex,Age,mod,data=dt,
                       x.val=seq(0,30,2), #定义数据分组
                       xlim=c(0,30), #横轴的起止点为-2，23
                       ylim=c(0.075,0.115),val=50) #纵轴的起止点为0，500


