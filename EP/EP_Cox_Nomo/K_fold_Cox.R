# 预测模型 | 10. K折交叉验证可视化 (Cox回归)
# https://mp.weixin.qq.com/s?__biz=Mzg2MjU2NDQwMg==&mid=2247497310&idx=1&sn=d0aade5c787e85e1689579679e8a6ba7&chksm=ce074f03f970c6157372da63431520b5009c590824f2b087f6ccc67c7d08b7e83ec08c296424&mpshare=1&scene=1&srcid=0904ZBKuwGpRm9OC14OkTKHm&sharer_sharetime=1662264266271&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# cox回归用
library(survival)
# K折交叉验证用
library(caret)
# cox回归时间AUC用
library(riskRegression)
# cox回归时间c指数用
library(pec)
###########################
# 清理工作环境
rm(list = ls())
# 读入数据
dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/Task2/TLE234group.csv")
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")

# 查看变量性质
str(dt)
# 批量数值转因子
for (i in names(dt)[c(4:7)]) {
  dt[, i] <- as.factor(dt[, i])
}
# 再次查看变量性质
str(dt)

# 多次K折交叉验证
# 建个空表res备用，盛放结果
res <- as.numeric()
# 设置随机种子，使数据分割可重复
set.seed(123)
# 设定K和N,预测3年AUC的5折200次验证
# status是结局1为复发
folds <- createMultiFolds(
  y = dt$Rel._in_5yrs,
  k = 5, times = 100
)

# 3年AUC的2000次验证
# for循环将上述1000种新数据均运行一次，
# 求1000个AUC
# 1-建立空表，放每次的AUC
auc_value36 <- as.numeric()
# 2-做成循环，3年
for (i in 1:1000) { # 1000次循环（1000组新数据）
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- Score(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, metrics = "auc", times = 36
  ) # 3年或5年
  auc_value36 <- append(auc_value36, as.numeric(mod$AUC$score$AUC))
}
# 3-将1000个结果放入res表中
res <- data.frame(auc_value36)

# 2.计算5年AUC的1000次交叉验证值
# 1-建立空表，放每次的AUC
auc_value60 <- as.numeric()
# 2-for循环
for (i in 1:1000) { # 1000次循环（1000组新数据）
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- Score(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, metrics = "auc", times = 60
  ) # 3年或5年
  auc_value60 <- append(auc_value60, as.numeric(mod$AUC$score$AUC))
}
# 3-将结果放入res表
res$auc_value60 <- with(res, auc_value60)

# 3年C-index的2000次验证
### 建立空值
c_value36 <- as.numeric()
# 做成循环
for (i in 1:1000) {
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- cindex(list(model1 = model),
    formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
    data = test, eval.times = 36
  )
  c_value36 <- append(c_value36, as.numeric(mod$AppCindex))
}
# 将36个月的c指数放入res
res$c_value36 <- with(res, c_value36)

#5年C-index的2000次验证 
###建空表
c_value60<-as.numeric()
#做成循环
for (i in 1:1000) {
  train <- dt[folds[[i]], ]
  test <- dt[-folds[[i]], ]
  model <- coxph(Surv(Follow_up_timemon, Rel._in_5yrs) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, x = T)
  mod <- cindex(list(model1 = model),
                formula = Surv(Follow_up_timemon, Rel._in_5yrs) ~ 1,
                data = test, eval.times = 60
  )
  c_value60 <- append(c_value60, as.numeric(mod$AppCindex))
}
res$c_value60 <- with(res,c_value60) 

# res表宽转长
library(reshape2)
res <- data.frame(res)
res1<- melt(res,
            measure.vars = c("auc_value36","auc_value60",
                             "c_value36","c_value60"),
            variable.name = "Sample",
            value.name = "value")
#需要转换的列名：measure.vars()  
#新数据标签列名：variable.name()
#新数数据数值名：value.name()      

# 交叉验证结果可视化
# 1. 小提琴图
library(ggplot2)
library(ggprism)
ggplot(res1, aes(Sample,value))+
  geom_violin(aes(fill = Sample))+
  geom_boxplot(width=0.1)+
  theme_prism(base_size =15,border =T)+
  theme(legend.position = "none")    

# 2. 箱线图+散点图
ggplot(data=res1)+ 
  geom_boxplot(mapping=aes(x=Sample,y=value,colour =Sample),
               alpha = 0.5,size=1.5,width = 0.6)+ 
  geom_jitter(mapping=aes(x=Sample,y=value,colour =Sample), 
              alpha = 0.3,size=2)+
  scale_color_manual(limits=c("auc_value36","auc_value60",
                              "c_value36","c_value60"), 
                     values=c("#c1121f","#0466c8","#2e7542","#fca311"))+ 
  theme_prism(base_size =15,border =T)+
  theme(legend.position = "none")       

# 云雨图（小提琴+蜂图）
#4.云雨图
library(see)
ggplot(res1, aes(x = Sample,y = value, fill = Sample)) +
  geom_violindot(binwidth = 0.0025,dots_size =1,color =NA) +
  coord_flip()+scale_fill_discrete()+ 
  theme_prism(border =T)+theme(legend.position = "none")     

# 5. 云雨图（小提琴+散点）
library(gghalves)
#小提琴+散点
ggplot(res1,aes(x = Sample,y = value, fill = Sample))+
  geom_half_violin(aes(fill = Sample),
                   position = position_nudge(x = .15, y = 0),side = 'r') +
  geom_point(aes(x=as.numeric(Sample)+0.05,y =value,color =Sample),
             position=position_jitter(width=0.05),size=0.25,shape = 20)+
  coord_flip()+#转90度
  theme_prism(border =T)+theme(legend.position = "none")    

# 6. 云雨图-3（小提琴+箱图+散点）
library(tidyverse)
library(dplyr)
library(ggsci)
# 先计算均值等,报错重装dplyr
summ_res1 <- res1 %>% 
  group_by(Sample) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Sample, levels = c("auc_value36","auc_value60","c_value36","c_value60"))
  );summ_res1


##小提琴+箱线图+散点+误差
ggplot(res1,aes(x = Sample,y = value, fill = Sample))+
  geom_half_violin(aes(fill =Sample),position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=T, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Sample)-0.1,y = value,color = Sample),
             position = position_jitter(width = 0.05),size = .25, shape = 20) +
  geom_half_boxplot(aes(x =as.numeric(Sample)+0.15,y = value, fill = Sample),
                    width = 0.25,color = "black")+
  geom_errorbar(data =summ_res1,aes(x =Sample,y = mean,group =Sample,
                                    colour =Sample, ymin = mean-se, 
                                    ymax = mean+se),width=0.05,
                position=position_nudge(x = -0.01, y = 0))+
  scale_color_jco()+
  scale_fill_jco()+
  coord_flip()+
  theme_prism(border =T)+theme(legend.position = "none")
