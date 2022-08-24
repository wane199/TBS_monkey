# https://mp.weixin.qq.com/s?__biz=MzAwMjY4MDE2Mg==&mid=2247602598&idx=1&sn=c899876f696a5ea4bf0a82bb32b55edd&chksm=9ac5a5abadb22cbd0a54635d4e5996d37e579982756630204bc9c2b2f277dbce9e01efb8a6e6&mpshare=1&scene=1&srcid=0803GrawYQzTYSmjlwiVrVEO&sharer_sharetime=1661260187746&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# https://www.bilibili.com/video/BV1Yu411U7KD/?spm_id_from=333.788&vd_source=23f183f0c5968777e138f31842bde0a0
# https://mp.weixin.qq.com/s?__biz=MzU1ODY5MzUyMQ==&mid=2247488469&idx=3&sn=d979ce87bc7a35253496dc14f105b7e3&chksm=fc23fe2bcb54773d5ed519640bc13234f4ec29c049bd813cf1d0096b4ad6ff6f4ae5d2b7388e&mpshare=1&scene=1&srcid=0824eo1m20LtK8cjp3v0EQF2&sharer_sharetime=1661301700925&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
library(mlr3) # 主体包
library(mlr3viz) # 执行可视化功能
library(mlr3learners) # 提供额外学习器
library(mlr3verse) # 扩展包
library(mlr3tuning)
library(data.table)
library("magrittr")
library(randomForest) # 执行随机森林算法
library(randomForestSRC)
library(varSelRF) # 挑选变量
library(reshape2)
library(readxl)
source("./EP/EP_Cox_Nomo/DCA/dca.r") # 执行DCA的脚本
rm(list = ls())
getwd()

dt0 <- read.csv("/media/wane/wade/EP/EPTLE_PET/TLE_pet_ind/process_PT_tem.csv")
dt0 <- read.csv("/home/wane/Desktop/EP/Structured_Data/PT_radiomic_features_temporal_ind2.csv")
dt0 <- dt0[-1]
dt1 <- read_excel("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.xlsx")

train <- subset(dt1, dt1$Group == "Training")
test <- subset(dt1, dt1$Group == "Test")
normal_para <- preProcess(x = train[, 5:1136], method = c("center", "scale")) # 提取训练集的标准化参数
train_normal <- predict(object = normal_para, newdata = train[, 5:1136])
test_normal <- predict(object = normal_para, newdata = test[, 5:1136])
test_normal <- mutate(test[, 1:4], test_normal)
write.csv(test1, "/home/wane/Desktop/EP/Structured_Data/Task2/test_nor.csv", row.names = F)

dt0 <- na.omit(dt0) # 按行删除缺失值
attach(dt0)
set.seed(123)
ind <- sample(2, nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
# 训练集
train <- dt0[ind == 1, ] # the training data set
# 测试集
test <- dt0[ind == 2, ] # the test data set

## analysis, 生存随机森林因变量不设置分类变量
set.seed(123)
vars <- paste0(names(train[c(3:5, 7:16, 18)]), collapse = "+")
rad.obj <- rfsrc(Surv(Follow_up_timemon, Rel._in_5yrs) ~ .,
  data = train, nsplit = 10, block.size = 10,
  mtry = 50, nodesize = 15, ntree = 400, importance = TRUE, samptype = "swor", splitrule = "logrank"
)

print(rad.obj$importance)
print(sort(rad.obj$importance, decreasing = F)[1:12])
plot(rad.obj)
top <- sort(rad.obj$importance, decreasing = T)[1:12]
plot(top)

vs.rad <- var.select(object = rad.obj)
