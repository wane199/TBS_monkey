# https://mp.weixin.qq.com/s?__biz=MzAwMjY4MDE2Mg==&mid=2247602598&idx=1&sn=c899876f696a5ea4bf0a82bb32b55edd&chksm=9ac5a5abadb22cbd0a54635d4e5996d37e579982756630204bc9c2b2f277dbce9e01efb8a6e6&mpshare=1&scene=1&srcid=0803GrawYQzTYSmjlwiVrVEO&sharer_sharetime=1661260187746&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# https://www.bilibili.com/video/BV1Yu411U7KD/?spm_id_from=333.788&vd_source=23f183f0c5968777e138f31842bde0a0
# 准备工作
library(mlr3)#主体包
library(mlr3viz)#执行可视化功能
library(mlr3learners)#提供额外学习器
library(mlr3verse)#扩展包
library(mlr3tuning)
library(data.table)
library("magrittr")
library(randomForest)#执行随机森林算法
library(randomForestSRC)
library(varSelRF)#挑选变量
library(reshape2)
source("./EP/EP_Cox_Nomo/DCA/dca.r")#执行DCA的脚本
getwd()
str(train)



## analysis
set.seed(123)
rad.obj <- rfsrc(Surv(Follow_up_timemon, Rel._in_5yrs==1) ~ radscore + SGS + familial_epilepsy + Durmon + SE, data = train, nsplit = 10, block.size = 10,
                  mtry = 50, nodesize = 15, ntree=400,importance=TRUE,samptype = "swor", splitrule = "logrank")

print(rad.obj$importance)
plot(rad.obj)

vs.rad <- var.select(object = rad.obj)






