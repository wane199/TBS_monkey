# https://www.nature.com/articles/s41467-022-28421-6#Sec24
# Lasso, Ridge, Enet, StepCox, SurvivalSVM, CoxBoost, SuperPC, plsRcox, RSF, GBM(https://mp.weixin.qq.com/s?__biz=MzAwMjY4MDE2Mg==&mid=2247619851&idx=1&sn=2df8ba1c261dab389f630eeea91d1040&chksm=9ac5e106adb26810faecd8195976fc3910886079c926c3569865255936b2d4dc27b1bc8ed3d2&mpshare=1&scene=1&srcid=1123lWmWuQAxJ26m48HGkQQz&sharer_sharetime=1669216079860&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(glmnet)
library(mlr3)

# 1.Lasso
# 代码解析
#### 1.glmnet包
cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1) # 核心函数(执行lasso回归)
#### 2.mlr3包
lrn <- lrn("classif.cv_glmnet", alpha = 1) # 核心函数(执行lasso回归)

# 2.Ridge
#代码解析
####1.glmnet包
cvfit <- cv.glmnet(x, y, family = "binomial",alpha = 0)#核心函数(执行Ridge回归)
####2.mlr3包
lrn <- lrn("classif.cv_glmnet",alpha = 0)#核心函数(执行Ridge回归)

# 3.Enet（弹性网络）
#代码解析
####1.glmnet包
cvfit <- cv.glmnet(x, y, family = "binomial",alpha = 0.5)#核心函数(执行Enet回归)
####2.mlr3包
lrn <- lrn("classif.cv_glmnet",alpha = 0.5)#核心函数(执行Enet回归)

# 4.StepCox
#代码解析
####1.survival包
for (direction in c("both", "backward")) {
  fit <- step(coxph(Surv(OS.time,OS)~.,est_dd),direction = direction)}##核心函数
####2.mlr3包
#这个目前在mlr3中并不可以得到实现，所以我们只有采用基本函数来实现这个应用

# 5.survivalSVM
#代码解析
survivalsvm(Surv(OS.time,OS)~., data= est_dd, gamma.mu = 1)##核心函数

# 6.CoxBoost
#代码解析
#github上有现成的R包可以完成这项操作
library(devtools)
install_github("binderh/CoxBoost")
RIF <- resample.CoxBoost(time=obs.time,status=obs.status,x=x,rep=100, maxstepno=200,multicore=FALSE,
                         mix.list=c(0.001, 0.01, 0.05, 0.1, 0.25, 0.35, 0.5, 0.7, 0.9, 0.99), 
                         stratum=group,stratnotinfocus=0,penalty=sum(obs.status)*(1/0.02-1),
                         criterion="hscore",unpen.index=NULL) 
#当然作者针对这里使用的是下面这套代码
fit <- CoxBoost(est_dd[,'OS.time'],est_dd[,'OS'],as.matrix(est_dd[,-c(1,2)]),
                stepno=cv.res$optimal.step,penalty=pen$penalty)


# 7.SuperPC(jedazard/superpc: Supervised Principal Components (github.com))
#代码解析
cv.fit <- superpc.cv(fit,data,n.threshold = 20,#default
                     n.fold = 10,
                     n.components=3,
                     min.features=5,
                     max.features=nrow(data$x),
                     compute.fullcv= TRUE,
                     compute.preval=TRUE)


# 8.plsRcox:高维数据中如果带有生存预后信息，那么可以通过plsRcox这个算法实现在高维数据中拟合cox模型
#代码实现
fit <- plsRcox(est_dd[,pre_var],time=est_dd$OS.time,event=est_dd$OS,nt=as.numeric(cv.plsRcox.res[5]))

# 9.RSF
#代码解析
library(randomForestSRC)
fit <- rfsrc(Surv(OS.time,OS)~.,data = est_dd,
             ntree = 1000,nodesize = rf_nodesize,##该值建议多调整
             splitrule = 'logrank',
             importance = T,
             proximity = T,
             forest = T,
             seed = seed)

# 10.GBM
#代码解析
fit <- gbm(formula = Surv(OS.time,OS)~.,data = est_dd,distribution = 'coxph',
           n.trees = 10000,
           interaction.depth = 3,
           n.minobsinnode = 10,
           shrinkage = 0.001,
           cv.folds = 10,n.cores = 6)


