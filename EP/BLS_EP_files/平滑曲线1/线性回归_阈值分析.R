library(mgcv)
library(writexl)
library(dplyr)

dt <- read.csv('jixian.csv')
colnames(dt) <- toupper(colnames(dt))
str(dt)
fml<- "HB~s(AGE,fx=FALSE)+GENDER"
gam1<-mgcv::gam(formula(fml),data=dt, family=gaussian(link="identity"))
summary(gam1)
plot(gam1,pages=1,col='blue')

###predict
pred<-predict.gam(gam1,type="terms",se.fit=TRUE)
mfit <- pred$fit[,"s(AGE)"]
sfit <- pred$se.fit[,"s(AGE)"]
mfit <- mfit+(mean(gam1$fitted.values)-mean(mfit))  ###考虑是随机截距
dt<-cbind(dt,mfit,sfit)

###95%CI
y.low <- dt$mfit-1.96*dt$sfit; y.upp<-dt$mfit+1.96*dt$sfit
dt <- cbind(dt,y.low,y.upp)
# 

###order

dt <- arrange(dt,AGE)
co <- c(24,84,94,138)  ##定义坐标
col <- c('blue','purple')

plot(dt$mfit~dt$AGE,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[1],type="l", lty=1, lwd=2, ylab="", xlab="")
par(new=TRUE); 
plot(dt$y.low~dt$AGE,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[2], type="l", lty=3, lwd=1, ylab="", xlab="")
par(new=TRUE); 
plot(dt$y.upp~dt$AGE,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[2], type="l", lty=3, lwd=1, ylab='HB', xlab='AGE')
rug(dt$AGE,col='blue')


####阈值效应分析

##model I 一条直线效应

fit1 <- lm(HB~AGE+GENDER,data=dt)
summary(fit1)

##model II 分段模型
fml <- "HB ~ AGE + factor(GENDER)"
source('get_cutoff_lm.R')
cut_off <- get_cutoff_lm('AGE',dt,fml)
cut_off <- 60


x<-dt[,'AGE']
X1<-(x<=cut_off)*(x-cut_off)
X2<-(x> cut_off)*(x-cut_off)
dt1<-cbind(dt,x,X1,X2)

mdl0<- glm(HB~x+X2+GENDER,family="gaussian",data=dt1)
mdl1<- glm(HB~X1+X2+GENDER,family="gaussian",data=dt1)
mdl2<- glm(HB~x+GENDER,family="gaussian",data=dt1); 

####model I 一条直线效应(线性回归)
summary(mdl2)
####折点
print(cut_off)
####< K 段效应(B+se)/P
####> K 段效应(B+se)/P
summary(mdl1)
###> K 段与< K的效应(B+se)/P
summary(mdl0)

###考虑分段后模型和不分段对数似然比检验
round(1-pchisq(2*(logLik(mdl0)[1]-logLik(mdl2)[1]),1),3)

####折点作图到图上

plot(dt$mfit~dt$AGE,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[1],type="l", lty=1, lwd=2, ylab="", xlab="")
par(new=TRUE); 
plot(dt$y.low~dt$AGE,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[2], type="l", lty=3, lwd=1, ylab="", xlab="")
par(new=TRUE); 
plot(dt$y.upp~dt$AGE,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[2], type="l", lty=3, lwd=1, ylab='HB', xlab='AGE')
rug(dt$AGE,col='blue')
abline(v = cut_off, col = "black",lty=2)


