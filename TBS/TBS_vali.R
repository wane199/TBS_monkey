# https://www.bilibili.com/video/BV1b44y127qs?from=search&seid=7396103782018758758&spm_id_from=333.337.0.0
# https://zhuanlan.zhihu.com/p/369191132(组间分析—T检验、R语言绘图)
rm(list = ls())
setwd("C:\\Users\\wane\\Desktop\\R&Py\\RDocu")
getwd()
list.files()  #查看当前工作目录下的文件
library(dplyr)
dt <- read.csv("/home/wane/Desktop/Disk/bk/RDocu/M-TBS.csv")
dt1 <- read.csv("/home/wane/Desktop/Disk/bk/RDocu/F-TBS.csv")
data <- read.csv("M_1018.csv")
data1 <- read.csv("F_3061.csv",fileEncoding="GBK",header=T)
data1 <- data1[complete.cases(data1[, c(1,2)]), ] 

data <- data[data$Age>=50 & data$Age<75,]
data1 <- data1[data1$Age>=50 & data1$Age<75,]

df <- data[,-c(1,24:28)]
df1 <- data1[,-c(1,24:28)]
df <- as.numeric(as.chracter(df))
str(res)
describe(df)
class(df)

res <- df %>% mutate(BMD_Group=cut(TscoreL1L4, breaks=c(-Inf, -1.0001, Inf), labels=c("Male BMD partially reduced","Male BMD normal")))
summary(res$BMD_Group)
t.test(res$TBSL1L4~res$BMD_Group)
df_M1 <- subset(df,df$TscoreL1L4>=-1,)
attach(df)
df_M2.5 <- subset(df,TscoreL1L4<-1,)
df_M2.5 <- subset(res,BMD_Group=='Male BMD partially reduced',)

df1_W1 <- df1[df1$TscoreL1L4>=-1,]
res1 <- df1 %>% mutate(BMD_Group=cut(TscoreL1L4, breaks=c(-Inf, -1.0001, Inf), labels=c("Female BMD partially reduced","Female BMD normal")))
summary(res1)
df1_W2.5 <- subset(res1,BMD_Group=='Female BMD partially reduced',)
#df1_W3 <- df1[df1$BMDL1L4<0.845,]

df_M1_TBS1 <- df_M1[df_M1$TBSL1L4>=1.39,]
df_M1_TBS2.5 <- df_M1[df_M1$TBSL1L4>=1.31 & df_M1$TBSL1L4<1.39,]
df_M1_TBS3 <- df_M1[df_M1$TBSL1L4<1.31,]
#res1 <- res %>% mutate(TBS_Group=cut(TBSL1L4, breaks=c(-Inf, 1.309, 1.389, Inf), labels=c("TBS reduced","TBS partially reduced","TBS normal")))
summary(res1$TBS_Group)

# https://www.sci666.com.cn/58293.html
library(agricolae) # 加载包
bon <- LSD.test(res1,"TBS_Group", p.adj="bonferroni")
bon$groups  # 显示归类结果
plot(bon)

t.test(mydata$Sepal.Length~mydata$Species)

df_M2.5_TBS1 <- df_M2.5[df_M2.5$TBSL1L4>=1.39,]
df_M2.5_TBS2.5 <- df_M2.5[df_M2.5$TBSL1L4>=1.31 & df_M2.5$TBSL1L4<1.39,]
df_M2.5_TBS3 <- df_M2.5[df_M2.5$TBSL1L4<1.31,]

# df_M3_TBS1 <- df_M3[df_M3$TBSL1L4>=1.39,]
# df_M3_TBS2.5 <- df_M3[df_M3$TBSL1L4>=1.31 & df_M3$TBSL1L4<1.39,]
# df_M3_TBS3 <- df_M3[df_M3$TBSL1L4<1.31,]

df1_W1_TBS1 <- df1_W1[df1_W1$TBSL1L4>=1.35,]
df1_W1_TBS2.5 <- df1_W1[df1_W1$TBSL1L4>=1.27 & df1_W1$TBSL1L4<1.35,]
df1_W1_TBS3 <- df1_W1[df1_W1$TBSL1L4<1.27,]

df1_W2.5_TBS1 <- df1_W2.5[df1_W2.5$TBSL1L4>=1.35,]
df1_W2.5_TBS2.5 <- df1_W2.5[df1_W2.5$TBSL1L4>=1.27 & df1_W2.5$TBSL1L4<1.35,]
df1_W2.5_TBS3 <- df1_W2.5[df1_W2.5$TBSL1L4<1.27,]
 
# df1_W3_TBS1 <- df1_W3[df1_W3$TBSL1L4>=1.35,]
# df1_W3_TBS2.5 <- df1_W3[df1_W3$TBSL1L4>=1.27 & df1_W3$TBSL1L4<1.35,]
# df1_W3_TBS3 <- df1_W3[df1_W3$TBSL1L4<1.27,]
library(readxl)
df <- read_excel("TBS_vali%.xlsx")
df
reshape2::melt(df,id.vars="Group") -> df1
df1
library(ggplot2)

df1$Group <- factor(df1$Group,
                    levels=df$Group)

df1$variable <- factor(df1$variable,
                    levels=c('TBS normal','TBS partially reduced','TBS reduced'))


ggplot(data=df1,aes(x=Group,y=value,fill=variable))+
  geom_bar(stat = "identity",position = "stack",width=0.6)+
  theme_classic()+
  scale_fill_manual(values=c("#9ec417","#13a983","#44c1f0"))+
  labs(fill=NULL,x="BMD Group",y="TBS Percentage (%)")+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_grey()+
  geom_text(aes(label = value),
            position = position_stack(vjust = .5))  

ggplot(data=df1,aes(x=Group,y=value,fill=variable))+
  geom_bar(stat = "identity",position = "dodge",width=0.8)+
  scale_fill_grey()+
  theme_classic()+
  labs(fill=NULL,x="BMD Group",y="TBS Percentage (%)")+
  scale_y_continuous(expand = c(0,0),limits=c(0,75))+
  geom_text(mapping = aes(label = value), size = 4, colour = 'black', vjust = -1.0, hjust = .5, position = position_dodge(0.8))

# https://www.jianshu.com/p/52e450b48f4a
richness_ACE <- subset(df1, Group == 'Male BMD normal')
richness_ACE$variable <- factor(richness_ACE$variable)
richness_ACE <- richness_ACE[-1,]

##Shapiro-Wilk 检验，当且仅当  两者 p 值都大于 0.05 时表明数据符合正态分布
shapiro_ACE <- tapply(richness_ACE$value, richness_ACE$Group, shapiro.test)
shapiro_ACE
shapiro_ACE1 <- tapply(df1$value, df1$Group, shapiro.test)
shapiro_ACE1

shapiro_ACE$'Male BMD normal'$p.value
shapiro_ACE$'TBS partially reduced'$p.value

##qq 图验证数据的正态性
library(car) #需要用到car包的qqplot
qqPlot(lm(value~group, data = richness_ACE), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
t.test(richness_ACE$value, richness_ACE$variable, paired = F)
t_test_ACE <- t.test(value~variable, richness_ACE, paired = FALSE, alternative = 'two.sided')

# https://shixiangwang.github.io/home/cn/post/ggpubr-add-pvalue-and-siglevels/
library(ggpubr)

# https://www.codenong.com/58744507/
library(dplyr)
df1_pct <- df1 %>%
  group_by(Group) %>%
  mutate(pct=prop.table(value))

ggplot(df1_pct,
       aes(x=Group, y=pct,fill=variable)) +
  geom_col(width=0.6)+
  scale_y_continuous(label = scales::percent)+
  theme_classic()+
  scale_fill_grey()+
  labs(fill=NULL,x="BMD Group",y="TBS Percentage")+
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(label = round(value,digits = 3)),
            position = position_stack(vjust = .5))


# http://www.360doc.com/content/21/0118/21/29540381_957669799.shtml
# https://rpubs.com/chixinzero/490992
library(readxl)
df <- read_excel("/home/wane/Desktop/Disk/bk/RDocu/Normal_female.xlsx")
str(df)
summary(df$Age)

con <- subset(dt, dt$age < 18)
sub <- subset(df, df$Group == "sub")
dt$Sex <- factor(dt$Sex)
psych::describe(df$Age)
glimpse(df)
library(ggplot2)
ggplot(data = df,aes(x=Age))+
  geom_histogram(bins = 20)
ggplot(data = df,aes(x=Age,fill=cut(Age,breaks = c(0,18,99)))) + 
  theme_classic() +
  ggtitle("Normal Female Age Distribution")+
  xlab("Age")+
  ylab("Distribution")+
  geom_vline(aes(xintercept=18), colour = "#990000", linetype="dashed") +
  geom_histogram(bins=50,show.legend = F)

# 特征分箱Binning(https://zhuanlan.zhihu.com/p/68865422)
# https://blog.csdn.net/qq_26867967/article/details/95895431
# http://r-graph-gallery.com
library(hrbrthemes)
ggplot(df, aes(x=Age) ) +
  # Top  geom_histogram/geom_density
  geom_histogram( aes(x = Age, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  # Bottom
  # geom_density( aes(x = var2, y = -..density..), fill= "#404080") +
  # geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")
hist(df$Age)
hist(df$TBSL1L4)
library(dplyr)
library(sqldf)
# 导入无监督分箱包——infotheo
library(infotheo)
# 分成几个区域
nbins <- 10 

### 等宽分箱的原理非常简单，就是按照相同的间距将数据分成相应的等分
# 将连续型数据分成三份，并以1、2、3赋值
equal_width <- discretize(df$Age,"equalwidth",nbins)

### 查看分箱情况
# 查看各分类数量
table(equal_width)
# 用颜色表明是等宽分箱
plot(df$Age, col = equal_width$X)

### 保存每个等分切割点的值（阙值）
# 计算各个分类相应的切割点
width <- (max(df$Age)-min(df$Age))/nbins
# 保存阙值
depreciation <- width * c(1:nbins) + min(df$Age)

###################等级划分###################
##########以按HT(优势树高)划分等级为例##################
###查看HT最小值、最大值以及均值
###按5m一个等级划分
# ##method1
# #从0开始向最大值划分的等级（水平）数：num；ceiling函数表示向上取整
# num <- ceiling(b/5) 
# #最接近HT最大值的能被“划分距离（这里HT按5m划分）”整除的数：n; 这里HT最大值为b = 22.3, 那么n = 5*5=25 
# n <- num*5

##method2
#不从0开始划分的等级数
num <- ceiling((102-8)/5)+1
#不从0开始划分
n1 <- floor(8/5)*5
n2 <- ceiling(102/5)*5

##采用cut函数将数据分组
# breaks <- seq(0,n,by = 5)#(method1)
breaks <- seq(n1,n2,by = 5)#(method2)
Age_Group_5 <- cut(df$Age,breaks = breaks)

##各水平按默认排序重命名
levels(Age_Group_5) <- c(1:num)

##确定各等级（水平）样本量
plot(Age_Group_5)
table(Age_Group_5)

##保存等级划分结果
x1 <- cbind(df,Age_Group_5)
write.csv(x1,file = "./TBS/Age_Group_method2.csv")



# World map is available in the maps package
library(maps)
# No margin
par(mar=c(0,0,0,0))
# World map
map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)






