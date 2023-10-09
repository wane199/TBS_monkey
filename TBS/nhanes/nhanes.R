# [NHANES, National Health and Nutrition Examination Survey](https://wwwn.cdc.gov/nchs/nhanes/Default.aspx)
# https://mp.weixin.qq.com/s?__biz=MzI1NjM3NTE1NQ==&mid=2247486750&idx=1&sn=90c3338d3a010e024252687b32207246&chksm=ea26ed02dd516414ef982e116c1a1f114a5c72b395854c41b7f4692d2e28919b9993a425bcdd&mpshare=1&scene=1&srcid=11029kwnODSUuMBjcI9ptQHa&sharer_sharetime=1667362719667&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
###### NHANES数据下载 ######
library(haven)
library(nhanesA)
library(tidyverse)
library(haven) # CRAN v2.5.3
library(nhanesA) # CRAN v0.7.4
library(tidyverse) # CRAN v2.0.0

mydata <- read_xpt("/home/wane/Downloads/P_DEMO.XPT") # NHANES 2017-March 2020 Pre-Pandemic Demographics Data
mydata1 <- nhanes("DEMO_E") # NHANES 2007-2008 Demographics Data；_H：2013-2014

# 对照变量说明提取需要的变量
dat1 <- mydata1 %>% select(
  SEQN, # 序列号
  RIAGENDR, # 性别
  RIDAGEYR, # 年龄
  RIDRETH1, # 种族
  DMDMARTL, # 婚姻状况
  WTINT2YR, WTMEC2YR, # 权重
  SDMVPSU, # psu
  SDMVSTRA  # strata
) 

# 关键的血糖和肺功能的指标，在化验室指标
xuetang <- nhanes("GLU_E")
# 对数据进行提取，序列号提取，
xuetang1 <- xuetang %>% select(
  SEQN, # 序列号
  LBDGLUSI, # 血糖mmol表示
  LBDINSI, # 胰岛素( pmmol/L)
  PHAFSTHR # 餐后血糖
)
# BPX_E:血压，SMQ_E:抽烟，ALQ_E:饮酒
# 同理依次取糖化血红蛋白、肺功能数据
tanghuadb <- nhanes("GHB_E")
tanghuadb1 <- tanghuadb %>% select(
  SEQN, # 序列号
  LBXGH
) # 糖化血红蛋白
feihuoliang <- nhanes("SPX_E")
feihuoliang1 <- feihuoliang %>% select(
  SEQN, # 序列号
  SPXNFEV1, # FEV1：第一秒用力呼气量
  SPXNFVC # FVC：用力肺活量，ml（估计肺容量）
)
dxaspn <- nhanes("DXXSPN_E")
dxaspn1 <- dxaspn %>% select(
  SEQN, # 序列号
  DXXTOTBS
) # Total Trabecular Bone Score

# 处理好数据以后把数据合并就好了
hdata <- plyr::join_all(list(dat1, xuetang1, tanghuadb1, feihuoliang1, dxaspn1), by = "SEQN", type = "full")

# 我们把它保存起来，今后的操作将在这个数据展开
getwd()
write.csv(hdata, file = "./TBS/nhanes/07-08.csv", row.names = F)

###### 基线表绘制(table1) ######
library(tableone)
library(survey)
library(tableone) # CRAN v0.13.2
library(survey) # CRAN v4.2-1
bc <- read.csv("./TBS/nhanes/07-08.csv", sep = ",", header = TRUE)
glimpse(bc)
Hmisc::describe(bc)

# 分组
hist(bc$DXXTOTBS)
bc$TBS <- ifelse(bc$DXXTOTBS < 1.352, 1, 2)
table(bc$TBS)
# ifelse(bc$DXXTOTBS>=1.352,

# 开始建立抽样调查函数svydesign
bcSvy2 <- svydesign(
  ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
  nest = TRUE, data = bc
)
summary(bcSvy2)

# 绘制基线表，使用的是svyCreateTableOne函数，先要定义全部变量和分类变量
dput(names(bc)) ## 输出变量名
allVars <- c(
  "SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDMARTL", "WTINT2YR",
  "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "LBDGLUSI", "LBDINSI", "PHAFSTHR",
  "LBXGH", "SPXNFEV1", "SPXNFVC", "DXXTOTBS", "TBS"
) ### 所有变量名
fvars <- c("RIAGENDR", "RIDRETH1", "DMDMARTL") # 分类变量定义为fvars

Svytab2 <- svyCreateTableOne(
  vars = allVars,
  strata = "TBS", data = bcSvy2, factorVars = fvars
)
Svytab2

###### 缺失值 ######
# https://www.yisu.com/zixun/444909.html
library(lattice) # CRAN v0.21-8
library(mice) # CRAN v3.16.0 # CRAN v3.16.0
library(VIM) # CRAN v6.2.2

summary(bc)
dim(bc)
aggr(bc) # VIM::aggr()查看缺失值

#  缺失状况了解
# 计算数据集中的缺失值的数量
is.na(bc)
# 计算缺失值个数，等于0则不存在缺失值
sum(is.na(bc))
# 计算数据集中完整样本的数量
complete.cases(bc)
# 通过列表显示数据集缺失情况
md.pattern(bc)

# 5.3 缺失值处理
# 1. 删除法
# 使用na.omit()函数来对缺失数据的行进行删除
newnhanes <- na.omit(bc)
newnhanes

# 2. 插补法
# 将第16列中为NA值的行号存入数据集sub中
is.na(bc[, 16])
sub <- which(is.na(bc[, 16]))
sub
# 第16列不为NA的数存入数据集A中
A <- bc[-sub, ]
A
# 将第16列为NA的数存入数据集B中
B <- bc[sub, ]
B
# 计算数据集第四列没有缺失值数据的均值
A_16mean <- mean(A[, 16])
A_16mean

# 平均值填补
bc$DMDMARTL[is.na(bc$DMDMARTL)] <- mean(bc$DMDMARTL, na.rm = T)
# 中位数填补
bc$ Solar.R[is.na(bc$ Solar.R)] <- median(bc$ Solar.R, na.rm = T)
# 相邻均值填补
for (i in 1:length(bc$ Ozone)) {
  bc$ Ozone[i] <- ifelse(is.na(bc$ Ozone[i]),
    mean(c(bc$ Ozone[i - 1], bc$ Ozone[i + 1]), na.rm = T),
    bc$ Ozone[i]
  )
}

# Plot incomplete or imputed data
# load packages
library(ggplot2) # CRAN v3.4.3 # CRAN v3.4.3
library(mice) # CRAN v3.16.0 # CRAN v3.16.0
library(ggmice) # CRAN v0.1.0
# load some data
dat <- boys
# visualize the incomplete data
ggmice(dat, aes(age, bmi)) + geom_point()

# impute the incomplete data
imp <- mice(dat, m = 1, seed = 1, printFlag = FALSE)
# visualize the imputed data
ggmice(imp, aes(age, bmi)) + geom_point()

###### nhanes数据库挖掘教程3--对数据进行多重插补 ######
# (https://mp.weixin.qq.com/s?__biz=MzI1NjM3NTE1NQ==&mid=2247487020&idx=1&sn=9d504788a6909af3f797e86bbfdba8f7&chksm=ea26ee30dd516726ec22d640de1c0ef07fed8aa1c897918413b06a84a81a5b4cde311d389709&mpshare=1&scene=1&srcid=1209ZZUWTDh4pyjNkbKHLWbW&sharer_sharetime=1670587658361&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(mi) # CRAN v1.1

# 列出缺失值列表，提前处理好分类变量
mdf <- missing_data.frame(bc)
show(mdf)

# 更改变量类型
mdf <- change(mdf,
  y = c("RIDRETH1"), what = "type",
  to = c("unorder")
)
mdf <- change(mdf,
  y = c("RIAGENDR"), what = "type",
  to = c("binary")
)
summary(mdf)
hist(mdf)
image(mdf)
# {naniar}:让NA（缺失值）可见的多种图～
library(naniar) # CRAN v1.0.0
library(ggplot2) # CRAN v3.4.3 # CRAN v3.4.3
# 大致查看一下包含缺失值的变量
n_var_miss(mdf)
gg_miss_which(mdf)
# 再查看所有变量的缺失值个数：
gg_miss_var(mdf)
# 缺失值所占百分比：
gg_miss_var(mdf,
  show_pct = TRUE
)
vis_miss(bc)

gg_miss_span(bc, LBDINSI, span_every = 1) +
  scale_x_continuous(breaks = seq(83, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(
    subtitle = "LBDINSI",
    x = "Observations"
  ) +
  theme_dark()
# 包含缺失值的变量们的交集情况，就可以使用Upset plot：
gg_miss_upset(mdf)

ggplot(
  bc,
  aes(
    x = LBDINSI,
    y = DMDMARTL
  )
) +
  geom_point() +
  theme_bw()

ggplot(bc, aes(x = LBDINSI, y = DMDMARTL)) +
  geom_miss_point() +
  scale_color_manual(values = c("grey", "tomato")) +
  theme_bw()

# 插补
imputation <- mi(mdf)
imputation1 <- mi(mdf, n.chains = 5)

round(mipply(imputation, mean, to.matrix = T), 3)
image(imputation)

# 提取数据
IMP.dat.all <- complete(imputation) # 导出全部数据
a1 <- IMP.dat.all[["chain:1"]]
image(a1)



