# R语言一次性读入多个excel文件然后根据指定的列批量合并多个数据框
                     # [R语言多个数据集根据某一列取交集然后合并](https://www.bilibili.com/video/BV1bK4y1f78q/?spm_id_from=333.788.recommend_more_video.0&vd_source=23f183f0c5968777e138f31842bde0a0)
rm(list=ls())
library(dplyr) # 加载程序包
library(readxl)      # CRAN v1.4.3 # CRAN v1.4.3
library(tidyverse)   # CRAN v2.0.0 # CRAN v2.0.0 # CRAN v2.0.0
getwd()

##### 身体成分_全部整理 #####
df0 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\IDXA患者名单整理.xlsx')

df1 <- read_excel('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0827\\TBS.xlsx')
df2 <- read_excel('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0827\\股骨.xlsx')
df3 <- read_excel('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0827\\脊椎.xlsx')
df4 <- read_excel('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0827\\科室.xlsx')
df5 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\身体成分_全部整理.xlsx')
df6 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\正位腰椎_全部整理.xlsx')
df0 <- base::unique(df0)
df_1 <- base::unique(df1)
df_2 <- base::unique(df2)
df_3 <- base::unique(df3)
df_4 <- base::unique(df4)
df5 <- base::unique(df5)
df6 <- base::unique(df6)

total <- rbind(df1,df2,df3) 
df1 <- base::unique(total)
write_excel_csv(df, file = "C:\\Users\\wane1\\Documents\\LYX\\csv\\AHA.csv")
df <- df0[,complete.cases(t(df0))]  # 提取不含空值的列

# df1 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\_股骨_side.csv')
# df2 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\_全身 CoreScan.xlsx')
# dim(df2)
# head(df2)
# str(df2)
# df0 <- base::unique(df0)
# df1 <- base::unique(df1)
# df2 <- base::unique(df2)
# 
df1 <- df1[,complete.cases(t(df1))]   # 提取不含空值的列
df1L <- df1  %>% filter(侧 == '左')
df1R <- df1  %>% filter(侧 == '右')
df11 <- merge(df1L,df1R, by = c('姓名','检查号'), suffixes = c(".L",".R"))
df110 <- df11[,complete.cases(t(df11))]   # 提取不含空值的列

write_excel_csv(df11, file = "C:\\Users\\wane1\\Documents\\LYX\\csv0210\\AHA_side.csv")
# df_12 <- merge(df11,df2, by = c('姓','病人ID号。','性别','年龄'))
# df_12 <- base::unique(df_12)
# 
# df3 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\_全身 成份.xlsx')
# df4 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\_全身 自定义.xlsx')
# df5 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\_全身 自定义成分.xlsx')
# df6 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\_全身.xlsx')
# df3 <- base::unique(df3)
# df4 <- base::unique(df4)
# df5 <- base::unique(df5)
# df6 <- base::unique(df6)
# 
# df_36 <- merge(df3,df6,by = c('姓','病人ID号。','性别','年龄')) # , all = TRUE
# df_45 <- merge(df4,df5,by = c('姓','病人ID号。','性别','年龄')) # , all = TRUE
# 
# df_16 <- merge(df_12, df_36, by = c('姓','病人ID号。','性别','年龄'))  # , incomparables = NA
# df_1_6 <- merge(df_16, df_45, by = c('姓','病人ID号。','性别','年龄'), all = TRUE
# )
# 
# df_1_6 <- base::unique(df_1_6)
# write_excel_csv(df_1_6, file = "C:\\Users\\wane1\\Documents\\LYX\\csv\\_股骨_side-_全身.csv")

df1 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\AHA_side.csv')
df3 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\股骨_side.csv')
dim(df1)
head(df2)
str(df1)

df3L <- df3  %>% filter(侧 == '左')
df3R <- df3  %>% filter(侧 == '右')
df33 <- merge(df3L,df3R, by = c('姓名','检查号'), suffixes = c(".L",".R"))
df33 <- base::unique(df33) 
df330 <- df33[,complete.cases(t(df33))]   # 提取不含空值的列
write_excel_csv(df33, file = "C:\\Users\\wane1\\Documents\\LYX\\csv0210\\股骨_side.csv")

df_10 <- merge(df0, df1, by = c('姓名','检查号'))
df_10 <- base::unique(df_10) 
write_excel_csv(df_10, file = "C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_AHA.csv")


df_23 <- merge(df_20, df_30, by = c('姓名','检查号'))
df_23 <- base::unique(df_23) 
df_234 <- merge(df_23, df_40, by = c('姓名','检查号'))
df_234 <- base::unique(df_234) 
write_excel_csv(df_234, file = "C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_CoreScan_股骨_全身BMD.csv")

df_234 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_CoreScan_股骨_全身BMD.csv')
df_50 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_身体成分.csv')
df_60 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_正位腰椎.csv')
df_56 <- merge(df_50, df_60, by = c('姓名','检查号'))
df_56 <- base::unique(df_56)
write_excel_csv(df_56, file = "C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_身体成分_正位腰椎.csv")

df_56 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_身体成分_正位腰椎.csv')
df_234560 <- merge(df_234, df_56, by = c('姓名','检查号','来源','性别','年龄','日期','住院号','科别','检查子类','生日','测量日期','身高','体重','BMI')) # , all = TRUE
df_234560 <- base::unique(df_234560) 
write_excel_csv(df_234560, file = "C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_CoreScan_股骨_全身BMD_身体成分_正位腰椎-28379.csv")


df_45 <- merge(df4, df5, by = c('姓名','性别','年龄','生日')) # , all = TRUE
df_67 <- merge(df6, df7, by = c('姓名','性别','年龄','生日'), all = TRUE) # , all = TRUE
df_4567 <- merge(df_45,df_67,by = c('姓名','性别','年龄','生日'), all = TRUE
) # , all = TRUE

df_1234567 <- merge(df_123,df_4567,by = c('姓名','性别','年龄','生日')) # , incomparables = NA

df8 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\正位脊柱.csv')
df8 <- base::unique(df8) 
df_12345678 <- merge(df_1234567,df8,by = c('姓名','性别','年龄','生日'))
df_0_8 <- merge(df_12345678,df0,by = c('姓名','性别','年龄'))
df_0_8 <- base::unique(df_0_8) 
df08 <- df_0_8[,complete.cases(t(df_0_8))]  
write_excel_csv(df_0_8, file = "C:\\Users\\wane1\\Documents\\LYX\\csv\\IDXA患者名单_合并.csv")




df1 <- df1[,complete.cases(t(df1))]   # 提取不含空值的列

x1 <- Reduce(intersect,list(dfi$gene_name,
                      df1$gene_name,
                      df3$gene_name,
                      df4$gene_name,
                      dfs$gene_name))

library("tidyverse") # CRAN vNA
data_list %>% reduce(inner_join, by = "id")   




df_56 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_身体成分_正位腰椎.csv')

df1 <- read.csv('C:\\Users\\wane1\\Downloads\\Most_Relevant_Countries_By_Corresponding_Author.csv', sep = ";")
df2 <- read.csv('C:\\Users\\wane1\\Downloads\\Most_Cited_Countries.csv', sep = ";")
df_12 <- merge(df1, df2, by = c('国家')) # , all = TRUE
df_12 <- base::unique(df_234560) 
write_excel_csv(df_12, file = "C:\\Users\\wane1\\Downloads\\Most_Cited_Countries-cancat.csv")


##### 0524代综pos #####
rm(list = ls())
library(tidyverse)   # CRAN v2.0.0 # CRAN v2.0.0 # CRAN v2.0.0
library(readxl)      # CRAN v1.4.3 # CRAN v1.4.3
library(reshape2)    # CRAN v1.4.4
df_0 <- read_excel('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0524\\data.xlsx')
df_1 <- read_excel('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0524\\代综pos3.xlsx')

df0 <- base::unique(df_0)
df1 <- base::unique(df_1)

df1b <- subset(df1, Time == "base")
df1p3 <- subset(df1, Time == "pos3")
df1p12 <- subset(df1, Time == "pos12")
df_1w0 <- merge(df1p3, df1p12, by = c('患者姓名','性别'))
df_1w <- merge(df1b, df_1w0, by = c('患者姓名','性别'))
df_1w <- merge(df1b, df1p3, by = c('患者姓名','性别'))
write_excel_csv(df_1w, file = "C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0524\\pos-代综pos3.csv")

df_1 <- read.csv('C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0524\\pos-代综pos3.csv',sep = ";")

df_01 <- merge(df_0, df_1, by = c('患者姓名'))
write_excel_csv(df_01, file = "C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0524\\data-pos代综pos3.csv")

##### 0827TBS #####
df2R <- df2  %>% filter(侧 == '右')
df2L <- df2  %>% filter(侧 == '左')
df_12R <- merge(df_1, df2R, by = c('姓名','性别','ID'))
df_12R3 <- merge(df_12R, df3, by = c('姓名','性别'))
df_12R34 <- merge(df_12R3, df4, by = c('姓名','性别'))
write_excel_csv(df_12R34, file = "C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0827\\TBS_股骨右_脊椎_科室.csv")

##### 0905总处理 #####
rm(list = ls())
library(xlsx)
setwd("C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\BIAO\\0827\\0903\\")
dt <- read_excel('data0+股骨L +前臂L+全身CoreScan+全身成份+全身+正位脊柱 2(2).xlsx', col_names =T)
# dt <- read.csv('data0+股骨L+前臂L+全身CoreScan+全身成份+全身+正位脊柱.csv',row.names=1)
dt1 <- read_excel('总处理-需要处理数据名单-pre.xlsx')
dt2 <- read_excel('总处理-需要处理数据名单-pos3.xlsx')
dt3 <- read_excel('总处理-需要处理数据名单-pos12.xlsx')

# rownames(dt)=dt[,1]  #取出第一列
# row.names(dt)<-dt$姓名
unique(dt$姓名)
dt0 <- dt[!duplicated(dt$姓名), ]      

dt_30 <- merge(dt3, dt0, by = c('姓名','性别'))
write_excel_csv(dt_30, file = "全部术后12m_data0+股骨L +前臂L+全身CoreScan+全身成份+全身+正位脊柱.csv", 
                delim = ",", quote = "all")
write.xlsx(dt_10, file = "全部术前_data0+股骨L +前臂L+全身CoreScan+全身成份+全身+正位脊柱.xlsx",
           row.names = F)


##### 样本量计算 #####
# pwr | 谁说样本量计算是个老大难问题！？（三）（配对样本与非等比样本篇）(https://zhuanlan.zhihu.com/p/596431672)
rm(list = ls())
library(pwr)         # CRAN v1.3-0
library(tidyverse)   # CRAN v2.0.0 # CRAN v2.0.0 # CRAN v2.0.0

# 4.1 计算Cohen’s d
mu_x <- 130     ### Baseline体重均值
mu_y <- 125     ### diet后体重均值

sd_x <- 11      ### Basline体重SD
sd_y <- 12      ### diet后体重SD

rho <- 0.5      ### diet前后相关性

sd_z <- sqrt(sd_x^2 + sd_y^2 - 2*rho*sd_x*sd_y)

d_z <- abs(mu_x - mu_y) / sd_z
d_z

# 4.2 pwr计算样本量
n.paired <- pwr.t.test(d = d_z, power = 0.80, sig.level = 0.05, type = "paired")
n.paired

# Power Analysis
# 效力分析（Power Analysis），配对t检验的样本量变化对power的影响吧。与之前的示例一样，随着我们增加样本量，估算的不确定性也随之减小。通过减少这种不确定性，我们在估算中更好地避免了II类错误。
n_z <- seq(1, 80, 5)
n_z.change <- pwr.t.test(d = d_z, n = n_z, sig.level = 0.05, type = "paired")
n_z.change.df <- data.frame(n_z, power = n_z.change$power * 100)
n_z.change.df

plot(n_z.change.df$n, 
     n_z.change.df$power, 
     type = "b", 
     xlab = "Sample size, n", 
     ylab = "Power (%)")

# 改变effect size时power如何变化。改一下患者节食导致的平均体重变化，看看减少到100磅时power如何变化。这里我们从50磅开始，逐渐增加到130磅，间隔5磅。
mu_y <- seq(50, 130, 5)

d_z <- abs(mu_x - mu_y) / sd_z

n_z.change <- pwr.t.test(d = d_z, n = 44, sig.level = 0.05)
n_z.change.df <- data.frame(d_z, power = n_z.change$power * 100)
n_z.change.df

plot(n_z.change.df$d_z, 
     n_z.change.df$power, 
     type = "b", 
     xlab = "Cohen's d_z", 
     ylab = "Power (%)",
     xlim = c(0, 2))

## 不等比样本量的Power Analysis
# 7.1 计算并合标准偏差
# 首先我们计算一下并合标准偏差(pooled standard deviation, σpooled)。
sd1 <- 1.25
sd2 <- 1.01
sd_pooled <- sqrt((sd1^2 +sd2^2) / 2)
sd_pooled

# 7.2 计算Cohen’s d
mu1 <- 1.5
mu2 <- 1.4
d <- (mu1 - mu2) / sd_pooled
d

# 7.3 Power Analysis
n1 <- 130
n2 <- 120

power.diff_n <- pwr.t2n.test(d = d, n1 = n1, n2 = n2, sig.level = 0.05)
power.diff_n


















