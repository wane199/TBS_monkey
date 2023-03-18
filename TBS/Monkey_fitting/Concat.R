# R语言一次性读入多个excel文件然后根据指定的列批量合并多个数据框
# [R语言多个数据集根据某一列取交集然后合并](https://www.bilibili.com/video/BV1bK4y1f78q/?spm_id_from=333.788.recommend_more_video.0&vd_source=23f183f0c5968777e138f31842bde0a0)
rm(list=ls())
library(dplyr) # 加载程序包
library(readxl)
library(tidyverse)
getwd()

df0 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\IDXA患者名单整理.xlsx')

df1 <- read_excel('x')
df2 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\CoreScan_全部整理.xlsx')
df3 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\股骨_全部整理.xlsx')
df4 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\全身BMD_全部整理.xlsx')
df5 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\身体成分_全部整理.xlsx')
df6 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel0210\\正位腰椎_全部整理.xlsx')
df0 <- base::unique(df0)
df1 <- base::unique(df1)
df2 <- base::unique(df2)
df3 <- base::unique(df3)
df4 <- base::unique(df4)
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

df1 <- read.csv('C:\\Users\\wane1\\Downloads\\Most_Relevant_Countries_By_Corresponding_Author.csv', sep = ";")
df2 <- read.csv('C:\\Users\\wane1\\Downloads\\Most_Cited_Countries.csv', sep = ";")

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

library("tidyverse")
data_list %>% reduce(inner_join, by = "id")   




df_56 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv0210\\IDXA患者名单_身体成分_正位腰椎.csv')
df_234560 <- merge(df1, df2, by = c('国家')) # , all = TRUE
df_234560 <- base::unique(df_234560) 
write_excel_csv(df_234560, file = "C:\\Users\\wane1\\Downloads\\Most_Cited_Countries-cancat.csv")
