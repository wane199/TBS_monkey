# R语言一次性读入多个excel文件然后根据指定的列批量合并多个数据框
# [R语言多个数据集根据某一列取交集然后合并](https://www.bilibili.com/video/BV1bK4y1f78q/?spm_id_from=333.788.recommend_more_video.0&vd_source=23f183f0c5968777e138f31842bde0a0)
rm(list=ls())
library(dplyr) # 加载程序包dplyr
library(readxl)
library(tidyverse)
getwd()

df0 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\IDXA患者名单.csv')

df1 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\2017_AHA.xlsx')
df2 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\2021_AHA.xlsx')
df3 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\d_AHA.xlsx')
df4 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\病人_正位脊柱.xlsx')
total <- rbind(df1,df2,df3) 
df1 <- base::unique(total)
write_excel_csv(df, file = "C:\\Users\\wane1\\Documents\\LYX\\csv\\AHA.csv")
df1 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\AHA.csv')
df <- df0[,complete.cases(t(df0))]  # 提取不含空值的列

# df1 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\_股骨_side.csv')
# df2 <- read_excel('C:\\Users\\wane1\\Documents\\LYX\\excel\\_全身 CoreScan.xlsx')
# dim(df1)
# head(df3)
# str(df1)
# df0 <- base::unique(df0)
# df1 <- base::unique(df1)
# df2 <- base::unique(df2)
# 
df1 <- df1[,complete.cases(t(df1))]   # 提取不含空值的列
df1L <- df1  %>% filter(侧 == '左')
df1R <- df1  %>% filter(侧 == '右')
df11 <- merge(df1L,df1R, by = c('姓名','检查号','性别','年龄'), suffixes = c(".L",".R"))
df11 <- df11[,complete.cases(t(df11))]   # 提取不含空值的列

write_excel_csv(df11, file = "C:\\Users\\wane1\\Documents\\LYX\\csv\\AHA_side.csv")
# 
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

df1 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\AHA_side.csv')
df2 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\股骨_side.csv')
df3 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\全身.csv')
dim(df1)
head(df2)
str(df1)
df1 <- base::unique(df1) 
df2 <- base::unique(df2) 
df3 <- base::unique(df3) 

df3L <- df3  %>% filter(侧 == '左')
df3R <- df3  %>% filter(侧 == '右')
df33 <- merge(df3L,df3R, by = c('姓','病人ID号。','性别','年龄'), suffixes = c(".L",".R"))
df33 <- base::unique(df33) 
# df33 <- df33[,complete.cases(t(df33))]   # 提取不含空值的列
write_excel_csv(df33, file = "C:\\Users\\wane1\\Documents\\LYX\\csv\\2021_AHA_side.csv")

df_12 <- merge(df1, df2, by = c('姓名','性别','年龄','生日'))
df_12 <- base::unique(df_12) 
df_123 <- merge(df_12, df3, by = c('姓名','性别','年龄','生日'))
df_123 <- base::unique(df_123) 

df4 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\全身CoreScan.csv')
df5 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\全身成份.csv')
df6 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\全身自定义.csv')
df7 <- read.csv('C:\\Users\\wane1\\Documents\\LYX\\csv\\全身自定义成分.csv')
df4 <- base::unique(df4) 
df5 <- base::unique(df5) 
df6 <- base::unique(df6) 
df7 <- base::unique(df7) 

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
