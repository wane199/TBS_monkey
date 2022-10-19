# An interpretable mortality prediction model for COVID-19 patients
# install.packages("treeheatr")
rm(list = ls())
library(treeheatr)

dt <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\Structured_Data\\process_PT-22.csv")
str(dt) ## 查看每个变量结构
summary(dt)

dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
dt <- dt[c(5:19)]
heat_tree(dt, target_lab = "Y", task = 'classification')

# 任意修改图片颜色
heat_tree(dt, target_lab = "Y",
          target_cols = c("royalblue1", "palegreen3")) # 修改颜色
# 选择将变量全部在热图中展示
heat_tree(dt, target_lab = "Y",
          show_all_feats = TRUE)
# 各个分类组别之间的距离加宽一些
heat_tree(dt, target_lab = "Y",
          panel_space = 0.03)   # 调整组别间的距离
# 只想要决策树，也可以去掉热图部分图片： 
heat_tree(dt, target_lab = "Y",show_all_feats = TRUE,
          show = "heat-only")  # 只显示决策树/热图show = "heat-tree",

