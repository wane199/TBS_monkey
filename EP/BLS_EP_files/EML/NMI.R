# An interpretable mortality prediction model for COVID-19 patients
install.packages("treeheatr")
library(treeheatr)

dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
summary(dt)
dt <- dt[c(5:19)]
heat_tree(dt, target_lab = "oneyr", task = 'classification')

# 任意修改图片颜色
heat_tree(dt, target_lab = "oneyr",
          target_cols = c("royalblue1", "palegreen3")) # 修改颜色
# 选择将变量全部在热图中展示
heat_tree(dt, target_lab = "oneyr",
          show_all_feats = TRUE)
# 各个分类组别之间的距离加宽一些
heat_tree(dt, target_lab = "oneyr",
          panel_space = 0.03)   # 调整组别间的距离
# 只想要决策树，也可以去掉热图部分图片： 
heat_tree(dt, target_lab = "oneyr",show_all_feats = TRUE,
          show = "heat-only")  # 只显示决策树/热图show = "heat-tree",

