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

# [多分组热图不用愁，Pheatmap](https://www.sohu.com/a/283377402_785442)
# [R 数据可视化 —— 聚类热图 pheatmap](https://www.jianshu.com/p/c7beb48e8398)














# Heat map & Radiomics 
library(data.table)
mydata <- read.csv("./TBS/app/data/heat.csv")
data <- mydata[-1]
dt <- transpose(data)
colnames(dt) <- c("RF","ETC","GBC","EGB","KNN","DTC")
rownames(dt) <- c("Acc","AUC","Recall","Prec.","F1","Kappa")
heatmap(as.matrix(mydata[-1]),Colv = NA, symm = F,
        Rowv = NA)
heatmap(as.matrix(dt),symm = F, add.expr,
        Colv = NA,
        Rowv = NA, scale = "column",
        xlab = "Performance",
        ylab = "Classifier",
        main = "Heatmap",
        col = cm.colors(256)) # 颜色  
# write.csv(dt,"./TBS/app/data/heat-ML.csv")
# https://www.jianshu.com/p/86ae39a227f4
library(pheatmap)
# 在单元格中显示对于的数值，可以设置 display_numbers = TRUE
pheatmap(dt, angle_col = 0, color = cm.colors(25),
         cluster_row = F, cluster_col = FALSE, fontsize = 8, display_numbers = TRUE)

# 对显示的数值进行格式化
pheatmap(dt,scale = "row",display_numbers = TRUE,
  # 显示为科学计数法
  number_format = "%.1e",
  # 设置颜色
  number_color = "#4daf4a",
  # 设置数值字体大小
  fontsize_number = 10
)






