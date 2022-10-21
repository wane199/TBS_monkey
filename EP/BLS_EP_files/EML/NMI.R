# An interpretable mortality prediction model for COVID-19 patients(nature machine intelligence)
# install.packages("treeheatr")
rm(list = ls())
library(treeheatr)

dt <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\Structured_Data\\process_PT-22.csv")
str(dt) ## 查看每个变量结构
summary(dt)

dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
dt <- dt[c(5:19)]
table(dt$oneyr)
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
library(pheatmap)
library(RColorBrewer)
set.seed(123)
dt <- as.matrix(dt)
heatmap(dt, Colv = NA, Rowv = NA, scale = "column", col = cm.colors(256))
heatmap(dt, Colv = NA, Rowv = NA, scale = "column", col = terrain.colors(256))
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(dt, Colv = NA, Rowv = NA, scale = "column",
        cex.axis = 0.5, cex.lab = 2, cex.main = 3, margins = c(5, 5), col = coul)

pheatmap(dt, cluster_row = F, cluster_col = FALSE, scale = "none", angle = 45,color = c(colorRampPalette(colors = c("blue","white"))(length(bk)/2),colorRampPalette(colors = c("white","red"))(length(bk)/2)))
pheatmap(dt, scale = "none", cluster_row = F, cluster_col = FALSE, fontsize = 6, angle = 45,
         color = colorRampPalette(colors = c("blue","white","red"))(100))
pheatmap(dt, scale = "row", cluster_row = F, cluster_col = FALSE, fontsize = 6, colour = colorRampPalette(brewer.pal(8, "PiYG"))(25), display_numbers = F)

# 9. 注释
Group <- unlist(dt$Y) # 定义列名
group_sample <- data.frame(Group)
rownames(group_sample) <- rownames(dt)
group_sample$Group <- factor(group_sample$Group)
# 病例分组文件
head(group_sample)
pheatmap(dt,
         angle_col = 45, annotation_row = group_sample, # 聚类结果分成两类
         # gaps_row = c(0), # 在5和10行添加分隔  cutree_rows = 2, # 分割行 cutree_cols=2, # 分割列
         scale = "column", # 列标准化 scale="row", # 行标准化
         annotation_legend = T, border_color = "black", # 设定每个格子边框的颜色，border=F则无边框
         cluster_rows = F, cluster_cols = F, # 对列聚类
         show_colnames = T, show_rownames = F # 是否显示行名
)

# [组内聚类](https://zhuanlan.zhihu.com/p/363769759)
library(ComplexHeatmap)
Heatmap()

# Heat map & Radiomics 
library(data.table)
mydata <- read.csv("./TBS/app/data/heat.csv",row.names=1)
# data <- mydata[-1]
dt <- transpose(mydata)
colnames(dt) <- c("RF","ETC","GBC","EGB","KNN","DTC")
rownames(dt) <- c("Acc","AUC","Recall","Prec.","F1","Kappa")
heatmap(as.matrix(mydata),Colv = NA, symm = F, Rowv = NA)
heatmap(as.matrix(dt),symm = F, add.expr, Colv = NA,
        Rowv = NA, scale = "column",  xlab = "Performance", ylab = "Classifier",
        main = "Heatmap", col = cm.colors(256)) # 颜色  
# write.csv(dt,"./TBS/app/data/heat-ML.csv")
# [pheatmap热图技巧合集](https://www.jianshu.com/p/86ae39a227f4)
library(pheatmap)
set.seed(123)
# 在单元格中显示对于的数值，可以设置 display_numbers = TRUE, colorRampPalette(brewer.pal(8, "PiYG"))(25)
pheatmap(as.matrix(dt), angle_col = "0", col = cm.colors(256), legend = FALSE, cluster_row = F, cluster_col = FALSE, display_numbers = TRUE)
# 对显示的数值进行格式化, 显示为科学计数法
pheatmap(dt, scale = "row", angle_col = "0", display_numbers = TRUE,cluster_row = F, cluster_col = FALSE, 
         number_format = "%.1e", number_color = "#4daf4a", fontsize_number = 10)

# [利用ggplot()绘制环状热图](https://mp.weixin.qq.com/s?__biz=MzkyODIyOTY5Ng==&mid=2247485815&idx=1&sn=1769b481c233d258b545d4b54bd08ae7&chksm=c21ab958f56d304ecc9724ffd440850e7616aa05fd50ea33fe8ae25f29ce568927d6fa3f8434&scene=178&cur_album_id=1871868632998215682#rd)
# 加载数据处理R包
library(tidyverse)
library(reshape2)








