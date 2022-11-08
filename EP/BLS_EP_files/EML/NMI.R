# An interpretable mortality prediction model for COVID-19 patients(nature machine intelligence)
# install.packages("treeheatr")
rm(list = ls())
library(treeheatr)

dt <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\Structured_Data\\process_PT-22.csv")
str(dt) ## 查看每个变量结构
summary(dt)

dt <- read.csv("/home/wane/Desktop/EP/sci/cph/XML/TLE234group_2019_factor.csv")
dt <- dt[c(7:23)]
dtx <- as.data.frame(scale(dt[4:17]))
dt <- mutate(dt[, 1:3], dtx) 
table(dt$oneyr)
train <- subset(dt, dt$Group1 == "Training")
test <- subset(dt, dt$Group1 == "Test")

heat_tree(dt, target_lab = "oneyr", task = 'classification')

# 任意修改图片颜色
heat_tree(dt, target_lab = "oneyr",
          target_cols = c("royalblue1", "palegreen3")) # 修改颜色
# 选择将变量全部在热图中展示
heat_tree(dt, target_lab = "oneyr",
          show_all_feats = TRUE)
# 各个分类组别之间的距离加宽一些
heat_tree(dt, target_lab = "Y",
          panel_space = 0.03)   # 调整组别间的距离
# 只想要决策树，也可以去掉热图部分图片： 
heat_tree(dt, target_lab = "oneyr",show_all_feats = TRUE,
          show = "heat-only")  # 只显示决策树/热图show = "heat-tree",

# change-in-estimate(https://mp.weixin.qq.com/s?__biz=MzIzMzc1ODc4OA==&mid=2247485666&idx=1&sn=f28aff82d66af79d099f237e8e302f89&chksm=e88181c9dff608df3a55a831f6d7dbcce32a29cb1f52ae16bf994c4585b06a52d1013084af29&mpshare=1&scene=24&srcid=1108Gz1ANkBpbNLK03j9kR93&sharer_sharetime=1667838586772&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(chest)

results <- chest_speedglm(
  crude = "Endpoint ~ Diabetes",
  xlist = c("Age", "Sex", "Married", "Smoke", 
            "Cancer", "CVD","Education", "Income"),
  data = diab_df)
chest_plot(results)
chest_forest(results)  

################################################
# Heat map & Radiomics 
rm(list = ls())
library(data.table)
mydata <- read.csv("./TBS/app/data/heat.csv", row.names=1)
dt <- t(mydata)
# dt <- transpose(mydata)
colnames(dt) <- c("RF","ETC","GBC","EGB","KNN","DTC")
rownames(dt) <- c("Acc","AUC","Recall","Prec.","F1","Kappa")
par(pin = c(5,3))#pin()函数控制图形的尺寸
heatmap(as.matrix(mydata),Colv = NA, symm = F, Rowv = NA)
heatmap(as.matrix(dt),symm = F, add.expr, Colv = NA,
        Rowv = NA, scale = "column",  xlab = "Performance", ylab = "Classifier",
        main = "Heatmap", col = cm.colors(256)) # 颜色  
# write.csv(dt,"./TBS/app/data/heat-ML.csv")
# [pheatmap热图技巧合集](https://www.jianshu.com/p/86ae39a227f4)
library(pheatmap)
set.seed(123)
# 在单元格中显示对于的数值，可以设置 display_numbers = TRUE, colorRampPalette(brewer.pal(8, "PiYG"))(25)
pheatmap(as.matrix(dt), angle_col = "0", col = cm.colors(256), legend = FALSE, margins = c(15, 15), 
         cellwidth = 70, cellheight = 65, number_format = "%.3f", fontsize_number = 20, fontsize = 15, cluster_row = F, cluster_col = FALSE, display_numbers = TRUE)
# 对显示的数值进行格式化, 显示为科学计数法
library(RColorBrewer)
pheatmap(dt, scale = "row", angle_col = "0", display_numbers = TRUE,cluster_row = F, cluster_col = FALSE, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100), 
         number_format = "%.1e", number_color = "#4daf4a", fontsize_number = 20)


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

# https://officeguide.cc/r-ggplot2-elegant-tiled-heat-maps-tutorial-examples/
library(reshape2)
library(ggplot2)
# 準備原始資料
x <- data.frame(scale(dt[2:16]))
rownames(x)
x$sub <- rownames(dt)
# 將資料表轉為長型表格，宽数据转长数据格式
# x.melt <- melt(x)
x.melt <- melt(x, id.vars = "sub")

# [使用 ggplot 繪製熱圖, ggplot做热图|数据处理|图表设置, waffle热图](https://www.jianshu.com/p/a77503548a79)
# [环状热图](https://mp.weixin.qq.com/s?__biz=MzkyODIyOTY5Ng==&mid=2247485815&idx=1&sn=1769b481c233d258b545d4b54bd08ae7&chksm=c21ab958f56d304ecc9724ffd440850e7616aa05fd50ea33fe8ae25f29ce568927d6fa3f8434&mpshare=1&scene=1&srcid=10211vFAPELgSVA6Rt3zrapA&sharer_sharetime=1666615221816&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# 基础版
ggplot(x.melt, aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value),color = 'black') +
  scale_fill_gradient2(midpoint = 7, # 需要修改的参数
                       low = '#3C8DAD',
                       mid="white",
                       high = '#FF6767')

ggplot(x.melt, aes(x = variable, y = sub, fill = value)) +
  geom_tile(colour = "white", size = 0.25) + # 繪製熱圖
  scale_y_discrete(expand = c(0, 0)) + # 移除多餘空白
  scale_x_discrete(expand = c(0, 0)) + # 移除多餘空白
  coord_fixed() + # 設定 X 與 Y 軸等比例
  scale_fill_gradientn(colours = terrain.colors(10)) + # 設定色盤
  theme(
    legend.text = element_text(face = "bold"), # 說明文字用粗體
    axis.ticks = element_line(size = 0.5), # 座標軸上的刻度寬度
    plot.background = element_blank(), # 移除背景
    panel.border = element_blank(), # 移除邊框
    axis.text.x = element_text(
      angle = 90, vjust = 0.5, hjust = 1
    ) # X 軸文字轉向
  )

# 绘制普通分类变量环状热图：
# X、Y轴为分类变量
ggplot(x.melt,aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value),color = 'white') +
  scale_fill_gradient2(midpoint = 7,# 需要修改的参数
                       low = '#3C8DAD',
                       mid="white",
                       high = '#FF6767') +
  scale_y_discrete(expand = expansion(mult = c(2,0))) +
  scale_x_discrete(expand = expansion(mult = c(0,0.05))) +
  coord_polar(theta = 'x') +
  theme_void() +
  geom_text(data = res,
            aes(x = as.numeric(rownames(res)),
                y = 13,# 需要修改的参数
                label = sub, angle = ang, hjust = hjust),
            size = 2)
# 绘制聚类封闭环状热图：
# 基因聚类

# 聚类, 使用原始宽数据
xclust <- hclust(dist(x[-16]))
# 加载包
library(ggh4x)
library(ggdendro)

# 封闭型
ggplot(x.melt,aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value),color = 'white') +
  # 关键函数，聚类作用
  scale_x_dendrogram(hclust = xclust) + scale_fill_gradientn(colours = cm.colors(10)) +
  # scale_fill_gradient2(midpoint = 7,# 需要修改的参数
  #                      low = '#3C8DAD',
  #                      mid="white",
  #                      high = '#FF6767') +
  scale_y_discrete(expand = expansion(mult = c(2,0))) +
  theme(axis.text.x = element_blank()) +
  coord_polar(theta = 'x') +
  theme_void() +
  geom_text(data = res,
            aes(x = as.numeric(rownames(res)),
                y = 17,# 需要修改的参数
                label = sub, angle = ang, hjust = hjust),
            size = 2.0)

# 开口型
ggplot(x.melt,aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value),color = 'white') +
  # 关键函数，聚类作用
  scale_x_dendrogram(hclust = xclust,
                     expand = expansion(mult = c(0,0.05))) + scale_fill_gradientn(colours = cm.colors(10)) +
  # scale_fill_gradient2(midpoint = 10,# 需要修改的参数
  #                      low = '#3C8DAD',
  #                      mid="white",
  #                      high = '#FF6767') +
  scale_y_discrete(expand = expansion(mult = c(2,0))) +
  theme(axis.text.x = element_blank()) +
  coord_polar(theta = 'x') +
  theme_void() +  theme(legend.position = c(0.5,0.5)) + 
  geom_text(data = res,
            aes(x = as.numeric(rownames(res)),
                y = 16,# 需要修改的参数
                label = sub, angle = ang, hjust = hjust),
            size = 2)

# [AUC/ACC及其森林图展示](https://blog.csdn.net/qazplm12_3/article/details/125139420)
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv", row = 2)
dt <- dt[-1]
table(dt$oneyr)
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")
library(psych)
library(epiDisplay)
library(rms)
library(patchwork)
p1 <- pairs.panels(train[-1])
p2 <- pairs.panels(test[-1])
p1 + p2 + plot_annotation(tag_levels = 'A') + plot_layout(guides='collect')
library(ggpubr)
ggarrange(p1, p2,
          ncol = 2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")

ddist <- datadist(train)
options(datadist = "ddist")
var <- paste0(colnames(train[c(-1:-2)]), collapse = "+")
var

## Model with age and sex
logit.clinic <- glm(oneyr ~side+Sex+Surgmon+Durmon+SE+SGS+early_brain_injury+familial_epilepsy+brain_hypoxia+Central_Nervous_System_Infections+traumatic_brain_injury+history_of_previous_surgery+MRI+radscore, 
                    data = train, family = binomial)
lroc(logit.clinic, graph = F)$auc

predict_ <- predict.glm(logit.clinic, type = "response", newdata = test)
predict <- ifelse(predict_ > 0.5, 1, 0)
test$predict <- predict
library(reportROC) # Confusion Matrix
reportROC(
  gold = test$oneyr, predictor = test$predict,
  plot = T, important = "se", exact = FALSE
)
reportROC(
  gold = test$oneyr, predictor.binary = test$predict,
  plot = T, important = "se", exact = FALSE
)

# 森林图绘制
library(forestploter)
dt <- read.csv("/home/wane/Documents/EP_code/git/Presentation/TBS/app/data/ACC_CI.csv")
# , row = 2
# 简单处理下数据格式，比如组别前面添加空格。
# dt$Model <- ifelse(is.na(dt$Accuracy),
#                        dt$Model,
#                        paste0("      ", dt$Model))
# 再创建一列空列，用来后面存放森林图的图形部分。
dt$` ` <- paste(rep(" ", 20), collapse = " ")
# 正常需要在图形显示数据的文本部分，这里可以使用代码实现，也可以提前在Excel中填好。
dt$`Accuracy(95% CI)` <- ifelse(is.na(dt$Accuracy), "",
                           sprintf("%.3f (%.3f to %.3f)",
                                   dt$Accuracy, dt$LowerCI, dt$UpperCI))
# 首先选中需要在图上显示的列别，这里只需要数据框中的第1，5，6列，2，3，4列用来绘制图形部分。
# 然后使用ci_column参数指定图形部分存放的位置。
plot <- forest(dt[, c(1, 8, 9)],
               est = dt$Accuracy,
               lower = dt$LowerCI,
               upper = dt$UpperCI,
               ci_column = 2, ref_line = 0.80, xlim = c(0.80, 0.9),
               # arrow_lab = c("Placebo Better", "Treatment Better"),
               footnote = "Accuracy (95%CI)")
plot



