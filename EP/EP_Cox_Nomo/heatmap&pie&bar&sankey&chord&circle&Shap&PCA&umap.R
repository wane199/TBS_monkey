# https://r-graph-gallery.com/215-the-heatmap-function.html
rm(list = ls())
# 读入数据
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv", row = 2)
# dt <- read.csv("/media/wane/UNTITLED/BLS-ep-pre/EP/Structured_Data/Task2/COX12mon/TLE234group.csv")
dt <- dt[-1]

## Create a variable indicating 1-year event**
dt <- within(dt, {
  outcome1yr <- NA
  outcome1yr[(Rel._in_5yrs == 1) & (Follow_up_timemon <= 1 * 12)] <- 1 # event+ within two years
  outcome1yr[(Rel._in_5yrs == 0) | (Follow_up_timemon > 1 * 12)] <- 0 # otherwise
})
summary(dt)

# 列名数组
cols <- colnames(dt)
# 最后一列移到第七列
n_cols <- c(cols[1:6], cols[length(cols)], cols[7:(length(cols) - 1)])
dt <- dt[, n_cols]

table(dt$oneyr)
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")
# dtx <- scale(dt[, c(7)])

# rownames(dt) <- dt[, 1]
dt <- dt[order(dt$oneyr), ] # 重排序
data <- as.matrix(dt[2:16])
# t(data) # transpose the matrix with to swap X and Y axis.
# Use 'scale' to normalize
# No dendrogram nor reordering for neither column or row
heatmap(data, Rowv = NA, margins = c(3, 3), Colv = NA, scale = "column")

# 1: native palette from R
heatmap(data, Colv = NA, Rowv = NA, scale = "column", col = cm.colors(256))
heatmap(data, Colv = NA, Rowv = NA, scale = "column", col = terrain.colors(256))
# 2: Rcolorbrewer palette
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data,
  Colv = NA, Rowv = NA, scale = "column",
  cex.axis = 0.5, cex.lab = 2, cex.main = 3, margins = c(10, 10), col = coul
)

# Example: grouping from the first letter:
my_group <- as.numeric(as.factor(substr(dt$oneyr, 0, 1)))
my_group
colSide <- brewer.pal(8, "Set3")[my_group]
colMain <- colorRampPalette(brewer.pal(9, "Blues"))(50)
heatmap(data, Colv = NA, Rowv = NA, scale = "column", RowSideColors = colSide, col = colMain)
heatmap(data,
  Colv = NA, Rowv = NA, scale = "column", RowSideColors = colSide,
  cex.axis = 0.5, cex.lab = 2, cex.main = 3, margins = c(5, 5), col = coul
)

# [多分组热图不用愁，Pheatmap](https://www.sohu.com/a/283377402_785442)
# [R 数据可视化 —— 聚类热图 pheatmap](https://www.jianshu.com/p/c7beb48e8398)
# [pheatmap热图技巧合集](https://www.jianshu.com/p/86ae39a227f4)
library(pheatmap)
set.seed(123)
pheatmap(data, scale = "column", cluster_row = F, cluster_col = FALSE, fontsize = 6, col = colMain)
pheatmap(data, scale = "column", cluster_row = F, cluster_col = FALSE, fontsize = 6, col = coul, display_numbers = TRUE)

# 9. 注释
Group <- unlist(dt$oneyr) # 定义列名
group_sample <- data.frame(Group)
rownames(group_sample) <- rownames(data)
group_sample$Group <- factor(group_sample$Group)
# 病例分组文件
head(group_sample)
pheatmap(data,
  angle_col = "45", annotation_row = group_sample, # 聚类结果分成两类
  gaps_row = 206, col = coul, # 在5和10行添加分隔  cutree_rows = 2, # 分割行 cutree_cols=2, # 分割列
  scale = "column", # 列标准化 scale="row", # 行标准化
  annotation_legend = F, border = F, # 设定每个格子边框的颜色，border=F则无边框
  cluster_rows = F, cluster_cols = F, # 对列聚类
  show_colnames = T, show_rownames = F # 是否显示行名
)

# [分组聚类的热图](https://www.jianshu.com/p/b94449be175a)
# [组内聚类](https://zhuanlan.zhihu.com/p/363769759)https://zhuanlan.zhihu.com/p/371525576
library(ComplexHeatmap)
# ComplexHeatmap 包并不会对数据进行标准化，为了让图形更好看，我们先手动对数据进行标准化
head(exp)
exp <- apply(data, 1, scale)
rownames(exp) <- colnames(data)
exp <- t(exp)
Heatmap(exp,
  name = "TLE", row_names_side = "left", column_names_side = "bottom", cluster_rows = F, cluster_columns = F,
  row_split = group_sample
)
# heatmap_legend_param = list(title = ""),

# dist mat
mat <- dist(data)
hclust_mat <- hclust(mat)
hclust_mat$order
hclust_mat$labels

# reorder row_clust
index <- seq(1, 234, by = 1)
hclust_mat$order <- index
pheatmap(data, cluster_rows = hclust_mat, scale = "column", cluster_row = F, cluster_col = FALSE, fontsize = 6, display_numbers = TRUE)

require(gridExtra)

# reorder 1
index <- order(rowSums(data), decreasing = TRUE)
dend <- reorder(as.dendrogram(hclust_mat),
  wts = index
)
row_cluster <- as.hclust(dend)
p1 <- pheatmap(data,
  cluster_rows = row_cluster,
  show_colnames = FALSE
)

# reorder 2
index <- order(rowSums(data), decreasing = FALSE)
dend <- reorder(as.dendrogram(hclust_mat),
  wts = index
)
row_cluster <- as.hclust(dend)
p2 <- pheatmap(data,
  cluster_rows = row_cluster,
  show_colnames = FALSE
)

# extract plot list
plot_list <- list(p1[[4]], p2[[4]])

grid.arrange(arrangeGrob(grobs = plot_list, ncol = 2))

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
  geom_tile(aes(fill = value), color = "black") +
  scale_fill_gradient2(
    midpoint = 7, # 需要修改的参数
    low = "#3C8DAD",
    mid = "white",
    high = "#FF6767"
  )

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
ggplot(x.melt, aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(
    midpoint = 7, # 需要修改的参数
    low = "#3C8DAD",
    mid = "white",
    high = "#FF6767"
  ) +
  scale_y_discrete(expand = expansion(mult = c(2, 0))) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +
  coord_polar(theta = "x") +
  theme_void() +
  geom_text(
    data = res,
    aes(
      x = as.numeric(rownames(res)),
      y = 13, # 需要修改的参数
      label = sub, angle = ang, hjust = hjust
    ),
    size = 2
  )
# 绘制聚类封闭环状热图：
# 基因聚类

# 聚类, 使用原始宽数据
xclust <- hclust(dist(x[-16]))
# 加载包
library(ggh4x)
library(ggdendro)

# 封闭型
ggplot(x.melt, aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value), color = "white") +
  # 关键函数，聚类作用
  scale_x_dendrogram(hclust = xclust) +
  scale_fill_gradientn(colours = cm.colors(10)) +
  # scale_fill_gradient2(midpoint = 7,# 需要修改的参数
  #                      low = '#3C8DAD',
  #                      mid="white",
  #                      high = '#FF6767') +
  scale_y_discrete(expand = expansion(mult = c(2, 0))) +
  theme(axis.text.x = element_blank()) +
  coord_polar(theta = "x") +
  theme_void() +
  geom_text(
    data = res,
    aes(
      x = as.numeric(rownames(res)),
      y = 17, # 需要修改的参数
      label = sub, angle = ang, hjust = hjust
    ),
    size = 2.0
  )

# 开口型
ggplot(x.melt, aes(x = sub, y = variable)) +
  geom_tile(aes(fill = value), color = "white") +
  # 关键函数，聚类作用
  scale_x_dendrogram(
    hclust = xclust,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_gradientn(colours = cm.colors(10)) +
  # scale_fill_gradient2(midpoint = 10,# 需要修改的参数
  #                      low = '#3C8DAD',
  #                      mid="white",
  #                      high = '#FF6767') +
  scale_y_discrete(expand = expansion(mult = c(2, 0))) +
  theme(axis.text.x = element_blank()) +
  coord_polar(theta = "x") +
  theme_void() +
  theme(legend.position = c(0.5, 0.5)) +
  geom_text(
    data = res,
    aes(
      x = as.numeric(rownames(res)),
      y = 16, # 需要修改的参数
      label = sub, angle = ang, hjust = hjust
    ),
    size = 2
  )
# library(maftools)
# plotVaf(maf = maf)


####################################
# pie charts
plot(dt[7:23]) # library
library(ggplot2)
library(dplyr)
library(ggsci)
library(patchwork)
theme_set(theme_classic())

group_by(train, oneyr) %>%
  summarise(percent = n() / nrow(train)) %>%
  ggplot(aes(x = "", y = percent, fill = factor(oneyr))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = -0.5)

blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )
p1 <- group_by(train, oneyr) %>%
  summarise(percent = n() / nrow(train)) %>%
  ggplot(aes(x = factor(1), y = percent, fill = factor(oneyr))) +
  coord_polar(theta = "y", start = -0.05) +
  scale_fill_brewer("Blues") +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_col(colour = "white") +
  geom_text(aes(label = paste0(round(percent * 100, 2), "%")),
    position = position_fill(vjust = 0.5)
  )
p2 <- group_by(test, oneyr) %>%
  summarise(percent = n() / nrow(test)) %>%
  ggplot(aes(x = factor(1), y = percent, fill = factor(oneyr))) +
  coord_polar(theta = "y", start = -0.05) +
  scale_fill_brewer("Blues") +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_col(colour = "white") +
  geom_text(aes(label = paste0(round(percent * 100, 2), "%")),
    position = position_fill(vjust = 0.5)
  )

p1 + p2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")

df <- group_by(train, oneyr) %>%
  summarise(percent = n() / nrow(train)) %>%
  arrange(desc(percent))
pie(df$percent, labels = df$oneyr)

# You can also call the palette using a name.
ggplot(train, aes(y = oneyr, x = radscore)) +
  geom_bin2d(bins = 170) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(data = train, mapping = aes(x = factor(oneyr), fill = factor(oneyr))) +
  geom_bar(stat = "count", fill = "steelblue", colour = "darkred")
# + geom_text(mapping = aes(label = 'count'))

# [跟着nature学绘图之绘制组合版环饼图](https://mp.weixin.qq.com/s?__biz=Mzg3MzQzNTYzMw==&mid=2247500203&idx=1&sn=1558e1f6c010e48b847d6ec094cd3ce2&chksm=cee29935f9951023867f9f58ba9a4ed81d4fbf17ff59fd45a8d44316374908ec12fb7e7cca5a&mpshare=1&scene=1&srcid=12045fVHEBZHTr16CgFuLiiT&sharer_sharetime=1670124218403&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(tidyverse)
library(readxl)
library(camcorder)
library(ggtext)
library(ggsci)
library(cowplot)
# 数据清洗
df <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\REFER\\BLS\\KAI\\coef.minPTcox_lat_14_bar.csv") %>%
  select(1, 2, 3, 4) %>%
  rownames_to_column(var = "id") %>%
  mutate_at(vars(c(S1)), ~ str_split(., " ", simplify = T)[, 1]) %>%
  mutate(group_3 = rep(LETTERS[1:4], times = c(5, 12, 10, 11))) %>%
  mutate(S1 = as.numeric(S1), ID = as.numeric(ID)) %>%
  group_by(group_2) %>%
  mutate(per = S1 / sum(S1) * 10) %>%
  arrange(desc(per)) %>%
  ungroup()
# 绘制饼图
myLabel = as.vector(df$group)   ## 转成向量，否则图例的标签可能与实际顺序不一致

df1 <-  df %>% group_by(group) %>%
  mutate(per = n() / nrow(df) * 100) %>%
  arrange(desc(per))

p <- df %>% group_by(group) %>%
  summarise(per = n() / nrow(df)) %>%
  ggplot(aes(x="",y=per,fill=group))+
  geom_col()+ coord_polar("y")+
  scale_fill_npg()+
  theme_void() + geom_text(aes(label = paste0(round(per * 100, 2), "%")),
                             position = position_fill(vjust = 0.5))+ 
  geom_text(aes(label = group,size=5,colour="blue"),
            position = position_fill(vjust = 0.7, reverse = F)) + 
  theme(legend.position = "non")
p
# 绘制环状条图
p2 <- df %>% group_by(group) %>%
  summarise(per = n() / nrow(df)) %>%
  ggplot(aes(df1)) +
  # 设置刻度线
  annotate("segment", x = -Inf, xend = Inf, y = seq(0,10,1), yend = seq(0,10,1),
           size = rep(c(0.25, 0.1),length.out =11), alpha = 0.5)+
  # 添加标签 
  annotate("label", x = 0, y = seq(0,10,1), label = seq(0,10,1),color="black",
           size = 3, fill = "#FEF8FA",label.padding = unit(0.1,"lines"),
           label.size = 0)+
  geom_col(aes(x = df1$Feature, y = per, fill = group),position = "dodge", width=0.6)+
  scale_y_continuous(limits = c(-4,10)) +
  scale_x_continuous(limits = c(-0.5,38.5), breaks = 1:38.5) +
  scale_fill_manual(values=c("#709AE1FF","#8A9197FF","#D2AF81FF","#FD7446FF",
                             "#D5E4A2FF","#197EC0FF","#F05C3BFF","#46732EFF",
                             "#71D0F5FF","#075149FF","#C80813FF","#91331FFF"))+
  coord_polar()+
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background =element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank())
p2

# 拼图
ggdraw(p2)+
  draw_plot(p,scale=0.23,x=0,y=0)




library(geomtextpath)
df <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\REFER\\BLS\\KAI\\coef.minPTcox_lat_14_bar.csv")
pie <- ggplot(df, aes(x = factor(1), fill = factor(Feature))) +
  geom_bar(width = 1)
pie + coord_curvedpolar(theta = "y")
# Demonstrating curved category labels
p <- ggplot(
  df,
  aes(Feature, Coef, fill = Feature)
) + geom_col() +  theme_bw() +
  geom_text(
    aes(label = Coef, y = Coef + 0.05),
    position = position_dodge(0.9), size = 3,
    vjust = 0
  ) + 
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 6.5, vjust = 0.5)
  )
# Standard bar chart in Cartesian Co-ordinates
p
# Standard coord_polar axis labels
p + coord_polar()
# Curved polar co-ordinate labels
p + coord_curvedpolar()

# [堆积条形图](https://mp.weixin.qq.com/s?__biz=MzI1NjUwMjQxMQ==&mid=2247512387&idx=1&sn=1633a49972f6d7cd39c162995b16067e&chksm=ea274ea7dd50c7b1a6d5af8e5ccd31ee27f8accaf0ae2619444a13d6142b6ce782c21c32b560&mpshare=1&scene=1&srcid=0922dn68ZyKRpn3sak6k8GUD&sharer_sharetime=1663855775145&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
library(ggplot2)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(reshape)

group_by(dt, Rel._in_5yrs) %>%
  summarise(percent = n() / nrow(train)) %>%
  ggplot(aes(x = "", y = percent, fill = factor(Rel._in_5yrs))) +
  geom_bar(width = 1, stat = "identity")
dt$side <- as.numeric(as.character(dt$side))
dt$side1 <- dt$side - 1
dt_re <- melt(dt[c(3, 9, 14:22)], id = c("Group"))
# Small multiple
ggplot(dt_re, aes(x = variable, fill = factor(value))) +
  geom_bar(position = "stack", stat = "count") +
  scale_fill_viridis(discrete = T) +
  ggtitle("") +
  geom_text(aes(label = count)) +
  theme_classic() +
  xlab("") +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  ) +
  coord_flip() # 转为横向

# From on a categorical column variable
g <- ggplot(dt_re, aes(variable))
g + geom_bar(aes(fill = factor(value)), width = 0.5) + coord_flip() + # 转为横向
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + theme_classic() +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  ) +
  labs(title = "Categorywise Bar Chart", subtitle = "", caption = "Source: Manufacturers from 'TLE' dataset")

# ggalluvial|炫酷桑基图(Sankey)
# https://cloud.tencent.com/developer/article/1675189
library(ggalluvial)
library(reshape)
library(patchwork)

summary(train)
# 对参数设置尝试的代码
train$Rad <- cut(train$radscore, breaks = c(-Inf, 0.1834, Inf), labels = c("0", "1"), right = TRUE, include.lowest = TRUE)
train$DurMonth <- cut(train$Durmon, breaks = c(-Inf, 108.00, Inf), labels = c("0", "1"), right = TRUE, include.lowest = TRUE)
test$Rad <- cut(test$radscore, breaks = c(-Inf, 0.1834, Inf), labels = c("0", "1"), right = TRUE, include.lowest = TRUE)
test$DurMonth <- cut(test$Durmon, breaks = c(-Inf, 108.00, Inf), labels = c("0", "1"), right = TRUE, include.lowest = TRUE)

dt_re1 <- melt(dt[c(1, 4, 5, 6, 7, 12, 14:15, 17)], id = c("ID"))
dt_re1$value <- as.factor(dt_re1$value)
ggplot(
  dt_re1,
  aes(
    x = variable, stratum = value, alluvium = ID,
    fill = value, label = value
  )
) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(
    stat = "alluvium", lode.guidance = "frontback",
    color = "darkgray"
  ) +
  geom_stratum() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle("")

train_re1 <- melt(train[c(4, 6, 14:15, 17, 23, 24)], id = c("ID"))
train_re1$value <- as.factor(train_re1$value)
a <- ggplot(train_re1, aes(
  x = variable, stratum = value, alluvium = ID,
  fill = value, label = value
)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(
    stat = "alluvium", lode.guidance = "frontback",
    color = "darkgray"
  ) +
  geom_stratum() +
  theme_classic() +
  theme(legend.position = "right") +
  ggtitle("")

test_re1 <- melt(test[c(4, 6, 14:15, 17, 23:24)], id = c("ID"))
test_re1$value <- as.factor(test_re1$value)
b <- ggplot(test_re1, aes(
  x = variable, stratum = value, alluvium = ID,
  fill = value, label = value
)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(
    stat = "alluvium", lode.guidance = "frontback",
    color = "darkgray"
  ) +
  geom_stratum() +
  theme_classic() +
  theme(legend.position = "right") +
  ggtitle("")

a / b + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")


# Chord chart [https://mp.weixin.qq.com/s?__biz=Mzg3MjA3MDUxNQ==&mid=2247489119&idx=1&sn=c87d0734cfb6f0773cbb05856b9ab28b&chksm=cef5ba43f98233553d7a1183dcfdf025745d27c126210be5ab14b458787708d217997cab3984&mpshare=1&scene=1&srcid=1004qGevG5LERqRiGiQcs2Ak&sharer_sharetime=1664861257374&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd]
# 使用和弦图展示变量间的相关性
library(corrplot)
library(circlize)
library(Hmisc)

# 读取数据
# rt <- read.csv(file.choose(), header = T)
rownames(train) <- train[, 4]
rt <- as.matrix(train[c(6, 7, 12, 14:15, 17)])
rownames(test) <- test[, 4]
rt1 <- as.matrix(test[c(6, 7, 12, 14:15, 17)])

# 计算指标间相关性
cor1 <- cor(rt)
# 显示P值
rt <- as.matrix(rt)
p <- rcorr(rt)
p
# 设置图形颜色
col <- c(rgb(1, 0, 0, seq(1, 0, length = 32)), rgb(0, 1, 0, seq(0, 1, length = 32)))
# 删掉相关性=1的数据
cor1[cor1 == 1] <- 0
c1 <- ifelse(c(cor1) >= 0, rgb(1, 0, 0, abs(cor1)), rgb(0, 1, 0, abs(cor1)))
col1 <- matrix(c1, nc = ncol(rt))
# 绘制和弦图
par(mar = c(2, 2, 2, 4))
circos.par(gap.degree = c(3, rep(2, nrow(cor1) - 1)), start.degree = 180)
chordDiagram(cor1, grid.col = rainbow(ncol(rt)), col = col1, transparency = 0.5, symmetric = T)
par(xpd = T)
# colorlegend(col,labels=c(1,0,-1))
colorlegend(col, vertical = T, labels = c(1, 0, -1), xlim = c(1.1, 1.3), ylim = c(-0.4, 0.4))

# https://www.jianshu.com/p/9477a3405545
chordDiagram(
  x = cor1, directional = 1, direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.01, annotationTrack = c("name", "grid", "axis"),
  annotationTrackHeight = c(0.05, 0.08), link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE, transparency = 0.25
)

# R语言绘制和弦图
library(circlize)
library(viridis)
library(reshape2)

df <- read.csv("示例数据1.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
df_melt <- melt(dt[c(4, 6, 9, 14:22)], id = c("ID"))
df_melt <- melt(df, id.vars = "Region")
colnames(df_melt) <- c("from", "to", "value")
df_melt$to <- as.character(df_melt$to)

# 排序
df_sum <- apply(df[, 2:ncol(df)], 2, sum) + apply(df[, 2:ncol(df)], 1, sum)
order <- sort(df_sum, index.return = TRUE, decreasing = TRUE)

df_melt$from <- factor(df_melt$from, levels = df$Region[order$ix], order = TRUE)
df_melt <- dplyr::arrange(df_melt, from)

# 颜色主题方案
mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
names(mycolor) <- df$Region

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

chordDiagram(
  x = cor1,
  # grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE
)

# 添加数据标签和坐标轴
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim <- get.cell.meta.data("xlim")
    sector.index <- get.cell.meta.data("sector.index")
    # 添加数据标签
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 1
    )
    # 添加坐标轴
    circos.axis(
      h = "top",
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2] > 10, yes = 2, no = 1)),
      minor.ticks = 1,
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE
    )
  }
)

library(eoffice)
topptx(filename = "和弦图.pptx")

##########################################
# 模型自带的feature importance, 环状柱状图展示；SHAP技术验证，PCA探究SHAP分析中top变量
# Circular barplot with groups，SHAP plot，biplot(https://mp.weixin.qq.com/s?__biz=MjM5NDM3NjczOA==&mid=2247486488&idx=1&sn=dfa1716520a4651d7de5c1c729f9c295&chksm=a689f15591fe7843c866711a72eb15adcdf7be2e19389c9521e2c3ce03bcbcfba765aef6d146&mpshare=1&scene=1&srcid=1114axjBUfHqmSUluMnmq6um&sharer_sharetime=1668438220853&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# library(https://r-graph-gallery.com/297-circular-barplot-with-groups.html)
library(tidyverse)
data <- read.csv("/home/wane/Desktop/EP/REFER/BLS/KAI/coef.minPTcox_lat_14_bar.csv")
data <- data %>% arrange(group)
summary(data)
# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# Make the plot
p <- ggplot(data, aes(x = as.factor(id), y = Coef, fill = group)) + # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat = "identity", alpha = 0.5) +
  # ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  coord_polar() + # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id), 7), y = c(seq(-0.2, 1.0, 0.2)), label = c("-0.2", "0", "0.2", "0.4", "0.6", "0.8", "1.0"), color = "grey", size = 6, angle = 0, fontface = "bold", hjust = 1) + # , label = c("0", "0.5", "1.0", "1.5", "2.0")
  geom_text(data = label_data, aes(x = id, y = Coef + 0.1, label = Feature, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE)
p

# Explaining individual machine learning predictions with Shapley values(https://cran.r-project.org/web/packages/shapr/vignettes/understanding_shapr.html)
library(xgboost)
library(shapr)
data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the combined approach
explanation_combined <- explain(
  x_test,
  approach = c("empirical", "copula", "gaussian", "gaussian"),
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_combined, plot_phi0 = FALSE, index_x_test = c(1, 6))

library(gbm)
#> Loaded gbm 2.1.5

xy_train <- data.frame(x_train, medv = y_train)

form <- as.formula(paste0(y_var, "~", paste0(x_var, collapse = "+")))

# Fitting a gbm model
set.seed(825)
model <- gbm::gbm(
  form,
  data = xy_train,
  distribution = "gaussian"
)

#### Full feature versions of the three required model functions ####

predict_model.gbm <- function(x, newdata) {
  if (!requireNamespace("gbm", quietly = TRUE)) {
    stop("The gbm package is required for predicting train models")
  }

  model_type <- ifelse(
    x$distribution$name %in% c("bernoulli", "adaboost"),
    "classification",
    "regression"
  )
  if (model_type == "classification") {
    predict(x, as.data.frame(newdata), type = "response", n.trees = x$n.trees)
  } else {
    predict(x, as.data.frame(newdata), n.trees = x$n.trees)
  }
}

get_model_specs.gbm <- function(x) {
  feature_list <- list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(x$Terms, "dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes == "factor"] <- NA # the model object doesn't contain factor levels info

  return(feature_list)
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(xy_train, model)
#> The columns(s) medv is not used by the model and thus removed from the data.
p0 <- mean(xy_train[, y_var])
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
# Plot results
plot(explanation)

#### Minimal version of the three required model functions ####
# Note: Working only for this exact version of the model class
# Avoiding to define get_model_specs skips all feature
# consistency checking between your data and model

# Removing the previously defined functions to simulate a fresh start
rm(predict_model.gbm)
rm(get_model_specs.gbm)

predict_model.gbm <- function(x, newdata) {
  predict(x, as.data.frame(newdata), n.trees = x$n.trees)
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(x_train, model)
#> get_model_specs is not available for your custom model. All feature consistency checking between model and data is disabled.
#> See the 'Advanced usage' section of the vignette:
#> vignette('understanding_shapr', package = 'shapr')
#> for more information.
#> The specified model provides feature labels that are NA. The labels of data are taken as the truth.
p0 <- mean(xy_train[, y_var])
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
# Plot results
plot(explanation)

# Principal Component Analysis with Biplot Analysis in R(https://medium.com/@RaharditoDP/principal-component-analysis-with-biplot-analysis-in-r-ee39d17096a1)
# create coloumn branch become row names
dataset <- read.csv("/home/wane/Desktop/EP/sci/cph/XML/TLE234group_2019.csv")
str(dataset)
datasetnew <- dataset[, c(-1:-7)]
rownames(datasetnew) <- dataset[, 4]
View(datasetnew)

# show eigen value score of PCA
library(factoextra)
library(FactoMineR)

res.pca <- PCA(datasetnew, graph = FALSE)
res.pca$eig
# show scree plot of PCA
fviz_screeplot(res.pca, addlabels = TRUE)
# show pricipal component score
res.pca$ind$coord
# show graph of two dimensional variable
fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# show graph of biplot analysis
fviz_pca_biplot(res.pca, repel = TRUE, ggtheme = theme_minimal())

fviz_pca_var(res.pca, col.var = "steelblue")
# Control variable colors using their contributions
fviz_pca_var(res.pca,
  col.var = "contrib",
  gradient.cols = c("white", "blue", "red"),
  ggtheme = theme_minimal()
)
# Graph of variables(https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html)
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
head(var$contrib)
# Correlation circle
library(corrplot)
corrplot(var$cos2, is.corr = FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca,
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE # Avoid text overlapping
)
# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

fviz_pca_ind(res.pca,
  label = "none", # hide individual labels
  habillage = dataset$oneyr, # color by groups
  addEllipses = TRUE, # Concentration ellipses
  palette = "jco"
)
fviz_pca_ind(res.pca,
  col.ind = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca,
  pointsize = "cos2",
  pointshape = 21, fill = "#E7B800",
  repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca,
  col.ind = "cos2", pointsize = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE # Avoid text overlapping (slow if many points)
)


fviz_cos2(res.pca, choice = "ind")
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

fviz_pca_ind(res.pca,
  geom.ind = "point", # show points only (nbut not "text")
  col.ind = factor(dataset$oneyr), # color by groups
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  addEllipses = TRUE, # Concentration ellipses
  legend.title = "Groups"
)

# Add confidence ellipses
fviz_pca_ind(res.pca,
  geom.ind = "point", col.ind = factor(dataset$oneyr),
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  addEllipses = TRUE, ellipse.type = "confidence",
  legend.title = "Groups"
)

fviz_pca_ind(res.pca,
  label = "none", # hide individual labels
  habillage = factor(dataset$oneyr), # color by groups
  addEllipses = TRUE, # Concentration ellipses
  palette = "jco"
)

# Add confidence ellipses
fviz_pca_ind(res.pca,
  geom.ind = "point",
  col.ind = factor(dataset$oneyr), # color by groups
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  addEllipses = TRUE, ellipse.type = "confidence",
  legend.title = "Groups"
)
# Convex hull
fviz_pca_ind(res.pca,
  geom.ind = "point",
  col.ind = factor(dataset$oneyr), # color by groups
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  addEllipses = TRUE, ellipse.type = "convex",
  legend.title = "Groups"
)

# umap(https://datavizpyr.com/how-to-make-umap-plot-in-r/)
# 使用umap包进行UMAP降维可视化分析
library(umap)
data.labels <- dataset$oneyr
# 使用umap函数进行UMAP降维分析
data.umap <- umap::umap(datasetnew)
data.umap
## umap embedding of 150 items in 2 dimensions
## object components: layout, data, knn, config

# 查看降维后的结果
head(data.umap$layout)

# 使用plot函数可视化UMAP的结果
plot(data.umap$layout,
  col = data.labels, pch = 16, asp = 1,
  xlab = "UMAP_1", ylab = "UMAP_2",
  main = "A UMAP visualization of the TLE dataset"
)
# 添加分隔线
abline(h = 0, v = 0, lty = 2, col = "gray")
# 添加图例
legend("topright",
  title = "Species", inset = 0.01,
  legend = unique(data.labels), pch = 16,
  col = unique(data.labels)
)

# 使用uwot包进行UMAP降维可视化分析
library(uwot)

head(iris)

# 使用umap函数进行UMAP降维分析
iris_umap <- uwot::umap(dataset)
head(iris_umap)


# 使用plot函数可视化UMAP降维的结果
plot(iris_umap,
  col = dataset$oneyr, pch = 16, asp = 1,
  xlab = "UMAP_1", ylab = "UMAP_2",
  main = "A UMAP visualization of the iris dataset"
)
# 添加分隔线
abline(h = 0, v = 0, lty = 2, col = "gray")
# 添加图例
legend("topright",
  title = "Species", inset = 0.01,
  legend = unique(dataset$oneyr), pch = 16,
  col = unique(dataset$oneyr)
)

# Supervised dimension reduction using the 'Species' factor column
data_sumap <- uwot::umap(dataset,
  n_neighbors = 15, min_dist = 0.001,
  y = dataset$oneyr, target_weight = 0.5
)
head(data_sumap)


data_sumap_res <- data.frame(data_sumap, Oneyr = dataset$oneyr)
head(data_sumap_res)


# 使用ggplot2包可视化UMAP降维的结果
library(ggplot2)

ggplot(data_sumap_res, aes(X1, X2, color = Oneyr)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  geom_vline(xintercept = 0, lty = 2, col = "blue", lwd = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "UMAP_1", y = "UMAP_2",
    title = "A UMAP visualization of the TLE dataset"
  )
