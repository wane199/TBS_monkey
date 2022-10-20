# https://r-graph-gallery.com/215-the-heatmap-function.html
# 清理工作环境
rm(list = ls())
# 读入数据
dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/Task2/TLE234group.csv")
dt <- read.csv("C:/Users/wane199/Desktop/EP/Structured_Data/Task2/TLE234group.csv")
dt <- read.csv("/media/wane/UNTITLED/BLS-ep-pre/EP/Structured_Data/Task2/COX12mon/TLE234group.csv")
table(dt$Freq)
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")

rownames(dt) <- dt[, 1]
data <- as.matrix(dt[5:22])
# t(data) # transpose the matrix with to swap X and Y axis.
# Default Heatmap
heatmap(data,
  Rowv = NA, margins = c(3, 3),
  Colv = NA, scale = "column"
)
# Use 'scale' to normalize
heatmap(data, scale = "column")
# No dendrogram nor reordering for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale = "column")

# 1: native palette from R
heatmap(data, Colv = NA, Rowv = NA, scale = "column", col = cm.colors(256))
heatmap(data, Colv = NA, Rowv = NA, scale = "column", col = terrain.colors(256))
# 2: Rcolorbrewer palette
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data,
  Colv = NA, Rowv = NA, scale = "column",
  cex.axis = 0.5, cex.lab = 2, cex.main = 3,
  margins = c(5, 5), col = coul
)

# Example: grouping from the first letter:
my_group <- as.numeric(as.factor(substr(rownames(data), 1, 1)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(9, "Blues"))(50)
heatmap(data, Colv = NA, Rowv = NA, scale = "column", RowSideColors = colSide, col = colMain)

# https://www.jianshu.com/p/d9e46f4909b9
library(pheatmap)
set.seed(123)
Group <- unlist(dt$Rel._in_5yrs)
group_sample <- data.frame(Group)
rownames(group_sample) <- rownames(data)
group_sample$Group <- factor(group_sample$Group)
# 病例分组文件
group_sample

pheatmap(data, cluster_cols = F, fontsize = 2)
pheatmap(data,
  angle_col = 45,
  annotation_row = group_sample,
  cutree_rows = 2, # 分割行
  # cutree_cols=2, # 分割列
  scale = "column", # 列标准化
  # scale="row", # 行标准化
  annotation_legend = T, border_color = "black", # 设定每个格子边框的颜色，border=F则无边框
  cluster_rows = F, # 对行聚类
  cluster_cols = F, # 队列聚类
  show_colnames = T, # 是否显示列名
  show_rownames = F # 是否显示行名
)
# dist mat
mat <- dist(data)
hclust_mat <- hclust(mat)
hclust_mat$order
hclust_mat$labels

# reorder row_clust
index <- seq(1, 234, by = 1)
hclust_mat$order <- index
pheatmap(data, cluster_rows = hclust_mat)

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
x <- data.frame(scale(dt[5:22]))
x$sub <- rownames(dt)
# 將資料表轉為長型表格
x.melt <- melt(x, id.vars = "sub")

# 使用 ggplot 繪製熱圖
ggplot(x.melt, aes(x = sub, y = variable, fill = value)) +
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

# pie charts
plot(dt[5:8]) # library
library(ggplot2)
library(dplyr)
library(ggsci)
library(patchwork)
theme_set(theme_classic())

group_by(train, Rel._in_5yrs) %>%
  summarise(percent = n() / nrow(train)) %>%
  ggplot(aes(x = "", y = percent, fill = factor(Rel._in_5yrs))) +
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
p1 <- group_by(train, Rel._in_5yrs) %>%
  summarise(percent = n() / nrow(train)) %>%
  ggplot(aes(x = factor(1), y = percent, fill = factor(Rel._in_5yrs))) +
  coord_polar(theta = "y", start = -0.85) +
  scale_fill_brewer("Blues") +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_col(colour = "white") +
  geom_text(aes(label = paste0(round(percent * 100, 2), "%")),
    position = position_fill(vjust = 0.5)
  )
p2 <- group_by(test, Rel._in_5yrs) %>%
  summarise(percent = n() / nrow(test)) %>%
  ggplot(aes(x = factor(1), y = percent, fill = factor(Rel._in_5yrs))) +
  coord_polar(theta = "y", start = -0.85) +
  scale_fill_brewer("Blues") +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_col(colour = "white") +
  geom_text(aes(label = paste0(round(percent * 100, 2), "%")),
    position = position_fill(vjust = 0.5)
  )

p1 + p2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")

df <- group_by(train, Rel._in_5yrs) %>%
  summarise(percent = n() / nrow(train)) %>%
  arrange(desc(percent))
pie(df$percent, labels = df$Rel._in_5yrs)


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

#对参数设置尝试的代码
train$Rad <- cut(train$radscore, breaks = c(-Inf, 0.1834, Inf), labels = c("0","1"), right=TRUE, include.lowest=TRUE)
train$DurMonth <- cut(train$Durmon, breaks = c(-Inf, 108.00, Inf), labels = c("0","1"), right=TRUE, include.lowest=TRUE)
test$Rad <- cut(test$radscore, breaks = c(-Inf, 0.1834, Inf), labels = c("0","1"), right=TRUE, include.lowest=TRUE)
test$DurMonth <- cut(test$Durmon, breaks = c(-Inf, 108.00, Inf), labels = c("0","1"), right=TRUE, include.lowest=TRUE)

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
a <- ggplot(train_re1,  aes( x = variable, stratum = value, alluvium = ID,
    fill = value, label = value)) + scale_fill_brewer(type = "qual", palette = "Set2") +
    geom_flow(stat = "alluvium", lode.guidance = "frontback",
    color = "darkgray") + geom_stratum() + theme_classic() + theme(legend.position = "right") + ggtitle("")

test_re1 <- melt(test[c(4,  6, 14:15, 17, 23:24)], id = c("ID"))
test_re1$value <- as.factor(test_re1$value)
b <- ggplot(test_re1,  aes( x = variable, stratum = value, alluvium = ID,
                             fill = value, label = value)) + scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") + geom_stratum() + theme_classic() + theme(legend.position = "right") + ggtitle("")

a / b + plot_layout(guides='collect') + plot_annotation(tag_levels = 'A')


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

