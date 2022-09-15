# https://r-graph-gallery.com/215-the-heatmap-function.html
# 清理工作环境
rm(list = ls())
# 读入数据
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.csv")
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")

rownames(dt)=dt[,4]
data <- as.matrix(dt[5:22])
# t(data) # transpose the matrix with to swap X and Y axis.
# Default Heatmap
heatmap(data, Rowv = NA,  Colv = NA, scale = "row")
# Use 'scale' to normalize
heatmap(data, scale="column")
# No dendrogram nor reordering for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale="column") 

# 1: native palette from R
heatmap(data, Colv = NA, Rowv = NA, scale="column", col = cm.colors(256))
heatmap(data, Colv = NA, Rowv = NA, scale="column", col = terrain.colors(256))
# 2: Rcolorbrewer palette
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, Colv = NA, Rowv = NA, scale="column", margins = c(6,6), col = coul)
# Example: grouping from the first letter:
my_group <- as.numeric(as.factor(substr(rownames(data), 1 , 1)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(9, "Blues"))(50)
heatmap(data, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide, col=colMain)





library(pheatmap)
set.seed(123)
# heatmap
pheatmap(data)

# dist mat
mat <- dist(data)
hclust_mat <- hclust(mat)
hclust_mat$order
hclust_mat$labels

# reorder row_clust
index <- seq(1,234, by = 1)
hclust_mat$order <- index
pheatmap(data, cluster_rows = hclust_mat)

require(gridExtra)

# reorder 1 
index <- order(rowSums(data), decreasing = TRUE)
dend = reorder(as.dendrogram(hclust_mat), 
               wts = index)
row_cluster <- as.hclust(dend)
p1 <- pheatmap(data, cluster_rows = row_cluster, 
               show_colnames = FALSE)

# reorder 2
index <- order(rowSums(data), decreasing = FALSE)
dend = reorder(as.dendrogram(hclust_mat), 
               wts = index)
row_cluster <- as.hclust(dend)
p2 <- pheatmap(data, cluster_rows = row_cluster, 
               show_colnames = FALSE)


# extract plot list
plot_list <- list(p1[[4]], p2[[4]])

grid.arrange(arrangeGrob(grobs= plot_list,ncol=2))



