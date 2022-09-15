# https://r-graph-gallery.com/215-the-heatmap-function.html
# 清理工作环境
rm(list = ls())
# 读入数据
dt <- read.csv("/Users/mac/Desktop/BLS-ep-pre/EP/Structured_Data/Task2/TLE234group.csv")
train <- subset(dt, dt$Group == "Training")
test <- subset(dt, dt$Group == "Test")

rownames(dt)=dt[,4]
data <- as.matrix(dt[5:22])
# t(data) # transpose the matrix with to swap X and Y axis.
# Default Heatmap
heatmap(data)
# Use 'scale' to normalize
heatmap(data, scale="column")
# No dendrogram nor reordering for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale="column")



