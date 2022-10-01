# https://cloud.tencent.com/developer/article/1828927
# Step 0、加载需要的R包
library(Seurat)
library(dplyr)
library(patchwork)
library(mindr)
library(Matrix)

# Step 1、数据准备
#文章测序了两个病人的样本，正文figure展示的是其中一个病人的单细胞测序结果。我们下面的分析使用正文中展示的样本
#读入数据并将表达矩阵转成稀疏矩阵，减少数据对空间资源的消耗以及运算资源的消耗
getwd()
GSM4546857<-read.csv("C:\\Users\\wane199\\Downloads\\GSE150321_RAW\\GSM4546857_LSCC01_DBEC_UMI.csv",comment.char = "#")
dim(GSM4546857)
GSM4546857[1:4,1:4]
rownames(GSM4546857)<-GSM4546857$Cell_Index
GSM4546857<-GSM4546857[,-1]
GSM4546857<-t(GSM4546857)
object.size(GSM4546857)#2331231144 bytes
GSM4546857_sparse<-as(as.matrix(GSM4546857),"dgCMatrix")
GSM4546857_sparse[1:4,1:4]
object.size(GSM4546857_sparse)#166367952 bytes
save(GSM4546857_sparse,file = "GSM4546857_sparse.Rdata")
dim(GSM4546857_sparse)

# Step 2、创建Seurat对象
#下面这段代码中，最重要的就是创建Seurat对象以及去除线粒体基因，其他都是对Seurat对象的可视化，其目的在于提高初学者对该对象的了解
## =============== 创建Seurat对象
tissu1 <- CreateSeuratObject(counts = GSM4546857_sparse, project = "LSCC", min.cells = 3, min.features = 200)
tissu1

## =============== 去除线粒体基因
# The [[ operator can add columns to object metadata. This is a great place to stash QC stats
#此次数据检测到大量线粒体基因
grep(pattern = "^MT\\.",rownames(tissu1),value = T)
tissu1[["percent.mt"]] <- PercentageFeatureSet(tissu1, pattern = "^MT\\.")
head(tissu1@meta.data,5)
summary(tissu1@meta.data$nCount_RNA)















