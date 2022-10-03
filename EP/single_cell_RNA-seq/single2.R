# https://mp.weixin.qq.com/s?__biz=MzkyMTI1MTYxNA==&mid=2247499548&idx=1&sn=426d102d24b03c7b2273e68cc695cba3&chksm=c184c56df6f34c7bf867573a6ee1d62b4aa2ebeb4ff7b33fbafd498501dd390b0ed0bcacb833&mpshare=1&scene=1&srcid=1003WBgMkSparXoV1DcLEjWn&sharer_sharetime=1664772991674&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# 批量读取数据并进行过滤
library(Seurat)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(ggsci)
library(reshape2)
library(clustree)
library(data.table)

# batch read data
file <- list.files(pattern = '.csv')[1:2]
sample <- c('culture','tail')
scRNAlist <- list()

# x = 1
for(x in 1:length(file)){
  count <- read.csv(file[x],header = T,row.names = 1)
  
  # create Seurat object
  scRNAlist[[sample[x]]] <- CreateSeuratObject(count,
                                               # min.cells = 3,
                                               # min.features = 300
  )
  
  # add group
  scRNAlist[[sample[x]]]$group <- sample[x]
  
  # mitocondral percent
  scRNAlist[[sample[x]]][["percent.mt"]] <- PercentageFeatureSet(scRNAlist[[sample[x]]],
                                                                 pattern = "^mt-")
  
  # calculate quantile 2-98
  qt <- quantile(scRNAlist[[sample[x]]]$nFeature_RNA,probs = c(0.02,0.98))
  
  # filter cells gene in quantile 2-98 and mitocondral below 7.5
  scRNAlist[[sample[x]]] <- subset(scRNAlist[[sample[x]]],
                                   subset = nFeature_RNA >= qt[1] & nFeature_RNA <= qt[2] & percent.mt <= 7.5)
  
  # SCTransform normalize
  scRNAlist[[sample[x]]] <- SCTransform(scRNAlist[[sample[x]]],
                                        method = "glmGamPoi",verbose = FALSE) %>%
    RunPCA(npcs = 30, verbose = FALSE)
}



