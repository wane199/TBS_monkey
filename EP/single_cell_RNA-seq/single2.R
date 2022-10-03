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
setwd('C:\\Users\\wane199\\Downloads\\GSE154525_RAW\\GSM\\')
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
# check
scRNAlist


# 查看质控结果
# vlnplot
lapply(1:length(scRNAlist), function(x){
  VlnPlot(scRNAlist[[x]], features = c("nFeature_RNA", "nCount_RNA", "percent.mt"),
          ncol = 3)
}) -> vlnlist

# plot
wrap_plots(vlnlist,nrow = 2)


# 数据整合
# select features that are repeatedly variable across datasets for integration
features <- SelectIntegrationFeatures(object.list = scRNAlist, nfeatures = 3000)

# Perform integration
scRNAlist <- PrepSCTIntegration(object.list = scRNAlist, anchor.features = features)
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=05s

# find Anchors !tail as refrence
combined.anchors <- FindIntegrationAnchors(object.list = scRNAlist,
                                           reference = c(2),
                                           normalization.method = "SCT",
                                           reduction = 'cca',
                                           anchor.features = features)

# intergrate
combined.sct <- IntegrateData(anchorset = combined.anchors, normalization.method = "SCT")


# 降维可视化
# Perform an integrated analysis
combined.sct <- RunPCA(combined.sct, verbose = FALSE)
combined.sct <- FindNeighbors(combined.sct, reduction = "pca", dims = 1:30)

# 不同分辨率下可视化聚类树:
# check different resolution and clusters
combined.sct <- FindClusters(combined.sct, resolution = c(seq(.1,1,0.1)))

# plot tree
clustree(combined.sct@meta.data, prefix = "integrated_snn_res.")

# resolution = 0.3
combined.sct <- FindClusters(combined.sct, resolution = 0.3)

# check groups
table(combined.sct@meta.data$group)

# 降维可视化:
# run umap
combined.sct <- RunUMAP(combined.sct, reduction = "pca", dims = 1:30, verbose = FALSE)

# DimPlot by group
p2 <- DimPlot(combined.sct, reduction = "umap",group.by = 'group') +
  scale_color_manual(values = c('#009999','#CC9933')) +
  theme(aspect.ratio = 1,
        legend.position = c(0.05,0.1)) +
  labs(title = '')

# DimPlot cluster
p1 <- DimPlot(combined.sct, reduction = "umap",label = T,
              label.size = 3,label.box = T) + NoLegend() +
  # scale_color_npg() +
  # scale_fill_npg(alpha = 0.5) +
  theme(aspect.ratio = 1)

# combine
p1 + p2


# Featureplot
# 默认可视化:
# gene plot
gene <- c('Scx','Dcn','Bgn','Col5a2','Col1a1','Col3a1','Fbn2','Postn','Tnc')

FeaturePlot(combined.sct,features = gene,
            cols = c('grey90','#CC0033'),ncol = 3,
            min.cutoff = 'q2',max.cutoff = 'q98')


# 提取数据重新绘图:
##################################
# self replot
pc <- Embeddings(combined.sct,reduction = 'umap') %>% data.frame()
# add gene name
exp <- FetchData(combined.sct,vars = gene)
# megrge
mer <- cbind(pc,exp)
# wide to long
dflong <- melt(mer,id.vars = colnames(mer)[1:2],
               value.name = 'expressiton',
               variable.name = 'gene')

# check
head(dflong,3)

# filter quantile in 0.02~0.98
quantile(dflong$expressiton,probs = c(0.02,0.98))


# filter expression
dflong <- dflong %>% filter(expressiton >= -3.322756 & expressiton <= 7.487187)

# 绘图:
# plot
ggplot(dflong,aes(x = UMAP_1,y = UMAP_2)) +
  geom_point(aes(color = expressiton),size = 0.1,alpha = 1) +
  scale_color_gradient(low = 'grey90',high = '#CC0033',
                       name = 'percentile \n expression',
                       breaks = c(-3.322756,7.487187),
                       limits = c(-3.322756,7.487187),
                       labels = c('< 2%','> 98%')
  ) +
  theme_classic(base_size = 16) +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 1,
        legend.position = 'right',
        strip.text.x = element_text(size = 16,face = 'italic'),
        strip.background = element_rect(fill = NA,color = NA)) +
  facet_wrap(~gene) +
  labs(x = '',y = '') +
  guides(color = guide_colorbar(title.position = 'left',
                                title.theme = element_text(angle = 90,hjust = 0.5)))


# 小提琴图
# vlionplot
vlngene <- c('Pax1','Col2a1','Pax9','Col9a1','Sox9','Col9a3','Sox5','Wwp2','Sox6','Matn4')

VlnPlot(combined.sct,features = vlngene,
        ncol = 2,idents = c(0,2,4),
        slot = 'counts',assay = 'RNA',
        log = T,pt.size = 0,cols = c('#99CCFF','#006699','#66CC99'))

# SCT 后的基因数量会少一些,因为前者会有基因的过滤指标:
rnacount <- combined.sct@assays$RNA@counts %>% data.frame()
dim(rnacount)

# SCTransform filter min.cells < 5
sctcount <- combined.sct@assays$SCT@counts %>% data.frame()
dim(sctcount)


# 同一 cluster 不同样本的基因差异分析
##################################################
# differential analysis
# add test for group
combined.sct$diffGroup <- paste(combined.sct$group,combined.sct$seurat_clusters,
                                sep = '|')

# make it to active ident
Idents(combined.sct) <- "diffGroup"

# check
table(combined.sct$diffGroup)


# diff test
test <- FindMarkers(combined.sct,ident.1 = 'tail|0',ident.2 = 'culture|0',
                    test.use = 'wilcox',logfc.threshold = 0.25)

# add gene name
test$gene_name <- rownames(test)

# check
head(test,3)


# 热图可视化
# heatmap genes
htgene <- c('Postn','Col1a2','Dcn','Col1a1','Col3a1','Igfbp5','Col5a2',
            'Mgp','Ogn','Thbs2','Rflnb','Mfap2','Fbn2','Fstl1','Mfap4',
            'Bgn','Fn1','Igfbp7','Igf1','Col8a1',
            'Hist1h1b','Mki67','Top2a','Pclaf','Prc1','Ube2c','Cenpf','Hist1h1e',
            'Hist1h2ae','Smc2','Cdk1','Nusap1','Cenpe','Kif11','Birc5','Smc4',
            'Hmmr','Tpx2','Aurkb','Arl6ip1','Col2a1','Col9a1','Matn4','Col9a3',
            'Wwp2','Col11a1','Sox9','Col9a2','Acan','Pax1','Plod2','Dlk1','Pcp4',
            'Fmod','Fibin','Itm2a','Stk26','Id1','H19','Ndufa4l2')

# get subgroup data
htdata <- subset(combined.sct,idents = c('tail|0','culture|0',
                                         'tail|2','culture|2',
                                         'tail|4','culture|4'))

# order
Idents(htdata) <- factor(Idents(htdata),levels = c('culture|0','tail|0',
                                                   'culture|2','tail|2',
                                                   'culture|4','tail|4'))

# heatmap
DoHeatmap(htdata,features = htgene,
          draw.lines = T,
          assay = 'RNA',
          slot = 'counts') +
  scale_fill_gradient(low = 'white',high = '#990033')

#####################################
# save
# saveRDS(combined.sct,file = 'combined.sct.Rds')
# combined.sct <- readRDS('combined.sct.Rds')


