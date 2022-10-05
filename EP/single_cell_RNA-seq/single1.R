# 单细胞初级8讲和高级分析8讲
# http://www.bio-info-trainee.com/7536.html

# 两样本原代乳腺癌细胞与肺部扩散癌细胞单细胞测序数据分析
# https://mp.weixin.qq.com/s?__biz=MzUzMTEwODk0Ng==&mid=2247505574&idx=1&sn=1db499b5bf572eb40ec4207105cc9ad0&chksm=fa45139bcd329a8d66c0357b38db0ac26a8032d2abe17be1ae33f34fbd193acc0aae0f826272&scene=178&cur_album_id=2546857217690632193#rd
# 导入数据
rm(list=ls())
options(stringsAsFactors = F) 
library(Seurat)
library(ggplot2)
library(clustree)
library(cowplot)
library(dplyr)

###### step1:导入数据 ######   
library(data.table)
setwd("/Users/mac/Downloads/")
getwd()
dir='./GSE196936_RAW/' 
samples=list.files(dir)
samples

library(data.table)
sceList = lapply(samples,function(pro){ 
  # pro=samples[1] 
  print(pro) 
  #ct=fread( file.path(dir,pro),data.table = F) 
  sce =CreateSeuratObject(counts =  read.csv( file.path(dir,pro)) ,
                          project =   gsub('.csv','',gsub('^GSM[0-9]*_MCF7_','',pro) ) ,
                          min.cells = 5,
                          min.features = 300 )
  return(sce)
})
sceList 
samples
# gsub('^GSM[0-9]*','',samples) 
sce.all=merge(x=sceList[[1]],
              y=sceList[ -1 ],
              add.cell.ids =   gsub('.csv','',gsub('^GSM[0-9]*_MCF7_','',samples) )  )

as.data.frame(sce.all@assays$RNA@counts[1:10, 1:2])
head(sce.all@meta.data, 10)
table(sce.all$orig.ident)

library(stringr)
sce.all$group=sce.all$orig.ident
table(sce.all@meta.data$group)
table(sce.all@meta.data$orig.ident)

#rm(list=ls())
###### step2:QC质控 ######
dir.create("./1-QC")
setwd("./1-QC")
# sce.all=readRDS("../sce.all_raw.rds")
#计算线粒体基因比例
# 人和鼠的基因名字稍微不一样 
mito_genes=rownames(sce.all)[grep("^MT-", rownames(sce.all))] 
mito_genes #13个线粒体基因
sce.all=PercentageFeatureSet(sce.all, "^MT-", col.name = "percent_mito")
fivenum(sce.all@meta.data$percent_mito)
#计算核糖体基因比例
ribo_genes=rownames(sce.all)[grep("^Rp[sl]", rownames(sce.all),ignore.case = T)]
ribo_genes
sce.all=PercentageFeatureSet(sce.all, "^RP[SL]", col.name = "percent_ribo")
fivenum(sce.all@meta.data$percent_ribo)
#计算红血细胞基因比例
rownames(sce.all)[grep("^Hb[^(p)]", rownames(sce.all),ignore.case = T)]
sce.all=PercentageFeatureSet(sce.all, "^HB[^(P)]", col.name = "percent_hb")
fivenum(sce.all@meta.data$percent_hb)
#可视化细胞的上述比例情况
feats <- c("nFeature_RNA", "nCount_RNA", "percent_mito", "percent_ribo", "percent_hb")
feats <- c("nFeature_RNA", "nCount_RNA")
p1=VlnPlot(sce.all, group.by = "orig.ident", features = feats, pt.size = 0, ncol = 2) + 
  NoLegend()
p1
library(ggplot2) 
ggsave(filename="Vlnplot1.pdf",plot=p1)
feats <- c("percent_mito", "percent_ribo", "percent_hb")
p2=VlnPlot(sce.all, group.by = "orig.ident", features = feats, pt.size = 0, ncol = 3, same.y.lims=T) + 
  scale_y_continuous(breaks=seq(0, 100, 5)) +
  NoLegend()
p2 
ggsave(filename="Vlnplot2.pdf",plot=p2)

p3=FeatureScatter(sce.all, "nCount_RNA", "nFeature_RNA", group.by = "orig.ident", pt.size = 0.5)
ggsave(filename="Scatterplot.pdf",plot=p3)
#根据上述指标，过滤低质量细胞/基因
#过滤指标1:最少表达基因数的细胞&最少表达细胞数的基因
selected_c <- WhichCells(sce.all, expression = nFeature_RNA > 300)
selected_f <- rownames(sce.all)[Matrix::rowSums(sce.all@assays$RNA@counts > 0 ) > 3]

sce.all.filt <- subset(sce.all, features = selected_f, cells = selected_c)
dim(sce.all) 
dim(sce.all.filt) 
#  可以看到，主要是过滤了基因，其次才是细胞
C=sce.all.filt@assays$RNA@counts
dim(C)
C=Matrix::t(Matrix::t(C)/Matrix::colSums(C)) * 100
# 这里的C 这个矩阵，有一点大，可以考虑随抽样
C=C[,sample(1:ncol(C),1000)]
most_expressed <- order(apply(C, 1, median), decreasing = T)[50:1]
pdf("TOP50_most_expressed_gene.pdf",width=14)
boxplot(as.matrix(Matrix::t(C[most_expressed, ])),
        cex = 0.1, las = 1, 
        xlab = "% total count per cell", 
        col = (scales::hue_pal())(50)[50:1], 
        horizontal = TRUE)
dev.off()
rm(C)
#过滤指标2:线粒体/核糖体基因比例(根据上面的violin图)
selected_mito <- WhichCells(sce.all.filt, expression = percent_mito < 15)
selected_ribo <- WhichCells(sce.all.filt, expression = percent_ribo > 3)
selected_hb <- WhichCells(sce.all.filt, expression = percent_hb < 1 )
length(selected_hb)
length(selected_ribo)
length(selected_mito)
sce.all.filt <- subset(sce.all.filt, cells = selected_mito)
sce.all.filt <- subset(sce.all.filt, cells = selected_ribo)
sce.all.filt <- subset(sce.all.filt, cells = selected_hb)
dim(sce.all.filt)
table(sce.all.filt$orig.ident) 

#可视化过滤后的情况
feats <- c("nFeature_RNA", "nCount_RNA")
p1_filtered=VlnPlot(sce.all.filt, group.by = "orig.ident", features = feats, pt.size = 0, ncol = 2) + 
  NoLegend()
ggsave(filename="Vlnplot1_filtered.pdf",plot=p1_filtered)

feats <- c("percent_mito", "percent_ribo", "percent_hb")
p2_filtered=VlnPlot(sce.all.filt, group.by = "orig.ident", features = feats, pt.size = 0, ncol = 3) + 
  NoLegend()
ggsave(filename="Vlnplot2_filtered.pdf",plot=p2_filtered)
#过滤指标3:过滤特定基因
# Filter MALAT1 管家基因

sce.all.filt <- sce.all.filt[!grepl("MALAT1", rownames(sce.all.filt),ignore.case = T), ]
# Filter Mitocondrial 线粒体基因
sce.all.filt <- sce.all.filt[!grepl("^MT-", rownames(sce.all.filt),ignore.case = T), ]
# 当然，还可以过滤更多
dim(sce.all.filt) 
dim(sce.all) 
saveRDS(sce.all.filt, "sce.all_qc2_noG.rds")
#sce.all.fit 212.5MB
setwd("/Users/mac/Downloads/")
source('step2-harmony.R')
source('step3.1-check-marker.R')

rm(list=ls())
dir.create("./2-harmony")
getwd()
setwd("./2-harmony")
sce.all=readRDS("./1-QC/sce.all_qc2_noG.rds")
sce=sce.all
sce
sce <- NormalizeData(sce, 
                     normalization.method = "LogNormalize",
                     scale.factor = 1e4) 
sce <- FindVariableFeatures(sce)
sce <- ScaleData(sce)
sce <- RunPCA(sce, features = VariableFeatures(object = sce))

library(harmony)
seuratObj <- RunHarmony(sce, "orig.ident")
names(seuratObj@reductions)
seuratObj <- RunUMAP(seuratObj,  dims = 1:15, 
                     reduction = "harmony")
DimPlot(seuratObj,reduction = "umap",label=T ) 

sce=seuratObj
sce <- FindNeighbors(sce, reduction = "harmony",
                     dims = 1:15) 
sce.all=sce
#设置不同的分辨率，观察分群效果
for (res in c(0.01, 0.05, 0.1, 0.2, 0.3, 0.5,0.8,1)) {
  sce.all=FindClusters(sce.all, #graph.name = "CCA_snn", 
                       resolution = res, algorithm = 1)
}
colnames(sce.all@meta.data)
apply(sce.all@meta.data[,grep("RNA_snn",colnames(sce.all@meta.data))],2,table)

p1_dim=plot_grid(ncol = 3, DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.0.01") + 
                   ggtitle("louvain_0.01"), DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.0.1") + 
                   ggtitle("louvain_0.1"), DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.0.2") + 
                   ggtitle("louvain_0.2"))
ggsave(plot=p1_dim, filename="Dimplot_diff_resolution_low.pdf",width = 14)

p1_dim=plot_grid(ncol = 3, DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.0.8") + 
                   ggtitle("louvain_0.8"), DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.1") + 
                   ggtitle("louvain_1"), DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.0.3") + 
                   ggtitle("louvain_0.3"))
ggsave(plot=p1_dim, filename="Dimplot_diff_resolution_high.pdf",width = 18)

p2_tree=clustree(sce.all@meta.data, prefix = "RNA_snn_res.")
ggsave(plot=p2_tree, filename="Tree_diff_resolution.pdf")

#接下来分析，按照分辨率为0.1进行 
sel.clust = "RNA_snn_res.0.1"
sce.all <- SetIdent(sce.all, value = sel.clust)
table(sce.all@active.ident) 
saveRDS(sce.all, "./2-harmony/sce.all_int.rds")
sce.all=readRDS("./2-harmony/sce.all_int.rds")


# 检查常见分群情况
###### step5:检查常见分群情况  ######
setwd('/Users/mac/Downloads')
dir.create("3-cell")
setwd("3-cell")  
getwd()
DimPlot(sce.all, reduction = "umap", group.by = "seurat_clusters",label = T) 
DimPlot(sce.all, reduction = "umap", group.by = "RNA_snn_res.0.1",label = T) 
ggsave('umap_by_RNA_snn_res.0.1.pdf')

library(stringr)
#刚开始只找了一些基础marker分不太开
#genes_to_check = c('PTPRC', 'CD3D', 'CD3E', 'CD8A',
# 'CD19', 'CD79A', 'MS4A1' , 'IGHG1', 
# 'MZB1', 'SDC1',
#   'CD68', 'CD163', 'CD14', 
# 'RCVRN',
# 'FGF7','MME',
# 'PECAM1', 'VWF', 
#'EPCAM' , 'KRT19', 'PROM1', 'ALDH1A1')

#genes_to_check=unique(genes_to_check)
#genes_to_check = str_to_upper(genes_to_check)
#因此又多找了一些marker
genes_to_check = c("PTPRC","CD3D","CD3E","CD8A","CD19",
                  "CD79A","MS4A1","IGHG1","MZB1","SDC1",
                  "CD68","CD163","CD14","RCVRN","FGF7",    
                  "MME", "PECAM1","VWF","EPCAM","KRT19",   
                  "PROM1","ALDH1A1","AGER","SFTPA1","SCGB1A1", 
                  "KRT17","TPPP3","KRT4","KRT14","KRT8", 
                  "KRT18","CD4","CCR7","SELL","TCF7",    
                  "CXCR6","ITGA1","FOXP3","IL2RA","CTLA4",   
                  "GZMB","GZMK","CCL5","IFNG","CCL4",    
                  "CCL3","PRF1","NKG7","CD22","TCL1A",  
                  "CD83","CD38","TNFRSF17", "IGHG4","TPSAB1", 
                  "TPSB2","Krt17","Krt14","Krt5","Acta2" ,  
                  "Myl9","Mylk", "Myh11","Krt19", "Krt18" , 
                  "Krt8","Prlr", "Cited1", "Pgr", "Prom1", 
                  "Esr1","Mfge8","Trf","Csn3","Wfdc18",  
                  "Elf5","Ltf", "Kit","Aldh1a3","Cd14",    
                  "CD45+", "CD10+","CD31+","ACTA2","APOE", 
                  "TIMP1", "TAGLN","SLPI","LTF","AGR2",   
                  "KLRD1","FCGR3B","RPE65","MKI67","TOP2A", 
                  "MLANA","MITF","DCT","PRAME", "GEP",   
                  "CD24","ZEB2","CDH1","ESR1","VIM",    
                  "FN1","ELN","COL3A1") 
# The following requested variables were not found (10 out of 36 shown): CD19, MS4A1, CD163, RCVRN, PECAM1, PROM1, ALDH1A1, SFTPA1, CD4, CXCR6
genes_to_check=unique(genes_to_check)
genes_to_check = str_to_upper(genes_to_check)

library(stringr)  
p_paper_markers <- DotPlot(sce.all, features = genes_to_check,
                           assay='RNA')  + coord_flip()

p_paper_markers
ggsave(plot=p_paper_markers,
       filename="check_paper_marker_by_seurat_cluster.pdf",width = 12)








