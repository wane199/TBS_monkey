# https://www.bilibili.com/video/BV1is411H7Hq?p=4&spm_id_from=pageDriver&vd_source=23f183f0c5968777e138f31842bde0a0
# https://mp.weixin.qq.com/s?__biz=MzAxMDkxODM1Ng==&mid=2247513030&idx=4&sn=9d53d5dad11909d6584e690a55fb401d&chksm=9b4bf37dac3c7a6b3b621e32e68b2e0b4803a2a241774d2b8de14d0ba127bb2771e57b9ede5d&mpshare=1&scene=1&srcid=1003QjfqL04enFIZ3Q92NGTB&sharer_sharetime=1664803953625&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
rm(list = ls())
options(warn = -1)
suppressPackageStartupMessages(library(CLL))
suppressPackageStartupMessages(library(GEOquery))
library(Seurat)
library(dplyr)
library(patchwork)
library(mindr)
library(Matrix)
library(scCATCH)


proj = "GSE150392"  # 可以套用在其他代码里面了


# 2.生存信息与临床信息
eSet = getGEO("GSE150392",destdir = ".",getGPL = F)
eSet = eSet[[1]]
exp = exprs(eSet)
pd = pData(eSet)


# 3.表达矩阵行名ID转换
dat = read.csv("C:\\Users\\wane199\\Downloads\\GSE150392_Cov_Mock_Raw_COUNTS.csv.gz",
               row.names = 1)
exp = dat

library(stringr)
library(dplyr)
# 行名ID转换：方法1(推荐)
head(rownames(exp))
x = !str_starts(rownames(exp),"ERCC");table(x)
exp = exp[x,]
b = rownames(exp) %>% 
  str_replace("_","///") %>%  # 因为这个数据的基因中有_,所以将第一个下划线改为///，方便后续的切割
  str_split("///",simplify = T)   # 这一步骤很重要
k = !duplicated(b[,2]);table(k)  # 如果重复，则返回false,不重复则返回true
exp = exp[k,]  # 表达矩阵中筛选只返回ture的行
b = b[k,]      # 也是筛选返回true的行，值和exp对应的
rownames(exp) = b[,2]
exp = as.matrix(exp)

### 可供参考
# b1 = rownames(exp) %>% 
#   str_replace("_","///") %>% 
#   str_split("///",simplify = T) %>% .[,2]
# b1 = !duplicated(b1)

eSet1 <- getGEO('GSE205661', destdir = ".",
                    getGPL = F) # AnnotGPL = T, 
exp1 <- exprs(eSet1[[1]])   # 表达矩阵
GPL1 <- eSet1[[1]]@annotation # 平台信息——提取芯片平台编号
GPL1
eSet1[[1]]@featureData@data
str(eSet)
str(eSet[[1]])
names(Meta(eSet[[1]]))
# 芯片ID的基因注释信息
Table(eSet)[1:5,1:5]
eset <- GDS2eSet(eSet, do.log2 = T)
# 下载.txt.gz后读取
a <- read.table('GSE186334_series_matrix.txt.gz',
                sep = '\t', quote = "", fill = T, comment.char = "!", header = T)
class(a)
str(a)
rownames(a) = a[,1]
a = a[,-1]
exprSet = exprs(a) # 表达矩阵
# ID转换 
# https://www.jianshu.com/p/78a64d2d998a
gpl20301 <- getGEO('GPL20301', destdir=".") ##根据GPL号下载的是芯片设计的信息！

# 读取在GEO官网下载的平台文件
GPL=getGEO(filename = '/home/wane/Downloads/GPL20301_family.soft.gz')
# 提取信息（可以通用）
gpl=GPL@gpls[[1]]@dataTable@table
# 我只要ID和symbol
ids=gpl[,c(1,12)]
# 写出文件
write.table(ids,file = "ids.txt",siep = "\t",row.names=F,col.names = T)

# 首先打开这两个R包
library(clusterProfiler) 
library(org.Hs.eg.db)
# 如果没有这两个R包，就运下面的代码进行下载
if(!require("BiocManager")) install.packages("BiocManager",update = F,ask = F)
options(BioC_mirror="https://mirrors.ustc.edu.cn/bioc/")
BiocManager::install("clusterProfiler")
BiocManager::install("org.Hs.eg.db")

# 查看数据类型（就是这个R包提供哪些个ID类型可供转化）
keytypes(org.Hs.eg.db) 
## 下面这个是R包org.Hs.eg.db拥有的ID类型，可供选择，对应原来的ids里面的类型

# 确保数据格式为数据框
ids1=as.data.frame(ids)

ids <- bitr(ids1$ID,  #你的数据框
            fromType = "ENSEMBLPROT", #你的ID的数据类型
            toType = "SYMBOL", #转化的数据类型
            OrgDb = org.Hs.eg.db)  #org.Hs.eg.db——人类
#最后得出的ids就是结果


# 4. 基因过滤
# 需要过滤一下那些在很多样本里表达量都为0或者表达量很低的基因。过滤标准不唯一。过滤之前基因数量：
nrow(exp)
# 常用过滤标准1：仅去除在所有样本里表达量都为零的基因
exp1 = exp[rowSums(exp)>0,]
nrow(exp1)

# 常用过滤标准2(推荐)：仅保留在一半以上样本里表达的基因
exp = exp[apply(exp, 1, function(x) sum(x > 0) > 0.5*ncol(exp)), ]
nrow(exp)


# 5.分组信息获取
Group = str_remove(colnames(exp),"\\d")  # 将列名中的数字删除，成为了两个分组
Group = factor(Group,levels = c("Mock","Cov"))  # 对照组在前，实验组在后
table(Group)


# 6.保存数据
save(exp,Group,proj,file = paste0(proj,".Rdata"))



# 7.三大R包差异分析
# DESeq2和edgeR和limma三大包
rm(list = ls())
setwd('./EP/single_cell_RNA-seq/')
load("./GSE150392.Rdata")
table(Group)
#deseq2----
library(DESeq2)
colData <- data.frame(row.names =colnames(exp), 
                      condition=Group)
if(!file.exists(paste0(proj,"_dd.Rdata"))){
  dds <- DESeqDataSetFromMatrix(
    countData = exp,
    colData = colData,
    design = ~ condition)
  dds <- DESeq(dds)
  save(dds,file = paste0(proj,"_dd.Rdata"))
}
load(file = paste0(proj,"_dd.Rdata"))
class(dds)
res <- results(dds, contrast = c("condition",rev(levels(Group))))
#constrast
c("condition",rev(levels(Group)))
class(res)
DEG1 <- as.data.frame(res)
DEG1 <- DEG1[order(DEG1$pvalue),] 
DEG1 = na.omit(DEG1)
head(DEG1)

#添加change列标记基因上调下调
logFC_t = 2
pvalue_t = 0.05

k1 = (DEG1$pvalue < pvalue_t)&(DEG1$log2FoldChange < -logFC_t);table(k1)
k2 = (DEG1$pvalue < pvalue_t)&(DEG1$log2FoldChange > logFC_t);table(k2)
DEG1$change = ifelse(k1,"DOWN",ifelse(k2,"UP","NOT"))
table(DEG1$change)
head(DEG1)

#edgeR----
library(edgeR)
exp = na.omit(exp)
dge <- DGEList(counts=exp,group=Group)
dge$samples$lib.size <- colSums(dge$counts)
dge <- calcNormFactors(dge) 

design <- model.matrix(~Group)

dge <- estimateGLMCommonDisp(dge, design)
dge <- estimateGLMTrendedDisp(dge, design)
dge <- estimateGLMTagwiseDisp(dge, design)

fit <- glmFit(dge, design)
fit <- glmLRT(fit) 

DEG2=topTags(fit, n=Inf)
class(DEG2)
DEG2=as.data.frame(DEG2)
head(DEG2)

k1 = (DEG2$PValue < pvalue_t)&(DEG2$logFC < -logFC_t);table(k1)
k2 = (DEG2$PValue < pvalue_t)&(DEG2$logFC > logFC_t);table(k2)
DEG2$change = ifelse(k1,"DOWN",ifelse(k2,"UP","NOT"))

head(DEG2)
table(DEG2$change)
###limma----
library(limma)
dge <- edgeR::DGEList(counts=exp)
dge <- edgeR::calcNormFactors(dge)
design <- model.matrix(~Group)
v <- voom(dge,design, normalize="quantile")

design <- model.matrix(~Group)
fit <- lmFit(v, design)
fit= eBayes(fit)

DEG3 = topTable(fit, coef=2, n=Inf)
DEG3 = na.omit(DEG3)

k1 = (DEG3$P.Value < pvalue_t)&(DEG3$logFC < -logFC_t);table(k1)
k2 = (DEG3$P.Value < pvalue_t)&(DEG3$logFC > logFC_t);table(k2)
DEG3$change = ifelse(k1,"DOWN",ifelse(k2,"UP","NOT"))
table(DEG3$change)
head(DEG3)

tj = data.frame(deseq2 = as.integer(table(DEG1$change)),
                edgeR = as.integer(table(DEG2$change)),
                limma_voom = as.integer(table(DEG3$change)),
                row.names = c("down","not","up")
);tj
save(DEG1,DEG2,DEG3,Group,tj,file = paste0(proj,"_DEG.Rdata"))

# 可视化
# 使用了生信技能树老师的包：tinyarray这个包里可以画pca, 热图，火山图，韦恩图，具体每个图的算法，可以看生信技能树GEO芯片分析
library(ggplot2)
library(tinyarray)
exp[1:4,1:4]
# cpm 去除文库大小的影响
dat = log2(cpm(exp)+1)  # dat 为使用count数据转化而成的cpm数据，使用这个数据画图，而count数据只是用来做差异分析
pca.plot = draw_pca(dat,Group);pca.plot
save(pca.plot,file = paste0(proj,"_pcaplot.Rdata"))

cg1 = rownames(DEG1)[DEG1$change !="NOT"]
cg2 = rownames(DEG2)[DEG2$change !="NOT"]
cg3 = rownames(DEG3)[DEG3$change !="NOT"]

h1 = draw_heatmap(dat[cg1,],Group,n_cutoff = 2)
h2 = draw_heatmap(dat[cg2,],Group,n_cutoff = 2)
h3 = draw_heatmap(dat[cg3,],Group,n_cutoff = 2)

v1 = draw_volcano(DEG1,pkg = 1,logFC_cutoff = logFC_t)
v2 = draw_volcano(DEG2,pkg = 2,logFC_cutoff = logFC_t)
v3 = draw_volcano(DEG3,pkg = 3,logFC_cutoff = logFC_t)

library(patchwork)
(h1 + h2 + h3) / (v1 + v2 + v3) +plot_layout(guides = 'collect') &theme(legend.position = "none")

ggsave(paste0(proj,"_heat_vo.png"),width = 15,height = 10)

# 三大R包差异基因对比
UP=function(df){
  rownames(df)[df$change=="UP"]
}
DOWN=function(df){
  rownames(df)[df$change=="DOWN"]
}

up = intersect(intersect(UP(DEG1),UP(DEG2)),UP(DEG3))
down = intersect(intersect(DOWN(DEG1),DOWN(DEG2)),DOWN(DEG3))
dat = log2(cpm(exp)+1)
hp = draw_heatmap(dat[c(up,down),],Group,n_cutoff = 2)

#上调、下调基因分别画维恩图
up_genes = list(Deseq2 = UP(DEG1),
                edgeR = UP(DEG2),
                limma = UP(DEG3))

down_genes = list(Deseq2 = DOWN(DEG1),
                  edgeR = DOWN(DEG2),
                  limma = DOWN(DEG3))

up.plot <- draw_venn(up_genes,"UPgene")
down.plot <- draw_venn(down_genes,"DOWNgene")

#维恩图拼图，终于搞定
library(patchwork)
#up.plot + down.plot
# 拼图
pca.plot + hp+up.plot +down.plot+ plot_layout(guides = "collect")
ggsave(paste0(proj,"_heat_ve_pca.png"),width = 15,height = 10)


# 分组聚类的热图
library(ComplexHeatmap)
library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("#2fa1dd", "white", "#f87669"))
top_annotation = HeatmapAnnotation(
  cluster = anno_block(gp = gpar(fill = c("#f87669","#2fa1dd")),
                       labels = levels(Group),
                       labels_gp = gpar(col = "white", fontsize = 12)))

m = Heatmap(t(scale(t(exp[c(up,down),]))),name = " ",
            col = col_fun,
            top_annotation = top_annotation,
            column_split = Group,
            show_heatmap_legend = T,
            border = F,
            show_column_names = F,
            show_row_names = F,
            use_raster = F,
            cluster_column_slices = F,
            column_title = NULL)
m






