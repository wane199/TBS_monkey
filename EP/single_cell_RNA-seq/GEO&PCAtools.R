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

proj <- "GSE150392" # 可以套用在其他代码里面了

# 2.生存信息与临床信息
eSet <- getGEO("GSE150392", destdir = ".", getGPL = F)
eSet <- eSet[[1]]
exp <- exprs(eSet)
pd <- pData(eSet)


# 3.表达矩阵行名ID转换
dat <- read.csv("/Users/mac/Downloads/GSE150392_Cov_Mock_Raw_COUNTS.csv.gz",
  row.names = 1
)
exp <- dat

library(stringr)
library(dplyr)
# 行名ID转换：方法1(推荐)
head(rownames(exp))
x <- !str_starts(rownames(exp), "ERCC")
table(x)
exp <- exp[x, ]
b <- rownames(exp) %>%
  str_replace("_", "///") %>% # 因为这个数据的基因中有_,所以将第一个下划线改为///，方便后续的切割
  str_split("///", simplify = T) # 这一步骤很重要
k <- !duplicated(b[, 2])
table(k) # 如果重复，则返回false,不重复则返回true
exp <- exp[k, ] # 表达矩阵中筛选只返回ture的行
b <- b[k, ] # 也是筛选返回true的行，值和exp对应的
rownames(exp) <- b[, 2]
exp <- as.matrix(exp)

### 可供参考
# b1 = rownames(exp) %>%
#   str_replace("_","///") %>%
#   str_split("///",simplify = T) %>% .[,2]
# b1 = !duplicated(b1)

eSet1 <- getGEO("GSE205661",
  destdir = ".",
  getGPL = F
) # AnnotGPL = T,
exp1 <- exprs(eSet1[[1]]) # 表达矩阵
GPL1 <- eSet1[[1]]@annotation # 平台信息——提取芯片平台编号
GPL1
eSet1[[1]]@featureData@data
str(eSet)
str(eSet[[1]])
names(Meta(eSet[[1]]))
# 芯片ID的基因注释信息
Table(eSet)[1:5, 1:5]
eset <- GDS2eSet(eSet, do.log2 = T)
# 下载.txt.gz后读取
a <- read.table("GSE186334_series_matrix.txt.gz",
  sep = "\t", quote = "", fill = T, comment.char = "!", header = T
)
class(a)
str(a)
rownames(a) <- a[, 1]
a <- a[, -1]
exprSet <- exprs(a) # 表达矩阵
# ID转换
# https://www.jianshu.com/p/78a64d2d998a
gpl20301 <- getGEO("GPL20301", destdir = ".") ## 根据GPL号下载的是芯片设计的信息！

# 读取在GEO官网下载的平台文件
GPL <- getGEO(filename = "/home/wane/Downloads/GPL20301_family.soft.gz")
# 提取信息（可以通用）
gpl <- GPL@gpls[[1]]@dataTable@table
# 我只要ID和symbol
ids <- gpl[, c(1, 12)]
# 写出文件
write.table(ids, file = "ids.txt", siep = "\t", row.names = F, col.names = T)

# 首先打开这两个R包
library(clusterProfiler)
library(org.Hs.eg.db)
# 如果没有这两个R包，就运下面的代码进行下载
if (!require("BiocManager")) install.packages("BiocManager", update = F, ask = F)
options(BioC_mirror = "https://mirrors.ustc.edu.cn/bioc/")
BiocManager::install("clusterProfiler")
BiocManager::install("org.Hs.eg.db")

# 查看数据类型（就是这个R包提供哪些个ID类型可供转化）
keytypes(org.Hs.eg.db)
## 下面这个是R包org.Hs.eg.db拥有的ID类型，可供选择，对应原来的ids里面的类型

# 确保数据格式为数据框
ids1 <- as.data.frame(ids)

ids <- bitr(ids1$ID, # 你的数据框
  fromType = "ENSEMBLPROT", # 你的ID的数据类型
  toType = "SYMBOL", # 转化的数据类型
  OrgDb = org.Hs.eg.db
) # org.Hs.eg.db——人类
# 最后得出的ids就是结果


# 4. 基因过滤
# 需要过滤一下那些在很多样本里表达量都为0或者表达量很低的基因。过滤标准不唯一。过滤之前基因数量：
nrow(exp)
# 常用过滤标准1：仅去除在所有样本里表达量都为零的基因
exp1 <- exp[rowSums(exp) > 0, ]
nrow(exp1)

# 常用过滤标准2(推荐)：仅保留在一半以上样本里表达的基因
exp <- exp[apply(exp, 1, function(x) sum(x > 0) > 0.5 * ncol(exp)), ]
nrow(exp)


# 5.分组信息获取
Group <- str_remove(colnames(exp), "\\d") # 将列名中的数字删除，成为了两个分组
Group <- factor(Group, levels = c("Mock", "Cov")) # 对照组在前，实验组在后
table(Group)


# 6.保存数据
save(exp, Group, proj, file = paste0(proj, ".Rdata"))



# 7.三大R包差异分析
# DESeq2和edgeR和limma三大包
rm(list = ls())
setwd("./EP/single_cell_RNA-seq/")
load("./GSE150392.Rdata")
table(Group)
# deseq2----
library(DESeq2)
colData <- data.frame(
  row.names = colnames(exp),
  condition = Group
)
if (!file.exists(paste0(proj, "_dd.Rdata"))) {
  dds <- DESeqDataSetFromMatrix(
    countData = exp,
    colData = colData,
    design = ~condition
  )
  dds <- DESeq(dds)
  save(dds, file = paste0(proj, "_dd.Rdata"))
}
load(file = paste0(proj, "_dd.Rdata"))
class(dds)
res <- results(dds, contrast = c("condition", rev(levels(Group))))
# constrast
c("condition", rev(levels(Group)))
class(res)
DEG1 <- as.data.frame(res)
DEG1 <- DEG1[order(DEG1$pvalue), ]
DEG1 <- na.omit(DEG1)
head(DEG1)

# 添加change列标记基因上调下调
logFC_t <- 2
pvalue_t <- 0.05

k1 <- (DEG1$pvalue < pvalue_t) & (DEG1$log2FoldChange < -logFC_t)
table(k1)
k2 <- (DEG1$pvalue < pvalue_t) & (DEG1$log2FoldChange > logFC_t)
table(k2)
DEG1$change <- ifelse(k1, "DOWN", ifelse(k2, "UP", "NOT"))
table(DEG1$change)
head(DEG1)

# edgeR----
library(edgeR)
exp <- na.omit(exp)
dge <- DGEList(counts = exp, group = Group)
dge$samples$lib.size <- colSums(dge$counts)
dge <- calcNormFactors(dge)

design <- model.matrix(~Group)

dge <- estimateGLMCommonDisp(dge, design)
dge <- estimateGLMTrendedDisp(dge, design)
dge <- estimateGLMTagwiseDisp(dge, design)

fit <- glmFit(dge, design)
fit <- glmLRT(fit)

DEG2 <- topTags(fit, n = Inf)
class(DEG2)
DEG2 <- as.data.frame(DEG2)
head(DEG2)

k1 <- (DEG2$PValue < pvalue_t) & (DEG2$logFC < -logFC_t)
table(k1)
k2 <- (DEG2$PValue < pvalue_t) & (DEG2$logFC > logFC_t)
table(k2)
DEG2$change <- ifelse(k1, "DOWN", ifelse(k2, "UP", "NOT"))

head(DEG2)
table(DEG2$change)
### limma----
library(limma)
dge <- edgeR::DGEList(counts = exp)
dge <- edgeR::calcNormFactors(dge)
design <- model.matrix(~Group)
v <- voom(dge, design, normalize = "quantile")

design <- model.matrix(~Group)
fit <- lmFit(v, design)
fit <- eBayes(fit)

DEG3 <- topTable(fit, coef = 2, n = Inf)
DEG3 <- na.omit(DEG3)

k1 <- (DEG3$P.Value < pvalue_t) & (DEG3$logFC < -logFC_t)
table(k1)
k2 <- (DEG3$P.Value < pvalue_t) & (DEG3$logFC > logFC_t)
table(k2)
DEG3$change <- ifelse(k1, "DOWN", ifelse(k2, "UP", "NOT"))
table(DEG3$change)
head(DEG3)

tj <- data.frame(
  deseq2 = as.integer(table(DEG1$change)),
  edgeR = as.integer(table(DEG2$change)),
  limma_voom = as.integer(table(DEG3$change)),
  row.names = c("down", "not", "up")
)
tj
save(DEG1, DEG2, DEG3, Group, tj, file = paste0(proj, "_DEG.Rdata"))

# 可视化
# 使用了生信技能树老师的包：tinyarray这个包里可以画pca, 热图，火山图，韦恩图，具体每个图的算法，可以看生信技能树GEO芯片分析
library(ggplot2)
library(tinyarray)
exp[1:4, 1:4]
# cpm 去除文库大小的影响
dat <- log2(cpm(exp) + 1) # dat 为使用count数据转化而成的cpm数据，使用这个数据画图，而count数据只是用来做差异分析
pca.plot <- draw_pca(dat, Group)
pca.plot
save(pca.plot, file = paste0(proj, "_pcaplot.Rdata"))

cg1 <- rownames(DEG1)[DEG1$change != "NOT"]
cg2 <- rownames(DEG2)[DEG2$change != "NOT"]
cg3 <- rownames(DEG3)[DEG3$change != "NOT"]

h1 <- draw_heatmap(dat[cg1, ], Group, n_cutoff = 2)
h2 <- draw_heatmap(dat[cg2, ], Group, n_cutoff = 2)
h3 <- draw_heatmap(dat[cg3, ], Group, n_cutoff = 2)

v1 <- draw_volcano(DEG1, pkg = 1, logFC_cutoff = logFC_t)
v2 <- draw_volcano(DEG2, pkg = 2, logFC_cutoff = logFC_t)
v3 <- draw_volcano(DEG3, pkg = 3, logFC_cutoff = logFC_t)

library(patchwork)
(h1 + h2 + h3) / (v1 + v2 + v3) + plot_layout(guides = "collect") & theme(legend.position = "none")

ggsave(paste0(proj, "_heat_vo.png"), width = 15, height = 10)

# 三大R包差异基因对比
UP <- function(df) {
  rownames(df)[df$change == "UP"]
}
DOWN <- function(df) {
  rownames(df)[df$change == "DOWN"]
}

up <- intersect(intersect(UP(DEG1), UP(DEG2)), UP(DEG3))
down <- intersect(intersect(DOWN(DEG1), DOWN(DEG2)), DOWN(DEG3))
dat <- log2(cpm(exp) + 1)
hp <- draw_heatmap(dat[c(up, down), ], Group, n_cutoff = 2)

# 上调、下调基因分别画维恩图
up_genes <- list(
  Deseq2 = UP(DEG1),
  edgeR = UP(DEG2),
  limma = UP(DEG3)
)

down_genes <- list(
  Deseq2 = DOWN(DEG1),
  edgeR = DOWN(DEG2),
  limma = DOWN(DEG3)
)

up.plot <- draw_venn(up_genes, "UPgene")
down.plot <- draw_venn(down_genes, "DOWNgene")

# 维恩图拼图，终于搞定
library(patchwork)
# up.plot + down.plot
# 拼图
pca.plot + hp + up.plot + down.plot + plot_layout(guides = "collect")
ggsave(paste0(proj, "_heat_ve_pca.png"), width = 15, height = 10)


# 分组聚类的热图
library(ComplexHeatmap)
library(circlize)
col_fun <- colorRamp2(c(-2, 0, 2), c("#2fa1dd", "white", "#f87669"))
top_annotation <- HeatmapAnnotation(
  cluster = anno_block(
    gp = gpar(fill = c("#f87669", "#2fa1dd")),
    labels = levels(Group),
    labels_gp = gpar(col = "white", fontsize = 12)
  )
)

m <- Heatmap(t(scale(t(exp[c(up, down), ]))),
  name = " ",
  col = col_fun,
  top_annotation = top_annotation,
  column_split = Group,
  show_heatmap_legend = T,
  border = F,
  show_column_names = F,
  show_row_names = F,
  use_raster = F,
  cluster_column_slices = F,
  column_title = NULL
)
m



# PCAtools
library(airway)
library(magrittr)

data("airway")
airway$dex %<>% relevel("untrt")

ens <- rownames(airway)

library(org.Hs.eg.db)
symbols <- mapIds(org.Hs.eg.db,
  keys = ens,
  column = c("SYMBOL"), keytype = "ENSEMBL"
)
symbols <- symbols[!is.na(symbols)]
symbols <- symbols[match(rownames(airway), names(symbols))]
rownames(airway) <- symbols
keep <- !is.na(rownames(airway))
airway <- airway[keep, ]

library("DESeq2")

dds <- DESeqDataSet(airway, design = ~ cell + dex)
dds <- DESeq(dds)
vst <- assay(vst(dds))

p <- pca(vst, metadata = colData(airway), removeVar = 0.1)

screeplot(p, axisLabSize = 18, titleLabSize = 22)

biplot(p)
biplot(p,
  showLoadings = TRUE,
  labSize = 5, pointSize = 5, sizeLoadingsNames = 5
)


# https://github.com/kevinblighe/PCAtools
# Here, we will instead start with data from Gene Expression Omnibus. We will load breast cancer gene expression data with recurrence free survival (RFS) from Gene Expression Profiling in Breast Cancer: Understanding the Molecular Basis of Histologic Grade To Improve Prognosis.
# First, let’s read in and prepare the data:
library(Biobase)
library(GEOquery)
library(PCAtools)

# load series and platform data from GEO
gset <- getGEO("GSE2990", GSEMatrix = TRUE, getGPL = FALSE)
mat <- exprs(gset[[1]])

# remove Affymetrix control probes
mat <- mat[-grep("^AFFX", rownames(mat)), ]

# extract information of interest from the phenotype data (pdata)
idx <- which(colnames(pData(gset[[1]])) %in%
  c(
    "relation", "age:ch1", "distant rfs:ch1", "er:ch1",
    "ggi:ch1", "grade:ch1", "size:ch1",
    "time rfs:ch1"
  ))
metadata <- data.frame(pData(gset[[1]])[, idx],
  row.names = rownames(pData(gset[[1]]))
)

# tidy column names
colnames(metadata) <- c(
  "Study", "Age", "Distant.RFS", "ER", "GGI", "Grade",
  "Size", "Time.RFS"
)

# prepare certain phenotypes of interest
metadata$Study <- gsub("Reanalyzed by: ", "", as.character(metadata$Study))
metadata$Age <- as.numeric(gsub("^KJ", NA, as.character(metadata$Age)))
metadata$Distant.RFS <- factor(metadata$Distant.RFS,
  levels = c(0, 1)
)
metadata$ER <- factor(gsub("\\?", NA, as.character(metadata$ER)),
  levels = c(0, 1)
)
metadata$ER <- factor(ifelse(metadata$ER == 1, "ER+", "ER-"),
  levels = c("ER-", "ER+")
)
metadata$GGI <- as.numeric(as.character(metadata$GGI))
metadata$Grade <- factor(gsub("\\?", NA, as.character(metadata$Grade)),
  levels = c(1, 2, 3)
)
metadata$Grade <- gsub(1, "Grade 1", gsub(2, "Grade 2", gsub(3, "Grade 3", metadata$Grade)))
metadata$Grade <- factor(metadata$Grade, levels = c("Grade 1", "Grade 2", "Grade 3"))
metadata$Size <- as.numeric(as.character(metadata$Size))
metadata$Time.RFS <- as.numeric(gsub("^KJX|^KJ", NA, metadata$Time.RFS))

# remove samples from the pdata that have any NA value
discard <- apply(metadata, 1, function(x) any(is.na(x)))
metadata <- metadata[!discard, ]

# filter the expression data to match the samples in our pdata
mat <- mat[, which(colnames(mat) %in% rownames(metadata))]

# check that sample names match exactly between pdata and expression data
all(colnames(mat) == rownames(metadata))

p <- pca(mat, metadata = metadata, removeVar = 0.1)
# A bi-plot
biplot(p, showLoadings = TRUE, lab = NULL)
# A pairs plot
pairsplot(p)
# A loadings plot
plotloadings(p, labSize = 3)
# An eigencor plot
eigencorplot(p,
  metavars = c(
    "Study", "Age", "Distant.RFS", "ER",
    "GGI", "Grade", "Size", "Time.RFS"
  )
)
# Access the internal data
# The rotated data that represents the observations / samples is stored in rotated, while the variable loadings are stored in loadings
p$rotated[1:5, 1:5]
p$loadings[1:5, 1:5]

# Advanced features
suppressMessages(require(hgu133a.db))
newnames <- mapIds(hgu133a.db,
  keys = rownames(p$loadings),
  column = c("SYMBOL"),
  keytype = "PROBEID"
)
#   # tidy up for NULL mappings and duplicated gene symbols
newnames <- ifelse(is.na(newnames) | duplicated(newnames),
  names(newnames), newnames
)
rownames(p$loadings) <- newnames


# Determine optimum number of PCs to retain
# Let’s perform Horn’s parallel analysis first:
horn <- parallelPCA(mat)
horn$n

# Now the elbow method:
elbow <- findElbowPoint(p$variance)
elbow

library(ggplot2)

screeplot(p,
          components = getComponents(p, 1:20),
          vline = c(horn$n, elbow)) +
  
  geom_label(aes(x = horn$n + 1, y = 50,
                 label = 'Horn\'s', vjust = -1, size = 8)) +
  geom_label(aes(x = elbow + 1, y = 50,
                 label = 'Elbow method', vjust = -1, size = 8))

which(cumsum(p$variance) > 80)[1]

# Modify bi-plots
biplot(p,
       lab = paste0(p$metadata$Age, ' años'),
       colby = 'ER',
       hline = 0, vline = 0,
       legendPosition = 'right')

# Supply custom colours and encircle variables by group
biplot(p,
       colby = 'ER', colkey = c('ER+' = 'forestgreen', 'ER-' = 'purple'),
       colLegendTitle = 'ER-\nstatus',
       # encircle config
       encircle = TRUE,
       encircleFill = TRUE,
       hline = 0, vline = c(-25, 0, 25),
       legendPosition = 'top', legendLabSize = 16, legendIconSize = 8.0)

biplot(p,
       colby = 'ER', colkey = c('ER+' = 'forestgreen', 'ER-' = 'purple'),
       colLegendTitle = 'ER-\nstatus',
       # encircle config
       encircle = TRUE, encircleFill = FALSE,
       encircleAlpha = 1, encircleLineSize = 5,
       hline = 0, vline = c(-25, 0, 25),
       legendPosition = 'top', legendLabSize = 16, legendIconSize = 8.0)

# Stat ellipses
biplot(p,
       colby = 'ER', colkey = c('ER+' = 'forestgreen', 'ER-' = 'purple'),
       # ellipse config
       ellipse = TRUE,
       ellipseType = 't',
       ellipseLevel = 0.95,
       ellipseFill = TRUE,
       ellipseAlpha = 1/4,
       ellipseLineSize = 1.0,
       xlim = c(-125,125), ylim = c(-50, 80),
       hline = 0, vline = c(-25, 0, 25),
       legendPosition = 'top', legendLabSize = 16, legendIconSize = 8.0)

biplot(p,
       colby = 'ER', colkey = c('ER+' = 'forestgreen', 'ER-' = 'purple'),
       # ellipse config
       ellipse = TRUE,
       ellipseType = 't',
       ellipseLevel = 0.95,
       ellipseFill = TRUE,
       ellipseAlpha = 1/4,
       ellipseLineSize = 0,
       ellipseFillKey = c('ER+' = 'yellow', 'ER-' = 'pink'),
       xlim = c(-125,125), ylim = c(-50, 80),
       hline = 0, vline = c(-25, 0, 25),
       legendPosition = 'top', legendLabSize = 16, legendIconSize = 8.0)

# Change shape based on tumour grade, remove connectors, and add titles
biplot(p,
       colby = 'ER', colkey = c('ER+' = 'forestgreen', 'ER-' = 'purple'),
       hline = c(-25, 0, 25), vline = c(-25, 0, 25),
       legendPosition = 'top', legendLabSize = 13, legendIconSize = 8.0,
       shape = 'Grade', shapekey = c('Grade 1' = 15, 'Grade 2' = 17, 'Grade 3' = 8),
       drawConnectors = FALSE,
       title = 'PCA bi-plot',
       subtitle = 'PC1 versus PC2',
       caption = '27 PCs ≈ 80%')

# Modify line types, remove gridlines, and increase point size
biplot(p,
       lab = NULL,
       colby = 'ER', colkey = c('ER+'='royalblue', 'ER-'='red3'),
       hline = c(-25, 0, 25), vline = c(-25, 0, 25),
       vlineType = c('dotdash', 'solid', 'dashed'),
       gridlines.major = FALSE, gridlines.minor = FALSE,
       pointSize = 5,
       legendPosition = 'left', legendLabSize = 14, legendIconSize = 8.0,
       shape = 'Grade', shapekey = c('Grade 1'=15, 'Grade 2'=17, 'Grade 3'=8),
       drawConnectors = FALSE,
       title = 'PCA bi-plot',
       subtitle = 'PC1 versus PC2',
       caption = '27 PCs ≈ 80%')

biplot(p,
       # loadings parameters
       showLoadings = TRUE,
       lengthLoadingsArrowsFactor = 1.5,
       sizeLoadingsNames = 4,
       colLoadingsNames = 'red4',
       # other parameters
       lab = NULL,
       colby = 'ER', colkey = c('ER+'='royalblue', 'ER-'='red3'),
       hline = 0, vline = c(-25, 0, 25),
       vlineType = c('dotdash', 'solid', 'dashed'),
       gridlines.major = FALSE, gridlines.minor = FALSE,
       pointSize = 5,
       legendPosition = 'left', legendLabSize = 14, legendIconSize = 8.0,
       shape = 'Grade', shapekey = c('Grade 1'=15, 'Grade 2'=17, 'Grade 3'=8),
       drawConnectors = FALSE,
       title = 'PCA bi-plot',
       subtitle = 'PC1 versus PC2',
       caption = '27 PCs ≈ 80%')

# add ESR1 gene expression to the metadata
p$metadata$ESR1 <- mat['205225_at',]

biplot(p,
       x = 'PC2', y = 'PC3',
       lab = NULL,
       colby = 'ESR1',
       shape = 'ER',
       hline = 0, vline = 0,
       legendPosition = 'right') +
  
  scale_colour_gradient(low = 'gold', high = 'red2')

biplot(p, x = 'PC10', y = 'PC50',
       lab = NULL,
       colby = 'Age',
       hline = 0, vline = 0,
       hlineWidth = 1.0, vlineWidth = 1.0,
       gridlines.major = FALSE, gridlines.minor = TRUE,
       pointSize = 5,
       legendPosition = 'left', legendLabSize = 16, legendIconSize = 8.0,
       shape = 'Grade', shapekey = c('Grade 1'=15, 'Grade 2'=17, 'Grade 3'=8),
       drawConnectors = FALSE,
       title = 'PCA bi-plot',
       subtitle = 'PC10 versus PC50',
       caption = '27 PCs ≈ 80%')


# Quickly explore potentially informative PCs via a pairs plot
pairsplot(p,
          components = getComponents(p, c(1:10)),
          triangle = TRUE, trianglelabSize = 12,
          hline = 0, vline = 0,
          pointSize = 0.4,
          gridlines.major = FALSE, gridlines.minor = FALSE,
          colby = 'Grade',
          title = 'Pairs plot', plotaxes = FALSE,
          margingaps = unit(c(-0.01, -0.01, -0.01, -0.01), 'cm'))

pairsplot(p,
          components = getComponents(p, c(4,33,11,1)),
          triangle = FALSE,
          hline = 0, vline = 0,
          pointSize = 0.8,
          gridlines.major = FALSE, gridlines.minor = FALSE,
          colby = 'ER',
          title = 'Pairs plot', titleLabSize = 22,
          axisLabSize = 14, plotaxes = TRUE,
          margingaps = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'))


# Determine the variables that drive variation among each PC
plotloadings(p,
             rangeRetain = 0.01,
             labSize = 4.0,
             title = 'Loadings plot',
             subtitle = 'PC1, PC2, PC3, PC4, PC5',
             caption = 'Top 1% variables',
             shape = 24,
             col = c('limegreen', 'black', 'red3'),
             drawConnectors = TRUE)

plotloadings(p,
             components = getComponents(p, c(4,33,11,1)),
             rangeRetain = 0.1,
             labSize = 4.0,
             absolute = FALSE,
             title = 'Loadings plot',
             subtitle = 'Misc PCs',
             caption = 'Top 10% variables',
             shape = 23, shapeSizeRange = c(1, 16),
             col = c('white', 'pink'),
             drawConnectors = FALSE)

plotloadings(p,
             components = getComponents(p, c(2)),
             rangeRetain = 0.12, absolute = TRUE,
             col = c('black', 'pink', 'red4'),
             drawConnectors = TRUE, labSize = 4) + coord_flip()


# Correlate the principal components back to the clinical data
eigencorplot(p,
             components = getComponents(p, 1:27),
             metavars = c('Study','Age','Distant.RFS','ER',
                          'GGI','Grade','Size','Time.RFS'),
             col = c('darkblue', 'blue2', 'black', 'red2', 'darkred'),
             cexCorval = 0.7,
             colCorval = 'white',
             fontCorval = 2,
             posLab = 'bottomleft',
             rotLabX = 45,
             posColKey = 'top',
             cexLabColKey = 1.5,
             scale = TRUE,
             main = 'PC1-27 clinical correlations',
             colFrame = 'white',
             plotRsquared = FALSE)

eigencorplot(p,
             components = getComponents(p, 1:horn$n),
             metavars = c('Study','Age','Distant.RFS','ER','GGI',
                          'Grade','Size','Time.RFS'),
             col = c('white', 'cornsilk1', 'gold', 'forestgreen', 'darkgreen'),
             cexCorval = 1.2,
             fontCorval = 2,
             posLab = 'all',
             rotLabX = 45,
             scale = TRUE,
             main = bquote(Principal ~ component ~ Pearson ~ r^2 ~ clinical ~ correlates),
             plotRsquared = TRUE,
             corFUN = 'pearson',
             corUSE = 'pairwise.complete.obs',
             corMultipleTestCorrection = 'BH',
             signifSymbols = c('****', '***', '**', '*', ''),
             signifCutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1))


# Plot the entire project on a single panel
pscree <- screeplot(p, components = getComponents(p, 1:30),
                    hline = 80, vline = 27, axisLabSize = 14, titleLabSize = 20,
                    returnPlot = FALSE) +
  geom_label(aes(20, 80, label = '80% explained variation', vjust = -1, size = 8))

ppairs <- pairsplot(p, components = getComponents(p, c(1:3)),
                    triangle = TRUE, trianglelabSize = 12,
                    hline = 0, vline = 0,
                    pointSize = 0.8, gridlines.major = FALSE, gridlines.minor = FALSE,
                    colby = 'Grade',
                    title = '', plotaxes = FALSE,
                    margingaps = unit(c(0.01, 0.01, 0.01, 0.01), 'cm'),
                    returnPlot = FALSE)

pbiplot <- biplot(p,
                  # loadings parameters
                  showLoadings = TRUE,
                  lengthLoadingsArrowsFactor = 1.5,
                  sizeLoadingsNames = 4,
                  colLoadingsNames = 'red4',
                  # other parameters
                  lab = NULL,
                  colby = 'ER', colkey = c('ER+'='royalblue', 'ER-'='red3'),
                  hline = 0, vline = c(-25, 0, 25),
                  vlineType = c('dotdash', 'solid', 'dashed'),
                  gridlines.major = FALSE, gridlines.minor = FALSE,
                  pointSize = 5,
                  legendPosition = 'none', legendLabSize = 16, legendIconSize = 8.0,
                  shape = 'Grade', shapekey = c('Grade 1'=15, 'Grade 2'=17, 'Grade 3'=8),
                  drawConnectors = FALSE,
                  title = 'PCA bi-plot',
                  subtitle = 'PC1 versus PC2',
                  caption = '27 PCs ≈ 80%',
                  returnPlot = FALSE)

ploadings <- plotloadings(p, rangeRetain = 0.01, labSize = 4,
                          title = 'Loadings plot', axisLabSize = 12,
                          subtitle = 'PC1, PC2, PC3, PC4, PC5',
                          caption = 'Top 1% variables',
                          shape = 24, shapeSizeRange = c(4, 8),
                          col = c('limegreen', 'black', 'red3'),
                          legendPosition = 'none',
                          drawConnectors = FALSE,
                          returnPlot = FALSE)

peigencor <- eigencorplot(p,
                          components = getComponents(p, 1:10),
                          metavars = c('Study','Age','Distant.RFS','ER',
                                       'GGI','Grade','Size','Time.RFS'),
                          cexCorval = 1.0,
                          fontCorval = 2,
                          posLab = 'all', 
                          rotLabX = 45,
                          scale = TRUE,
                          main = "PC clinical correlates",
                          cexMain = 1.5,
                          plotRsquared = FALSE,
                          corFUN = 'pearson',
                          corUSE = 'pairwise.complete.obs',
                          signifSymbols = c('****', '***', '**', '*', ''),
                          signifCutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                          returnPlot = FALSE)

library(cowplot)
library(ggplotify)

top_row <- plot_grid(pscree, ppairs, pbiplot,
                     ncol = 3,
                     labels = c('A', 'B  Pairs plot', 'C'),
                     label_fontfamily = 'serif',
                     label_fontface = 'bold',
                     label_size = 22,
                     align = 'h',
                     rel_widths = c(1.10, 0.80, 1.10))

bottom_row <- plot_grid(ploadings,
                        as.grob(peigencor),
                        ncol = 2,
                        labels = c('D', 'E'),
                        label_fontfamily = 'serif',
                        label_fontface = 'bold',
                        label_size = 22,
                        align = 'h',
                        rel_widths = c(0.8, 1.2))

plot_grid(top_row, bottom_row, ncol = 1,
          rel_heights = c(1.1, 0.9))



# Make predictions on new data
p <- pca(mat, metadata = metadata, removeVar = 0.1)

p.prcomp <- list(sdev = p$sdev,
                 rotation = data.matrix(p$loadings),
                 x = data.matrix(p$rotated),
                 center = TRUE, scale = TRUE)

class(p.prcomp) <- 'prcomp'

# for this simple example, just use a chunk of
# the original data for the prediction
newdata <- t(mat[,seq(1,20)])
predict(p.prcomp, newdata = newdata)[,1:5]




