# https://www.bilibili.com/video/BV1is411H7Hq?p=4&spm_id_from=pageDriver&vd_source=23f183f0c5968777e138f31842bde0a0
rm(list = ls())
options(warn = -1)
suppressPackageStartupMessages(library(CLL))
suppressPackageStartupMessages(library(GEOquery))

eSet <- getGEO('GSE205661', destdir = ".",
                    getGPL = F) # AnnotGPL = T, 
exp <- exprs(eSet[[1]])   # 表达矩阵
GPL <- eSet[[1]]@annotation # 平台信息——提取芯片平台编号
GPL
eSet[[1]]@featureData@data
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
# exprSet = exprs(a)
# ID转换 https://www.jianshu.com/p/78a64d2d998a
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
## ID的格式，你挑一个出来和下面的是对应的
# [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT"  "ENSEMBLTRANS" [图片上传中...(661771789232197568.jpg-f54963-1593781817202-0)]
"ENTREZID"    
# [7] "ENZYME"       "EVIDENCE"     "EVIDENCEALL"  "GENENAME"     "GO"           "GOALL"       
# [13] "IPI"          "MAP"          "OMIM"         "ONTOLOGY"     "ONTOLOGYALL"  "PATH"        
# [19] "PFAM"         "PMID"         "PROSITE"      "REFSEQ"       "SYMBOL"       "UCSCKG" 

# 确保数据格式为数据框
ids1=as.data.frame(ids)

ids <- bitr(ids1$ID,  #你的数据框
            fromType = "ENSEMBLPROT", #你的ID的数据类型
            toType = "SYMBOL", #转化的数据类型
            OrgDb = org.Hs.eg.db)  #org.Hs.eg.db——人类
#最后得出的ids就是结果







