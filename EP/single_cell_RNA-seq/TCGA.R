# https://mp.weixin.qq.com/s/5LAcZR7mjFLLw_YwM5iG6g
# 绘制基因生存曲线
rm(list = ls())
library(SummarizedExperiment)
library(TCGAbiolinks)
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)

# 自定义函数代码
# define function
SurvivalAnalysis <- function(project = NULL,
                             matrix = NULL,
                             clinical = NULL,
                             gene = NULL,
                             return.res = 'all',
                             col = c("red","black"),
                             risk.table = FALSE,
                             conf.int = TRUE,
                             conf.int.alpha = 0.05,
                             ...){
  # ======================================
  # whether supply own file
  if(is.null(matrix)){
    # RNASEQ
    query <- TCGAbiolinks::GDCquery(project = project,
                                    data.category = "Transcriptome Profiling",
                                    data.type = "Gene Expression Quantification",
                                    workflow.type = "STAR - Counts"
    )
    # DOWNLOAD
    TCGAbiolinks::GDCdownload(query, files.per.chunk = 100)
    data <- TCGAbiolinks::GDCprepare(query,save = T,save.filename = paste0(project,"_RNA.Rdata"))
  }else{
    load(matrix)
  }
  # ======================================
  if(is.null(clinical)){
    # get clinlical
    query <- TCGAbiolinks::GDCquery(project = project,
                                    data.category = "Clinical",
                                    file.type = "xml")
    TCGAbiolinks::GDCdownload(query)
    clinical <- TCGAbiolinks::GDCprepare_clinic(query, clinical.info = "patient") %>%
      dplyr::select(bcr_patient_barcode,
                    days_to_death,
                    days_to_last_followup,
                    vital_status) %>%
      unique()
  }else{
    clinical <- readRDS(clinical) %>%
      dplyr::select(bcr_patient_barcode,
                    days_to_death,
                    days_to_last_followup,
                    vital_status) %>%
      unique()
  }
  # ======================================
  # prepare gene infomation
  gene.anno <- SummarizedExperiment::rowData(data) %>% data.frame()
  
  # get tpm
  tpm <- SummarizedExperiment::assay(data,"tpm_unstrand")
  
  # selection of tumor samples "TP"
  samplesTP <- TCGAbiolinks::TCGAquery_SampleTypes(
    barcode = colnames(data),
    typesample = c("TP"))
  
  # =================================================
  # loop for genes
  df.res <- lapply(1:length(gene), function(g){
    gene.id <- gene.anno %>%
      dplyr::filter(gene_name == gene[g]) %>%
      dplyr::select(gene_id) %>% as.character()
    
    gene.tpm <- tpm[gene.id,samplesTP]
    gene.median <- median(gene.tpm)
    
    # assign gene groups
    gene.id.group <- purrr::map_df(1:length(gene.tpm), function(x){
      if(gene.tpm[x] >= gene.median){
        group.info <- data.frame(id = names(gene.tpm[x]),type = 'high',
                                 tpm = gene.tpm[x],
                                 bcr_patient_barcode = paste(unlist(strsplit(names(gene.tpm[x]),split = '-'))[1:3],collapse = '-'))
      }else{
        group.info <- data.frame(id = names(gene.tpm[x]),type = 'low',
                                 tpm = gene.tpm[x],
                                 bcr_patient_barcode = paste(unlist(strsplit(names(gene.tpm[x]),split = '-'))[1:3],collapse = '-'))
      }
      return(group.info)
    })
    
    # merge
    df.final <- merge(clinical,gene.id.group,by = 'bcr_patient_barcode',all = F)
    
    # print(head(df.final[1:3,1:3]))
    
    # 死亡状态为1，其他状态为0
    df.final$event <- ifelse(df.final$vital_status == "Dead",1,0)
    df.final$days_to_death[is.na(df.final$days_to_death)] = 0
    df.final$days_to_last_followup[is.na(df.final$days_to_last_followup)] = 0
    
    # days_to_last_follow_up不为NA表示为删失，用此时间作为生存时间
    df.final$days <- df.final$days_to_death + df.final$days_to_last_followup
    df.final$time <- df.final$days/30
    sfit <- survival::survfit(Surv(time, event) ~ type,data = df.final)
    data.survdiff <- survival::survdiff(Surv(time, event) ~ type,data = df.final)
    p.val = round(1 - stats::pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1),digits = 3)
    
    # res info
    res <- data.frame(gene = gene[g],pvalue = p.val)
    
    # plot
    # p <- ggsurvplot(sfit,data = df.final)
    p <- survminer::ggsurvplot(sfit,
                               data = df.final,
                               pval = F,
                               risk.table = risk.table,
                               conf.int = conf.int,
                               conf.int.style = "ribbon",
                               conf.int.alpha = conf.int.alpha,
                               xlab = "Time (months)",
                               legend= c(.9,.9),
                               ggtheme = ggplot2::theme_bw(base_size = 14) +
                                 ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=rel(1.2)),
                                                legend.background = ggplot2::element_blank(),
                                                # aspect.ratio = 0.8,
                                                axis.title = ggplot2::element_text(size = rel(1.2)),
                                                panel.grid = ggplot2::element_blank(),
                                                plot.margin = ggplot2::unit(rep(2,4),'lines')),
                               title = paste0(gene[g]," (Pvalue = ",sprintf("%.3f",p.val),")"),
                               palette = col,
                               legend.title = "",
                               legend.labs = c("high","low"),
                               ...)
    
    # results
    sur.res <- list(data = res,plotdata = sfit,plot = p)
    return(sur.res)
  })
  
  # return type
  if(return.res == "pval"){
    # pvalue info
    ret.res <- purrr::map_df(1:length(df.res), function(x){
      df.res[[x]]$data
    })
  }else{
    ret.res <- df.res
  }
  return(ret.res)
}

# auto download data
SurvivalAnalysis(project = "TCGA-LUAD",
                 gene = "METTL3")

# load local data
SurvivalAnalysis(matrix = "All-cancer/RNA-data/TCGA-LUAD_RNA.Rdata",
                 clinical = "All-cancer/Clinic-data/TCGA-LUAD_clinical.rds",
                 gene = "METTL3")

# RNASEQ
project = "TCGA-LUAD"
query <- TCGAbiolinks::GDCquery(project = project,
                                data.category = "Transcriptome Profiling",
                                data.type = "Gene Expression Quantification",
                                workflow.type = "STAR - Counts"
)
# DOWNLOAD
TCGAbiolinks::GDCdownload(query, files.per.chunk = 100)
data <- TCGAbiolinks::GDCprepare(query,save = T,save.filename = paste0(project,"_RNA.Rdata"))

############################
# get clinlical
query <- TCGAbiolinks::GDCquery(project = project,
                                data.category = "Clinical",
                                file.type = "xml")
TCGAbiolinks::GDCdownload(query)
clinical <- TCGAbiolinks::GDCprepare_clinic(query, clinical.info = "patient")
saveRDS(clinical,file = paste0(project,"_clinical.rds"))


SurvivalAnalysis(matrix = "All-cancer/RNA-data/TCGA-LUAD_RNA.Rdata",
                 clinical = "All-cancer/Clinic-data/TCGA-LUAD_clinical.rds",
                 gene = "METTL3")

# 只输出p value
SurvivalAnalysis(matrix = "All-cancer/RNA-data/TCGA-LUAD_RNA.Rdata",
                 clinical = "All-cancer/Clinic-data/TCGA-LUAD_clinical.rds",
                 gene = c("ACTB","MYC","METTL3","METTL14"),
                 return.res = 'pval')

# 多基因生存曲线:
suv <-
  SurvivalAnalysis(matrix = "All-cancer/RNA-data/TCGA-LUAD_RNA.Rdata",
                   clinical = "All-cancer/Clinic-data/TCGA-LUAD_clinical.rds",
                   gene = c("ACTB","MYC","METTL3","METTL14"))

# combine
arrange_ggsurvplots(x = lapply(1:4, function(x){suv[[x]]$plot}),
                    print = T,
                    ncol = 2,nrow = 2)























