#### Mendelian Randomization Analyses(https://www.bilibili.com/video/BV1Vu411H7EH/?spm_id_from=pageDriver&vd_source=23f183f0c5968777e138f31842bde0a0)####
# https://blog.csdn.net/Timo_CSDN/article/details/123183047
# MR-Base platform数据库上找到合适的数据集作为暴露，然后再找到合适的数据集作为结局
# 安装相关的R包
# install.packages("devtools")
# devtools::install_github("MRCIEU/TwoSampleMR")
rm(list = ls())
library(TwoSampleMR)
library(doParallel) # 并行

## 两样本MR(在线提取)
# 读取数据与统计分析
# 从数据库获取结局相关的GWAS
## Get GWAS data from the IEU GWAS database that rooted in the package
ao <- available_outcomes()
# getwd()
# write.csv(ao, "C:\\Users\\wane1\\Documents\\file\\sci\\Shuntak\\MR\\ao_2023_08_06.csv")
ao <- read.csv("C:\\Users\\wane1\\Documents\\file\\sci\\Shuntak\\MR\\ao_2023_08_06.csv")
# 根据id和trait提取组学数据
# [代谢物] #  a b c d四个数据来源
metabolites <- ao[grep("met", ao$id), ]
# [蛋白质]
protein <- ao[grep("prot", ao$id), ]
# [菌群]就是Mibiogen上面的 #详细着菌群推文
bacteria <- ao[grep("Gut microbiota abundance", ao$trait), ]
# [eQTL]
eQTL <- ao[grep("eqtl", ao$id), ]
## 提取FinnGen的数据信息
finn <- ao[grep("finn", ao$id), ]
## 获取研究id的样本量信息等
ao[ao$id == "ieu-a-7", c("sample_size", "ncase", "ncontrol")]

## 提取工具变量（以eqtl为例）
results <- extract_instruments(outcomes = eQTL$id, p1 = 1e-5, clump = TRUE)
# 如频繁网络中断，可for循环追加
# 保留，下次可以直接调用，进行不同结局探索
df_dat_exp <-rbind(results,res)

# for循环追加
df_dat_exp = NULL
for(i in 1:1000){
  res <- extract_instruments(
    outcomes=eQTL$id[i],p1 = 1e-5,
    clump=TRUE)
  #保留，下次可以直接调用，迹行不同结同探素
  df_dat_exp <- rbind(df_dat_exp,res)
}

saveRDS(df_dat_exp,"C:\\Users\\wane1\\Documents\\file\\sci\\Shuntak\\MR\\eQTL_exp_dat.rds")
df_dat_exp <- readRDS("C:\\Users\\wane1\\Documents\\file\\sci\\Shuntak\\MR\\eQTL_exp_dat.rds")

#### -一键寻找暴露因素,[提供初步筛选] ####
# 读取暴露集 [直接读取最新数据,直接提供]
exp_data <- readRDS("exp_data_2023.4.3_p5e8_idALL.rds")
ao <- read.csv("ao_2023.4.2.csv")
length(unique(exp_data$id.exposure))
# 总共26279个暴露信息
# 去掉eqtl的
# 表达数量性状位点（expression quantitative trait locus, eQTL）
# 是一类能够影响基因表达量的遗传位点（大部分都是单核苷酸多态性，SNP）
delete_eqtl <- stringr::str_detect(
  string = exp_data$id.exposure,
  pattern = "eqtl-a", negate = TRUE )
exp_data <- subset(exp_data,delete_eqtl)
length(unique(exp_data$id.exposure))
# 有10090个暴露信息
# 筛选人群, 你研究的人群确定下
table(ao$population) # 先看看  
id2 <- subset(ao,ao$population=="European")
length(unique(id2$id))
# # 测试选取10000个id部分
id2 <- id2[1:10000,]  # 实际运行注解掉
dplyr::count(id2,population)
nrow(exp_data)
exp_data <- subset(exp_data,
                   exp_data$id.exposure %in% id2$id)
nrow(exp_data)


# IEU在线IEU的
outcome=extract_outcome_data(
  snps = unique(exp_data$SNP),
  outcomes = "ebi-a-GCST90000514", # Lung cancer UK Biobank
  proxies = TRUE)


# 如果outcome比较小,尝试并行
exp_data_list=split(exp_data,list(exp_data$id.exposure))
har_loop <- function(exp_data=exp_data_list){
  BBB=TwoSampleMR::harmonise_data(
    exposure_dat = exp_data, outcome_dat = outcome)
  return(BBB)
}
# 
detectCores(logical = FALSE)
cl<- makeCluster(4)   # CPU数/2
# 登记
registerDoParallel(cl)
# #添加并行计算中用到的包
clusterEvalQ(cl,library(TwoSampleMR))
# #添加并行计算中用到的环境变量
clusterExport(cl = cl ,
              varlist = c("exp_data_list","outcome",
                          "har_loop"))
# 
start1=Sys.time()
dat_list=parLapply(cl = cl,X = exp_data_list,
                   fun = har_loop)
end1=Sys.time();end1-start1
stopCluster(cl)
# 预计耗时__23秒___
dat <- do.call(rbind,dat_list)
dat <- subset(dat,mr_keep)

dat <- split(dat,list(dat$id.exposure))
length(dat)
names(dat) <- paste0("A",1:length(dat))
deleteSNP=names(dat)[sapply(dat,nrow)<3]
length(deleteSNP)
# [1]-去掉list里面小于3个SNP的
for (deleteSNPid in deleteSNP) {
  dat[[deleteSNPid]] <- NULL
}
length(dat)
dat2 <- do.call(rbind,dat)
length(unique(dat2$id.exposure)) 
# 并行
library(doParallel)

choose_MR <- function(dat=dat){ 
  res_hete <- mr_heterogeneity(dat) 
  if (res_hete$Q_pval[2]<0.05) {
    res=mr(dat, method_list = c("mr_egger_regression",
                                "mr_weighted_median", "mr_ivw_mre"))
  } else{
    res=mr(dat, method_list = c("mr_egger_regression",
                                "mr_weighted_median", "mr_ivw_fe"))
  }
  return(res)
}

cl<- makeCluster(4)   # CPU数/2
# 登记
registerDoParallel(cl)
# #添加并行计算中用到的包
clusterEvalQ(cl,library(TwoSampleMR))
# #添加并行计算中用到的环境变量
clusterExport(cl = cl ,varlist = c("dat","choose_MR"))
# 
start1=Sys.time()
res_list=parLapply(cl = cl,X = dat,fun = choose_MR)
end1=Sys.time();end1-start1
stopCluster(cl)
# 预计耗时__43秒___
res <- do.call(rbind,res_list)

# 结果提取
res$pval=round(res$pval,3)

res_ALL <- split(res,list(res$id.exposure))
#
judge_1 <- function(mr_res=res2) {
  mr_res$b_direction <- as.numeric(sign(mr_res$b))
  mr_res$b_direction=ifelse(abs(sum(mr_res$b_direction))==3 ,
                            NA,"Inconsistent direction")
  mr_res$p_no <- NA
  mr_res[mr_res$method=="MR Egger","p_no"] <- ifelse(
    mr_res[mr_res$method=="MR Egger","pval"]<0.05," ",
    "MR Egger")
  mr_res[mr_res$method=="Weighted median","p_no"] <- ifelse(
    mr_res[mr_res$method=="Weighted median","pval"]<0.05," ",
    "Weighted median")
  mr_res[grep(x = mr_res$method,pattern = "Inverse variance"),"p_no"] <- ifelse(
    mr_res[grep(x = mr_res$method,pattern = "Inverse variance"),"pval"]<0.05,
    " ","IVW")
  mr_res$p_no <- paste(mr_res$p_no,collapse = " ")
  mr_res$p_no=trimws(mr_res$p_no,which = c("both"))
  return(mr_res)
}

res_ALL=purrr::map(.x =res_ALL,.f = ~judge_1(.x) )
res_ALL2 <- do.call(rbind,res_ALL)
res_ALL3 <- subset(res_ALL2,
                   is.na(res_ALL2$b_direction) )
bool=stringr::str_detect(string =res_ALL3$p_no,
                         pattern = "IVW",negate = TRUE )
res_ALL4 <- subset(res_ALL3,bool)

res_ALL4_1=subset(res_ALL4,select = exposure)
res_ALL4_1 <- unique(res_ALL4_1)
res_ALL4_1[1,1]
res_ALL4_2 <- tidyr::separate(
  data = res_ALL4_1,col = exposure,sep = "\\|",
  into = c("exposure","delete")) %>%
  dplyr::select(-delete)

# 导出
library(openxlsx)
wb <- createWorkbook("My name here")
## Add a worksheets
addWorksheet(wb, "sheet1", gridLines = FALSE)
addWorksheet(wb, "sheet2", gridLines = FALSE)
## write data to worksheet 1
writeData(wb,x = res_ALL4,sheet = "sheet1",
          rowNames = FALSE)
writeData(wb,x = res_ALL4_2,sheet = "sheet2",
          rowNames = FALSE)
## style for body
bodyStyle <- createStyle(border = "TopBottom",
                         bgFill ="#e3e9f4",  
                         fgFill = "#e3e9f4")
a=seq(2,nrow(res_ALL4)+1,6)
b=seq(3,nrow(res_ALL4)+1,6)
c=seq(4,nrow(res_ALL4)+1,6)
d=sort(c(a,b,c))
d
addStyle(wb, sheet = 1, bodyStyle, 
         rows = d,
         cols = 1:11, 
         gridExpand = TRUE)
setColWidths(wb, 1, cols = 1, widths = 21) ## set column width for row names column
## Not run: 
saveWorkbook(wb, "Example_LungCancer.xlsx", 
             overwrite = TRUE)
# 可自行增加判断
# mr_heterogeneity
# mr_pleiotropy_test


##################################################
ao_outcome <- ao[grepl("epilepsy", ao$trait), ]
View(ao_outcome)

# 提取工具变量的SNP
exposure_dat <- extract_instruments(c("ukb-b-13806"))
# 从结局GWAS提取工具变量
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c("ieu-b-8"), proxies = 1, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)

# 3. 数据预处理
# 对数据进行预处理，使其效应等位与效应量保持统一，就是调整exp和out上位点的方向和beta，使其统一。
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)

# 4. MR和敏感性分析等
## MR
res <- mr(dat) # mr(dat, method_list=c("mr_egger_regression", "mr_ivw"))

# 异质性检验(Heterogeneity statistics)
mr_heterogeneity(dat)
run_mr_presso(dat)

# 多效性检验(pleiotropy test)
mr_pleiotropy_test(dat)

# leave-one-out analysis
res_loo <- mr_leaveoneout(dat)

# 可视化
# scatter plot
p1 <- mr_scatter_plot(res, dat)
length(p1) # to see how many plots are there
p1[[1]]

# forest plot
res_single <- mr_singlesnp(dat) # ,all_method=c("mr_ivw", "mr_two_sample_ml")) to specify method used
p2 <- mr_forest_plot(res_single)
p2[[1]]

## funnel plot for all
p3 <- mr_funnel_plot(res_single)
p3[[1]]

### save images
library(ggplot2)
ggsave(p1[[1]],
  file = "mr_scatter_plot.png",
  width = 7, height = 7, dpi = 900
)

# 6. 结果整合
mr_report(dat)

## To combine all resultsres<-mr(dat)
het <- mr_heterogeneity(dat)
plt <- mr_pleiotropy_test(dat)
sin <- mr_singlesnp(dat)
all_res <- combine_all_mrresults(res, het, plt, sin, ao_slc = T, Exp = T, split.exposure = F, split.outcome = T)
head(all_res[, c("Method", "outcome", "exposure", "nsnp", "b", "se", "pval", "intercept", "intercept_se", "intercept_pval", "Q", "Q_df", "Q_pval", "consortium", "ncase", "ncontrol", "pmid", "population")])

# 7. Variance explained
out <- directionality_test(dat)
library(knitr)
kable(out)

mr_steiger(
  p_exp = dat$pval.exposure,
  p_out = dat$pval.outcome,
  n_exp = dat$samplesize.exposure,
  n_out = dat$samplesize.outcome,
  r_xxo = 1,
  r_yyo = 1,
  r_exp = 0
)












