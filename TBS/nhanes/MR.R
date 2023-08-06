# Mendelian Randomization Analyses（https://www.bilibili.com/video/BV1Vu411H7EH/?spm_id_from=pageDriver&vd_source=23f183f0c5968777e138f31842bde0a0）
# https://blog.csdn.net/Timo_CSDN/article/details/123183047

# MR-Base platform数据库上找到合适的数据集作为暴露，然后再找到合适的数据集作为结局
# 安装相关的R包
# install.packages("devtools")
# devtools::install_github("MRCIEU/TwoSampleMR")
library(TwoSampleMR)

## 两样本MR(在线提取)
# 读取数据与统计分析
# 从数据库获取结局相关的GWAS
## Get GWAS data from the IEU GWAS database that rooted in the package 
ao <- available_outcomes()
#> API: public: http://gwas-api.mrcieu.ac.uk/
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

## Heterogeneity statistics
mr_heterogeneity(dat)

# pleiotropy test
mr_pleiotropy_test(dat)


# leave-one-out analysis
res_loo <- mr_leaveoneout(dat)

# 可视化
# scatter plot
p1 <-mr_scatter_plot(res, dat)
length(p1) # to see how many plots are there
p1[[1]]

# forest plot
res_single <- mr_singlesnp(dat)#,all_method=c("mr_ivw", "mr_two_sample_ml")) to specify method used
p2 <- mr_forest_plot(res_single) 
p2[[1]]

## funnel plot for all
p3 <- mr_funnel_plot(res_single)
p3[[1]]

### save images ##########
library(ggplot2)
ggsave(p1[[1]], file="mr_scatter_plot.png", 
       width = 7, height=7, dpi=900)

# 6. 结果整合
mr_report(dat)

## To combine all resultsres<-mr(dat)
het<-mr_heterogeneity(dat)
plt<-mr_pleiotropy_test(dat)
sin<-mr_singlesnp(dat)
all_res<-combine_all_mrresults(res,het,plt,sin,ao_slc=T,Exp=T,split.exposure=F,split.outcome=T)
head(all_res[,c("Method","outcome","exposure","nsnp","b","se","pval","intercept","intercept_se","intercept_pval","Q","Q_df","Q_pval","consortium","ncase","ncontrol","pmid","population")])

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
  r_exp=0)



