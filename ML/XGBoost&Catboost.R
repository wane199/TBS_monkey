# XGBoost模型的可解释性
# https://mp.weixin.qq.com/s?__biz=Mzg3ODg5MzU5NA==&mid=2247485717&idx=1&sn=bba1a9959d5c2cc418791fc60e692bcf&chksm=cf0d8660f87a0f76d247d22a1f3cd172e984e4533e5c77dc7f1139bb7f5f5c3a2f5489be6ff8&scene=178&cur_album_id=2918226271971786756#rd
# 1、加载R包及数据
rm(list = ls())
library(xgboost)
library(tidymodels)

dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\sci\\cph\\XML\\TLE234group_2019.csv")
dt <- na.omit(dt)
dt <- dt[c(7:24)]
vfactor <- c("oneyr","side","Sex")
dt[vfactor] <- lapply(dt[vfactor], factor)
# 批量数值转因子
for (i in names(dt)[c(-2,-3,-6:-8)]) {
  dt[, i] <- as.factor(dt[, i])
}
str(dt)

# 2、模型拟合
xgb_fit <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  fit(oneyr~.,data=dt)

# 3、构建解释器
library(DALEXtra)
xgb_exp <- explain_tidymodels(xgb_fit,
                              data = dt[,-1],
                              y=dt$oneyr,
                              label = "xgboost")
# 4、模型解释
# 4.1 Breakdown
xgb_bd <- predict_parts(xgb_exp,
                        new_observation=dt[2,])
plot(xgb_bd)

# 4.2 SHAP值
xgb_shap <- predict_parts(xgb_exp,
                          type = "shap",
                          new_observation=dt[2,])
plot(xgb_shap,show_boxplots=FALSE)

# 4.3 ROC曲线
library(auditor)
plot(model_evaluation(xgb_exp))

# 4.5 部分依赖图（PDP）
xgb_profiles <- model_profile(xgb_exp)
plot(xgb_profiles)

# 4.6 变量重要性
library(vivo)
xgb_vp <- global_variable_importance(xgb_profiles)
plot(xgb_vp)

# 4.7 CP图
xgb_cp <- predict_profile(xgb_exp,
                          new_observation = dt[2,])
plot(xgb_cp)


################################
# 基于SHAP的XGBoost解释
# https://mp.weixin.qq.com/s?__biz=Mzg3ODg5MzU5NA==&mid=2247485738&idx=1&sn=f556c52c2beeae1b72d629e4f51e4e98&chksm=cf0d865ff87a0f4966b37278722aceb06d3065257ea5b11c6e0e7f30544c7a77e373aa53c82d&mpshare=1&scene=1&srcid=0511spKAjyHQ5vpNrYV76XN1&sharer_sharetime=1683762576579&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# 1、加载R包及数据集
rm(list = ls())
library(shapviz)
library(ggplot2)
library(xgboost)
dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\sci\\cph\\XML\\TLE234group_2019.csv")
dt <- na.omit(dt)
dt <- dt[c(7:24)]
vfactor <- c("oneyr","side","Sex")
dt[vfactor] <- lapply(dt[vfactor], factor)
# 批量数值转因子
for (i in names(dt)[c(-2,-3,-6:-8)]) {
  dt[, i] <- as.factor(dt[, i])
}
str(dt)
# 2、构建模型
set.seed(123)
dtrain <- xgb.DMatrix(data.matrix(dt[,-1]), 
                      label = dt[,1])
fit <- xgb.train(params = list(eta = 0.1,
                               nthread=2,
                               eval_metric="auc"),
                 data = dtrain,
                 prediction=TRUE,
                 nrounds = 100)
# 计算SHAP值
shap <- shapviz(fit,X_pred =data.matrix(dt[,-1]))

# 3、waterfall plot
sv_waterfall(shap, 
             row_id = 2,
             fill_colors=c("#FF0000", "#0085FF"))

# 4、force plot
# 其实force plot图是waterfall plot 图的另外一种展示形式，本质上是一样的。
sv_force(shap,
         row_id = 2,
         max_display = 10,
         fill_colors=c("#FF0000", "#0085FF"))

# 5、基于SHAP值的变量重要性（SHAP summary plot）
sv_importance(shap, kind = "beeswarm")

# 条形图
sv_importance(shap,fill = "#0085FF")

# 6、SHAP dependence plots（SHAP依赖图）
sv_dependence(shap, 
              v = "ADAPE",
              color_var = NULL)

# 7、SHAP Interaction Plot
shp_i <- shapviz(fit, 
                 X_pred =data.matrix(dt[,-1]), 
                 interactions = TRUE)

sv_interaction(shp_i)+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


#############################################
# 快速构建基于树模型的自动机器学习工具(https://mp.weixin.qq.com/s?__biz=Mzg3ODg5MzU5NA==&mid=2247485792&idx=1&sn=77ad95f7ac6f718ebf7f1e3bcca7939b&chksm=cf0d8615f87a0f03a5dec26adb485a928aa77bcbba3a057b8ae47a0566583ab955e65ff40ac7&mpshare=1&scene=24&srcid=0516XMXzydd0R29ZlZV8vgaS&sharer_sharetime=1684193591197&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd)
# 1、安装forester及依赖包
install.packages("devtools")
devtools::install_github("ModelOriented/forester")
# 还需要安装依赖包
#安装CatBoost
devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.1.1/catboost-R-Darwin-1.1.1.tgz', 
                      INSTALL_opts = c("--no-multiarch", "--no-test-load", "--no-staged-install"))
#或者本地安装
devtools::install_github('catboost/catboost', 
                         subdir = 'catboost/R-package')
# 绘制radar图
devtools::install_github('ricardo-bion/ggradar', 
                         dependencies = TRUE)

# 导出报告依赖包
install.packages('tinytex')
tinytex::install_tinytex()

# 2、加载R包及数据
library(forester)
library(DALEX)
dt <- read.csv("/home/wane/Desktop/EP/sci/cph/XML/TLE234group_2019.csv")
dt <- na.omit(dt)
dt <- dt[c(7:24)]
dt$oneyr <- factor(dt$oneyr)

# 3、数据分析
check_data(dt,"oneyr")

# 4、构建自动机器学习模型
# forester包支持的树模型算法: ranger，xgboost，lightgbm，decision_tree 和 catboost。
# 设定模型（一个或多个）
engine = c( 'ranger',
            'xgboost',
            'lightgbm',
            'decision_tree',
            'catboost')
# 并行计算
doParallel::registerDoParallel() 

# 构建自动机器学习模型
mod1 <- train(data = dt,        # 数据集
              y = 'oneyr',        #  因变量
              bayes_iter = 3,   # 贝叶斯优化迭代次数
              engine=engine,    # 设定的模型
              verbose = FALSE,  # 是否显示过程信息
              random_evals = 3, # 随机搜索迭代次数        
              advanced_preprocessing = TRUE # 数据预测处理
)

# 5、模型评估
# 5.1  模型预测
#预测
mod1$predictions_best[1]

# 5.2 绘制ROC曲线
draw_roc_plot(mod1$best_models[[1]],
              mod1$test_data,
              mod1$test_observed)

# 5.3 变量重要性
draw_feature_importance(
  mod1$best_models[[1]],
  test_data = mod1$test_data,
  mod1$y)

# 6、输出报告
report(mod1,
       output_file = "html_document")

