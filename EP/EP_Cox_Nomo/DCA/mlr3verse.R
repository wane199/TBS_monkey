# https://zhuanlan.zhihu.com/p/441806989
library(mlr3verse)

dt0 <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\REFER\\BLS\\KAI\\process_rad_lat_7.csv")
dat <- dt0[c(-1:-3)]
dat$Y <- factor(dat$Y)

train <- subset(dt0, dt0$Group == "Training")
train <- train[-1]
test <- subset(dt0, dt0$Group == "Test")

task$select(cols = setdiff(task$feature_names, "telephone"))
# 1. 创建任务
task <- as_task_classif(dat, target = "Y")
task

# 2. 划分训练集测试集
set.seed(123)
split <- partition(task, ratio = 0.7) # 默认stratify=TRUE,按目标变量分层

# 3. 管道操作器原理
po()
po_pca <- po("pca")

train <- task$clone()$filter(split$train) # 训练任务(集)
test <- task$clone()$filter(split$test) # 测试任务(集)

trained <- po_pca$train(list(train))
trained$output$data()

po_pca$state
pred <- po_pca$predict(list(test))
pred$output$data() # 部分

# 4. 构建管道
# 构建管道就是取出相应的PipeOps，用%>>%操作符连接成管道。
linear_pipeline <- po("scale") %>>%
  po("pca") %>>%
  po("learner", learner = lrn("classif.rpart"))
linear_pipeline

linear_pipeline$plot(html = FALSE)

# 5. 图学习器
# 再将该“图”（必须包含至少1个学习器的图）转化为图学习器：
glrn <- as_learner(linear_pipeline)
glrn

# 6. 在训练集上训练图学习器
glrn$train(task, row_ids = split$train)
glrn$model$classif.rpart # 查看训练好的决策树模型

glrn$model$scale
glrn$model$pca
glrn$model # 一次输出全部模型

# 7. 在测试集上做预测并评估性能
predictions <- glrn$predict(task, row_ids = split$test)
predictions
# 性能评估
predictions$confusion # 混淆矩阵
predictions$score(msr("classif.acc")) # 预测准确率

###########
# 搭建装袋森林(Bagging)
# 创建一个简单管道，在训练PipeOpLearner之前，使用PipeOpSubsample：
single_pred <- po("subsample", frac = 0.7) %>>%
  po("learner", lrn("classif.rpart"))
# pipeline_greplicate将上述操作复制10次。pipeline_greplicate允许通过创建一个包含输入图的n个副本的图来并行化一个操作的许多副本。也可以用语法糖函数plpl()来创建
pred_set <- ppl("greplicate", single_pred, 10L)

# 10条管道汇总，形成一个单一的模型
bagging <- pred_set %>>%
  po("classifavg", innum = 10)

bagging$plot(html = FALSE)

baglrn <- as_learner(bagging)
baglrn$train(task, split$train)
bagpred <- baglrn$predict(task, split$test) # 在测试集上做预测并评估性能
bagpred

# 性能评估
bagpred$confusion # 混淆矩阵
bagpred$score(msr("classif.acc")) # 预测准确率
# 绘制ROC曲线,需要precrec包
autoplot(bagpred, type = "roc")

#############
# mlr3verse基准测试：对比多个算法的平均性能
lrns()
learners <- lrns(c("classif.earth", "classif.fnn", "classif.gam", "classif.featureless", "classif.rpart", "classif.kknn", "classif.ranger", "classif.svm"),
  predict_type = "prob"
) # 计算AUC值，预测类型需要改为"prob"

design <- benchmark_grid(task, learners, rsmp("cv", folds = 5))
bmr <- benchmark(design) # 执行基准测试
measures <- list(msr("classif.acc"), msr("classif.auc"))
bmr$aggregate(measures) # 汇总基准测试结果

autoplot(bmr)
autoplot(bmr, type = "roc") # ROC曲线
autoplot(bmr, type = c("prc")) # PR曲线
autoplot(bmr, measure = msr("classif.auc")) # #AUC箱线图可视化


################################
# [R机器学习：mlr3verse技术手册](https://gitee.com/zhjx19/rconf15)
# 选择随机森林分类学习器,需要ranger包
tasks <- as_task_classif(dat, target = "Y")
learner <- lrn("classif.ranger",
  num.trees = 100,
  predict_type = "prob"
)
learner
# 在训练集上训练模型
learner$train(tasks, row_ids = split$train)
learner$model
# 在测试集上做预测
prediction <- learner$predict(tasks, row_ids = split$test)
prediction
# 性能评估
prediction$score(msr("classif.acc"))
prediction$score(msr("classif.auc"))
# 绘制ROC曲线,需要precrec包
autoplot(prediction, type = "roc")

# 重抽样
cv5 <- rsmp("cv", folds = 10)
rr <- resample(tasks, learner, cv5, store_models = TRUE)
rr$aggregate(msr("classif.acc")) # 所有重抽样的平均准确率
rr$prediction() # 所有预测合并为一个预测(宏平均)
rr$score(msr("classif.acc")) # 各个重抽样的准确率

# 基准测试（benchmark）
# tasks=tsk("sonar")#可以是多个任务
learners <- lrns(
  c(
    "classif.rpart", "classif.kknn", "classif.cforest", "classif.ctree", "classif.cv_glmnet",
    "classif.ranger", "classif.svm"
  ),
  predict_type = "prob"
)
design <- benchmark_grid(
  tasks, learners,
  rsmps("cv", folds = 5)
)

bmr <- benchmark(design) # #执行基准测试
bmr$aggregate(list(msr("classif.acc"), msr("classif.auc")))

autoplot(bmr, type = "roc")
autoplot(bmr, measure = msr("classif.auc"))

# 图学习器
# 创建特征工程
graph <- po("scale") %>>% po("pca", rank. = 2)
graph$plot()
# 调试：查看特征工程对数据做了什么
graph$train(tasks)[[1]]$data()
# 将特征工程用于新数据
graph$predict(tasks$filter(1:347))[[1]]$data()
# 用于机器学习：再接一个学习器，转化成图学习器
graph <- graph %>>% lrn("classif.rpart")
glrn <- as_learner(graph)
# 因子特征编码
poe <- po("encode", method = "one-hot")
poe$train(list(tasks))[[1]]$data()

# 缺失值插补
tasks$missings()
po <- po("imputehist")
task <- po$train(list(task = tasks))[[1]]
tasks$missings()

# 集成学习
# 装袋法（Bagging）/“有放回”抽样（Bootstrap法）
# 单分支:数据子抽样+决策树
single_path <- po("subsample") %>>% lrn("classif.rpart")
# 复制10次得到10个分支,再接类平均
graph_bag <- ppl("greplicate", single_path, n = 10) %>>% po("classifavg")
graph_bag$plot()
# 堆叠法（Stacking）
graph_stack <- gunion(list(
  po("learner_cv", lrn("regr.lm")),
  po("learner_cv", lrn("regr.svm")),
  po("nop")
)) %>>%
  po("featureunion") %>>%
  lrn("regr.ranger")
graph_stack$plot()

# 处理不均衡数据
table(tasks$truth())
# 只支持double型特征,需安装smotefamily包
pop <- po("colapply",
  applicator = as.numeric,
  affect_columns = selector_type("integer")
) %>>%
  po("encodeimpact") %>>%
  po("smote", K = 5, dup_size = 1) # 少数类增加1倍
result <- pop$train(tasks)[[1]]
table(result$truth())

# 分块训练
graph_chunks <- po("chunk", 4) %>>% ppl("greplicate", lrn("classif.rpart"), 4) %>>% po("classifavg", 4)
graph_chunks$plot()

# 嵌套重抽样

# 超参数调参
# 变换
library(tidyverse)
tibble(x=1:20,
       y=exp(seq(log(3),log(50),length.out=20)))%>%
  ggplot(aes(x,y))+
  geom_point()
# 依赖关系

prep=gunion(list(
  po("imputehist"),
  po("missind",affect_columns=
       selector_type(c("numeric","integer")))))%>>%
  po("featureunion")%>>%
  po("encode")%>>%
  po("removeconstants")

learners=list(
  knn=lrn("classif.kknn",id="kknn"),
  svm=lrn("classif.svm",id="svm",
          type="C-classification"),
  rf=lrn("classif.ranger",id="ranger"))
graph=ppl("branch",learners)
# 将预处理图和算法图连接得到整个图
graph=prep%>>%graph
graph$plot()
# 转化为图学习器，查看其超参数：
glearner=as_learner(graph)
glearner$param_set
# 嵌套重抽样超参数调参，为了加速计算，启动并行：
#future::plan("multicore")
future::plan("multisession") # #win系统不支持多核,只支持多线程(异步)
# 设置搜索空间：
search_space=ps(
  branch.selection=p_fct(c("kknn","svm","ranger")),
  kknn.k=p_int(3,50,logscale=TRUE,
               depends=branch.selection=="kknn"),
  svm.cost=p_dbl(-1,1,trafo=function(x)10^x,
                 depends=branch.selection=="svm"),
  ranger.mtry=p_int(1,8,
                    depends=branch.selection=="ranger"))
# 用tune_nested()做嵌套调参：外层4折交叉验证、内层3折交叉验证
rr=tune_nested(
  method="random_search",
  task=tasks,
  learner=glearner,
  inner_resampling=rsmp("cv",folds=3),
  outer_resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=10)
# method:调参方法，支持”grid_search”（网格搜索）、“random_search”（随机搜索）、gensa（广义模拟退火）、“nloptr”（非线性优化）。
# 查看调参结果
rr$aggregate() # 总的平均模型性能
rr$score() #外层4次迭代每次的模型性能
extract_inner_tuning_results(rr) # 内层调参结果
extract_inner_tuning_archives(rr) # 内层的调参档案
# 其它调参包：mlr3hyperband包（基于逐次减半算法的multifidelity优化）、mlr3mbo包（灵活贝叶斯优化）、miesmuschel包（混合整数进化策略）。

# 6 特征选择
# 过滤法, (1)基于重要度指标
filter=flt("auc")
as.data.table(filter$calculate(tasks))
# (2)基于学习器的变量重要度
learner=lrn("classif.ranger",importance="impurity")
filter=flt("importance",learner=learner)
filter$calculate(tasks)
as.data.table(filter)
# # filter.frac 应该做调参, 还有其它可选 filter.nfeat, filter.cutoff, filter.permuted
graph=po("filter",filter=flt("auc"),
         filter.frac=0.5)%>>%
  po("learner",lrn("classif.rpart"))
learner=as_learner(graph)
rr=resample(tasks,learner,rsmp("cv",folds=5))
graph$plot()

# 包装法
# 6.2.1独立特征选择
task$set_row_roles(split$test,"holdout")
learner=lrn("classif.rpart")
instance=fselect(
  method="rfe",#递归特征消除法
  task=tasks,
  learner=learner,
  resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=10,
  store_models=TRUE)
# 查看特征选择结果：
instance$result #最佳特征子集
instance$archive#特征选择档案

# (4)对任务选择特征子集，拟合最终模型
tasks$select(instance$result_feature_set)
learner$train(tasks)

# 6.2.2自动特征选择
tasks
learner=lrn("classif.rpart")

afs=auto_fselector(
  method="random_search",
  learner=lrn("classif.rpart"),
  resampling=rsmp("cv",folds=4),
  measure=msr("classif.ce"),
  term_evals=10,
  batch_size=5)
rr=resample(tasks,afs,rsmp("cv",folds=5),store_models=TRUE)

rr$aggregate()#总的平均模型性能,也可以提供其它度量
rr$score()#外层5次特征选择的结果
extract_inner_fselect_results(rr)#内层特征选择的结果
extract_inner_fselect_archives(rr)#内层特征选择档案

# 嵌套重抽样特征选择实例：
rr=fselect_nested(
  method="random_search",
  task=tasks,
  learner=lrn("classif.rpart"),
  inner_resampling=rsmp("cv",folds=4),
  outer_resampling=rsmp("cv",folds=5),
  measure=msr("classif.ce"),
  term_evals=10,
  batch_size=5)
rr$aggregate() #总的平均模型性能,也可提供其它度量
# 查看具体结果
rr$score() #外层5次特征选择的结果
extract_inner_fselect_results(rr) #内层特征选择的结果
extract_inner_fselect_archives(rr) #内层特征选择档案

#  7模型解释
# dat=tsk("penguins")$data()|>na.omit()
# task=as_task_classif(dat,target="species")
learner=lrn("classif.glmboost",predict_type="prob") # classif.ranger
learner$train(tasks)
learner$model

library(iml)
# 7.1.1特征效应(Feature Eﬀects)
# 计算所有给定特征对模型预测的影响。实现了不同的方法：累积局部效应（ALE）图，部分依赖图（PDP）和个体条件期望（ICE）曲线。
mod=Predictor$new(learner,data=dat,y="Y")
effect=FeatureEffects$new(mod)
var = paste0(unlist(colnames(dat[-1])))
effect$plot(features=var)
# 7.1.2夏普利值
# 计算具有夏普利（Shapley）值的单个观测的特征贡献（一种来自合作博弈理论的方法），即单个数据点的特征值是如何影响其预测的。
mod=Predictor$new(learner,data=dat,y="Y")
shapley=Shapley$new(mod,x.interest=dat[1,])
plot(shapley)
# 7.1.3特征重要度
# 根据特征重排后模型的预测误差的增量，计算特征的重要度。
mod=Predictor$new(learner,data=dat,y="Y")
imp=FeatureImp$new(mod,loss="ce")
imp$plot(features=var)

# 7.2 DALEX包
library(DALEX)
library(DALEXtra)
dat1=dat%>%
  select(c(var))
# 创建任务，选择包含250棵决策树的随机森林学习器，训练模型
ranger=lrn("classif.ranger", num.trees=250, predict_type="prob")
ranger$train(tasks)
ranger$model

# 在开始解释模型行为之前，先创建一个解释器：
ranger_exp=explain_mlr3(ranger,data=dat,y=as.numeric(as.character(dat$Y)),
                                                     label="RangerRF",colorize=FALSE)
# 7.2.1特征层面的解释
# model_parts()函数基于排列组合的重要度来计算变量的重要度：
dat_vi=model_parts(ranger_exp)
head(dat_vi)
plot(dat_vi,max_vars = 12, show_boxplots = FALSE)

dat_pd=model_profile(ranger_exp,variables=var)$agr_profiles
dat_pd
plot(dat_pd)+
  scale_y_continuous("TLE Prediction Probability(%)",
                     labels=scales::dollar_format(suffix="",prefix=""))+
  ggtitle("")
# 7.2.2观测层面的解释
# 探索模型在单个观测/sample身上的表现
sub1=dat[1,]
sub1_bd=predict_parts(ranger_exp,new_observation=sub1)
head(sub1_bd)
plot(sub1_bd)
# SHapleyAdditiveexPlanations（SHAP）
sub1_shap=predict_parts(ranger_exp,new_observation=sub1,
                           type="shap")
plot(sub1_shap)+
  scale_y_continuous("TLE Prediction Probability(%)",
                     labels=scales::dollar_format(suffix="",prefix=""))
# 特征效应有部分依赖关系图，对于观测也有相应的版本：CeterisParibus。它显示了当只改变一个变量而其他变量保持不变时，模型对观测的反应（蓝点代表原始值）：
sub1_cp=predict_profile(ranger_exp,sub1,variables=var)
plot(sub1_cp,variables=var)+scale_y_continuous("sub001 TLE Prediction Probability(%)",
                                                   labels=scales::dollar_format(suffix="",prefix=""))


# Interpretable Machine Learning: A Guide for Making Black Box Models Explainable.
