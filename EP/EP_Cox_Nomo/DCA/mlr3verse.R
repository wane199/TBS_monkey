# https://zhuanlan.zhihu.com/p/441806989
library(mlr3verse)

dt0 <- read.csv("C:\\Users\\wane199\\Desktop\\EP\\REFER\\BLS\\KAI\\process_rad_lat_7.csv")
dat <- dt0[c(-1:-3)]
dat$Y <- factor(dat$Y)

train <- subset(dt0, dt0$Group == "Training")
train <- train[-1]
test <- subset(dt0, dt0$Group == "Test")
# 1. 创建任务
task <- as_task_classif(dat, target = "Y")
task

# 2. 划分训练集测试集
set.seed(123)
split <- partition(task, ratio = 0.7)

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
single_pred = po("subsample", frac =0.7) %>>%
  po("learner", lrn("classif.rpart")) 
# pipeline_greplicate将上述操作复制10次。pipeline_greplicate允许通过创建一个包含输入图的n个副本的图来并行化一个操作的许多副本。也可以用语法糖函数plpl()来创建
pred_set = ppl("greplicate", single_pred, 10L)

# 10条管道汇总，形成一个单一的模型
bagging = pred_set %>>%
  po("classifavg", innum =10)

bagging$plot(html = FALSE)

baglrn=as_learner(bagging)
baglrn$train(task, split$train)
bagpred <- baglrn$predict(task, split$test) # 在测试集上做预测并评估性能
bagpred

# 性能评估
bagpred$confusion # 混淆矩阵
bagpred$score(msr("classif.acc")) # 预测准确率
#绘制ROC曲线,需要precrec包
autoplot(bagpred,type="roc")

#############
# mlr3verse基准测试：对比多个算法的平均性能
lrns()
learners = lrns(c("classif.earth", "classif.fnn", "classif.gam", "classif.featureless", "classif.rpart", "classif.kknn", "classif.ranger", "classif.svm"), 
                  predict_type = "prob")  # 计算AUC值，预测类型需要改为"prob"

design = benchmark_grid(task, learners, rsmp("cv", folds = 5))
bmr = benchmark(design)            # 执行基准测试 
measures = list(msr("classif.acc"), msr("classif.auc"))    
bmr$aggregate(measures)            # 汇总基准测试结果

autoplot(bmr)
autoplot(bmr,type="roc") # ROC曲线
autoplot(bmr, type = c("prc")) # PR曲线
autoplot(bmr, measure = msr("classif.auc"))     # #AUC箱线图可视化

# 选择随机森林分类学习器,需要ranger包
learner <- lrn("classif.ranger",
  num.trees = 100,
  predict_type = "prob"
)
learner
