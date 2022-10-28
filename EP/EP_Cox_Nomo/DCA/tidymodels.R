# https://mp.weixin.qq.com/s?__biz=MzUzOTQzNzU0NA==&mid=2247492016&idx=1&sn=6b3ae709bb7d5168ffd122b70bac57d2&chksm=facad137cdbd5821564525708a481747548e5c1dde78f34b493b17333033d633a4a321523a73&mpshare=1&scene=1&srcid=1028R1yw4ekz8tkKX8vjrlef&sharer_sharetime=1666921135962&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
rm(list = ls())
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidymodels))
library(kknn)
tidymodels_prefer()

set.seed(123)
dt0 <- read.csv("C:/Users/wane199/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS2.csv")
# dt1 <- read_excel("/home/wane/Desktop/EP/Structured_Data/Task2/TLE234group.xlsx")
dt0 <- dt0[c(-1, -2)]
dt <- dt0[c(-1, -3)] # 获取数据
dt$oneyr # 查看阳性结局
# 2.2 Clean data To get a first impression of the data we take a look at the top 4 rows:
library(gt)
dt %>% 
  slice_head(n = 4) %>% 
  gt() # print output using gt
glimpse(dt)
library(visdat)
vis_dat(dt)

dt %>% 
  count(oneyr,
        sort = TRUE)

# convert to numeric
dt <- 
  dt %>% 
  mutate(
    housing_median_age = as.numeric(housing_median_age),
    median_house_value = as.numeric(median_house_value)
  )

# convert all remaining character variables to factors 
dt <- 
  dt %>% 
  mutate(across(where(is.character), as.factor))

set.seed(123)
# 数据划分，根据oneyr分层
split_ep <- initial_split(dt, 0.70, strata = oneyr)
train <- training(split_ep)
test <- testing(split_ep)

train <- subset(dt0, dt0$Group == "Training")
test <- subset(dt0, dt0$Group == "Test")
normal_para <- preProcess(x = train[, 3:16], method = c("center", "scale")) # 提取训练集的标准化参数
train_normal <- predict(object = normal_para, newdata = train[, 3:16])
test_normal <- predict(object = normal_para, newdata = test[, 3:16])
library(dplyr)
train <- mutate(train[, 2:2], train_normal)
train <- cbind(train[, 2], train_normal)
test <- mutate(test[, 2], test_normal)
test <- cbind(test[, 2], test_normal)
# 重命名
colnames(train)[1] <- "oneyr"
colnames(test)[1] <- "oneyr"

# 数据预处理
ep_rec <- recipe(oneyr ~ ., data = train) %>%
  step_rm(ID) %>% # 移除这3列
  step_string2factor(oneyr) %>%  # 变为因子类型
  #update_role(yards_gained, game_id, new_role = "ID") %>% 
  # 去掉高度相关的变量
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # 中心化
  step_zv(all_predictors())  # 去掉零方差变量
# 选择模型
# 直接选择4个模型，你想选几个都是可以的。
lm_mod <- logistic_reg(mode = "classification", engine = "glm")
knn_mod <- nearest_neighbor(mode = "classification", engine = "kknn")
rf_mod <- rand_forest(mode = "classification", engine = "ranger")
tree_mod <- decision_tree(mode = "classification", engine = "rpart")

# 选择重抽样方法
set.seed(123)
folds <- vfold_cv(train, v = 10)
folds

# 构建workflow
# 这一步就是不用重复写代码的关键，把所有模型和数据预处理步骤自动连接起来。
library(workflowsets)
four_mods <- workflow_set(list(rec = ep_rec),
  list(
    lm = lm_mod,
    knn = knn_mod,
    rf = rf_mod,
    tree = tree_mod
  ),
  cross = T
)
four_mods

# 运行模型
# 首先是一些运行过程中的参数设置：
keep_pred <- control_resamples(save_pred = T, verbose = T)
# 然后就是运行4个模型（目前一直是在训练集中），我们给它加速一下：
library(doParallel)
cl <- makePSOCKcluster(6) # 加速，用12个线程
registerDoParallel(cl)
four_fits <- four_mods %>%
  workflow_map("fit_resamples",
    seed = 123,
    verbose = T,
    resamples = folds,
    control = keep_pred
  )
four_fits

# 查看结果
# 查看模型在训练集中的表现：
collect_metrics(four_fits)
# 查看每一个预测结果，这个就不运行了，毕竟好几万行，太多了。。。
collect_predictions(four_fits)

# 可视化结果
# 直接可视化4个模型的结果，感觉比ROC曲线更好看，还给出了可信区间。这个图可以自己用ggplot2语法修改。
four_fits %>% autoplot(metric = "roc_auc") + theme_bw()



# 选择随机森林，建立workflow：
rf_spec <- rand_forest(mode = "classification") %>% 
  set_engine("ranger",importance = "permutation")
rf_wflow <- workflow() %>% 
  add_recipe(ep_rec) %>% 
  add_model(rf_spec)

# 在训练集建模：
fit_rf <- rf_wflow %>% 
  fit(train)
