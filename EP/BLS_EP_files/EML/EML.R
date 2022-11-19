# [Interpretable Machine Learning](https://christophm.github.io/interpretable-ml-book/)
# [机器学习特征选择](https://blog.csdn.net/ARPOSPF/article/details/84979032)
# https://www.youtube.com/playlist?list=PLV8yxwGOxvvovp-j6ztxhF3QcKXT6vORU
rm(list = ls()) 
# 移除冗余特征，移除高度关联的特征
set.seed(123)
library(mlbench)
library(caret)
# data(PimaIndiansDiabetes)
# Matrix <- PimaIndiansDiabetes[,1:8]
dt <- read.csv("/Volumes/UNTITLED/sci/cph/XML/TLE234group_2019.csv")
head(dt)
# dtx <- as.data.frame(scale(dt[4:17]))
# dt <- mutate(dt[, 1:3], dtx) 
table(dt$oneyr)
train <- subset(dt, dt$Group == "Training")
train <- train[c(7:23)]
test <- subset(dt, dt$Group == "Test")

library(Hmisc)
up_CorMatrix <- function(cor,p) {ut <- upper.tri(cor) 
data.frame(row = rownames(cor)[row(cor)[ut]] ,
           column = rownames(cor)[col(cor)[ut]], 
           cor =(cor)[ut] ) }

res <- rcorr(as.matrix(train))
cor_data <- up_CorMatrix (res$r)
cor_data <- subset(cor_data, cor_data$cor > 0.5)
cor_data

# 根据重要性进行特征排序
# 构建一个Learning Vector Quantization（LVQ）模型。varImp用于获取特征重要性
# ensure results are repeatable
set.seed(123)
# load the library
library(mlbench)
library(caret)
# load the dataset
# data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model(http://topepo.github.io/caret/train-models-by-tag.html)
model <- train(factor(oneyr)~., data=train, method="rf", preProcess="scale", trControl=control) # Learning Vector Quantization
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


library(mlr)
# 创建task
train.task <- makeClassifTask(data = train, target = "oneyr")
# 查看变量选择可选方法listFilterMethods()
# 选择计算方差，进行特征选择
var_imp <- generateFilterValuesData(train.task, method = "variance", nselect = 6) 
var_imp
# 对衡量特征指标进行绘图
plotFilterValues(var_imp, feat.type.cols = TRUE, n.show = 10)

# 模型自带的feature importance, 环状柱状图展示；SHAP技术验证，PCA探究SHAP分析中top变量
library(tidyverse)
data <- read.csv("/home/wane/Desktop/EP/Structured_Data/process_PT_nor_cn+epcoef_min22_bar.csv")
data <- data %>% arrange(group)
summary(data)
# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=Coef, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  # ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, .50, 1.00, 1.50, 2.00), label = c("0", "0.5", "1.0", "1.5", "2.0") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  geom_text(data=label_data, aes(x=id, y=Coef+0.1, label=Feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p

# Explaining individual machine learning predictions with Shapley values(https://cran.r-project.org/web/packages/shapr/vignettes/understanding_shapr.html)
library(xgboost)
library(shapr)
data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the combined approach
explanation_combined <- explain(
  x_test,
  approach = c("empirical", "copula", "gaussian", "gaussian"),
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_combined, plot_phi0 = FALSE, index_x_test = c(1, 6))

library(gbm)
#> Loaded gbm 2.1.5

xy_train <- data.frame(x_train,medv = y_train)

form <- as.formula(paste0(y_var,"~",paste0(x_var,collapse="+")))

# Fitting a gbm model
set.seed(825)
model <- gbm::gbm(
  form,
  data = xy_train,
  distribution = "gaussian"
)

#### Full feature versions of the three required model functions ####

predict_model.gbm <- function(x, newdata) {
  
  if (!requireNamespace('gbm', quietly = TRUE)) {
    stop('The gbm package is required for predicting train models')
  }
  
  model_type <- ifelse(
    x$distribution$name %in% c("bernoulli","adaboost"),
    "classification",
    "regression"
  )
  if (model_type == "classification") {
    
    predict(x, as.data.frame(newdata), type = "response",n.trees = x$n.trees)
  } else {
    
    predict(x, as.data.frame(newdata),n.trees = x$n.trees)
  }
}

get_model_specs.gbm <- function(x){
  feature_list = list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)
  
  feature_list$classes <- attr(x$Terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes=="factor"] <- NA # the model object doesn't contain factor levels info
  
  return(feature_list)
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(xy_train, model)
#> The columns(s) medv is not used by the model and thus removed from the data.
p0 <- mean(xy_train[,y_var])
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
# Plot results
plot(explanation)

#### Minimal version of the three required model functions ####      
# Note: Working only for this exact version of the model class 
# Avoiding to define get_model_specs skips all feature         
# consistency checking between your data and model             

# Removing the previously defined functions to simulate a fresh start
rm(predict_model.gbm)
rm(get_model_specs.gbm)

predict_model.gbm <- function(x, newdata) {
  predict(x, as.data.frame(newdata),n.trees = x$n.trees)
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(x_train, model)
#> get_model_specs is not available for your custom model. All feature consistency checking between model and data is disabled.
#> See the 'Advanced usage' section of the vignette:
#> vignette('understanding_shapr', package = 'shapr')
#> for more information.
#> The specified model provides feature labels that are NA. The labels of data are taken as the truth.
p0 <- mean(xy_train[,y_var])
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
# Plot results
plot(explanation)

# Principal Component Analysis with Biplot Analysis in R(https://medium.com/@RaharditoDP/principal-component-analysis-with-biplot-analysis-in-r-ee39d17096a1)
# create coloumn branch become row names
dataset <- read.csv('/home/wane/Desktop/EP/sci/cph/XML/TLE234group_2019.csv')
str(dataset)
datasetnew <- dataset[,c(-1:-7)]
rownames(datasetnew) <- dataset[,4]
View(datasetnew)

# show eigen value score of PCA
library(factoextra)
library(FactoMineR)

res.pca <- PCA(datasetnew,  graph = FALSE)
res.pca$eig
# show scree plot of PCA
fviz_screeplot(res.pca, addlabels = TRUE)
# show pricipal component score
res.pca$ind$coord
# show graph of two dimensional variable
fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
# show graph of biplot analysis
fviz_pca_biplot(res.pca, repel = TRUE, ggtheme = theme_minimal())

fviz_pca_var(res.pca, col.var = "steelblue")
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())
# Graph of variables(https://f0nzie.github.io/machine_learning_compilation/detailed-study-of-principal-component-analysis.html)
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
head(var$contrib)
# Correlation circle
library(corrplot)
corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = dataset$oneyr, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             palette = "jco"
)
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


fviz_cos2(res.pca, choice = "ind")
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = factor(dataset$oneyr), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

# Add confidence ellipses
fviz_pca_ind(res.pca, geom.ind = "point", col.ind = factor(dataset$oneyr), 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)

fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = factor(dataset$oneyr), # color by groups
             addEllipses = TRUE, # Concentration ellipses
             palette = "jco"
)

# Add confidence ellipses
fviz_pca_ind(res.pca, geom.ind = "point", 
             col.ind = factor(dataset$oneyr), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)
# Convex hull
fviz_pca_ind(res.pca, geom.ind = "point",
             col.ind = factor(dataset$oneyr), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "convex",
             legend.title = "Groups"
)

# umap(https://datavizpyr.com/how-to-make-umap-plot-in-r/)
# 使用umap包进行UMAP降维可视化分析
library(umap)
data.labels = dataset$oneyr
# 使用umap函数进行UMAP降维分析
data.umap = umap::umap(datasetnew)
data.umap
## umap embedding of 150 items in 2 dimensions
## object components: layout, data, knn, config

# 查看降维后的结果
head(data.umap$layout)

# 使用plot函数可视化UMAP的结果
plot(data.umap$layout,col=data.labels,pch=16,asp = 1,
     xlab = "UMAP_1",ylab = "UMAP_2",
     main = "A UMAP visualization of the TLE dataset")
# 添加分隔线
abline(h=0,v=0,lty=2,col="gray")
# 添加图例
legend("topright",title = "Species",inset = 0.01,
       legend = unique(data.labels),pch=16,
       col = unique(data.labels))

# 使用uwot包进行UMAP降维可视化分析
library(uwot)

head(iris)

# 使用umap函数进行UMAP降维分析
iris_umap <- uwot::umap(dataset)
head(iris_umap)


# 使用plot函数可视化UMAP降维的结果
plot(iris_umap,col=dataset$oneyr,pch=16,asp = 1,
     xlab = "UMAP_1",ylab = "UMAP_2",
     main = "A UMAP visualization of the iris dataset")
# 添加分隔线
abline(h=0,v=0,lty=2,col="gray")
# 添加图例
legend("topright",title = "Species",inset = 0.01,
       legend = unique(dataset$oneyr),pch=16,
       col = unique(dataset$oneyr))

# Supervised dimension reduction using the 'Species' factor column
data_sumap <- uwot::umap(dataset, n_neighbors = 15, min_dist = 0.001,
                         y = dataset$oneyr, target_weight = 0.5)
head(data_sumap)


data_sumap_res <- data.frame(data_sumap,Oneyr=dataset$oneyr)
head(data_sumap_res)


# 使用ggplot2包可视化UMAP降维的结果
library(ggplot2)

ggplot(data_sumap_res,aes(X1,X2,color=Oneyr)) + 
  geom_point() + theme_bw() + 
  geom_hline(yintercept = 0,lty=2,col="red") + 
  geom_vline(xintercept = 0,lty=2,col="blue",lwd=1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x="UMAP_1",y="UMAP_2",
       title = "A UMAP visualization of the TLE dataset")


##############################################
# [mlr3book.pdf](https://mlr3book.mlr-org.com/interpretation.html)
# Model Interpretation/or more in omnixai(python)
data("penguins", package = "palmerpenguins")
str(penguins)

library("iml")
library("mlr3")
library("mlr3learners")
set.seed(123)
penguins = na.omit(penguins)
task_peng = as_task_classif(penguins, target = "species")

learner = lrn("classif.ranger")
learner$predict_type = "prob"
learner$train(task_peng)
learner$model

x = penguins[which(names(penguins) != "species")]
model = Predictor$new(learner, data = x, y = penguins$species)

num_features = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g", "year")
effect = FeatureEffects$new(model)
plot(effect, features = num_features)

x = penguins[which(names(penguins) != "species")]
model = Predictor$new(learner, data = penguins, y = "species")
x.interest = data.frame(penguins[1, ])
shapley = Shapley$new(model, x.interest = x.interest)
plot(shapley)

# FeatureImp
effect = FeatureImp$new(model, loss = "ce")
effect$plot(features = num_features)

# Independent Test Data
train_set = sample(task_peng$nrow, 0.8 * task_peng$nrow)
test_set = setdiff(seq_len(task_peng$nrow), train_set)
learner$train(task_peng, row_ids = train_set)
prediction = learner$predict(task_peng, row_ids = test_set)

# plot on training
model = Predictor$new(learner, data = penguins[train_set, ], y = "species")
effect = FeatureImp$new(model, loss = "ce")
plot_train = plot(effect, features = num_features)
# plot on test data
model = Predictor$new(learner, data = penguins[test_set, ], y = "species")
effect = FeatureImp$new(model, loss = "ce")
plot_test = plot(effect, features = num_features)
# combine into single plot
library("patchwork")
plot_train + plot_test

model = Predictor$new(learner, data = penguins[train_set, ], y = "species")
effect = FeatureEffects$new(model)
plot(effect, features = num_features)

model = Predictor$new(learner, data = penguins[test_set, ], y = "species")
effect = FeatureEffects$new(model)
plot(effect, features = num_features)


# DALEX
library("DALEX")
fifa[1:2, c("value_eur", "age", "height_cm", "nationality", "attacking_crossing")]

dim(fifa)

fifa[, c("nationality", "overall", "potential", "wage_eur")] = NULL
for (i in 1:ncol(fifa)) fifa[, i] = as.numeric(fifa[, i])

library("mlr3")
library("mlr3learners")

fifa_task = as_task_regr(fifa, target = "value_eur")
fifa_ranger = lrn("regr.ranger")
fifa_ranger$param_set$values = list(num.trees = 250)
fifa_ranger$train(fifa_task)
fifa_ranger

library("DALEX")
library("DALEXtra")

ranger_exp = explain_mlr3(fifa_ranger,
                          data     = fifa,
                          y        = fifa$value_eur,
                          label    = "Ranger RF",
                          colorize = FALSE)

fifa_vi = model_parts(ranger_exp)
head(fifa_vi)

plot(fifa_vi, max_vars = 12, show_boxplots = FALSE)

selected_variables = c("age", "movement_reactions",
                       "skill_ball_control", "skill_dribbling")

fifa_pd = model_profile(ranger_exp,
                        variables = selected_variables)$agr_profiles
fifa_pd

library("ggplot2")
plot(fifa_pd) +
  scale_y_continuous("Estimated value in Euro", labels = scales::dollar_format(suffix = "€", prefix = "")) +
  ggtitle("Partial Dependence profiles for selected variables")


ronaldo = fifa["Cristiano Ronaldo", ]
ronaldo_bd_ranger = predict_parts(ranger_exp,
                                  new_observation = ronaldo)
head(ronaldo_bd_ranger)

plot(ronaldo_bd_ranger)

ronaldo_shap_ranger = predict_parts(ranger_exp,
                                    new_observation = ronaldo,
                                    type = "shap")

plot(ronaldo_shap_ranger) +
  scale_y_continuous("Estimated value in Euro", labels = scales::dollar_format(suffix = "€", prefix = ""))


selected_variables = c("age", "movement_reactions",
                       "skill_ball_control", "skill_dribbling")

ronaldo_cp_ranger = predict_profile(ranger_exp, ronaldo, variables = selected_variables)

plot(ronaldo_cp_ranger, variables = selected_variables) +
  scale_y_continuous("Estimated value of Christiano Ronaldo", labels = scales::dollar_format(suffix = "€", prefix = ""))



