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

# PCA
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



