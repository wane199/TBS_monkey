# file:///Users/mac/Downloads/mlr3book.pdf
# Model Interpretation
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





