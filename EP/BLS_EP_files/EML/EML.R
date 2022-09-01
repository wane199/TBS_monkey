# [mlr3book.pdf](https://mlr3book.mlr-org.com/interpretation.html)
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



