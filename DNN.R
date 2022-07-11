# Deep Neural Networks with TensorFlow & Keras in R
# https://www.bilibili.com/video/BV13E411r7wE?spm_id_from=333.337.search-card.all.click&vd_source=23f183f0c5968777e138f31842bde0a0
# Libraries
rm(list = ls())
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

# Data
data <- read.csv("/home/wane/Desktop/TBS/Monkey/VoxelNumbers.csv")
data <- data[, 10:15]
data <- data[-1]
any(is.na(data))
str(data)

# several factor variables convert to numeric
data %<>% mutate_if(is.factor, as.numeric)

# Neural Network Visualization
N <- names(data)
f <- as.formula(paste("TBVBW ~", paste(N[!N %in% "TBVBW"], collapse = " + ")))
n <- neuralnet(f,
  data = data,
  hidden = c(8, 4),
  linear.output = F,
  lifesign = 'full',
  rep = 1
)
plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

# Matrix
data <- as.matrix(data)
dimnames(data) <- NULL

# Min-Max Normalization
data$TBV.BW <- (data$TBV.BW - min(data$TBV.BW)) / (max(data$TBV.BW) - min(data$TBV.BW))

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind == 1, 1:2]
test <- data[ind == 2, 1:2]
trainingtarget <- data[ind == 1, 3]
testtarget <- data[ind == 2, 3]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

# Create Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = "relu", input_shape = c(2)) %>%
  layer_dense(units = 1)
summary(model)

# Compile
model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = "mae"
)

# Fit Model
mymodel <- model %>%
  fit(training,
    trainingtarget,
    epochs = 160,
    batch_size = 32,
    validation_split = 0.2
  )

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget - pred)^2)
plot(testtarget, pred)

# Fine-tune Model
model1 <- keras_model_sequential()
model1 %>%
  layer_dense(units = 20, activation = "relu", input_shape = c(5)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 10, activation = "relu", input_shape = c(5)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)
summary(model1)

# Compile
model1 %>% compile(
  loss = "mse",
  # optimizer = "rmsprop",
  optimizer = optimizer_rmsprop(learning_rate = 0.001),
  metrics = "mae"
)

# Fit Model
mymodel1 <- model1 %>%
  fit(training,
    trainingtarget,
    epochs = 200,
    batch_size = 32,
    validation_split = 0.2
  )

# Evaluate
model1 %>% evaluate(test, testtarget)
pred <- model1 %>% predict(test)
mean((testtarget - pred)^2)
plot(testtarget, pred)

# More Changes...
# layer_dropout(rate = 0.4) %>%
# optimizer = optimizer_rmsprop(lr = 0.002)


#################################################
# Binary classification
library(keras)
# install_keras()

# Read data
data <- read.csv(file.choose(), header = T)
str(data)
data <- data[, 5:18]

# Change to Matrix
data <- as.matrix(data)
dimnames(data) <- NULL

# Normalize
data[, 2:14] <- normalize(data[, 2:14])
data[, 1] <- as.numeric(data[, 1])
summary(data)
# Min-Max Normalization
data$TBV.BW <- (data$TBV.BW - min(data$TBV.BW)) / (max(data$TBV.BW) - min(data$TBV.BW))

# Data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind == 1, 2:14]
test <- data[ind == 2, 2:14]
trainingtarget <- data[ind == 1, 1]
testtarget <- data[ind == 2, 1]

# One Hot Encoding
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
print(testLabels)

# Create sequential model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(13)) %>%
  layer_dense(units = 2, activation = "sigmoid")
# activation: 'sigmoid' for multi-class; 'sigmoid' for binary classification
summary(model)

# Compile
model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

# Fit Model
history <- model %>%
  fit(training,
    trainLabels,
    epochs = 200,
    batch_size = 32,
    validation_split = 0.2
  )
plot(history)

# Evaluate model with test data
# check accuracy of model
model |>
  evaluate(test, testLabels)
# Prediction & confusion matrix - test data
prob <- model %>%
  predict(test)
prob <- round(prob)
# Confusion matrix
confusion_matrix <- table(prob, testLabels)
confusion_matrix
pred <- model %>%
  predict(test) %>%
  `>`(0.5) %>%
  k_cast("int32")
pred <- round(pred)
cbind(prob, pred, testtarget)

mean((testtarget - pred)^2)
plot(testtarget, pred)

# Fine-tune Model
model1 <- keras_model_sequential()
model1 %>%
  layer_dense(units = 50, activation = "relu", input_shape = c(13)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 2, activation = "sigmoid")
summary(model1)

# Compile
model1 %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

# Fit Model
history1 <- model1 %>%
  fit(training,
    trainLabels,
    epochs = 200,
    batch_size = 32,
    validation_split = 0.2
  )
plot(history1)
