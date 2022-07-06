# Deep Neural Networks with TensorFlow & Keras in R
# https://www.bilibili.com/video/BV13E411r7wE?spm_id_from=333.337.search-card.all.click&vd_source=23f183f0c5968777e138f31842bde0a0
# Libraries
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

# Data
data <- read.csv("/home/wane/Desktop/TBS/Monkey/VoxelNumbers_InMachin_atlas_whole.csv")
data <- data[, 10:15]
any(is.na(data))
str(data)

# several factor variables convert to numeric
data %<>% mutate_if(is.factor, as.numeric)

# Neural Network Visualization
n <- neuralnet(f,
               data=train_,
               hidden=3,
               linear.output=TRUE
               )
plot(n)

# Matrix
data <- as.matrix(data)
dimnames(data) <- NULL

# Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1, 2:6]
test <- data[ind==2, 2:6]
trainingtarget <- data[ind==1, 1]
testtarget <- data[ind==2, 1]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd) s
training <- scale(training, center = m, scale =s)
test <- scale(test, center = m, scale =s)

# Create Model
model <- keras_model_sequential()
model %>% 
        layer_dense(units = 5, activation = 'relu', input_shape = c(5)) %>%
        layer_dense(units = 1)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
          fit(training,
              trainingtarget,
              epochs = 100,
              batch_size =32,
              validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)

# Fine-tune Model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = c(5)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dense(units = 1)
summary(model)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size =32,
      validation_split = 0.2)

# Evaluate
model %>% evaluate(test, testtarget)
pred <- model %>% predict(test)
mean((testtarget-pred)^2)
plot(testtarget, pred)

# More Changes...
# layer_dropout(rate = 0.4) %>%
# optimizer = optimizer_rmsprop(lr = 0.001)

