# Binary image classification using Keras in R: Using CT scans to predict patients with Covid
# https://oliviergimenez.github.io/blog/image-classif/
library(tidyverse)
theme_set(theme_light())
library(keras)

# Load libraries/Packages
# library(keras)
# library(lime)
# library(tidyquant)
# library(rsample)
# library(recipes)
# library(yardstick)
# library(corrr)

# Read in and process data
process_pix <- function(lsf) {
  img <- lapply(lsf, image_load, color_mode = "grayscale") # grayscale the image
  arr <- lapply(img, image_to_array) # turns it into an array
  arr_resized <- lapply(arr, image_array_resize,
    height = 100,
    width = 100
  ) # resize
  arr_normalized <- normalize(arr_resized, axis = 1) # normalize to make small numbers
  return(arr_normalized)
}

# with covid
lsf <- list.files("/home/wane/Downloads/COVID_CT/COVID/", full.names = TRUE)
covid <- process_pix(lsf)
covid <- covid[, , , 1] # get rid of last dim
covid_reshaped <- array_reshape(covid, c(nrow(covid), 100 * 100))
# without covid
lsf <- list.files("/home/wane/Downloads/COVID_CT/non-COVID", full.names = TRUE)
ncovid <- process_pix(lsf)
ncovid <- ncovid[, , , 1] # get rid of last dim
ncovid_reshaped <- array_reshape(ncovid, c(nrow(ncovid), 100 * 100))

# visualise these scans
scancovid <- reshape2::melt(covid[10, , ])
plotcovid <- scancovid %>%
  ggplot() +
  aes(x = Var1, y = Var2, fill = value) +
  geom_raster() +
  labs(x = NULL, y = NULL, title = "CT scan of a patient with covid") +
  scale_fill_viridis_c() +
  theme(legend.position = "none")

scanncovid <- reshape2::melt(ncovid[10, , ])
plotncovid <- scanncovid %>%
  ggplot() +
  aes(x = Var1, y = Var2, fill = value) +
  geom_raster() +
  labs(x = NULL, y = NULL, title = "CT scan of a patient without covid") +
  scale_fill_viridis_c() +
  theme(legend.position = "none")

library(patchwork)
plotcovid + plotncovid

# Put altogether and shuffle.
df <- rbind(
  cbind(covid_reshaped, 1), # 1 = covid
  cbind(ncovid_reshaped, 0)
) # 0 = no covid
set.seed(1234)
shuffle <- sample(nrow(df), replace = F)
df <- df[shuffle, ]

# Convolutional neural network (CNN)
# build our training and testing datasets using a 80/20 split.
set.seed(2022)
split <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
train <- df[split == 1, ]
test <- df[split == 2, ]
train_target <- df[split == 1, 10001] # label in training dataset
test_target <- df[split == 2, 10001] # label in testing dataset

# build our model
model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 2, activation = "softmax")

# Compile the model with defaults specific to binary classification.
model %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )

# use one-hot encoding (to_categorical() function) aka dummy coding in statistics.
train_label <- to_categorical(train_target)
test_label <- to_categorical(test_target)

# fit our model to the training dataset.
fit_covid <- model %>%
  fit(
    x = train,
    y = train_label,
    epochs = 25,
    batch_size = 512, # try also 128 and 256
    verbose = 2,
    validation_split = 0.2
  )

# quick visualization of the performances
plot(fit_covid)

# the performances on the testing dataset?
model %>%
  evaluate(test, test_label)
pred <- model %>% predict(test)
mean((test_label - pred)^2)
plot(test_label, pred)

#  predictions on the testing dataset, and compare with ground truth.
predictedclasses <- model %>%
  predict(test) %>%
  `>`(0.5) %>%
  k_cast("int32")

prob <- round(predictedclasses)
# Confusion matrix
confusion_matrix <- table(prob, test_label)
confusion_matrix

# save our model for further use.
save_model_tf(model, "model/covidmodel") # save the model


##################### ----------------------
# Image Recognition & Classification with Keras in R.
# Load Packages
library(EBImage)
library(keras)

# Read images
setwd("/home/wane/Downloads/COVID_CT/TEST/")
pics <- c(
  "Non-Covid (1).png", "Non-Covid (2).png", "Non-Covid (3).png", "Non-Covid (4).png", "Non-Covid (5).png",
  "Non-Covid (101).png", "Non-Covid (102).png", "Non-Covid (103).png", "Non-Covid (104).png", "Non-Covid (105).png"
)

mypic <- list()
for  (i in 1:10) {
  mypic[[i]] <- readImage(pics[i])
}

# Expolre
print(mypic[[1]])
display(mypic[[3]])
summary(mypic[[6]])
hist(mypic[[6]])
str(mypic)

# Resize
for  (i in 1:10) {
  mypic[[i]] <- resize(mypic[[i]], 28, 28)
}

# Reshape
for  (i in 1:10) {
  mypic[[i]] <- array_reshape(mypic[[i]], c(28, 28, 4))
}
str(mypic)

# Row Bind
trainx <- NULL
for (i in 1:6) {
  trainx <- rbind(trainx, mypic[[i]])
}
str(trainx)
testx <- rbind(mypic[[7]], mypic[[10]])
trainy <- c(0, 0, 0, 0, 1, 1)
testy <- c(1,1)

# One Hot Encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
trainLabels

# Create Model
model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = 3136) %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 2, activation = "softmax")
summary(model)

# Compile
model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )

# Fit Model
history <- model %>%
  fit(trainx,
      trainLabels,
      epochs = 30,
      batch_size = 32,
      validation_split = 0.2)
plot(history)

# Evaluation & Prediction Test Data
model |> 
  evaluate(trainx, trainLabels)

pred <- model %>% predict(trainx) %>% `>`(0.5) %>% k_cast("int32")
table(Predicted = pred, Actual = trainy)
prob <- model |> predict(trainx) 

# Confusion matrix
confusion_matrix <- table(prob, trainLabels)
confusion_matrix
cbind(prob, pred, trainy)

# Prediction & confusion matrix - test data
model %>% 
  evaluate(testx, testLabels)

prob <- model |> predict(testx) 
