# Binary image classification using Keras in R: Using CT scans to predict patients with Covid
# https://oliviergimenez.github.io/blog/image-classif/
library(tidyverse)
theme_set(theme_light())
library(keras)

# Load libraries
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
# Read in and process data
process_pix <- function(lsf) {
  img <- lapply(lsf, image_load, grayscale = TRUE) # grayscale the image
  arr <- lapply(img, image_to_array) # turns it into an array
  arr_resized <- lapply(arr, image_array_resize, 
                        height = 100, 
                        width = 100) # resize
  arr_normalized <- normalize(arr_resized, axis = 1) #normalize to make small numbers 
  return(arr_normalized)
}

# visualise these scans
scancovid <- reshape2::melt(covid[10,,])
plotcovid <- scancovid %>%
  ggplot() +
  aes(x = Var1, y = Var2, fill = value) + 
  geom_raster() +
  labs(x = NULL, y = NULL, title = "CT scan of a patient with covid") + 
  scale_fill_viridis_c() + 
  theme(legend.position = "none")

scanncovid <- reshape2::melt(ncovid[10,,])
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
df <- rbind(cbind(covid_reshaped, 1), # 1 = covid
            cbind(ncovid_reshaped, 0)) # 0 = no covid
set.seed(1234)
shuffle <- sample(nrow(df), replace = F)
df <- df[shuffle, ]

# Convolutional neural network (CNN)
# build our training and testing datasets using a 80/20 split.
set.seed(2022)
split <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
train <- df[split == 1,]
test <- df[split == 2,]
train_target <- df[split == 1, 10001] # label in training dataset
test_target <- df[split == 2, 10001] # label in testing dataset




