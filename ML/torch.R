# Brain image segmentation with torch(https://blogs.rstudio.com/ai/posts/2020-11-30-torch-brain-segmentation/)
# R原生支持pytorch了，应该叫torch for R
library(torch)

# creates example tensors. x requires_grad = TRUE tells that 
# we are going to take derivatives over it.
dense <- nn_module(
  clasname = "dense",
  # the initialize function tuns whenever we instantiate the model
  initialize = function(in_features, out_features) {
    
    # just for you to see when this function is called
    cat("Calling initialize!") 
    
    # we use nn_parameter to indicate that those tensors are special
    # and should be treated as parameters by `nn_module`.
    self$w <- nn_parameter(torch_randn(in_features, out_features))
    self$b <- nn_parameter(torch_zeros(out_features))
    
  },
  # this function is called whenever we call our model on input.
  forward = function(x) {
    cat("Calling forward!")
    torch_mm(x, self$w) + self$b
  }
)

model <- dense(3, 1)

# you can get all parameters 
model$parameters

# create an input tensor
x <- torch_randn(10, 3)
y_pred <- model(x)
y_pred



########################################
# deep learning (incl. dependencies)
library(torch)
library(torchvision)

# data wrangling
library(tidyverse)
library(zeallot)

# image processing and visualization
library(magick)
library(cowplot)

# dataset loading 
library(pins)
library(zip)

torch_manual_seed(777)
set.seed(777)

# use your own kaggle.json here
pins::board_register_kaggle(token = "~/kaggle.json")

files <- pins::pin_get("mateuszbuda/lgg-mri-segmentation", board = "kaggle",  extract = FALSE)

train_dir <- "data/mri_train"
valid_dir <- "data/mri_valid"

if(dir.exists(train_dir)) unlink(train_dir, recursive = TRUE, force = TRUE)
if(dir.exists(valid_dir)) unlink(valid_dir, recursive = TRUE, force = TRUE)

zip::unzip(files, exdir = "data")

file.rename("data/kaggle_3m", train_dir)

# this is a duplicate, again containing kaggle_3m (evidently a packaging error on Kaggle)
# we just remove it
unlink("data/lgg-mri-segmentation", recursive = TRUE)

dir.create(valid_dir)








