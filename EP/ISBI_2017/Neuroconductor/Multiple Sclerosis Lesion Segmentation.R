# https://neuroconductor.org/tutorials/ms_lesion
# Multiple Sclerosis Lesion Segmentation
## ----setup, include=FALSE-----------------------------------------------------
# install.packages("devtools")
devtools::install_github("muschellij2/extrantsr")
library(methods)
library(oasis)
library(extrantsr)
library(neurobase)
library(dplyr)
library(git2r)
knitr::opts_chunk$set(echo = TRUE, cache = FALSE, comment = "")


## ----ver_check----------------------------------------------------------------
library(dplyr)
loaded_package_version = function(pkg) {
  packs = devtools::session_info()$packages
  ver = packs %>% 
    filter(package %in% pkg) %>% 
    select(package, loadedversion)
  return(ver)
}
check_package_version = function(pkg, min_version){
  stopifnot(length(pkg) == 1)
  ver = loaded_package_version(pkg = pkg)
  ver = as.character(ver$loadedversion)
  min_version = as.character(min_version)
  # check to see if version is at least the min_version
  utils::compareVersion(a = ver, b = min_version) >= 0
}
check = check_package_version("oasis", min_version = "2.2")
if (!check) {
  source("https://neuroconductor.org/neurocLite.R")
  neuroc_install("oasis")  
}


## ----get_data-----------------------------------------------------------------
library(git2r)
if (!dir.exists("data")) {
  repo = clone(url = "https://github.com/muschellij2/fslr_data",
               local_path = "data/")
  unlink(file.path("data/.git"), recursive = TRUE)
  file.remove(file.path("data", "SS_Image.nii.gz"))
  file.remove(file.path("data", "Brain_Mask.nii.gz"))
}


## ----filenames, cache = FALSE-------------------------------------------------
df = list.files(path = "data", 
                pattern = "[.]nii[.]gz$", 
                full.names = TRUE)
df = data.frame(file = df, stringsAsFactors = FALSE)
print(head(df))


## ----filenames2, cache = FALSE------------------------------------------------
df$fname = nii.stub(df$file, bn = TRUE)
df$id = gsub("^(\\d\\d)-.*", "\\1", df$fname)
df$timepoint = gsub("^\\d\\d-(.*)_.*$", "\\1", df$fname)
df$modality = gsub("\\d\\d-.*_(.*)$", "\\1", df$fname)
print(unique(df$id))
print(unique(df$modality))
print(head(df))


## ----split_data---------------------------------------------------------------
ss = split(df, df$timepoint)
ss = lapply(ss, function(x){
  mods = x$modality
  xx = x$file
  names(xx) = mods
  return(xx)
})


## ----oasis_stuff, cache = FALSE, eval = TRUE----------------------------------
dat = ss[[1]]
print(dat)
# preparing output filenames
outfiles = nii.stub(dat)
brain_mask = gsub("_T1$", "", outfiles["T1"])
brain_mask = paste0(brain_mask, "_Brain_Mask.nii.gz")
outfiles = paste0(outfiles, "_preprocessed.nii.gz")
names(outfiles) = names(dat)
outfiles = c(outfiles, brain_mask = brain_mask)
outfiles = outfiles[ names(outfiles) != "mask"]

if (!all(file.exists(outfiles))) {
  pre = oasis_preproc(
    flair = dat["FLAIR"], 
    t1 = dat["T1"],
    t2 = dat["T2"],
    pd = dat["PD"],
    cores = 1)
  
  writenii(pre$t1, filename = outfiles["T1"])
  writenii(pre$t2, filename = outfiles["T2"])
  writenii(pre$flair, filename = outfiles["FLAIR"])
  writenii(pre$pd, filename = outfiles["PD"])
  writenii(pre$brain_mask, filename  = outfiles["brain_mask"])
}


## ----read_imgs, cache=FALSE---------------------------------------------------
imgs = lapply(outfiles[c("T1", "T2", "FLAIR", "PD")], readnii)
brain_mask = readnii(outfiles["brain_mask"])
imgs = lapply(imgs, robust_window)
norm_imgs = lapply(imgs, zscore_img, margin = NULL, mask = brain_mask)


## ----drop_dims----------------------------------------------------------------
dd = dropEmptyImageDimensions(brain_mask, other.imgs = norm_imgs)
red_mask = dd$outimg
norm_imgs = dd$other.imgs
norm_imgs = lapply(norm_imgs, mask_img, mask = red_mask)


## ----overlay_plots------------------------------------------------------------
z = floor(nsli(norm_imgs[[1]])/2)
multi_overlay(
  norm_imgs, 
  z = z, 
  text = names(norm_imgs),
  text.x = 
    rep(0.5, length(norm_imgs)),
  text.y = 
    rep(1.4, length(norm_imgs)), 
  text.cex = 
    rep(2.5, length(norm_imgs)))


## ----oasis_df, cache = FALSE, eval = TRUE-------------------------------------
df_list = oasis_train_dataframe(
  flair = outfiles["FLAIR"],
  t1 = outfiles["T1"],
  t2 = outfiles["T2"],
  pd = outfiles["PD"],
  preproc = FALSE,
  brain_mask = outfiles["brain_mask"],
  eroder = "oasis")

oasis_dataframe = df_list$oasis_dataframe
brain_mask = df_list$brain_mask
top_voxels = df_list$voxel_selection


## ----pred, cache = FALSE------------------------------------------------------
## make the model predictions
predictions = predict( oasis::oasis_model,
                       newdata = oasis_dataframe,
                       type = 'response')
pred_img = niftiarr(brain_mask, 0)
pred_img[top_voxels == 1] = predictions
library(fslr)
##smooth the probability map
prob_map = fslsmooth(pred_img, sigma = 1.25,
                     mask = brain_mask, retimg = TRUE,
                     smooth_mask = TRUE)
threshold = 0.16
binary_map = prob_map > threshold


## ----pred_plot, cache = TRUE--------------------------------------------------
library(scales)

reduced_binary_map = apply_empty_dim(img = binary_map,
                                     inds = dd$inds)
ortho2(norm_imgs$FLAIR, reduced_binary_map,
       col.y = scales::alpha("red", 0.5))
double_ortho(norm_imgs$FLAIR, reduced_binary_map, col.y = "red")

multi_overlay(
  norm_imgs, 
  y = list(reduced_binary_map,
           reduced_binary_map,
           reduced_binary_map,
           reduced_binary_map),
  col.y = scales::alpha("red", 0.5) ,
  z = z, 
  text = names(norm_imgs),
  text.x = 
    rep(0.5, length(norm_imgs)),
  text.y = 
    rep(1.4, length(norm_imgs)), 
  text.cex = 
    rep(2.5, length(norm_imgs)))


## ----extrantsr_ver_check------------------------------------------------------
check = check_package_version("extrantsr", min_version = "2.2.1")
if (!check) {
  source("https://neuroconductor.org/neurocLite.R")
  neuroc_install("extrantsr") 
}


## ----ants_preproc, cache = FALSE, eval = TRUE---------------------------------
dat = ss[[1]]
print(dat)
# preparing output filenames
ants_outfiles = nii.stub(dat)
n4_brain_mask = gsub("_T1$", "", ants_outfiles["T1"])
n4_brain_mask = paste0(n4_brain_mask, "_N4_Brain_Mask.nii.gz")
ants_outfiles = paste0(ants_outfiles, "_ants_preprocessed.nii.gz")
names(ants_outfiles) = names(dat)
ants_outfiles = ants_outfiles[ names(ants_outfiles) != "mask"]

if (!all(file.exists(ants_outfiles))) {
  pre = preprocess_mri_within(
    files = dat[c("T1", "T2", "FLAIR", "PD")],
    outfiles = ants_outfiles[c("T1", "T2", "FLAIR", "PD")],
    correct = TRUE,
    correction = "N4",
    skull_strip = FALSE,
    typeofTransform = "Rigid",
    interpolator = "LanczosWindowedSinc")
  
  ss = fslbet_robust(
    ants_outfiles["T1"], 
    correct = FALSE,
    bet.opts = "-v")
  ss = ss > 0
  writenii(ss, filename = n4_brain_mask)
  
  imgs = lapply(ants_outfiles[c("T1", "T2", "FLAIR", "PD")],
                readnii)
  imgs = lapply(imgs, mask_img, ss)
  
  imgs = lapply(imgs, bias_correct, correction = "N4",
                mask = ss)
  mapply(function(img, fname){
    writenii(img, filename = fname)
  }, imgs, ants_outfiles[c("T1", "T2", "FLAIR", "PD")])
  
}


## ----ants_df, eval = FALSE----------------------------------------------------
## L = oasis_train_dataframe(
##   flair = ants_outfiles["FLAIR"],
##   t1 = ants_outfiles["T1"],
##   t2 = ants_outfiles["T2"],
##   pd = ants_outfiles["PD"],
##   preproc = FALSE,
##   brain_mask = n4_brain_mask,
##   eroder = "oasis")
## 
## ants_oasis_dataframe = L$oasis_dataframe
## ants_brain_mask = L$brain_mask
## ants_top_voxels = L$voxel_selection

## ----cluster, eval = FALSE----------------------------------------------------
## library(cluster)
## km = kmeans(x = ants_oasis_dataframe, centers = 4)
## km_img = niftiarr(ants_brain_mask, 0)
## km_img[ants_top_voxels == 1] = km$cluster
## n4_flair = readnii(ants_outfiles["FLAIR"])
## res = clara(x = ants_oasis_dataframe, k = 4)
## cl_img = niftiarr(ants_brain_mask, 0)
## cl_img[ants_top_voxels == 1] = res$clustering
## ortho2(n4_flair, cl_img > 3, col.y = scales::alpha("red", 0.5))

