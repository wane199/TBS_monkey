# https://johnmuschelli.com/imaging_in_r/
if (!"devtools" %in% installed.packages()[, "Package"]) {
  install.packages("devtools")
}
if (!"ms.lesion" %in% installed.packages()[, "Package"]) {
  devtools::install_github("muschellij2/ms.lesion")

  }
source("https://neuroconductor.org/neurocLite.R")
pkgs = c("neurobase", "fslr", "dcm2niir","divest", 
         "RNifti", "oro.dicom", 
         "oro.nifti", "WhiteStripe", "neurohcp", "papayar",
         "papayaWidget", "oasis", "kirby21.t1")
neuro_install(pkgs)

# data for whitestripe
library(dcm2niir); install_dcm2nii()
library(WhiteStripe); download_img_data()

neuro_install(c("ITKR", "ANTsRCore", "ANTsR", "extrantsr"),
              upgrade_dependencies = FALSE)

neuro_install("malf.templates")

## ---- echo = FALSE-------------------------------------------------------
library(methods)
library(knitr)
opts_chunk$set(comment = "")

## ----readin, echo = FALSE, message=FALSE---------------------------------
# install.packages("remotes")
# remotes::install_github("muschellij2/ms.lesion")
library(ms.lesion)
library(neurobase)
files = get_image_filenames_list_by_subject(type = "coregistered")
files = files$training02
img_fnames = files[c("MPRAGE", "T2", "FLAIR", "PD")]
mask_fname = files["mask1"]
brain_mask = readnii(files["Brain_Mask"])
imgs = check_nifti(img_fnames)
mask = readnii(mask_fname)
zimgs = lapply(imgs, zscore_img, mask = brain_mask)
inds = getEmptyImageDimensions(brain_mask)
zimgs = lapply(zimgs, applyEmptyImageDimensions, inds = inds)
mask = applyEmptyImageDimensions(mask, inds = inds)
zimgs = c(zimgs, Lesion_Mask = list(mask * 10))
xyz = xyz(mask)
multi_overlay(zimgs, z = xyz[3], text = names(zimgs), 
              text.x = 0.5, text.y = 1.25, text.cex = 2)

