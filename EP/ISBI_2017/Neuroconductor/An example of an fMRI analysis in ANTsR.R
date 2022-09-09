# https://neuroconductor.org/tutorials/fmri_analysis_ANTsR
# An example of an fMRI analysis in ANTsR
# Data Packages
packages = installed.packages()
packages = packages[, "Package"]
if (!"kirby21.base" %in% packages) {
  source("https://neuroconductor.org/neurocLite.R")
  neuroc_install("kirby21.base")    
}
if (!"kirby21.fmri" %in% packages) {
  source("https://neuroconductor.org/neurocLite.R")
  neuroc_install("kirby21.fmri")      
} 

## ----setup, include=FALSE-----------------------------------------------------
library(kirby21.fmri)
library(kirby21.base)
library(dplyr)
library(neurobase)
library(ANTsR)
library(R.utils)
library(RColorBrewer)
library(matrixStats)
library(ggplot2)
library(reshape2)
library(animation)
library(zoo)
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, comment = "")


## ---- eval = FALSE------------------------------------------------------------
## packages = installed.packages()
## packages = packages[, "Package"]
## if (!"kirby21.base" %in% packages) {
##   source("https://neuroconductor.org/neurocLite.R")
##   neuroc_install("kirby21.base")
## }
## if (!"kirby21.fmri" %in% packages) {
##   source("https://neuroconductor.org/neurocLite.R")
##   neuroc_install("kirby21.fmri")
## }


## ----data---------------------------------------------------------------------
library(kirby21.fmri)
library(kirby21.base)
fnames = get_image_filenames_df(ids = 113, 
                                modalities = c("T1", "fMRI"), 
                                visits = c(1),
                                long = FALSE)
t1_fname = fnames$T1[1]
fmri_fname = fnames$fMRI[1]


## ----par_data-----------------------------------------------------------------
library(R.utils)
par_file = system.file("visit_1/113/113-01-fMRI.par.gz", 
                       package = "kirby21.fmri")
# unzip it
con = gunzip(par_file, temporary = TRUE, 
             remove = FALSE, overwrite = TRUE)
info = readLines(con = con)
info[11:23]


## ----fmri, cache = TRUE-------------------------------------------------------
library(neurobase)
fmri = readnii(fmri_fname)
ortho2(fmri, w = 1, add.orient = FALSE)
rm(list = "fmri") # just used for cleanup 


## ----subset_run, eval = TRUE--------------------------------------------------
library(extrantsr)
library(oro.nifti)
library(ANTsR)
ants_fmri = antsImageRead(fmri_fname)
tr = 2 # 2 seconds
first_scan = floor(10.0 / tr) + 1 # 10 seconds "stabilization of signal"
sub_fmri = extrantsr::subset_4d(ants_fmri, first_scan:ntim(ants_fmri))


## ----motion_corr_run, echo = TRUE, message=FALSE, dependson="subset_run"------
library(dplyr)
library(neurobase)
library(ANTsR)
base_fname = nii.stub(fmri_fname, bn = TRUE)
avg_img = getAverageOfTimeSeries(sub_fmri)

#####################
# Full with Half Max twice the vox size
##################
all_vox_dim = voxdim(sub_fmri)


#####################
# Motion Calculation
##################
moco_file = paste0(base_fname, 
                   "_Motion_Params.rda")
moco_fname = paste0(base_fname, "_moco_img.nii.gz")
if (all(file.exists(c(moco_file, 
                      moco_fname)))) { 
  load(moco_file)
  moco_img = antsImageRead(moco_fname)
  motion_res$moco_img = 
    moco_img
} else {
  motion_res = 
    antsMotionCalculation(sub_fmri, 
                          fixed = avg_img, 
                          moreaccurate = 1,
                          txtype = "Rigid",
                          verbose = TRUE)
  save(motion_res, 
       file = moco_file)
  moco_img = 
    motion_res$moco_img
  antsImageWrite(moco_img, 
                 filename = moco_fname)
}
moco_params = 
  motion_res$moco_params
moco_params = moco_params %>% 
  select(starts_with("MOCO"))
nuisanceVariables = moco_params
mp = round(moco_params, 4)
print(head(mp, 3))
rm(list = c("mp"))


## ----moco_run_plot, echo = TRUE, fig.height = 4, fig.width= 8, dependson="motion_corr_run"----
mp = moco_params
mp[, 1:3] = mp[, 1:3] * 50
r = range(mp)
plot(mp[,1], type = "l", xlab = "Scan Number", main = "Motion Parameters",
     ylab = "Displacement (mm)",
     ylim = r * 1.25, 
     lwd = 2,
     cex.main = 2,
     cex.lab = 1.5,
     cex.axis = 1.25)
for (i in 2:ncol(mp)) {
  lines(mp[, i], col = i)
}
rm(list = "mp")


## ----ts_run, echo = TRUE, dependson="motion_corr_run"-------------------------
moco_img = antsImageRead(moco_fname)
moco_avg_img = 
  getAverageOfTimeSeries(moco_img)
maskImage = getMask(moco_avg_img, 
                    mean(moco_avg_img), 
                    Inf, cleanup = 2)
mask_fname = paste0(base_fname, "_mask.nii.gz")
antsImageWrite(maskImage, filename = mask_fname)
double_ortho(moco_avg_img, maskImage, 
             col.y = "white")
moco_avg_img[maskImage == 0] = 0
boldMatrix = timeseries2matrix(
  moco_img, 
  maskImage)


## ----compute_dvars, echo = TRUE, dependson = "ts_run"-------------------------
dvars = computeDVARS(boldMatrix)
uncorr_dvars = motion_res$dvars
plot(dvars, uncorr_dvars,
     xlab = "Realigned DVARS",
     ylab = "Non-Realigned DVARS")
abline( a = 0, b = 1, col = "red")


## ----dvars_show, echo = TRUE, eval = FALSE------------------------------------
## mp = moco_params
## mp[, 1:3] = mp[, 1:3] * 50
## mp = apply(mp, 2, diff)
## mp = rbind(rep(0, 6), mp)
## mp = abs(mp)
## fd = rowSums(mp)
## plot(fd, type ="h",
##   xlab = "Scan", ylab = "FD")


## ----ts_heatmap, echo = TRUE, dependson="ts_run", fig.height = 3.5, fig.width = 8----
library(RColorBrewer)
library(matrixStats)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
mat = scale(boldMatrix)
image(x = 1:nrow(mat), 
      y = 1:ncol(mat), 
      mat, useRaster=TRUE, 
      col = r,
      xlab = "Scan Number", ylab = "Voxel",
      main = paste0("Dimensions: ", 
                    dim(mat)[1], "×", dim(mat)[2]),
      cex.main = 2,
      cex.lab = 1.5,
      cex.axis = 1.25)
rmeans = rowMeans(mat)
bad_ind = which.max(rmeans)
print(bad_ind)
abline(v = bad_ind)
sds = rowSds(mat)
print(which.max(sds))
rm(list = "mat")


## ----plot_bad_ortho, echo = TRUE, dependson="ts_heatmap"----------------------
library(animation)
ani.options(autobrowse = FALSE)
gif_name = "bad_dimension.gif"
if (!file.exists(gif_name)) {
  arr = as.array(moco_img)
  pdim = pixdim(moco_img)
  saveGIF({
    for (i in seq(bad_ind - 1, bad_ind + 1)) {
      ortho2(arr[,,,i], pdim = pdim, text = i)
    }
  }, movie.name = gif_name)
}


## ----ccor_run, echo = TRUE, dependson="motion_corr_run"-----------------------
library(reshape2)
library(ggplot2)
ccor_file = paste0(base_fname, 
                   "_CompCor.rda")
if (all(file.exists(ccor_file))) { 
  load(ccor_file)
} else {
  highvar = compcor(
    moco_img, 
    maskImage, 
    ncompcor = 6, 
    variance_extreme = 0.975,
    returnhighvarmatinds = TRUE)
  compCorNuisanceVariables = compcor(
    moco_img, 
    maskImage, 
    ncompcor = 6, 
    variance_extreme = 0.975)
  save(compCorNuisanceVariables, 
       highvar,
       file = ccor_file)
}
n = ncol(compCorNuisanceVariables)
r = range(compCorNuisanceVariables)
long = reshape2::melt(compCorNuisanceVariables)
colnames(long) = c("scan_num", "component", "value")

ggplot(long, aes(x = scan_num, y = value)) + geom_line() + facet_wrap(~component, ncol = 1)


## ---- cache = FALSE-----------------------------------------------------------
devtools::session_info()

