## ----echo=FALSE, message =FALSE, include= FALSE--------------------------
install.packages("oro.nifti")
install.packages("fslr")
rmpkg = function(pkg){
  rn = rownames( installed.packages() )
  search_item = paste0("package:", pkg)
  if (  search_item %in% search() ){
    detach(search_item, unload=TRUE, character.only = TRUE)
  }
  if (pkg %in% rn){
    remove.packages(pkg)
  }
}
rmpkg("extrantsr")

## ----knit-setup, echo=FALSE, results='hide'------------------------------
library(knitr)
opts_chunk$set(echo=TRUE, prompt=FALSE, message=FALSE, warning=FALSE, comment="", results='hide')

## ----echo=FALSE----------------------------------------------------------
rm(list=ls()[ ! ls() %in% "rmpkg"])
library(fslr)
library(methods)
homedir = path.expand("~/Dropbox/FSLR")
datadir = file.path(homedir, "data")
resdir = file.path(homedir, "figure")

## ----fslr_setup, echo=TRUE-----------------------------------------------
library(fslr)
options(fsl.path="/usr/local/fsl")
options(fsl.outputtype = "NIFTI_GZ")

## ----echo = FALSE--------------------------------------------------------
# Get Files
mods = c("T1", "T2", "FLAIR", "PD")
# mods = c("T1")
# mods = paste0(mods, "norm")
  files = c(outer(
    outer(c("01-", "02-"), 
          c("Baseline_", "Followup_"), 
          paste0),
    paste0(mods, ".nii.gz"),
    paste0))
files = c(sapply(c("Baseline_", "Followup_"), function(x){
  paste0("01-", x, mods, ".nii.gz")
}))
files = file.path(datadir, files)

#### remove .nii.gz extension and use basename
stub = nii.stub(files, bn=TRUE)
bias_files = file.path(datadir, paste0(stub, "_FSL_BiasCorrect"))
df = data.frame(file = basename(files), 
                bias_file = basename(bias_files),
                stringsAsFactors = FALSE)

## ----run_fast, eval= TRUE, echo=FALSE, cache=TRUE------------------------
ext = get.imgext()
for (ifile in seq_along(files)){

  file = file.path(datadir, df$file[ifile])
  bias_file = file.path(datadir, df$bias_file[ifile])
  bfile = paste0(bias_file, ext)  
  file = files[ifile]
  if (!file.exists(bfile)){
    fsl_biascorrect(file, opts= "-v", outfile=bias_file)
  }
}

## ----run_fast_df, eval = FALSE, results='markup', echo=FALSE-------------
## head(df, 2)

## ----run_fast_show, eval = FALSE-----------------------------------------
## fsl_biascorrect(file = "01-Baseline_T1.nii.gz",
##                 outfile= "01-Baseline_T1_FSL_BiasCorrect",
##                 opts= "-v")

## ----run_fast_show_old, eval = FALSE, echo=FALSE-------------------------
## for (ifile in seq_along(df)){
##   bias_file = df$bias_file[ifile]
##   file = df$file[ifile]
##   fsl_biascorrect(file, opts= "-v", outfile=bias_file)
## }

## ----make_bc_data, eval= TRUE, cache=TRUE, echo=FALSE--------------------
t1 = readNIfTI(file.path(datadir, "01-Baseline_T1"), reorient=FALSE)
bc_t1 = readNIfTI(file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect"), 
                  reorient=FALSE)
t1 = cal_img(t1)
bc_t1 = cal_img(bc_t1)
dat = data.frame(t1 = c(t1), bc_t1 = c(bc_t1))
dimg = dim(t1)
all.ind = expand.grid(dim1=seq(dimg[1]), 
                                   dim2=seq(dimg[2]), 
                                   dim3=seq(dimg[3]))
dat = cbind(dat, all.ind)
# non-zero voxels
dat = dat[ dat$t1 != 0, ]
u_z= sort(unique(dat$dim3))
l_z = length(u_z)
cuts = seq(from = u_z[1], l_z, by=25)
## subsample for plotting
dat = dat[ sample(nrow(dat), 10000), ]
library(ggplot2)
q = ggplot(aes(x=t1, y=bc_t1), data=dat) + geom_point() +
  xlab("T1 Values (a.u.)") +
  ylab("Bias-Field-Corrected T1 Values (a.u.)") +
  ggtitle("Comparison of T1 Values and Bias-Corrected Values") + 
  geom_abline(intercept = 0, slope = 1, aes(colour="X = Y Line")) + geom_smooth(aes(colour="GAM"), se=FALSE, alpha= I(0.2))

## ----plot_flair_t1, include= FALSE, cache=TRUE---------------------------
flair_t1_base = file.path(datadir, "01-Baseline_FLAIR_FSL_BiasCorrect_rigid_to_T1")

# pngname = file.path(resdir, "BC_T1_Ortho_A_FLIRT.png")


flair_t1 = readNIfTI(flair_t1_base, reorient = TRUE)
flair_t1 = cal_img(flair_t1)
flair_t1 = robust_window(flair_t1, replace = "window")

pngname = file.path(resdir, "FLAIR_Ortho_A_FLIRT.png")
png(pngname)
orthographic(flair_t1, text = "A\nBaseline\nCo-Registered\n FLAIR Image", text.cex = 3)
dev.off()

pngname = file.path(resdir, "FLAIR_Ortho_B_FLIRT.png")
png(pngname)
orthographic(flair_t1, text = "B\nBaseline\nCo-Registered\n FLAIR Image", text.cex = 3)
dev.off()


fdat = data.frame(flair_t1 = c(flair_t1), bc_t1 = c(bc_t1))
dimg = dim(flair_t1)
all.ind = expand.grid(dim1=seq(dimg[1]), 
                                   dim2=seq(dimg[2]), 
                                   dim3=seq(dimg[3]))
fdat = cbind(fdat, all.ind)
# non-zero voxels
fdat = fdat[ fdat$bc_t1 != 0, ]
u_z= sort(unique(fdat$dim3))
l_z = length(u_z)
cuts = seq(from = u_z[1], l_z, by=25)
## subsample for plotting
set.seed(20150330)
fdat = fdat[ sample(nrow(fdat), 10000), ]

pflair = ggplot(aes(x=bc_t1, y=flair_t1), data=fdat) + geom_point() +
  xlab("Bias-Field-Corrected T1 Values (a.u.)") +
  ylab("Bias-Field-Corrected FLAIR Values (a.u.)") +
  ggtitle("Comparison of Baseline T1 and FLAIR Values")  + geom_smooth(aes(colour="GAM"), se=FALSE, alpha= I(0.2))

d = data.frame(label="C")
tsize = 16
pflair = pflair + scale_colour_manual("", 
                            values = c("GAM"="deeppink"),
                            guide = guide_legend(reverse=TRUE)) 
plotx = round(max(fdat$bc_t1) - 25)
pflair + geom_text(data=d, x= plotx, y=50, size=20,
                  aes(label=label), colour="black") +
  theme(legend.position = c(.25, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        plot.title = element_text(hjust = 0.8),        
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))

## ----other_to_t1, cache=TRUE, echo= FALSE--------------------------------
imgs = paste0("01-",  
                        outer(c("Baseline_", "Followup_"), 
                              c("T2", "FLAIR", "PD"), paste0), 
                        "_FSL_BiasCorrect")
t1.imgs = gsub("T2|FLAIR|PD", "T1", imgs)
reg_df = data.frame(t1 = t1.imgs, img = imgs, stringsAsFactors = FALSE)
reg_df$omat = paste0(reg_df$img, "_rigid_to_T1.mat")
reg_df$ofile = paste0(reg_df$img, "_rigid_to_T1")

## ----other_to_t1_print, cache=FALSE, echo= FALSE, eval= FALSE, results='markup'----
## head(reg_df, 2)

## ----other_to_t1_fpath, cache=FALSE, echo= FALSE-------------------------
reg_df$omat = file.path(datadir, reg_df$omat)
reg_df$ofile = file.path(datadir, reg_df$ofile)

## ----other_to_t1_show, eval=FALSE----------------------------------------
## flirt(reffile = "01-Baseline_T1_FSL_BiasCorrect",
##       infile = "01-Baseline_T2_FSL_BiasCorrect",
##       omat = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1.mat",
##       dof = 6,
##       outfile = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1",
##       opts = "-v")

## ----other_to_t1_run, cache=FALSE, echo=FALSE----------------------------
for (iimg in seq(nrow(reg_df))){
  ofile = paste0(reg_df$ofile[iimg], ext)
  if (!file.exists(ofile)){
    flirt(reffile = reg_df$t1[iimg], infile = reg_df$img[iimg], 
          omat =  reg_df$omat[iimg], dof = 6,
          outfile = reg_df$ofile[iimg], opts = "-v")
  }
}

## ----plot_bc_data, include= FALSE, cache=TRUE----------------------------
d = data.frame(label="C")
tsize = 16
p = q + scale_colour_manual("", 
                            values = c("X = Y Line" ="deepskyblue", "GAM"="deeppink"),
                            guide = guide_legend(reverse=TRUE)) 
plotx = round(max(dat$t1) - 25)
p + geom_text(data=d, x= plotx, y=50, size=20,
                  aes(label=label), colour="black") +
  theme(legend.position = c(.25, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        plot.title = element_text(hjust = 0.8),        
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))

## ----plot_bc_data_zoom, include= FALSE, cache=TRUE, eval=TRUE------------
library(ggplot2)
d = data.frame(label="D")
tsize = 16
pzoom = q + scale_colour_manual("", 
                            values = c("X = Y Line" ="deepskyblue", "GAM"="deeppink"),
                            guide = guide_legend(reverse=TRUE)) 
plotx = 37.5
pzoom = pzoom + geom_text(data=d, x= plotx, y=10, size=20,
                  aes(label=label), colour="black") +
  theme(legend.position = c(.25, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        plot.title = element_text(hjust = 0.8),        
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))
pzoom = pzoom %+% dat[dat$t1 < 40,]
print(pzoom)

## ----plot_ortho, eval= TRUE, echo= FALSE, cache=TRUE---------------------
t1_2 = robust_window(t1, replace = "window")
png(file.path(resdir, "T1_Ortho.png"))
orthographic(t1_2, text = "A\n T1 Image", text.cex = 3)
dev.off()

bc_t1_2 = robust_window(bc_t1, replace = "window")
png(file.path(resdir, "BC_T1_Ortho.png"))
orthographic(bc_t1_2, text = "B\nBias-Corrected\n T1 Image", text.cex = 3)
dev.off()
png(file.path(resdir, "BC_T1_Ortho_A.png"))
orthographic(bc_t1_2, text = "A\nBias-Corrected\n T1 Image", text.cex = 3)
dev.off()

## ----t1_to_t1, cache=FALSE, echo=FALSE-----------------------------------
base_t1 = file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect")
fup_t1 = file.path(datadir, "01-Followup_T1_FSL_BiasCorrect")
omat = paste0(fup_t1, "_rigid_to_BaseT1.mat")
flirt_ofile = ofile = paste0(fup_t1, "_rigid_to_BaseT1")

## ----t1_to_t1_show, eval=FALSE-------------------------------------------
## flirt(reffile = "01-Baseline_T1_FSL_BiasCorrect",
##       infile = "01-Followup_T1_FSL_BiasCorrect",
##       omat = "01-Followup_T1_FSL_BiasCorrect_rigid_to_BaseT1.mat",
##       dof = 6,
##       outfile = "01-Followup_T1_FSL_BiasCorrect_rigid_to_BaseT1",
##       opts = "-v")

## ----t1_to_t1_run, cache=FALSE, echo=FALSE-------------------------------
ofile = paste0(ofile, ext)

if (!file.exists(ofile)){
  flirt(reffile = base_t1, infile = fup_t1, 
      omat = omat, dof = 6,
      outfile = ofile, opts = "-v")
}

## ----plot_flirt, cache= TRUE, echo=FALSE---------------------------------
bc_t1_2 = robust_window(bc_t1, replace = "window")
pngname = file.path(resdir, "BC_T1_Ortho_A.png")
png(pngname)
orthographic(bc_t1_2, text = "A\nBaseline\n Bias-Corrected\n T1 Image", text.cex = 3)
dev.off()

pngname = file.path(resdir, "BC_T1_Ortho_A_FLIRT.png")
png(pngname)
orthographic(bc_t1_2, text = "A\nBaseline\n T1 Image", text.cex = 3)
dev.off()


fup_bc_t1 = readNIfTI(flirt_ofile, reorient = TRUE)
fup_bc_t1 = cal_img(fup_bc_t1)
fup_bc_t1_2 = robust_window(fup_bc_t1, replace = "window")

pngname = file.path(resdir, "FLIRT_Followup_T1.png")
png(pngname)
orthographic(fup_bc_t1_2, text = "B\nFollow-up\n Bias-Corrected\n T1 Image", text.cex = 3)

pngname = file.path(resdir, "E_FLIRT_Followup_T1.png")
png(pngname)
orthographic(fup_bc_t1_2, text = "E\nFollow-up\nT1 Image\nRegistered", text.cex = 3)
dev.off()

## ----across_visit_run, cache=TRUE, echo=FALSE----------------------------
run_df = reg_df[ grepl("Follow", reg_df$t1), ]
run_df$reg_file = gsub("_to_T1", "_to_BaseT1", run_df$ofile)
for (iimg in seq(nrow(run_df))){
  ofile = paste0(run_df$reg_file[iimg], ext)
  if (!file.exists(ofile)){
    flirt_apply(reffile = base_t1,
                infile = run_df$ofile[iimg], 
                initmat = omat, 
                outfile = run_df$reg_file[iimg], opts = '-v')
  }
}

## ----across_visit_show, eval=FALSE,  echo=TRUE---------------------------
## flirt_apply(reffile = "01-Baseline_T1_FSL_BiasCorrect", # register to this
##             infile = "01-Followup_T2_FSL_BiasCorrect_rigid_to_T1", # reg to Followup T1
##             initmat = "01-Followup_T1_FSL_BiasCorrect_rigid_to_BaseT1.mat", #transform
##             outfile = "01-Followup_T2_FSL_BiasCorrect_rigid_to_BaseT1" # output file
##             )

## ----across_visit_print, cache= TRUE, echo=FALSE-------------------------
print_df = reg_df
print_df$ofile[grepl("Follow", print_df$t1)] = 
  gsub("_to_T1", "_to_BaseT1", print_df$ofile[grepl("Follow", print_df$t1)])
files = print_df$ofile
names(files)[seq(1, 6, by=2)] = c("B", "C", "D")
names(files)[seq(2, 6, by=2)] = c("F", "G", "H")
for (iimg in seq_along(files)){
  ofile = print_df$ofile[iimg]
  stub = basename(ofile)
  pngname = file.path(resdir, paste0(stub, ".png"))
  print(pngname)
  if (!file.exists(pngname)){
    let = names(files)[iimg]
    mod = gsub(".*[Baseline|Followup]_(.*)_FSL.*", "\\1", ofile)
    time = gsub(".*(Baseline|Followup).*", "\\1", ofile)
    time = gsub("Followup", "Follow-up", time)
    oimg = readNIfTI(ofile, reorient=FALSE)
    png(pngname)
    orthographic(oimg, 
                 text = paste0(let, "\n", time, "\n", mod, " Registered"), 
                 text.cex = 3)
    dev.off()
  }
}

## ----bet_t1_run, echo=FALSE----------------------------------------------
orig_t1 = file.path(datadir, "01-Baseline_T1")
bet_base_t1 = paste0(base_t1, "_Brain")
base_t1_maskfile = paste0(bet_base_t1, "_mask")
if (!file.exists(paste0(bet_base_t1, ext))){
  fslbet(infile = orig_t1, outfile = bet_base_t1, opts = "-B -f 0.1 -v", betcmd = "bet")
}

## ----bet_t1, eval = FALSE------------------------------------------------
## fslbet(infile =  '01-Baseline_T1',
##        outfile = "01-Baseline_T1_FSL_BiasCorrect_Brain",
##        opts = "-B -f 0.1 -v",  # from Popescu et al.
##        betcmd = "bet",
##        intern=FALSE)

## ----plot_bet_mask, cache= TRUE, echo=FALSE------------------------------
library(scales)
base_t1_mask = readNIfTI(base_t1_maskfile, reorient=FALSE)
base_t1_mask[base_t1_mask == 0] = NA

pngname = file.path(resdir, "plot_bet_mask.png")
png(pngname)
orthographic(bc_t1_2, base_t1_mask, col.y=alpha("red", 0.5), text = "A\n T1 Image\nwith Mask", text.cex = 3)
dev.off()
base_t1_brain = readNIfTI(bet_base_t1, reorient=FALSE)
base_t1_brain = cal_img(base_t1_brain)
base_t1_brain = robust_window(base_t1_brain, replace = "window")
pngname = file.path(resdir, "plot_bet_brain.png")
png(pngname)
orthographic(base_t1_brain, text = "B\n T1 Image\nBrain Extracted", text.cex = 3)
dev.off()

## ----bet_mask_print, echo=TRUE, eval=FALSE-------------------------------
## mask <- fslmask(file="01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1",
##                 mask = "01-Baseline_T1_FSL_BiasCorrect_Brain_mask",
##                 outfile = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1_Brain",
##                 retimg = TRUE)

## ----bet_mask_run, echo=FALSE, cache=TRUE--------------------------------
files = c(
  file.path(datadir, 
            paste0("01-Baseline_", c("T2", "FLAIR", "PD"), 
                   "_FSL_BiasCorrect_rigid_to_T1")), 
  file.path(datadir, 
            paste0("01-Followup_", c("T1", "T2", "FLAIR", "PD"), 
                   "_FSL_BiasCorrect_rigid_to_BaseT1")))
bet_files = paste0(files, "_Brain")
for (ifile in seq_along(files)){
  fslmask(file=files[ifile], mask = base_t1_maskfile,
          outfile = bet_files[ifile])
}

## ----t1_to_mni, cache=TRUE, echo=FALSE-----------------------------------
bet_base_t1 = file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect_Brain")
template = file.path(fsldir(), "data", "standard", "MNI152_T1_1mm_brain")
omat = paste0(bet_base_t1, "_affine_toMNI.mat")
flirt_ofile = paste0(bet_base_t1, "_affine_toMNI")
fnirt_ofile = paste0(bet_base_t1, "_toMNI")
fnirt_owarp = paste0(flirt_ofile, "_warpcoef")

## ----t1_to_mni_show, eval=FALSE------------------------------------------
## fnirt_with_affine(infile = "01-Baseline_T1_FSL_BiasCorrect_Brain",
##                   reffile = file.path(fsldir(), "data", "standard", "MNI152_T1_1mm_brain"),
##                   flirt.omat = "01-Baseline_T1_FSL_BiasCorrect_Brain_affine_toMNI.mat",
##                   flirt.outfile = "01-Baseline_T1_FSL_BiasCorrect_Brain_affine_toMNI",
##                   outfile = "01-Baseline_T1_FSL_BiasCorrect_Brain_toMNI")

## ----t1_to_mni_run, cache=TRUE, echo=FALSE, eval=FALSE-------------------
## fnirt_ofile = paste0(fnirt_ofile, ext)
## 
## if (!file.exists(fnirt_ofile)){
##   fnirt_with_affine(infile = bet_base_t1,
##                     reffile = template,
##                     flirt.omat = omat,
##                     flirt.outfile = flirt_ofile,
##                     outfile = fnirt_ofile, opts = '-v')
## }

## ----plot_warp, eval= TRUE, echo= FALSE, cache=TRUE----------------------
fnirt_img = readNIfTI(fnirt_ofile, reorient = FALSE)
fnirt_img = robust_window(fnirt_img, replace = "window")
png(file.path(resdir, "T1_MNI_Warp.png"))
orthographic(fnirt_img, text = "A\nT1\nWarped Image", text.cex = 3)
dev.off()

temp_img = cal_img(readNIfTI(template, reorient = FALSE))
# temp_img = robust_window(temp_img, replace = "window")

png(file.path(resdir, "MNI_Ortho.png"))
par(xpd = NA)
orthographic(temp_img, text = "B\nMNI Template\n T1 Image", text.cex = 3)
dev.off()

X = nrow(temp_img)
xcoord = X/2
Y = ncol(temp_img)
ycoord = Y - 10

i  = 1
for (islice in c(25, 45, 92, 137)){
  pngname = file.path(resdir, paste0("MNI_Warp_Slice_", islice, ".png"))
  png(pngname)
  par(oma = rep(0, 4), mar = rep(0, 4), bg="black")
#   image(temp_img, z = islice, plot.type="single")
  image(1:X, 1:Y, temp_img[,,islice], 
        col = gray(0:64/64))
  text(labels=paste0("Template Image: Slice ", islice), 
       x=xcoord, y=ycoord, col="white", cex=3)
  text(labels=paste0(LETTERS[i]), 
       x=X - 20, y=30, col="white", cex=5)
  dev.off()
  i = i + 1

  pngname = file.path(resdir, paste0("T1_MNI_Ortho_Slice_", islice, ".png"))
#   png(pngname)
#   image(fnirt_img, z = islice, plot.type="single")
#   dev.off()
  png(pngname)
  par(oma = rep(0, 4), mar = rep(0, 4), bg="black")
#   image(temp_img, z = islice, plot.type="single")
  image(1:X, 1:Y, fnirt_img[,,islice], 
        col = gray(0:64/64))
  text(labels=paste0("Registered Image: Slice ", islice), 
       x=xcoord, y=ycoord, col="white", cex=3)
  text(labels=paste0(LETTERS[i]), 
       x=X - 20, y=30, col="white", cex=5)
  dev.off()
  i = i + 1

}



## ----all_to_mni_show, cache=FALSE, eval=FALSE, echo=FALSE----------------
## for (ifile in bet_files){
##   ofile = paste0(ifile, '_toMNI', ext)
##     flirt_apply(infile = ifile,
##                 reffile = template,
##                 initmat = omat,
##                 outfile = ofile)
##     fsl_applywarp(infile = ofile,
##                   reffile = template,
##                   warpfile = fnirt_owarp,
##                   outfile = ofile,
##                   opts = '-v')
## }

## ----all_to_mni_show2, cache=FALSE, eval=FALSE---------------------------
## flirt_apply(infile = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1_Brain",
##             reffile = file.path(fsldir(), "data", "standard", "MNI152_T1_1mm_brain"),
##             initmat = "01-Baseline_T1_FSL_BiasCorrect_Brain_affine_toMNI.mat",
##             outfile = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1_Brain_toMNI")
## fsl_applywarp(infile = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1_Brain_toMNI",
##               reffile = file.path(fsldir(), "data", "standard", "MNI152_T1_1mm_brain"),
##               warpfile = "01-Baseline_T1_FSL_BiasCorrect_Brain_affine_toMNI_warpcoef",
##               outfile = "01-Baseline_T2_FSL_BiasCorrect_rigid_to_T1_Brain_toMNI")

## ----all_to_mni_run, cache=TRUE, echo=FALSE------------------------------
ifile = bet_files[1]
for (ifile in bet_files){
  ofile = paste0(ifile, '_toMNI', ext)
  if (!file.exists(ofile)){
    flirt_apply(infile = ifile,
                reffile = template,
                initmat = omat,
                outfile = ofile)
    fsl_applywarp(infile = ofile,
                  reffile = template,          
                  warpfile = fnirt_owarp,
                  outfile = ofile, 
                  opts = '-v')
  }
}

## ----smoother_shower,echo=FALSE, eval=FALSE------------------------------
## smooth = fslsmooth(base_t1_brain, sigma = 3, retimg=TRUE)

## ----smoother_plot, echo=FALSE, cache=TRUE, dev='png', eval=FALSE--------
## pngname = file.path(resdir, "Bet_Brain_A.png")
## png(pngname)
## orthographic(base_t1_brain, text = "A\n T1 Image\nBrain Extracted", text.cex = 3)
## dev.off()
## 
## pngname = file.path(resdir, "T1_Smooth_Img.png")
## png(pngname)
## orthographic(smooth, text = "A\n T1 Image\nBrain Smoothed",
##              text.cex = 3)
## dev.off()

## ----read_to_temp, cache=FALSE, echo=FALSE-------------------------------
t1_to_temp = readNIfTI(file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect_Brain_toMNI"), reorient=FALSE)

## ----read_base_show, eval=FALSE------------------------------------------
## t1_to_temp = readNIfTI("01-Baseline_T1_FSL_BiasCorrect_Brain_toMNI", reorient=FALSE)

## ----smoother,echo=FALSE, cache=TRUE, dev='png', eval=TRUE, include=FALSE----
smooth = fslsmooth(t1_to_temp, sigma = 3, retimg=TRUE)
orthographic(smooth, text = "A\nSmoothed Image", 
               text.cex = 3)

## ----smoother_show,echo=TRUE, cache=TRUE, dev='png', eval=FALSE----------
## smooth = fslsmooth(t1_to_temp, sigma = 3, retimg=TRUE)

## ----binned,echo=FALSE, cache=TRUE, dev='png', eval=TRUE, results='hide', include=FALSE----
binned = fslbin(t1_to_temp, retimg=TRUE)
binned = cal_img(binned)
orthographic(cal_img(binned), text = "B\nBinarized\nRegistered T1\nImage", 
               text.cex = 3)

## ----binned_show,echo=TRUE, cache=TRUE, dev='png', eval=FALSE, results='markup'----
## binned = fslbin(t1_to_temp, retimg=TRUE)

## ----thresh,echo=FALSE, cache=TRUE, dev='png', eval=TRUE, results='hide', include=FALSE----
thresh = fslthresh(t1_to_temp, thresh = 30, uthresh = 50, retimg=TRUE)
orthographic(cal_img(thresh), text = "C\nThresholded\nRegistered T1\nImage", 
               text.cex = 3)

## ----thresh_show,echo=TRUE, cache=TRUE, dev='png', eval=FALSE------------
## thresh = fslthresh(t1_to_temp, thresh = 30, uthresh = 50, retimg=TRUE)

## ----ero,echo=FALSE, cache=TRUE, dev='png', eval=TRUE, include=FALSE-----
eroded = fslerode(binned, retimg=TRUE)
orthographic(niftiarr(binned, binned - eroded), text = "D\nEroded Voxels", 
               text.cex = 3)

## ----ero_show,echo=TRUE, cache=TRUE, dev='png', eval=FALSE---------------
## eroded = fslerode(binned, retimg=TRUE)

## ----fslval_show, echo=TRUE, eval = FALSE--------------------------------
## fslval("01-Baseline_T1_FSL_BiasCorrect_Brain", keyword = "dim3")

## ----fslval, echo=FALSE, eval = TRUE, results='markup'-------------------
fslval(file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect_Brain"), keyword = "dim3", verbose = FALSE)

## ----fslhd_show, echo=TRUE, eval = FALSE---------------------------------
## img_hdr = fslhd("01-Baseline_T1_FSL_BiasCorrect_Brain")

## ----fslmean_show, echo=TRUE, eval = FALSE-------------------------------
## fslstats("01-Baseline_T1_FSL_BiasCorrect_Brain", opts = "-M")

## ----fslmean, echo=FALSE, eval = TRUE, results='markup'------------------
fslstats(file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect_Brain"), 
         opts = "-M", verbose = FALSE)

## ----read_diff, eval=TRUE, echo = FALSE, cache=TRUE----------------------
base_t1 = readNIfTI(file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect"), reorient=FALSE)
base_t1_mask = readNIfTI(file.path(datadir, "01-Baseline_T1_FSL_BiasCorrect_Brain_mask"), reorient=FALSE)

## ----read_diff_show, eval=FALSE------------------------------------------
## base_t1 = readNIfTI("01-Baseline_T1_FSL_BiasCorrect", reorient=FALSE)
## base_t1_mask = readNIfTI("01-Baseline_T1_FSL_BiasCorrect_Brain_mask", reorient=FALSE)

## ----masking_multiply, cache=FALSE, results='markup'---------------------
base_t1_1 = base_t1 * base_t1_mask
class(base_t1_1)

## ----masking_niftiarr, cache=FALSE, results='markup'---------------------
base_t1_1 = niftiarr(base_t1, base_t1_1)
class(base_t1_1)

## ----masking_2, cache=FALSE, results='markup'----------------------------
base_t1_2 = base_t1
base_t1_2[base_t1_mask == 0] = 0
class(base_t1_2)

## ----cal_img, cache=FALSE, results='markup'------------------------------
range(base_t1_2)
c(base_t1_2@cal_min, base_t1_2@cal_max)

## ----cal_img2, cache=FALSE, results='markup'-----------------------------
base_t1_2 = cal_img(base_t1_2)
c(base_t1_2@cal_min, base_t1_2@cal_max)

## ----allequal, cache=FALSE, results='markup'-----------------------------
identical(base_t1_1, base_t1_2)

## ----diff_img, cache=TRUE, echo=FALSE------------------------------------
fup_t1file = file.path(datadir, 
                       "01-Followup_T1_FSL_BiasCorrect_rigid_to_BaseT1")
fup_t1_reg = readNIfTI(fup_t1file, reorient=FALSE)
fup_t1_reg[base_t1_mask == 0] = 0

diff.img = niftiarr(base_t1_1, fup_t1_reg - base_t1_1)
dat = data.frame(fup = c(fup_t1_reg), base = c(base_t1), mask = c(base_t1_mask))
# non-zero voxels
dat = dat[ dat$mask == 1, ]
dat$mask = NULL
## subsample for plotting
dat = dat[ sample(nrow(dat), 10000), ]
q = qplot(x=base, y=fup, data=dat, 
     xlab = "Baseline Bias-Corrected T1 Values (a.u.)", 
     ylab = "Followup Bias-Corrected T1 Values (a.u.)",
     geom= c("point", "smooth"), 
     main='T1 Values: Baseline vs Followup', 
     se=FALSE, alpha= I(0.2)) + 
  geom_abline(intercept = 0, slope =1, col="red")

## ----plot_base_follow_data, include= FALSE, cache=TRUE, echo=FALSE-------
d = data.frame(label="C")
tsize = 16
plotx = round(max(dat$base) - 25)
q  + geom_text(data=d, x= plotx, y=25, size=20,
                  aes(label=label), colour="black") +
  theme(legend.position = c(.5, .75),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent", 
                                  color="transparent"),
        legend.text = element_text(size=tsize+2), 
        legend.title = element_text(size=tsize),
        title = element_text(size=tsize),
        plot.title = element_text(hjust = 0.8),        
        strip.text = element_text(size = tsize+4),
        axis.text  = element_text(size=tsize-2))

## ----plot_diff, cache = TRUE, echo=FALSE---------------------------------
pngname = file.path(resdir, "T1_Diff_Img.png")
png(pngname)
  diff.img[base_t1_mask == 0] = NA
  orthographic(diff.img, text = "A\n Difference\nT1 Image", 
               text.cex = 3)
dev.off()
pngname = file.path(resdir, "T1_Diff_Img_Col.png")
library(RColorBrewer)
cdiff = c(diff.img[base_t1_mask == 1])
quants = quantile(cdiff, probs = seq(0, 1, by=0.1))
cuts = cut(cdiff, breaks=quants, include.lowest = TRUE)
numcuts = as.numeric(cuts)
dimg = diff.img
dimg[base_t1_mask == 1] = numcuts
dimg = cal_img(dimg)
png(pngname)
  orthographic(dimg, text = "A\n Difference\nT1 Image", 
               text.cex = 3, col = 
                 rev(brewer.pal(n = 11, name = "RdBu")))
dev.off()
pngname = file.path(resdir, "T1_Diff_Hist.png")
png(pngname)
options(scipen=8)
  hist(cdiff, breaks=200, 
       main = "Histogram of Followup - Baseline T1 Image",
       xlab = "Followup - Baseline T1 Image (a.u.)", las=1)
dev.off()


## ----dsmoother_show,echo=FALSE, eval= FALSE------------------------------
## smooth = fslsmooth(diff.img, sigma = 3, retimg=TRUE)

## ----smoother_diff_img,echo=FALSE, cache=TRUE, dev='png', eval=FALSE-----
## smooth = fslsmooth(diff.img, sigma = 3, retimg=TRUE)
## smooth[base_t1_mask == 0] = NA
## orthographic(smooth,text = "A\nDifference\nT1 Image\nSmoothed",
##                text.cex = 2)

## ----smoother_run,echo=FALSE, cache=TRUE,  eval=TRUE---------------------
smooth = fslsmooth(diff.img, sigma = 3, retimg=TRUE)
smooth[base_t1_mask == 0] = NA
png(file.path(resdir, "smoother.png"))
  orthographic(smooth, text = "A\nDifference\nT1 Image\nSmoothed", 
               text.cex = 2)
dev.off()
cdiff = c(smooth[base_t1_mask == 1])
quants = quantile(cdiff, probs = seq(0, 1, by=0.1))
cuts = cut(cdiff, breaks=quants, include.lowest = TRUE)
numcuts = as.numeric(cuts)
dimg = smooth
dimg[base_t1_mask == 1] = numcuts
dimg = cal_img(dimg)
pngname = file.path(resdir, "T1_Diff_Img_Col_Smoothed.png")
mycols = rev(brewer.pal(n = 11, name = "RdBu"))
leg.cuts = levels(cuts)
png(pngname)
  ortho2(dimg, text = "B\n Difference\nT1 Image\nSmoothed\n& Colored", 
               text.cex = 2, col = mycols,
         text.x = 18, text.y = 42,
         addlegend = TRUE,
         legend = leg.cuts,
         leg.x = 30,
         leg.y = 58,
         leg.col = mycols, leg.cex= 1.5, 
         add.orient = FALSE)
dev.off()

## ----white_stripe, cache=FALSE, include = FALSE, eval=FALSE--------------
## library(WhiteStripe)
## base_MNI.file = file.path(datadir,
##                           "01-Baseline_T1_FSL_BiasCorrect_Brain_toMNI")
## fup_MNI.file = file.path(datadir,
##                      "01-Followup_T1_FSL_BiasCorrect_rigid_to_BaseT1_Brain_toMNI")
## base_MNI.img = readNIfTI(base_MNI.file, reorient=TRUE)
## fup_MNI.img = readNIfTI(fup_MNI.file, reorient=TRUE)
## 
## mask.file = file.path(fsldir(), "data", "standard", "MNI152_T1_1mm_brain_mask")
## mask.img = readNIfTI(mask.file, reorient = TRUE)
## 
## 
## 
## base.ws = whitestripe(base_MNI.img, type = "T1")
## fup.ws = whitestripe(fup_MNI.img, type = "T1")
## 
## base_MNI.norm = whitestripe_norm(base_MNI.img,
##                                  indices = base.ws$whitestripe.ind)
## fup_MNI.norm = whitestripe_norm(fup_MNI.img,
##                                  indices = fup.ws$whitestripe.ind)
## 
## fup_MNI.norm2 = whitestripe_norm(fup_MNI.img,
##                                  indices = base.ws$whitestripe.ind)
## 
## base_MNI.norm[mask.img == 0] = NA
## fup_MNI.norm[mask.img == 0] = NA
## fup_MNI.norm2[mask.img == 0] = NA
## 
## 
## 
## diff.norm = niftiarr(base_MNI.norm, fup_MNI.norm2 - base_MNI.norm)
## 
## sdiff.norm = fslsmooth(diff.norm, sigma = 3, retimg=TRUE)
## sdiff.norm[mask.img == 0] = NA
## 
## 
## cdiff = c(sdiff.norm[mask.img == 1])
## quants = quantile(cdiff, probs = seq(0, 1, by=0.1))
## cuts = cut(cdiff, breaks=quants, include.lowest = TRUE)
## numcuts = as.numeric(cuts)
## dimg = sdiff.norm
## dimg[mask.img == 1] = numcuts
## dimg = cal_img(dimg)
## pngname = file.path(resdir, "T1_Norm_Diff_Img_Col_Smoothed.png")
## mycols = rev(brewer.pal(n = 11, name = "RdBu"))
## leg.cuts = levels(cuts)
## png(pngname)
##   ortho2(dimg, text = "B\n Difference\nT1 Image\nSmoothed\n& Colored",
##                text.cex = 2, col = mycols,
##          text.x = 18, text.y = 42,
##          addlegend = TRUE,
##          legend = leg.cuts,
##          leg.x = 30,
##          leg.y = 58,
##          leg.col = mycols, leg.cex= 1.5,
##          add.orient = FALSE)
## dev.off()
## 

## ----diff_z, echo=FALSE, cache=TRUE, dev='png', eval=FALSE---------------
## base_z = base_t1_1
## vals = base_z[base_t1_mask == 1]
## base_z = cal_img( (base_z - mean(vals)) / sd(vals))
## 
## fup_z = fup_t1_reg
## vals = fup_z[base_t1_mask == 1]
## fup_z = cal_img( (fup_z - mean(vals)) / sd(vals))
## 
## diff.z = niftiarr(base_t1_1, fup_z - base_z)
## diff.z[base_t1_mask == 0] = NA
## orthographic(diff.z)

## ----theshimg, echo=FALSE, cache=TRUE, dev='png', eval=FALSE-------------
## thresh.img = t1
## thresh.img = (thresh.img - mean(thresh.img))/sd(thresh.img)
## thresh.img[thresh.img < 2] = 0
## thresh.img = cal_img(thresh.img)
## orthographic(thresh.img)

## ----smooth.thresh, echo=FALSE, cache = TRUE, dev='png', eval=FALSE------
## smooth.thresh = fslsmooth(thresh.img, sigma = 4, retimg=TRUE)
## orthographic(smooth.thresh)

## ----echo=FALSE, message =FALSE, include= FALSE--------------------------
rmpkg("fslr")
rmpkg("oro.nifti")
rmpkg("extrantsr")
devtools::install_github("muschellij2/oro.nifti")
devtools::install_github("muschellij2/fslr")
devtools::install_github("muschellij2/extrantsr")

