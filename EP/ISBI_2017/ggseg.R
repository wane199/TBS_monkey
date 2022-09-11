# ggseg/ggseg3d
library(ggseg3d)
vignette("ggseg3d")
library(ggsegAal)
library(dplyr)

data(aal)
a <- data(aal_3d)
a
ggseg3d()
ggseg3d(hemisphere = "left")
ggseg3d(surface = "inflated")
ggseg3d(show.legend = FALSE)

p <- ggseg3d(atlas=dk_3d) %>% 
  remove_axes() %>% 
  pan_camera("right lateral")
p
p1 <- ggseg3d(atlas=aseg_3d) %>% 
  add_glassbrain() %>% 
  remove_axes() %>% 
  pan_camera("right lateral")
p1

library(ggsegSchaefer)
library(ggseg)
library(ggplot2)
ggseg3d(atlas = schaefer7_400_3d, surface = "inflated") %>% 
  pan_camera("right lateral")





