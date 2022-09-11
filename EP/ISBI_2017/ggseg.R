# https://mp.weixin.qq.com/s?__biz=MzIzOTQ4NjAwOQ==&mid=2247503138&idx=1&sn=13f7bb12d682dd515798dbc8b8eaaa0a&chksm=e92bd874de5c5162441dbdf65b85ed70bfa86ac3f07f1f7fc9cf122e7db4dfec666d63263359&mpshare=1&scene=1&srcid=0911ak6lRZ2Q1jrU8elRuzYR&sharer_sharetime=1662873400498&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# R|ggsegExtra绘制Desterieux统计结果
# ggseg/ggseg3d
library(ggseg3d)
vignette("ggseg3d")
library(ggsegAal)
library(dplyr)
library(ggsegExtra)
ggseg_atlas_repos()

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

# Enable this universe
options(repos = c(
  ggseg = 'https://ggseg.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Install some packages
install.packages('ggsegSchaefer')
library(ggsegSchaefer)
library(ggseg)
library(ggplot2)
ggseg3d(atlas = schaefer7_400_3d, surface = "inflated") %>% 
  pan_camera("right lateral")

ggsegDesterieux::desterieux
ggsegDesterieux::desterieux_3d
ggsegAal::aal_3d


ats_info=desterieux
someData = tibble(
  label = ats_info$data$label,
  p = sample(seq(0,.5,.001), length(ats_info$data$label))
) %>% 
  brain_join(desterieux) %>% 
  reposition_brain(hemi ~ side) %>% 
  ggplot() + 
  geom_sf(aes(fill = p))


# Enable this universe
options(repos = c(
  ggseg = 'https://ggseg.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('ggsegYeo2011')
install.packages('ggseg')
library(ggsegYeo2011)
ats_info=yeo17
someData = tibble(
  label = ats_info$data$label,
  p = sample(seq(0,.5,.001), length(ats_info$data$label))
)
someData %>% 
  brain_join(yeo17) %>% 
  reposition_brain(hemi ~ side) %>% 
  ggplot() + 
  geom_sf(aes(fill = p))


# 注意此处数据结果和2d绘图数据的结构略微不同
someData = aal_3d %>% 
  filter(surf == "inflated" & hemi == "right") %>% 
  tidyr::unnest(ggseg_3d) %>% 
  ungroup() %>% 
  select(region) %>% 
  na.omit() %>% 
  mutate(p = sample(seq(0,.5, length.out = 1000 ), nrow(.)) %>% 
           round(2)) 

ggseg3d(.data = someData, 
        atlas = aal_3d,
        colour = "p", text = "p") %>% 
  remove_axes() %>% #去网格
  pan_camera("right medial")
