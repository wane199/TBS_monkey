# https://mp.weixin.qq.com/s?__biz=MzIzOTQ4NjAwOQ==&mid=2247503138&idx=1&sn=13f7bb12d682dd515798dbc8b8eaaa0a&chksm=e92bd874de5c5162441dbdf65b85ed70bfa86ac3f07f1f7fc9cf122e7db4dfec666d63263359&mpshare=1&scene=1&srcid=0911ak6lRZ2Q1jrU8elRuzYR&sharer_sharetime=1662873400498&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
# R|ggsegExtra绘制Desterieux统计结果
# http://learning-archive.org/wp-content/uploads/2022/01/%E4%BD%BF%E7%94%A8ggseg%E8%BF%9B%E8%A1%8C%E5%8F%AF%E8%A7%86%E5%8C%96.pdf
library(ggseg3d) # Desikan模板
# vignette("ggseg3d")
library(ggsegAal)
library(dplyr)
library(ggplot2)
library(ggsegExtra)
ggseg_atlas_repos()

print(aal)
force(aal_3d)
print(aal_3d)
print(dk_3d)

ggseg(atlas=dk, mapping=aes(fill=region))
ggseg(atlas=aseg, mapping=aes(fill=label))
ggseg(atlas=aal, mapping=aes(fill=region))
ggseg3d(atlas=glasser_3d)
ggseg3d(hemisphere = "left")
ggseg3d(surface = "inflated")
ggseg3d(show.legend = FALSE)
ggseg3d(atlas="aseg_3d") %>%
  add_glassbrain("left")
ggseg3d(atlas="dk_3d") %>%
  add_glassbrain("left")

repo=ggseg_atlas_repos("yeo", ignore.case = TRUE)
install_ggseg_atlas(repo$Package)

p <- ggseg3d(atlas=dk_3d) %>% 
  remove_axes() %>% 
  pan_camera("right lateral")
p
p1 <- ggseg3d(atlas=aal_3d) %>% 
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
ggsegYeo2011::yeo17_3d
ggsegAal::aal_3d

ats_info=aal
someData = tibble(
  label = ats_info$data$label,
  p = sample(seq(0,.5,.001), length(ats_info$data$label))
) %>% 
  brain_join(aal) %>% 
  reposition_brain(hemi ~ side) %>% 
  ggplot() + 
  geom_sf(aes(fill = p))

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
someData = glasser_3d %>% 
  filter(surf == "inflated" & hemi == "right") %>% 
  tidyr::unnest(ggseg_3d) %>% 
  ungroup() %>% 
  select(region) %>% 
  na.omit() %>% 
  mutate(p = sample(seq(0,.5, length.out = 1000 ), nrow(.)) %>% 
           round(2)) 
ggseg3d(.data = someData, 
        atlas = glasser_3d,
        colour = "p", text = "p") %>% 
  remove_axes() %>% #去网格
  pan_camera("right medial")
