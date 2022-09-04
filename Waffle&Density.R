# 
library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)

# current verison
packageVersion("waffle")

data.frame(
  vals = c(79, 62, 56, 37),
  col = rep(c("left", "right"), 2),
  fct = c(
    rep("Female", 2),
    rep("Male", 2)
  )
) -> xdf

data.frame(
  vals = c(1, 2),
  col = rep(c("left", "right"), 1),
  fct = c(
    rep("Female", 1),
    rep("Male", 1)
  )
) -> xdf

xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(fill = fct, values = n)) +
  geom_waffle(n_rows = 20, size = 0.397, colour = "white", flip = T) +
  scale_fill_manual(
    name = NULL,
    values = c("#c68958", "#ae6056"),
    labels = c("Female", "Male")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle()


xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(label = fct, values = n)) +
  geom_pictogram(n_rows = 20, aes(colour = fct), flip = T, make_proportional = F) +
  scale_color_manual(
    name = NULL,
    values = c("#c68958", "#ae6056"),
    labels = c("Male", "Female")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("male", "female"),
    labels = c("Male", "Female")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.5))

xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(label = fct, values = n)) +
  geom_pictogram(
    n_rows = 20, size = 10, aes(colour = fct), flip = T,
    family = "FontAwesome5Brands-Regular"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#073f9c", "#f34323"),
    labels = c("Female", "Male")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("female", "male"),
    labels = c("Female", "Male")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme(legend.text = element_text(hjust = 0, vjust = 1))


xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(label = fct, values = n)) +
  geom_pictogram(n_rows = 20, aes(colour = fct), size = 10, flip = T, make_proportional = F) +
  scale_color_manual(
    name = NULL,
    values = c("#c68958", "grey"),
    labels = c("Male", "Male")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("male", "male"),
    labels = c("Male", "Male")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.5))

# 2d density plot/密度散点热图
library(dplyr)
library(viridis) # 使用viridis提供的翠绿色标度：scale_fill_viridis()
library(ggpointdensity) # 绘制密度散点图
library(ggpubr)
library(ggplot2)
library(cowplot) # 图形组合，可以自动对其坐标轴


dt <- read.csv("/home/wane/Desktop/TBS&Mon/BIAO/PTH1/CKD2.csv", header = T)
dt <- dt[-1]

p1 <- ggplot(data = dt, mapping = aes(x = age,
                                      y = TBS)) + 
  geom_pointdensity() + #密度散点图（geom_pointdensity）
  scale_color_viridis() + 
  #geom_smooth(method = lm) +  ##省略拟合曲线
  stat_cor(method = "spearman") + 
  xlab("x") + 
  theme(axis.title.x = element_text(size = 16,
                                    face = "bold", 
                                    vjust = 0.5, 
                                    hjust = 0.5))+
  ylab("y") + 
  theme(axis.title.y = element_text(size = 16,
                                    face = "bold", 
                                    vjust = 0.5, 
                                    hjust = 0.5))+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="serif")) +
  theme(legend.position='none')  ##去除legend
p1

# Show the area only
ggplot(dt, aes(x=age, y=TBS) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

# Area + contour
ggplot(dt, aes(x=age, y=TBS)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

# Using raster
ggplot(dt, aes(x=age, y=TBS) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

# Call the palette with a number
ggplot(dt, aes(x=age, y=TBS) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# The direction argument allows to reverse the palette
ggplot(dt, aes(x=age, y=TBS) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 
  # theme(
  #   legend.position='none'
  # )

# You can also call the palette using a name.
ggplot(dt, aes(x=age, y=TBS) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

p4 <- ggplot(data=dt,aes(age,TBS)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()

p5 <- ggplot(data = dt, mapping = aes(x = age, y = TBS)) +
  geom_pointdensity(adjust = .1) +
  scale_color_viridis() +
  labs(tag = "A", title = "adjust = 0.1") +
  theme_classic()

plot_grid(p5, p4, nrow = 1)


library(LSD)
heatscatter(dt$age,dt$TBS)

