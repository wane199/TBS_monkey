# Cleveland's Dot Plots
# Load data
library(ggpubr)
data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
df$name <- rownames(df)
head(df[, c("wt", "mpg", "cyl")], 3)

# Basic plot
ggdotchart(df,
  x = "name", y = "mpg",
  ggtheme = theme_bw()
)

# Change colors by  group cyl
ggdotchart(df,
  x = "name", y = "mpg",
  group = "cyl", color = "cyl",
  palette = c("#999999", "#E69F00", "#56B4E9"),
  rotate = TRUE, # 横竖放置
  sorting = "des", # des/asc
  ggtheme = theme_bw(),
  y.text.col = TRUE
)

ggdotchart(df,
  x = "name", y = "mpg",
  color = "cyl", # 按组别配色
  palette = c("#FF0000", "#00868B", "#E066FF"), # 配置调色板
  rotate = TRUE, # 横竖放置
  sorting = "descending", # 降序排列
  dot.size = 2, # 点的大小
  x.text.col = TRUE, # x轴上的文字按组别配色
  ggtheme = theme_pubr() # 设置主题
) +
  theme_cleveland() # 加入虚线网格

# Lollipop charts
ggdotchart(df,
  x = "name", y = "mpg",
  color = "cyl",
  palette = c("#FF4040", "#009ACD", "#00FA9A"), # 配置颜色
  sorting = "des", sort.by.groups = TRUE, # 按组升序排列
  add = "segments", # 有none或segments两种选项
  add.params = list(color = "lightgray", size = 2), # add的参数，有颜色、大小形状等
  group = "cyl",
  rotate = TRUE,
  dot.size = 4,
  ggtheme = theme_pubclean()
) +
  font("x.text", size = 8, vjust = 0.5)

ggdotchart(df,
  x = "name", y = "mpg",
  color = "cyl",
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  sorting = "descending",
  add = "segments",
  group = "cyl", # 按组排序
  rotate = TRUE, # 横竖放置
  dot.size = 6,
  label = round(df$mpg), # 加入mpg的值作为点标签
  font.label = list(
    color = "white", size = 9,
    vjust = 0.5
  ), # 调整标签参数
  ggtheme = theme_pubr()
)


# Plot with multiple groups
# +++++++++++++++++++++
# Create some data
df2 <- data.frame(
  supp = rep(c("VC", "OJ"), each = 3),
  dose = rep(c("D0.5", "D1", "D2"), 2),
  len = c(6.8, 15, 33, 4.2, 10, 29.5)
)
print(df2)

ggdotchart(df2,
  x = "dose", y = "len",
  color = "supp", size = 3,
  add = "segment",
  add.params = list(color = "lightgray", size = 1.5),
  position = position_dodge(0.3),
  palette = "jco",
  ggtheme = theme_pubclean()
)



# Load library
library(VennDiagram)
# Generate 3 sets of 200 words
set1 <- paste(rep("word_", 200), sample(c(1:1000), 200, replace = F), sep = "")
set2 <- paste(rep("word_", 200), sample(c(1:1000), 200, replace = F), sep = "")
set3 <- paste(rep("word_", 200), sample(c(1:1000), 200, replace = F), sep = "")

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1", "Set 2 ", "Set 3"),
  filename = "#14_venn_diagramm.png",
  output = TRUE
)

# Load library
library(VennDiagram)
# Generate 3 sets of 200 words
set1 <- paste(rep("word_", 200), sample(c(1:1000), 200, replace = F), sep = "")
set2 <- paste(rep("word_", 200), sample(c(1:1000), 200, replace = F), sep = "")
set3 <- paste(rep("word_", 200), sample(c(1:1000), 200, replace = F), sep = "")

# Prepare a palette of 3 colors with R colorbrewer:
library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1", "Set 2 ", "Set 3"),
  filename = "#14_venn_diagramm.png",
  output = TRUE,

  # Output features
  imagetype = "png",
  height = 480,
  width = 480,
  resolution = 300,
  compression = "lzw",

  # Circles
  lwd = 2,
  lty = "blank",
  fill = myCol,

  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",

  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)

# https://blog.csdn.net/weixin_41929524/article/details/86436232
library(eulerr)

v <- euler(c(
  TLE = 234, MRIneg = 98, 
  "TLE&MRIneg" = 21
))

par(cex.axis=5.0)
plot(v,
  fills = list(fill = c( "#b3cde3", "#fbb4ae","#ccebc5"), alpha = 0.8),
  labels = list(col = "white", font = 2),
  edges = FALSE,
  quantities = TRUE
)
