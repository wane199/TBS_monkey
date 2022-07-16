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
  filename = "venn_diagramm.png",
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
  TLE = 220, MRIneg = 98,
  "TLE&MRIneg" = 20
))

par(cex.axis = 5.0)
plot(v,
  fills = list(fill = c("#b3cde3", "#fbb4ae", "#ccebc5"), alpha = 0.8),
  labels = list(col = "white", font = 2),
  edges = FALSE,
  quantities = TRUE
)


# 添加统计图表及文本信息
# https://zhuanlan.zhihu.com/p/77167731
# 密度图
library(ggpubr)
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS.csv")
psych::describe(dt)
dt$oneyr <- as.factor(dt$oneyr)
dt$Sex <- as.factor(dt$Sex)
dt <- base::transform(dt, age = Surgmon / 12)
density.p <- ggdensity(dt,
  x = "age",
  fill = "oneyr", palette = "jco"
)
# Sepal.Length描述性统计
stable <- desc_statby(dt,
  measure.var = "age",
  grps = "oneyr"
)
stable <- stable[, c("oneyr", "length", "mean", "sd")]
# 设置table的主题
stable.p <- ggtexttable(stable,
  rows = NULL,
  theme = ttheme("mOrange") # ttheme(): customize table theme, mBlue/classic
)
#  text 信息
text <- paste("iris data set gives the measurements in cm",
  "of the variables sepal length and width",
  "and petal length and width, reScatter_plotsectively,",
  "for 234 flowers from each of 2 Scatter_plotsecies of iris.",
  "The Scatter_plotsecies are Iris setosa, and virginica.",
  sep = " "
)
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")
# 组图展示，调整高度和宽度
ggarrange(density.p, stable.p, text.p,
  ncol = 1, nrow = 3,
  heights = c(1, 0.5, 0.3)
)

# 子母图展示
density.p + annotation_custom(ggplotGrob(stable.p),
  xmin = 40, ymin = 0.05,
  xmax = 60
)


# Raincloud
library(ggdist)
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS.csv")
dt <- base::transform(dt, age = Surgmon / 12)
# pdf("/media/wane/wade/EP/EPTLE_PET/CN_PET_csv/raincloud.pdf",width=20, height=10)
ggplot(data = dt, aes(y = age, x = factor(oneyr), fill = factor(oneyr))) +
  ggdist::stat_halfeye(adjust = 0.30, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.15, outlier.color = NA) + theme_classic() +
  ggdist::stat_dots(side = "left", justification = 1.1)
# dev.off()
con <- subset(monk, monk$Group == "con")
sub <- subset(monk, monk$Group == "sub")
summary(con)
summary(sub)
psych::describe(sub)
glimpse(sub)

