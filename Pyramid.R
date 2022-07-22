# R语言可视化（三十二）：金字塔图绘制
# https://www.jianshu.com/p/a758a4884c38
# 清除当前环境中的变量
rm(list = ls())
# 设置工作目录
getwd()
library(skimr)
# 读取数据
data <- read.csv("/home/wane/Desktop/EP/Structured_Data/MRIneg-98-2.csv", header = T)
View(data)
# 批量数值转因子
for (i in names(data)[c(3:5, 7:16)]) {
  data[, i] <- as.factor(data[, i])
}

# 提取子集
dt <- data[7:16]
paste0(names(dt), collapse = ",")
summary(dt)
table(dt)
skim(dt)

# 使用plotrix包绘制人口金字塔图
# 安装并加载所需的R包
# install.packages("plotrix")
library(plotrix)

# 构建数据
L.pop <- c(25, 3, 1, 23, 2)
R.pop <- c(17, 2, 3, 28, 5)
lobelabels <- c("Frontal", "Parietal", "Occipital", "Temporal", "Insular")
mcol <- color.gradient(c(0, 1), c(1, 0.6), c(0.1, 0.6), 5)
fcol <- color.gradient(c(0, 1), c(1, 0.6), c(0.1, 0.6), 5)

library(RColorBrewer)
# 查看sequential连续型颜色画板，每个颜色画板中包含9种颜色
display.brewer.all(type="seq")
mcol <- colorRampPalette(brewer.pal(5,"Reds"))(5) ->fcol
mcol <- c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC", "#DDDDDD"
)->fcol
head(L.pop)
head(R.pop)
head(lobelabels)
head(mcol)
head(fcol)

# 使用pyramid.plot函数绘制人口金字塔图
par(mar = pyramid.plot(L.pop, R.pop,
  labels = lobelabels,
  top.labels = c("L-side", "Lobe", "R-side"), unit = "No.",
  main = "MRI negative population pyramid",
  lxcol = mcol, rxcol = fcol, ndig = 0,
  laxlab = c(0, 10, 20, 30), raxlab = c(0, 10, 20, 30),
  gap = 2.0, show.values = TRUE
))

# 使用DescTools包绘制人金字塔图
# 安装并加载所需的R包
# install.packages("DescTools")
library(DescTools)

# 构建示例数据
d.sda <- data.frame(
  kt_x =  c("Frontal", "Parietal", "Occipital", "Temporal", "Insular"),
  L.pop = c(25, 3, 1, 23, 2),
  R.pop = c(17, 2, 3, 28, 5)
)
head(d.sda)

# 使用PlotPyramid函数绘制人口金字塔图
PlotPyramid(
  lx = d.sda[, "L.pop"],
  rx = d.sda[, "R.pop"],
  ylab = d.sda$kt_x,
  col = c("lightslategray", "orange2"),
  border = NA, ylab.x = 0,
  xlim = c(-30, 30),
  gapwidth = NULL,
  cex.lab = 0.8, cex.axis = 0.8,
  xaxt = TRUE,
  lxlab = "L-side",
  rxlab = "R-side",
  main = "MRI negative population pyramid",
  space = 0.5, args.grid = list(lty = 1)
)

op <- par(mfrow = c(1, 3))

L.pop <- c(25, 3, 1, 23, 2)
R.pop <- c(17, 2, 3, 28, 5)
lobelabels <- c("Frontal", "Parietal", "Occipital", "Temporal", "Insular")
# 查看数据
head(L.pop)
head(R.pop)
head(lobelabels)

# 左侧图
PlotPyramid(L.pop, R.pop,
  ylab = lobelabels, space = 0,
  col = c("cornflowerblue", "indianred"),
  xlim = c(-40, 40),
  main = "MRI negative population pyramid",
  lxlab = "L-side", rxlab = "R-side"
)
# 中间图
PlotPyramid(L.pop, R.pop,
  ylab = age, space = 1,
  col = c("blue", "red"),
  xlim = c(-30, 30),
  main = "MRI negative population pyramid",
  lxlab = "L-side", rxlab = "R-side",
  gapwidth = 0, ylab.x = -5
)
# 右侧图
PlotPyramid(c(1, 3, 5, 2, 0.5), c(2, 4, 6, 1, 0),
  ylab = LETTERS[1:5], space = 0.3,
  col = rep(rainbow(5), each = 2),
  xlim = c(-30, 30), args.grid = NA,
  cex.names = 1.5, adj = 1,
  lxlab = "L-side", rxlab = "R-side",
  gapwidth = 1, ylab.x = -8, xaxt = "n"
)
par(las = 0)
par(las = 3)
