# R语言可视化（三十二）：金字塔图绘制
# https://www.jianshu.com/p/a758a4884c38
# 清除当前环境中的变量
rm(list = ls())
# 设置工作目录
getwd()
library(skimr)
# 读取数据
data <- read.csv("/home/wane/Desktop/EP/Structured_Data/MRIneg-98-3.csv", header = T)
View(data)
# 批量数值转因子
for (i in names(data)[c(37:46)]) {
  data[, i] <- as.factor(data[, i])
}

# 提取子集
dt <- data[37:46]
paste0(names(dt), collapse = " ")
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
library(plotrix)
mcol <- color.gradient(c(0, 1), c(1, 0.6), c(0.1, 0.6), 5)
fcol <- color.gradient(c(0, 1), c(1, 0.6), c(0.1, 0.6), 5)

# [colorspace](https://mycolor.space/)
library(RColorBrewer)
# 查看sequential连续型颜色画板，每个颜色画板中包含9种颜色
display.brewer.all(type="seq")
mcol <- colorRampPalette(brewer.pal(7,"Set2"))(7) ->fcol
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
  main = "MRI negative population distribution",
  lxcol = mcol, rxcol = fcol, ndig = 0,
  laxlab = c(0, 10, 20, 30), raxlab = c(0, 10, 20, 30),
  gap = 2.5, show.values = TRUE
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
par(las = 0) # las 参数控制x轴和y轴的刻度线上的标签与两条轴的防线，可选值为0,1,2,3


##################################
# Population Pyramids
# https://www.youtube.com/watch?v=WzJhWZQfhfU
rm(list = ls())
#install rCharts
#install.packages("devtools")
library(devtools)
#install_github('ramnathv/rCharts')

#load packages
#install.packages("reshape2")
library(reshape2)
library(rCharts)
#----------------------------------------------------------------
# PYRAMID CODE
#library(XML)
#library(reshape2)
#library(rCharts)
#library(plyr)

# Highcharts pyramid
hPyramid <- function(dat, year, colors = NULL) {
  ord <- 1:nrow(dat)
  dat <- cbind(dat, ord)
  dat$Male <- -1 * dat$Male
  
  dat$Age <- factor(dat$Age, levels = rev(dat$Age), labels = rev(dat$Age))
  
  keep <- c("Male", "Female", "Age")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars='Age' )
  
  h1 <- hPlot(
    y = 'Population', 
    x = 'Age', 
    type = 'bar', 
    data = dat.melt,
    group = 'Gender')
  
  h1$plotOptions(series = list(stacking = 'normal', pointPadding = 0, borderWidth = 0))
  
  h1$tooltip(formatter = "#! function() { return '<b>'+ this.series.name +', age '+ this.point.category +'</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);} !#")
  
  h1$legend(reversed = "true")
  
  if (max(dat.melt$Population >= 1000000)) {
    h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value) / 1000000) + 'M';} !#"), 
             title = list(enabled = TRUE, text = 'Population'))
  } else {
    h1$yAxis(labels = list(formatter = "#! function() { return (Math.abs(this.value) / 1000) + 'K';} !#"), 
             title = list(enabled = TRUE, text = 'Population'))
  }
  
  if (!is.null(colors)) {
    h1$colors(colors)
  }
  if (length(year) > 1) {
    stop('Right now, hPyramid only accepts one year')
  }
  
  h1$exporting(enabled = TRUE)
  
  h1
}

# NVD3 pyramid
nPyramid <- function(dat, year, colors = NULL) {
  ord <- 1:nrow(dat)
  dat <- cbind(dat, ord)
  dat$Male <- -1 * dat$Male
  
  dat <- dat[order(rev(dat$ord)), ]
  
  keep <- c("Male", "Female", "Age")
  
  dat.sub <- dat[,keep]
  
  dat.melt <- melt(dat.sub, 
                   value.name='Population', 
                   variable.name = 'Gender', 
                   id.vars='Age' )
  
  dat.melt$abs <- abs(dat.melt$Population)
  
  n1 <- nPlot(
    y = 'Population', 
    x = 'Age', 
    group = 'Gender', 
    type = 'multiBarHorizontalChart', 
    data = dat.melt)
  
  # n1$xAxis(axisLabel = "Age") ## Need to work out label placement
  
  n1$chart(stacked = TRUE)
  
  n1$chart(tooltipContent = "#! function(key, x, y, e){
        var format = d3.format('0,000');
        return '<h3>' + key + ', age ' + x + '</h3>' + 
        '<p>' + 'Population: ' + format(e.point.abs) + '</p>'
        } !#")
  
  
  if (max(dat.melt$Population >= 1000000)) {    
    n1$yAxis(axisLabel = "Population",  
             tickFormat = "#! function(d) {
                          return d3.format(',.1f')(Math.abs(d) / 1000000) + 'M'
                          } !#")
  } else {
    n1$yAxis(axisLabel = "Population",  
             tickFormat = "#! function(d) {
                          return d3.format(',.0f')(Math.abs(d) / 1000) + 'K'
                          } !#")    
    
  }
  
  if (!is.null(colors)) {
    n1$chart(color = colors)
  }
  
  n1
}

#----------------------------------------------------------------
#2021 data
Age = c("Under 5 years","5 to 9 years", "10 to 14 years", "15 to 19 years",
        "20 to 24 years","25 to 29 years", "30 to 34 years", "35 to 39 years",
        "40 to 44 years","45 to 49 years","50 to 54 years","55 to 59 years",
        "60 to 64 years","65 to 69 years","70 to 74 years","75 to 79 years",
        "80 to 84 years","85 years and over")

Male = c(9552445, 10284656, 11155383, 11171156, 10925030, 11211933,
         11595901, 11321797, 10702211, 9867371, 10418635, 10438687, 10513756,
         8699905,  7204611,  4420293,  2695951,  2170982)
Female = c(9108800,  9726157, 10666109, 10652932, 10457613, 10888520, 11382784,
           11049601, 10659952,9914954, 10472757, 10702465, 11160126,  9651880,  8221808,
           5452475,  3582418,3791691)
dat2021 = as.data.frame(cbind(Age, Male, Female))
dat2021$Male = as.numeric(dat2021$Male)
dat2021$Female = as.numeric(dat2021$Female)

#dat2021 = read.csv("/Downloads/2021data.txt",sep="\t")
#Pyramid code expects a Male, Female, and Age column
#highcharts JS
h1 = hPyramid(dat2021, year=2021, colors = c('pink', 'blue'))
h1
h1$save("h1.html", standalone = TRUE)
# h1$save("h1.png", standalone = TRUE)

#NVD3 JS
n1 = nPyramid(dat2021, year=2021, colors = c('pink', 'blue'))
n1
n1$save("n1.html", standalone = TRUE)

