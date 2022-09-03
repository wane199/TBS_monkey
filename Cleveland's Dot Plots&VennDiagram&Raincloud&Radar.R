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
  supp = c("VC", "OJ", "D0.5", "D1", "D2"), 
  disease = c("Stroke", "AD", "Migraine","CNS Ca", "Epilepsy"), 
  rank = c(60, 50, 40, 30, 20)
)
print(df2)

ggdotchart(df2,
  x = "disease", y = "rank",
  color = "supp", size = 3,
  add = "segment", rotate = TRUE, # 横竖放置
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


# [添加统计图表及文本信息](https://zhuanlan.zhihu.com/p/77167731)
# [可视化神器ggstatsplot = 绘图+统计](https://www.jianshu.com/p/ca566bc17ba7)
library(ggpubr)
library(readxl)
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/PET-TLE234-radscore-RCS.csv")
dt <- read_xlsx('/home/wane/Desktop/EP/Structured_Data/2014-2019TLE220-2.xlsx')
psych::describe(dt)
str(dt)
dt$side <- factor(dt$side, levels = c(1,2),labels = c('Left', 'Right'))
dt$Sex <- factor(dt$Sex)
                 # , levels = c(1,0),labels = c('Male', 'Female'))
aggregate(dt$Sex, by=list(type=dt$side, dt$Sex),length)
dt$oneyr <- as.factor(dt$oneyr)
dt <- base::transform(dt, age = Surgmon / 12)
density.p <- ggdensity(dt,
  x = "Age", rug = T, xlim = c(2,60),
  fill = "side", palette = "Tableau"
)
# Sepal.Length描述性统计
stable <- desc_statby(dt,
  measure.var = "Age",
  grps = "side"
)
stable <- stable[, c("side", "length", "mean", "sd")]
# 设置table的主题
stable.p <- ggtexttable(stable,
  rows = NULL, theme = ttheme("classic")
  # theme = ttheme(
  #   colnames.style = colnames_style(fill = "white"),
  #   tbody.style = tbody_style(fill = get_palette("grey", 6)))
  # ttheme(): customize table theme, mBlue/classic
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
  xmin = 55, ymin = .05,
  xmax = 50
)

set.seed(123)
# as a default this function outputs a correlalogram plot
ggstatsplot::ggcorrmat(
  data = ggplot2::msleep,
  corr.method = "robust", # correlation method
  sig.level = 0.001, # threshold of significance
  p.adjust.method = "holm", # p-value adjustment method for multiple comparisons
  cor.vars = c(sleep_rem, awake:bodywt), # a range of variables can be selected
  cor.vars.names = c(
    "REM sleep", # variable names
    "time awake",
    "brain weight",
    "body weight"
  ),
  matrix.type = "upper", # type of visualization matrix
  colors = c("#B2182B", "white", "#4D4D4D"),
  title = "Correlalogram for mammals sleep dataset",
  subtitle = "sleep units: hours; weight units: kilograms"
)

# Raincloud
library(ggdist)
library(ggplot2)
library(readxl)
dt <- read_excel("/media/wane/Data/CN/CN_T1.xlsx")
dt <- read.csv('/media/wane/Data/CN/t1popu.csv')
dt <- base::transform(dt, Age = Surgmon / 12)
dt <- base::transform(dt, Age_of_onset = Onsetmon / 12)
dt <- base::transform(dt, Duration_of_epilepsy = Durmon / 12)
dt$side <- factor(dt$side, levels = c('L','R'),labels = c('Left', 'Right'))
psych::describe(dt$age)
dt$Sex <- factor(dt$Sex, levels = c(1,0),labels = c('Male', 'Female'))
dt$Sex <- factor(dt$Sex)
aggregate(dt$Sex, by=list(type=dt$side, dt$Sex),length)
# pdf("/media/wane/wade/EP/EPTLE_PET/CN_PET_csv/raincloud.pdf",width=20, height=10)
ggplot(data = dt, aes(y = Age, x = factor(side), fill = factor(side))) +
  ggdist::stat_halfeye(adjust = 0.50, justification = -0.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.15, outlier.color = NA) + theme_classic() +
  ggdist::stat_dots(side = "left", dotsize = 0.5, justification = 1.1)
# dev.off()
con <- subset(dt, dt$Age < 18)
sub <- subset(monk, monk$Group == "sub")
dt$Sex <- factor(dt$Sex)
summary(dt)
psych::describe(dt$age)
glimpse(sub)
write.csv(dt, file = "/media/wane/Data/CN/TLE220.csv")

# https://zhuanlan.zhihu.com/p/261741176
library(ggplot2)
library(ggannotate)
dt <- read.csv("/home/wane/Desktop/EP/Structured_Data/RAW/MRIneg-98-0.csv")

p <- ggplot(data = dt,aes(x=Age,fill=cut(Age,breaks = c(2,18,56)))) + 
  theme_classic() + scale_x_continuous(breaks=seq(2,56,2)) +
  ggtitle("TLE Patients Age Distribution") +
  xlab("Age(years)") + ylab("Distribution") + #labs(fill='Age(years)') + 
  geom_vline(aes(xintercept=18), colour = "#990000", linetype="dashed") +
  geom_histogram(bins=45,show.legend = F)  
p
ggannotate(p)

ggplot(data = dt,aes(x=Age,y=..density..))+
  geom_histogram(bins = 50)+
  geom_density(size=1)
ggplot(data = dt,aes(x=Age,fill=cut(Age,breaks = c(0,18,60))))+
  geom_histogram(bins=40)+
  scale_fill_discrete() +
  ggtitle("MRI Negative Epilepsy Patients Age Distribution")+
  xlab("Age") + ylab("Distribution")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0,10),breaks = c(5))


# https://cloud.tencent.com/developer/article/1801036
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/
library(fmsb)
# Demo data
exam_scores <- data.frame(
  row.names = c("BLS-Siamese net", "Siamese net","RF(radiomics)","KNN(radiomics)",
                "LR(radiomics)","Senior level","Junior level"),
  Accuracy = c(.961, .956, .811, .632, .661, .811, .586),
  F1_score = c(.971, .968, .876, .706, .746, .890, .723),
  Precision = c(.955, .944, .938, .756, .745, .935, .910),
  Specificity = c(.907, .883, .764, .573, .489, .458, .458),
  Sensitivity = c(.988, .993, .835, .661, .746, .850, .600),
  AUC = c(.969, .949, .855, .855, .676, .643, .532)
)
exam_scores
# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Accuracy = c(1, 0.00), F1_score = c(1, 0.00), Precision = c(1, 0.00), 
  Specificity = c(1, 0.00), Sensitivity = c(1, 0.00), AUC = c(1, 0.00)
)
rownames(max_min) <- c("Max", "Min")
# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df
# 使用radarchart函数绘制雷达图
radarchart(df, caxislabels = c("10%", "", "", "", "100%"),
           axistype = 1, 
           vlcex = 1.0, # 设置标签的字体粗细大小
           vlabels = c(
             "Accuracy", "AUC",
             "Sensitivity", "Specificity",
             "Precision","F1-score"
           ),
           title = "PET",
           pcol = topo.colors(10))
legend(x=1.5, y=1, legend = rownames(df[-c(1,2),]), 
       bty = "n", pch=20, col = topo.colors(10),
       text.col = "black", cex=0.80, pt.cex=3.0)

opar <- par() 
# Define settings for plotting in a 3x4 grid, with appropriate margins:
par(mar = rep(0.8,4))
par(mfrow = c(3,3))
# Produce a radar-chart for each student
p1 <- for (i in 3:nrow(df)) {
  radarchart(
    df[c(1:2, i), ],#caxislabels = c("10%", "", "", "", "100%"),
    pfcol = c(rgb(0.2,0.5,0.5,0.2),alpha=0.1),
    pcol= c(5), plty = 4, plwd = 2,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
    title = row.names(df)[i]
  )
}


# col = c("#00AFBB", "#E7B800", "#FC4E07","#E69F00", "#56B4E9"), 
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(5, "BrBG")
colors_border <- coul
library(scales)
colors_in <- alpha(coul, 0.7)
radarchart(df, caxislabels = c("0%", "", "", "", "100%"),
           axistype = 1, axislabcol = "grey", 
           vlcex = 1, # 设置标签的字体粗细大小
           vlabels = c(
             "Accuracy", "AUC",
             "Sensitivity", "Specificity",
             "Precision","F1-score"
           ),
           title = "'BLS-Siamese net' vs 'Siamese net' in T1",
           pcol = colors_in)
# Add a legend
legend(x=1.5, y=1, legend = rownames(df[-c(1,2),]), 
       bty = "n", pch=20, col = colors_in,
       text.col = "black", cex=0.80, pt.cex=3.0)
# Add an horizontal legend
# x = "right", legend = rownames(df[-c(1,2),]), horiz = TRUE,
radarchart(df,
           axistype = 1, # 设定axes的类型,1 means center axis label only
           seg = 5, # 设定网格的数目
           plty = 1, # 设定point连线的线型
           pcol = colors_in,
           vlabels = c(
             "Accuracy", "AUC",
             "Sensitivity", "Specificity",
             "Precision","F1-score"
           ),
           title = "'BLS-Siamese net' vs 'Siamese net' in PET",
           vlcex = 1 # 设置标签的字体粗细大小
)
radarchart(df,
           axistype = 3, pty = 16, plty = 2,
           axislabcol = "grey", na.itp = FALSE,
           title = "(no points, axis=3, na.itp=FALSE)"
)
radarchart(df,
           axistype = 1, plwd = 1:3,  centerzero = TRUE,
           seg = 5, caxislabels = c("0%", "", "", "", ""),
           vlabels = c(
             "Accuracy", "AUC",
             "Sensitivity", "Specificity",
             "Precision","F1-score"
           ),
           title = "'BLS-Siamese net' vs 'Siamese net' in PET & T1"
)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}
# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(df, caxislabels = c(0, 5, 10, 15, 20))
par(op)

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

