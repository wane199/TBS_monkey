# B_A plot
rm(list = ls())
dev.off() # clear plots
cat("\014") # ctrl+L
? cat
pacman::p_load(psych)
pacman::p_unload()
pacman::p_help(ggseg, web = TRUE) # web=TRUE, build.pdf
library(readxl)
library(ggplot2)
library(ggExtra)
set.seed(123)

BAdata2 <-
  read_xlsx("C:/Users/wane1/Documents/file/TBS&Mon/Monkey/Ziqing/vali.xlsx")
# BAdata2 <- BAdata2[,c(-2,-5)]
summary(BAdata2)

library(pastecs)
options(digits = 3) # 设定三位小数
Sys.setlocale(category = "LC_ALL", locale = "Chinese") # 将本地语言默认为中文
dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\sci\\aiep\\Kfold.csv")
stat.desc(dt, norm = TRUE)
summary(dt)
# 宽数据转长数据
library(tidyr) # 使用的gather & spread
library(reshape2) # 使用的函数 melt & dcast
# 使用gather函数将宽数据gd1转换为长数据gd1_long
dt_long <- gather(dt, Kfold, Parameter, AUC_12月:Brier_36月)

# 使用melt 函数将宽数据gd1转换为长数据gd1_long1
dt1_long <- melt(dt,
  id.vars = c("Kfold"), # 需要保留不参与聚合的变量,
  # measure.vars = c('AUC_12月','Brier_36月'), #用于聚合的变量,
  variable.name = "Parameter",
  value.name = "value"
)
# ps: id_vars和 measure.vars只需要制定一个即可;另外一个默认是除指定的变量外的所有变量.

# 使用spread函数将gd1_long长数据转换为宽数据gd1_wide
gd1_wide <- spread(gd1_long, year, gdp) # year为需要分解的变量，gdp为分解后的列的取值

# 使用dcast函数将gd1_long长数据转换为宽数据gd1_wide1
gd1_wide1 <- dcast(gd1_long1, 地区 ~ gd1_long1$year, value.var = "gdp")

names(dt1_long)
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) {
      sum(!is.na(x))
    } else {
      length(x)
    }
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars,
    .drop = .drop,
    .fun = function(xx, col) {
      c(
        N = length2(xx[[col]], na.rm = na.rm),
        mean = mean(xx[[col]], na.rm = na.rm),
        sd = sd(xx[[col]], na.rm = na.rm)
      )
    },
    measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
write.csv(dt1_long, "C:\\Users\\wane1\\Documents\\file\\sci\\aiep\\Kfold.csv", fileEncoding = "UTF-8")
# 分组汇总
dt.summary <- dt %>%
  group_by(Age, Sex) %>% # Sex
  summarise(
    sd = sd(TBV),
    TBV = mean(TBV)
  )


library(ggplot2)
library(tidyverse)
library(Rmisc)
dt1_long <- as_data_frame(dt1_long)
carss <- summarySE(as_tibble(dt), measurevar = "value", groupvars = c("Para", "month", "Group"))
# carss$月 <- paste(rep(c("12月","24月","36月"),3),carss$月,sep="") #某一列添加X
pd <- position_dodge(1.2) # move them .05 to the left and right
ggplot(carss, aes(x = month, y = value, colour = Group, fill = Para)) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.05)) +
  xlab("随访时间(月)") +
  ylab("") +
  theme_classic() +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd)

library(ggstatsplot)
ggbetweenstats(dt, Group, by = Para, value)

# grouped boxplot
ggplot(dt, aes(x = Para, y = value, fill = Group)) +
  stat_compare_means(aes(group = Group), label = "p.format") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.0, 1.2, 0.05), limits = c(0.00,1.00)) +
  theme_classic() +
  ylab("") +
  xlab("评价指标") +
  geom_violin()

library(tidyverse)
library(ggpubr)
dt %>%
  mutate_at(vars(Group, Para), as.factor) %>%
  ggplot(aes(Para, value, fill = Group)) +
  geom_boxplot(aes(fill = Group)) +
  geom_violin(aes(fill = Group))+ 
  theme_classic() +
  ylab("") +
  xlab("评价指标") +
  # scale_y_continuous(expand = c(0, 0), breaks = seq(0.0, 1.2, 0.05), limits = c(0.00,1.00)) +
  # annotate(geom = "segment",x=1,xend = 2,y=43,yend=43,size=2,color="red")+
  # annotate(geom = "segment",x=1,xend = 1,y=43,yend=35,size=2,color="red")+
  # annotate(geom = "text",x=1.5,y=44,label="***",size=10,color="black")+
  # annotate(geom = "segment",x=1,xend = 3,y=33,yend=33,size=2,color="red")+
  # annotate(geom = "segment",x=1,xend = 1,y=33,yend=28,size=2,color="red")+
  # annotate(geom = "text",x=2.8,y=34,label="**",size=10,color="black")+
  stat_compare_means(aes(group = Group), label = "p.format")

library(showtext)
font_add_google(name = "Gochi Hand", family = "gochi")
font_add_google(name = "Schoolbell", family = "bell")
showtext_auto() # 后面字体均可以使用导入的字体


library(ggplot2)
library(ggprism)
ggplot(dt, aes(Para,value,fill = Group))+
  geom_violin(aes(fill = Group))+ 
  # geom_boxplot(aes(fill = Group))+
  # theme_prism(base_size =15,border =T)+  theme(legend.position = "none")     
  theme_classic() +   
  ylab("") +
  xlab("评价指标") +
  theme(
    plot.margin = margin(t = 10 # 顶部边缘距离
                         )) + # 左边边缘距离
  # scale_x_discrete(breaks = c(12,24,36)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.0, 1.2, 0.05), limits = c(0.00,1.00)) +
  # stat_compare_means(aes(group = ))
  stat_compare_means(aes(group = Group), label = "p.format",method = "t.test")


BAdata2 <-
  transform(
    BAdata2,
    diff = Measured_TFM - Predicted_TFM,
    meand = (Measured_TFM + Predicted_TFM) / 2
  )

BAplot <- ggplot(BAdata2, aes(meand, diff)) +
  geom_point(
    size = 2,
    shape = 21,
    colour = "blue"
  ) +
  geom_hline(
    yintercept = 0,
    lty = 3,
    lwd = 1,
    color = "pink"
  ) +
  geom_hline(
    yintercept = mean(BAdata2$diff),
    lty = 1,
    lwd = 1,
    color = "black"
  ) +
  geom_hline(
    yintercept = mean(BAdata2$diff) + 1.96 * sd(BAdata2$diff),
    linetype = 3,
    color = "grey"
  ) +
  geom_hline(
    yintercept = mean(BAdata2$diff) - 1.96 * sd(BAdata2$diff),
    linetype = 3,
    color = "grey"
  ) +
  labs(x = "mean of TFM and TAT4M", y = "") +
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "black", size = 1),
    axis.line.y = element_line(color = "black", size = 1)
  ) +
  # theme(axis.text= element_text(face="bold",  size=12),
  #       axis.title.x=element_text(size=18,face="bold"),
  #       axis.title.y =element_text(size=18,face="bold"))+
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  ) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-1.0, 1.5)) +
  geom_text(
    x = 4.8,
    y = 0.9,
    label = "+1.96SD=0.83",
    size = 5
  ) +
  geom_text(
    x = 4.8,
    y = -0.9,
    label = "-1.96SD=-0.77",
    size = 5
  ) +
  geom_text(
    x = 4.8,
    y = 0.1,
    label = "Mean=0.03",
    size = 5
  )
# geom_text(x=155, y=-18, label="Mean=0.03 \n 95%CI(-16.9,-0.9) ",size=5)
BAplot
ggsave(BAplot,
  file = "/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/Ziqing/B_A_1.tiff",
  device = "tiff",
  dpi = 300
)
# Bland-Altman Plot的packages还有qwraps2、PairedData和blandr
ggMarginal(BAplot,
  type = "histogram",
  bins = 20,
  fill = "grey"
)
# type的类型包括density, histogram, boxplot, violin
# BAdata+theme( panel.background = element_rect(fill = "white"))

# 拟合
library(ggpmisc)
library(ggpubr)
colnames(BAdata2)
my.formula <- y ~ x
p0 <- ggplot(BAdata2, aes(Predicted_TFM, Measured_TFM)) +
  geom_point() +
  stat_cor(aes(), label.x = 4) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 5, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  stat_smooth(
    method = lm,
    se = TRUE,
    formula = my.formula
  ) +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p0
# https://cloud.tencent.com/developer/article/1854758
lm <-
  ggplot(data = BAdata2, aes(x = Predicted_TFM, y = Measured_TFM)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    formula = my.formula
  ) +
  stat_poly_eq(
    formula = my.formula,
    aes(label = paste(..eq.label..,
      ..rr.label..,
      sep = "~~~"
    )),
    parse = TRUE
  ) +
  geom_point(
    size = 2,
    shape = 21,
    colour = "black"
  ) +
  coord_cartesian(
    ylim = c(0, 4.000),
    xlim = c(0, 4.000)
  ) +
  # scale_y_continuous(breaks = c(0,100,200,300))+
  # scale_x_continuous(breaks = c(0,25,50,75,100,105))+#x轴比上个图多个105
  theme_bw() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  ) +
  geom_smooth(
    method = "lm",
    color = "black",
    # fill="black",
    alpha = .7,
    size = 0.7,
    se = T,
    formula = y ~ x
  ) +
  theme(panel.grid = element_blank())
ggsave(lm,
  file = "/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/Ziqing/lm_TFM.tiff",
  device = "tiff",
  dpi = 300
)
