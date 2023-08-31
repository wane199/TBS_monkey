#### 如何建立非线性回归预测模型(https://zhuanlan.zhihu.com/p/101906049) ####
# https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0
# https://mp.weixin.qq.com/s?__biz=MzI1NjM3NTE1NQ==&mid=2247484446&idx=1&sn=487c68752949698fea9102b15fc5d2c0&chksm=ea26e402dd516d14667cc1171151d3c1527e9a32fed6f390813d31ddb87dac815a50b4194735&mpshare=1&scene=1&srcid=0612zm7B4uWhTaFCwvNQLrEs&sharer_sharetime=1655014649840&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
##### 加载包 #####
rm(list = ls())
library(ggplot2)
library(segmented)
library(splines)
library(Hmisc)
library(rms)
library(mgcv)
library(caret)
library(readxl)
library(dlookr)
library(DataExplorer)
library(ggpmisc)
library(ggsci)
options(digits = 3) # 限定输出小数点后数字的位数为3位
theme_set(theme_classic() + theme(legend.position = "bottom"))
# 读取数据
dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\T1_TBV.csv", sep = ";", fileEncoding = "GBK") # , sep = '\t'
dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\PET_SUVr.csv", fileEncoding = "GBK", sep = ";")

dt <- read.csv("C:\\Users\\Administrator\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\T1_TBV.csv", fileEncoding = "GBK", sep = "\t")
dt1 <- read.csv("C:\\Users\\Administrator\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\PET_SUVr.csv", fileEncoding = "GBK", sep = ",")

dt <- dt[c(-1, -2, -3)]
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
summary(dt)

##### 数据预处理 #####
library(gt)
library(dplyr)
dt %>%
  slice_head(n = 4) %>%
  gt() # print output using gt
glimpse(dt)
library(visdat)
vis_dat(dt, palette = "qual") # cb_safe

dt %>%
  count(Age, Sex,
    sort = F
  )

# 年龄段分组汇总
dt.summary <- dt %>%
  group_by(Age, Sex) %>% # Sex
  summarise(
    sd = sd(TBV),
    TBV = mean(TBV)
  )

# TLM <- read_excel("/home/wane/Desktop/TBS/TLMey/BMC.xlsx")
# 数据探索
TLM <- TLM[c(-1, -2, -5)]
summary(TLM)
glimpse(TLM)
sum(!is.na(TLM))
fix(TLM)
TLM <- base::transform(TLM, LM_L3 = LM_L3 / 12)
TLM <- TLM[, 2:8]
N <- names(TLM)
f <- as.formula(paste(" ~", paste(N[!N %in% "TBVBW"], collapse = " + ")))
f
TLM$Gender <- factor(TLM$Gender)
eda_pLM_L3d_report(TLM, output_file = "/home/wane/Desktop/TBS&Mon/TLMey/Ziqing/ZZQ.pdf")
cover <- file.path(system.file(packLM_L3 = "dlookr"), "report", "/home/wane/Desktop/TBS&Mon/TLMey/Ziqing/hist-B.png")
eda_pLM_L3d_report(TLM,
  cover_img = cover, title_color = "gray", target = "Gender",
  output_file = "/home/wane/Desktop/TBS/TLMey/Voxel_EDA.pdf"
)
eda_pLM_L3d_report(TLM,
  target = "Gender",
  output_file = "/home/wane/Desktop/TBS/TLMey/Voxel_EDA.pdf"
)
TLM <- TLM[, 10:15]
create_report(TLM)

# 异常值检测, Draws Overview of Outliers (O3) Plots
library(OutliersO3)
boxplot(TLM$volume)
d <- as.data.frame(TLM[c(3, 6:7)])
car::outlierTest(TLM$volume)
c1 <- O3prep(d, method = c("HDo", "BAC", "DDC"), tolHDo = 0.025, tolBAC = 0.01, tolDDC = 0.05)
pPa <- O3prep(d, method = c("PCS", "adjOut"), tolPCS = 0.01, toladj = 0.01, boxplotLimits = 10)
c2 <- O3plotM(c1)
c2$nOut
c2$gpcp
c2$gO3
c2$outsTable

a2 <- O3prep(d, method = "PCS", tols = c(0.05), boxplotLimits = c(3)) # Identify outliers for different combinations of variables
a2 <- O3prep(d,
  k1 = 1, K = ncol(d), method = "HDo", tols = 0.05, boxplotLimits = c(6, 10, 12),
  tolHDo = 0.05, tolPCS = 0.01, tolBAC = 0.001, toladj = 0.05, tolDDC = 0.01, tolMCD = 0.000001
)
a3 <- O3plotT(a2)
a3$nOut
a3$gpcp
a3$gO3
a3$outsTable

# 雨云图(Raincloud)
library(ggdist)
TLM <- TLM[c(-1, -3)]
# pdf("/media/wane/wade/EP/EPTLE_PET/CN_PET_csv/raincloud.pdf",width=20, height=10)
ggplot(data = TLM, aes(y = L2_4, x = factor(Group), fill = factor(Group))) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = 0.2, outlier.color = NA) +
  ggdist::stat_dots(side = "left", justification = 1.1)
# dev.off()
con <- subset(TLM, TLM$Group == "con")
sub <- subset(TLM, TLM$Group == "sub")
summary(con)
summary(sub)
psych::describe(sub)

# 分层分析, 预测模型公平性评价
F <- subset(TLM, TLM$Sex == 0)
M <- subset(TLM, TLM$Sex == 1)

##### 散点图 #####
dd <- datadist(dt) # 为后续程序设定数据环境
options(datadist = "dd")
ggplot(dt, aes(Age, TBV)) +
  geom_point() # 绘制散点图
p <- ggplot() +
  geom_point(data = dt, mapping = aes(x = Age, y = Weight)) + # , colour = Sex
  theme_classic()
p
ggplot(dt, aes(Age, Weight, group = Age > 5.0, colour = Sex)) +
  geom_smooth() +
  geom_point(data = dt, mapping = aes(x = Age, y = Weight))
ggplot(data = dt, x = Age, y = Weight, group = Age > 5.0) + # , colour = Sex
  theme_classic() +
  geom_smooth()

##### 建立线性回归模型 ##### 
model.lm <- lm(Weight ~ Age, data = dt) # 构建线性回归模型 SUVr_whole_refPons
summary(model.lm) # 查看回归模型结果，
p1 <- ggplot(dt, aes(Age, Weight)) + # , colour = Sex
  geom_point() +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ x) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) + # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label),  sep = "~~~~")),
    formula = y ~ x, parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Weight~(Kg)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_refPons
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p1

##### 建立曲线方程(log,exp) #####
model.log <- lm(Weight ~ log(Age), data = dt) # 建立对数曲线方程
summary(model.log) # 查看模型概况
# 拟合曲线
p2 <- ggplot(dt, aes(Age, Weight)) + # , colour = Sex
  geom_point() +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ log(x)) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) + # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ log(x), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Weight~(Kg)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p2
model.log10 <- lm(Weight ~ log(Age), data = dt) # 建立指数曲线方程
summary(model.log10) # 查看模型概况
# 拟合曲线
ggplot(dt, aes(Age, Weight)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log10(x))

##### 建立分段回归模型 #####
# https://blog.csdn.net/weixin_40575651/article/details/107575012
library(segmented)
model.segmented <- segmented(model.lm, seg.Z = ~Age) # 构建分段回归模型
summary(model.segmented) # 查看模型概况
slope(model.segmented) # the slopes of the segmented relationship
# 查看拟合效果
plot(dt$Age, dt$Weight, pch = 1, cex = 1.5)
abline(a = coef(model.lm)[1], b = coef(model.lm)[2], col = "red", lwd = 2.5)
plot(model.segmented, col = "blue", lwd = 2.5, add = T)

p3 <- ggplot(dt, aes(Age, Weight)) + # , colour = Sex
  geom_point() +
  geom_smooth(
    data = dt, mapping = aes(x = Age, y = Weight), # , color = Sex
    method = "lm", formula = y ~ x + I((x - 11) * (x > 11))
  ) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) + # expand = c(0, 0),
  # scale_x_continuous(expand = c(0, 0), breaks = c(0, 1, 3, 5, 13, 20)) + # seq(0, 32, 1)
  # scale_y_continuous(expand = c(0, 0)) + scale_x_log10() +
  # coord_trans(x = squash_axis(0, 5, 0.40)) +
  # geom_vline(xintercept = 5.0, linetype = 2, color = "red") +
  # geom_vline(aes(xintercept = c(4.61)), colour = "#990000", linetype = "dashed") +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ x + I((x - 11) * (x > 11)), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Weight~(Kg)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_refPons
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p3

# 手动设置拐点，分三段回归
model.segmented2 <- segmented(model.lm, seg.Z = ~Age) # 构建分段回归模型
summary(model.segmented2) # 查看模型概况
model.segmented3 <- segmented(model.lm, psi = c(5, 20)) # 构建分段回归模型
summary(model.segmented3) # 查看模型概况
# 查看拟合效果
plot(TLM$LM_L3, TLM$TLM, pch = 1, cex = 1.5)
abline(a = coef(model.lm)[1], b = coef(model.lm)[2], col = "red", lwd = 2.5)
plot(model.segmented2, col = "blue", lwd = 2.5, add = T)
p4 <- ggplot(dt, aes(Age, Weight)) + # , colour = Sex
  geom_point() +
  geom_smooth(
    data = dt, mapping = aes(x = Age, y = Weight), # , color = Sex
    method = "lm", formula = y ~ x + I((x - 11.5) * (x > 11.5)) + I((x - 15.2) * (x > 15.2))
  ) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # geom_vline(xintercept = 5.0, linetype = 2, color = "red") +
  # geom_vline(aes(xintercept = c(5.6)), colour = "#990000", linetype = "dashed") +
  # geom_vline(aes(xintercept = c(26.2)), colour = "#990000", linetype = "dashed") +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ x + I((x - 11.5) * (x > 11.5)) + I((x - 15.2) * (x > 15.2)), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Weight~(Kg)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) 
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p4

##### 样条回归 #####
model.spline <- lm(SUVr_whole_refPons ~ rcs(Age, 5), data = dt) # 建立样条回归，设置3~5个节点。+ factor(dt$Sex)
summary(model.spline) # 查看模型概况
# new <- data.frame(Age = c(0.5,26.5,29.5))
pr <- predict(model.spline, newdata = dt, interval = "confidence")
pre <- cbind(dt[1],pr)

str(dt)
M <- dt %>% filter(Sex == "M")
Fe <- dt %>% filter(Sex == "F")
dd <- datadist(Fe)
options(datadist = "dd")
model.spline <- lm(SUVr_whole_refPons ~ rcs(Age, 5), data = Fe) # 建立样条回归，设置3~5个节点。+ factor(dt$Sex)
summary(model.spline) # 查看模型概况
# new <- data.frame(Age = c(0.5,26.5,29.5))
pr <- predict(model.spline, newdata = Fe, interval = "confidence")
pre <- cbind(Fe[1],pr)

# 样条回归拟合效果
p5 <- ggplot(dt, aes(Age, Weight)) + # , colour = Sex
  geom_point() +
  # geom_errorbar(aes(ymin = SUV_Whole - sd, ymax = SUV_Whole + sd), width = 0.1) +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 5)) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) +  # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ rcs(x, 5), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Volume~(cm^3)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p5

# 设定数据环境
library(dplyr)
str(dt)
M <- dt %>% filter(Sex == "M")
Fe <- dt %>% filter(Sex == "F")
dd <- datadist(Fe)
options(datadist = "dd")
fit <- ols(SUVr_whole_refPons ~ rcs(Age, 5), data = Fe) #  + Sex
summary(fit)
an <- anova(fit)
# 计算拟合值
# fml <- "TBV ~ rcs(Age, 5)" # + factor(Sex)
# source("C:\\Users\\wane\\Documents\\rdocu\\平滑曲线1\\get_cutoff_lm.R")
# source("C:\\Users\\wane1\\Downloads\\平滑曲线1\\get_cutoff_lm.R")
# cut_off <- get_cutoff_lm("Age", dt, fml)
# print(cut_off)
# Predict(fit, 0.5) # 生成预测值
# fun=exp
plot(Predict(fit, Age), anova = an, pval = T)
OLS1 <- Predict(fit,  Age, ref.zero = F)


H6 <- ggplot(dt, aes(Age, SUVr_whole_refPons)) + # , colour = Sex
  geom_point(aes(), alpha = 1.0, size = 2.5) +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 5)) + # colour = "black", 
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) +  # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ rcs(x, 5), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(SUVr_refPons))  + # Volume~(cm^3) Weight~(Kg) TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_refPons
  # annotate("point", x = 11.15, y = 7.48, shape = 16, size = 3, label = "Highest Point", vjust = -1.5) +
  geom_vline(xintercept = 3.83, colour = "black", linetype = "dashed") +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

H62 <- ggplot(dt, aes(Age, SUVr_whole_refPons, colour = Sex)) + # 
  geom_point(aes(colour = Sex,shape = Sex), alpha = 1.0, size = 2.5) +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 5)) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) +  # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ rcs(x, 5), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(SUVr_refPons))  + # Volume~(cm^3) Weight~(Kg) TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_refPons
  geom_vline(xintercept = 5.06, colour = "#00BFC4", linetype = "dashed") + 
  geom_vline(xintercept = 4.16, colour = "#F8766D", linetype = "dashed") + #  colour = "black", 
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

ggplot(dt, aes(Age, TBV)) + # , colour = Sex
  # geom_point(aes(), alpha = 1.0, size = 2.5) +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 5), colour = "black", linewidth = 1.8) + # colour = "black", 
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) +  # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ rcs(x, 5), parse = TRUE, size = 8,
  ) +
  xlab("Age (year)") +
  ylab(bquote(Volume~(cm^3)))  + # Volume~(cm^3) Weight~(Kg) TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_refPons
  # annotate("point", x = 11.15, y = 7.48, shape = 16, size = 3, label = "Highest Point", vjust = -1.5) +
  geom_vline(xintercept = 6.88, colour = "black", linetype = "dashed") +
  guides(fill = guide_legend(label.theme = element_text(size = 18, face = "bold"))) + 
  theme(
    axis.title = element_text(size = 23),
    panel.border = element_rect(fill=NA,color="black", linewidth=1.5, linetype="solid"),
    axis.text = element_text(size = 18, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

ggplot(dt1, aes(Age, whole)) + # , colour = Sex
  # geom_point(aes(), alpha = 1.0, size = 2.5) +
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 5), colour = "yellow", linewidth = 3) + # colour = "black", 
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) +  # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ rcs(x, 5), parse = TRUE, size = 8,
  ) +
  xlab("Age (year)") +
  ylab(bquote('Uptake Value'~(kBq/cc)))  + # Volume~(cm^3) Weight~(Kg) TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_refPons
  # annotate("point", x = 11.15, y = 7.48, shape = 16, size = 3, label = "Highest Point", vjust = -1.5) +
  scale_y_continuous(position= "right") +
  geom_vline(xintercept = 4.63, colour = "yellow", linetype = "dashed") +
  theme(    
    axis.title = element_text(size = 23),
    panel.border = element_rect(fill=NA,color="black", linewidth=1.5, linetype="solid"),
    axis.text = element_text(size = 18, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )


# triple in one
p51 <- ggplot(dt, aes(Age, Weight, colour = Sex)) +
  geom_point(aes(colour = Sex,shape = Sex), alpha = 1.0, size = 2.5) +
  theme_classic() +
  ylab(bquote(TBV/Weight~(cm^3/kg))) + # TBV(cm^3) TBV/BW(cm^3/kg) Weight(kg) SUVr_whole_refPons Whole(cm^3/kg) SUVr_whole_refPons(KBq/cc)
  stat_smooth(method = lm, formula = y ~ rcs(x, 5)) +
  # stat_smooth(method = lm, se = TRUE, colour = "black", formula = y ~ rcs(x, 3)) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ rcs(x, 5), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote('Uptake Value'~(kBq/cc)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

library(patchwork) # 拼图
H12 + H22 + H32 + H42 + H52 + H62 + plot_annotation(tag_levels = "a", theme = theme(plot.title = element_text(size = 16))) + plot_layout(ncol = 3) +
  plot_layout(guides = "collect") -> D2
D2
ggsave("./TBS/Monkey_fitting/1209.pdf", p, width = 20, height = 9, dpi = 900) # 保存为精度为600 dpi的tiff文件

# for循环
theme_set(theme_classic() + theme(legend.position = "bottom"))
plot_list <- list()
for (i in 3:ncol(dt)) {
  print(p <- ggplot(dt, aes_string(x = "Age", y = colnames(dt)[i], colour = "Side", fill = "Side", linetype = "Side")) +
    scale_x_continuous(limits = c(0, 30), expand = c(0, 0), breaks = seq(0, 30, 2)) +
    # geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 4), se = T) +
    stat_smooth(method = lm, formula = y ~ rcs(x, 3)) +
    geom_point(aes(colour = Side, shape = Side, fill = Side), size = 1.2, alpha = 0.5, shape = 21) +
    xlab("Age (year)") +
    ylab(bquote(SUVr_ref~Whole))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_ref~Whole
    scale_fill_brewer(palette = "Paired") +
    # scale_fill_npg() + scale_color_npg() +
    ggtitle(paste0(colnames(dt)[i])) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(    
      axis.title = element_text(size = 16, face = "bold"),
      # panel.border = element_rect(fill=NA,color="black", linewidth=1.5, linetype="solid"),
      axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.12, "cm"),
      axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
      axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
    ))
  plot_list[[i - 2]] <- p
}

for (i in 3:ncol(dt)) {
  print(p <- ggplot(dt, aes_string(x = "Age", y = colnames(dt)[i], colour = "Sex", fill = "Sex", linetype = "Sex")) +
          scale_x_continuous(limits = c(0, 30), expand = c(0, 0), breaks = seq(0, 30, 2)) +
          stat_smooth(method = lm, formula = y ~ rcs(x, 3)) +
          geom_point(aes(colour = Sex, shape = Sex, fill = Sex), size = 1.2, alpha = 0.5, shape = 21) +
          xlab("Age (year)") +
          ylab(bquote(SUVr_ref~Whole))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml) SUVr_ref~Whole
          scale_fill_npg() + scale_color_npg() +
          ggtitle(paste0(colnames(dt)[i])) +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(    
            axis.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.12, "cm"),
            axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
            axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
          ))
  plot_list[[i - 2]] <- p
}

wrap_plots(plot_list, byrow = T, ncol = 5) + plot_annotation(tag_levels = "a", theme = theme(plot.title = element_text(size = 16))) +
  plot_layout(guides = "collect")

# 不分组
ggplot(dt, aes(Age, Frontal.Lobe)) + # , colour = Sex
  theme_classic() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 5), colour = "Black", linewidth = 1.5) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(position = "right") + 
  xlab("Age (year)") +
  ylab(bquote(Volume~(cm^3)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    # panel.border = element_rect(fill=NA,color="black", linewidth=1.5, linetype="solid"),
    axis.text = element_text(size = 14, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.title.y = element_text(margin = margin(r = -2)),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

plot_list <- list()
for (i in 3:ncol(dt)) {
  print(p <- ggplot(dt, aes_string(x = "Age", y = colnames(dt)[i])) +
          scale_x_continuous(limits = c(0, 30), expand = c(0, 0), breaks = seq(0, 30, 2)) +
          scale_y_continuous(position = "right") + 
          stat_smooth(method = lm, formula = y ~ rcs(x, 3), colour = "yellow", linewidth = 1.5) +
          # geom_point(aes(), size = 1.5, shape = 21) + # colour = Sex, shape = Sex, fill = Sex
          xlab("Age (year)") +
          ylab(bquote(SUVr_refWhole))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
          scale_fill_brewer(palette = "Paired") +
          ggtitle(paste0(colnames(dt)[i])) +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(    
            axis.title = element_text(size = 13, face = "bold"),
            # panel.border = element_rect(fill=NA,color="black", linewidth=1.5, linetype="solid"),
            axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
            axis.title.y = element_text(margin = margin(r = -2)),
            axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
            axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
          ))
  plot_list[[i - 2]] <- p
}

# 拼图
library("patchwork")
wrap_plots(plot_list, byrow = T, ncol = 4) + plot_annotation(tag_levels = "a", theme = theme(plot.title = element_text(size = 16))) +
  plot_layout(guides = "collect")

##### Lowess/Loess函数建立局部加权回归 #####
model.lowess <- loess(dt$SUVr_whole_refPons ~ dt$Age) # 建立局部加权回归
summary(model.lowess) # 查看概况
print(model.lowess)
model.lowess$s
# 查看拟合
p8 <- ggplot(dt, aes(Age, SUVr_whole_refPons)) + # , colour = Sex
  geom_point() +
  # geom_errorbar(aes(ymin = SUVr_whole_refPons - sd, ymax = SUVr_whole_refPons + sd), width = 0.1) +
  theme_classic() +
  stat_smooth(method = loess, formula = y ~ x) +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) + # expand = c(0, 0),
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ x, parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Weight~(Kg)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p8

# rcssci(linear models with RCS splines were performed to explore the shape linear or nonlinear(U, inverted U,J,S,L,log,-log,temporary plateau shape)
library(rcssci)
data <- sbpdata
rcssci_linear(
  data = sbpdata, y = "sbp", x = "age", covs = c("status", "gender"), time = "time", ref.zero = F,
  prob = 0.1, filepath = "/Volumes/UNTITLED/"
) + # 默认prob = 0.5
  ggplot2::theme_classic()

##### 广义可加模型gam #####
# https://mp.weixin.qq.com/mp/appmsgalbum?__biz=MzI1NjM3NTE1NQ==&action=getalbum&album_id=2077935014574374912&scene=173&from_msgid=2247485011&from_itemidx=1&count=3&nolastread=1#wechat_redirect
model.gam <- gam(Weight ~ s(Age, k = 4, bs = "cs"), data = dt) # 建立gam模型 k = 4,k = 8,
summary(model.gam) # 查看模型概况
pr.gam <- predict(model.gam, dt) # 生成预测值
# 计算RSME和R方
data.frame(
  RMSE = RMSE(pr.gam, dt$Weight),
  R2 = R2(pr.gam, dt$Weight)
)
# 查看模型拟合情况
library(ggpmisc)
library(ggpubr)
my.formula <- y ~ s(x, k=3, bs = "cs")
ggplot(dt, aes(Age, Weight)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = y ~ s(x, bs = "tp"), parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(SUVr_refPons))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  geom_point(size = 0.05)
p9 <- ggplot(dt, aes(Age, Weight)) + #  colour = Sex
  geom_point() +
  # geom_errorbar(aes(ymin = SUV_Whole - sd, ymax = SUV_Whole + sd), fatten = 2.5, width = 0.2) +
  # stat_cor(aes(), label.x = 6) +
  stat_poly_eq(
    # aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = my.formula, parse = TRUE
  ) +
  xlab("Age (year)") +
  ylab(bquote(Weight~(Kg)))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 30, 1)) + # expand = c(0, 0),
  # scale_y_continuous(breaks = seq(45, 85, 5)) + # expand = c(0, 0),
  # geom_vline(xintercept = 5, colour = "#990000", linetype = "dashed") +
  stat_smooth(method = mgcv::gam, se = TRUE, formula = my.formula) +
  theme(
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p11

# triple in one,分组与不分组曲线拟合汇总
summary(dt)
summary(dt.summary)
my.formula <- y ~ bs(x, knots = 1, degree = 4, bs = "cs")
my.formula <- y ~ s(x, k = 6, bs = "cs")
ggplot(dt.summary, aes(Age, Weight)) +
  geom_point(aes(colour = Sex), alpha = 1.0, size = 1.5) +
  theme_classic() +
  ylab(bquote(TBV/Weight~(cm^3/kg))) + # TBV(cm^3) TBV/BW(cm^3/kg)
  scale_x_continuous(breaks = seq(0, 30, 1), expand = c(0, 0)) + # expand = c(0, 0),
  scale_y_continuous(breaks = seq(55.0, 80.0, 1.0), expand = c(0, 0)) + # expand = c(0, 0),
  geom_vline(xintercept = 5.0, colour = "#990000", linetype = "dashed") +
  stat_smooth(method = mgcv::gam, se = TRUE, colour = "black", formula = my.formula) +
  # stat_smooth(method = mgcv::gam, se = TRUE, formula = y ~ s(x, bs = "cs")) +
  geom_smooth(
    data = dt, mapping = aes(x = Age, y = Weight, colour = Sex),
    method = "gam", formula = my.formula
  ) +
  xlab("Age (year)") +
  ylab(bquote(SUVr_refPons))  + # Volume~(cm^3) Weight~(Kg)  TBV/Weight~(cm^3/kg) 'Uptake Value'~(kBq/cc) SUV~(g/ml)
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

# 构建一个 squash_axis 函数来实现坐标轴压缩功能，这个函数需要使用scales包(https://zhuanlan.zhihu.com/p/358781655)
library(scales)
squash_axis <- function(from, to, factor) {
  # Args:from: left end of the axis；to: right end of the axis，factor: the compression factor of the range [from, to]
  trans <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    # apply transformation
    x[isq] <- from + (x[isq] - from) / factor
    x[ito] <- from + (to - from) / factor + (x[ito] - to)
    return(x)
  }
  inv <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from) / factor
    ito <- x >= from + (to - from) / factor
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from) / factor))
    return(x)
  }
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}

# gam
coef(model.gam)[1] # Intercept
par(mfrow = c(1, 2)) # 2*2画布
plot(model.gam, shade = T, shade.col = "lightblue")
plot(model.gam,
  rug = T, residuals = T, seWithMean = T,
  pch = 2, cex = 1
)
# vis.gam(model.gam, type = "link", plot.type = "contour")
vis.gam(model.gam, type = "link", plot.type = "contour")
vis.gam(model.gam, type = "response", plot.type = "persp", border = NA, phi = 30, theta = 30)

gam.check(model.gam)
concurvity(model.gam, full = F)
anova(model.log, model.gam)

# https://cloud.tencent.com/developer/article/1972411
library(patchwork)
p4 + p1 + p6 + plot_layout(nrow = 2, byrow = FALSE) #  从上到下
label <- c("LM", "LOG", "SEGMENTED", "SEGMENTED2", "RCS3", "RCS4", "RCS5", "LOWESS", "GAM")
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p9 + p10 + p11 + plot_annotation(tag_levels = list(label)) +
  plot_layout(ncol = 2, guides = "collect") # 从左到右
p4 / p5 | (p6)
# https://zhuanlan.zhihu.com/p/384189537
library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11,
  label_size = 12, ncol = 2, 
  hjust = -0.2, vjust = 1.4,
  labels = c("LM", "LOG", "SEGMENTED", "SEGMENTED2", "RCS3", "RCS4", "RCS5", "GAM3","GAM4", "GAM5")
)
cowplot::plot_grid(p0, p1, p2, p3, p4, p5, p6,
  ncol = 2, labels = "AUTO"
)

##### R语言绘制限制立方条图2（基于logistic回归和线性回归）#####
data1 <- read.csv("/home/wane/Desktop/TBS/TLMey/VoxelNumbers_InMachin_atlas_whole.csv")
library(rms) # 限制性立方样条需要的包
library(survminer) # 曲线
library(ggplot2) # 画图
library(ggsci) # 调色板 作者：data小白 https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0
library(splines)

names(TLM)
# 选取出来icu_patients_per_million, new_deaths_per_million 这两组数据
data2 <- data.frame(TLM, LM_L3)
# 查看选取出来的数据
head(data2, 20)
data2 <- TLM
summary(data2)
dd <- datadist(data2) # 为后续程序设定数据环境
options(datadist = "dd") # 为后续程序设定数据环境
# fit <-ols(new_deaths_per_million ~ rcs(icu_patients_per_million, 4),data=data2) #做变量的回归
fit <- ols(TscoreL1L4 ~ rcs(LM_L3, nk = 5) + rcs(BMI, nk = 5) + Gender,
  data = RCS
)
fit <- ols(TLM ~ rcs(LM_L3, nk = 5),
  data = TLM, x = TRUE, y = TRUE
)
summary(fit)
an <- anova(fit)

Predict(fit, LM_L3)
# fun=exp
plot(Predict(fit, LM_L3), anova = an, pval = T)
OLS1 <- Predict(fit, LM_L3, ref.zero = F)
OLS1

ggplot() +
  geom_line(data = OLS1, aes(LM_L3, yhat), linetype = 1, size = 1, alpha = 0.9, colour = "red") +
  geom_ribbon(data = OLS1, aes(LM_L3, ymin = lower, ymax = upper), alpha = 0.3, fill = "red") +
  theme_classic() +
  labs(title = "RCS", x = "LM_L3", y = "TLM")

# 不分组/全人群HR与协变量变化关系
Pre0 <- rms::Predict(fit, LM_L3,
  type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2
)
## 其中fun是转化函数,fun=exp
ggplot(Pre0)
View(Pre0)

ggplot() +
  geom_line(
    data = Pre0,
    aes(LM_L3, yhat, colour = Gender), alpha = 0.7
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre0,
    aes(LM_L3, ymin = lower, ymax = upper, fill = Gender), alpha = 0.1
  ) +
  scale_fill_nejm() +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75)

# 不同分组(男女组)之间HR与协变量变化关系
Pre1 <- rms::Predict(fit, LM_L3, Gender = c("1", "0"), type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1) +
  geom_line(
    data = Pre1,
    aes(LM_L3, yhat), alpha = 0.7
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre1,
    aes(LM_L3, ymin = lower, ymax = upper), alpha = 0.1
  ) +
  scale_fill_nejm() +
  scale_colour_discrete(
    name = "Gender", breaks = c("1", "0"),
    labels = c("M", "F")
  ) +
  scale_shape_discrete(
    name = "Gender", breaks = c("1", "0"),
    labels = c("M", "F")
  ) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75)
View(Pre1)


# 线性回归 作者：男二号关陆陆 https://www.bilibili.com/read/cv13828406/ 出处：bilibili
# 样条回归并绘制限制立方条图 https://blog.csdn.net/dege857/article/details/113842472
ggplot(TLM, aes(x = LM_L3 + weight, y = TLM, colour = Gender)) +
  geom_point() +
  geom_smooth(col = "red", method = "lm")

ggplot(data = TLM, aes(x = LM_L3 + weight, y = TLM, colour = Gender)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3))


model.spline <- lm(TLM$TLM ~ rcs(TLM$LM_L3, 4)) # 建立样条回归，设置3个节点
summary(model.spline) # 查看模型概况
# P值，小于0.05表明非线性关系
anova(model.spline)
names(TLM)
ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3)) ## 绘制样条回归拟合效果图

model.spline1 <- lm(TLM$TLM ~ rcs(TLM$LM_L3, c(5, 10, 20))) # 建立样条回归，设置4个节点
ggplot(TLM, aes(LM_L3, TLM)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ rcs(x, c(5, 10, 20))) ## 绘制样条回归拟合效果图

ggplot(TLM, aes(LM_L3, TLM, fill = weight, size = weight)) +
  geom_point(shape = 21, size = 4, col = "black") +
  stat_smooth(method = lm, formula = y ~ rcs(x, 4)) ## 美化一下图形

ggplot(TLM, aes(LM_L3, TLM, fill = Gender, group = Gender)) +
  geom_point(shape = 21, size = 4, col = "black") +
  stat_smooth(method = lm, formula = y ~ rcs(x, 3)) ## 分组表示

ggplot(TLM, aes(LM_L3, TLM, fill = Gender, color = Gender, Group = Gender)) +
  geom_point(shape = 21, size = 4, col = "black") +
  stat_smooth(method = lm, aes(color = Gender), formula = y ~ rcs(x, 3)) ## 分组表示

library(ggpubr)
library(ggsci)
library(cowplot)
str(TLM$Gender)
ggplot(TLM, aes(x = LM_L3, y = TLM, color = Gender)) +
  geom_point(aes(color = Gender), size = 5) +
  scale_fill_nejm() +
  scale_colour_nejm() +
  geom_smooth(method = "lm", formula = y ~ rcs(x, 3), se = T)
ggplot(dt, aes(x = Age, y = whole, color = Sex)) +
  geom_point(aes(color = Sex), size = 5) +
  geom_smooth(method = "lm", formula = y ~ rcs(x, 3), se = T) +
  stat_cor(data = dt, method = "spearman")
# 显著性检验stat_cor(data=data, method = "pearson")意为用pearson相关进行相关性分析，可编辑更改。


# 平滑曲线与阈值效应分析——RCS(https://cran.r-project.org/web/packages/ggrcs/vignettes/ggrcs_vignette.html)
library(rms)
library(ggplot2)
library(scales)
library(ggrcs)

dt <- read.csv("C:\\Users\\wane\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\T1_TBV.csv", sep = ";", fileEncoding = "GBK") # , sep = '\t'
dt <- read.csv("C:\\Users\\wane\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\PET_SUVr.csv", sep = ";", fileEncoding = "GBK")

dt <- dt[c(-1, -2, -3)]
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
summary(dt)
str(dt)

dd <- datadist(dt)
options(datadist = "dd")

fit <- ols(TBV ~ rcs(Age, 5) + factor(Sex), data = dt)
cut.tab(fit, "Age", dt)

OR <- Predict(fit, Age)

singlercs(data = dt, fit = fit, x = "Age", group = "Sex")


# 平滑曲线与阈值效应分析(一)gam
rm(list = ls())
dt <- read.csv("/home/wane/Desktop/TBS/TLMey/VoxelNumbers_InMachin_atlas_whole.csv")
str(dt)
# 2.3拟合平滑曲线
# 2.3.1 构建模型，使用mgcv::gam()函数拟合平滑曲线。
fml <- "TBV ~ s(Age,fx=FALSE)"
gam <- mgcv::gam(formula(fml),
  # weights = dt$weights,
  data = dt, family = gaussian(link = "identity")
)
gam1 <- gam(dt ~ s(Age, k = 4, bs = "fs") + Sex,
  data = dt, method = "REML"
)
gam2 <- gam(dt ~ s(Age, k = 3) + Sex,
  weights = dt$weights,
  data = dt, family = gaussian(link = "identity")
)
summary(gam)
coef(gam)[1] # Intercept
par(mfrow = c(2, 2)) # 2*2画布
plot(gam, shade = T, shade.col = "lightblue")
plot(gam,
  rug = T, residuals = T, seWithMean = T,
  pch = 2, cex = 1
)
# vis.gam(gam1, type = "link", plot.type = "contour")
vis.gam(gam, type = "response", plot.type = "contour")
vis.gam(gam, type = "response", plot.type = "persp", border = NA, phi = 30, theta = 30)

gam.check(gam)
gam.check(gam1)
concurvity(gam, full = F)

# 2.3.2 计算拟合值
pred <- predict.gam(gam, type = "terms", se.fit = TRUE)
mfit <- pred$fit[, "s(LM_L3)"]
sfit <- pred$se.fit[, "s(LM_L3)"]
# mfit <- mfit+(mean(mdl$fitted.values)-mean(mfit))  ###考虑是随机截距
dt <- cbind(dt, mfit, sfit)
# 2.3.3 计算95%CI
y.low <- dt$mfit - 1.96 * dt$sfit
y.upp <- dt$mfit + 1.96 * dt$sfit
dt <- cbind(dt, y.low, y.upp)
# 2.3.4 绘制曲线
summary(dt)
dt <- arrange(dt, LM_L3)
co <- c(0, 30, -10, 15) ## 定义坐标
col <- c("blue", "purple")
# ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),
plot(dt$mfit ~ dt$LM_L3, col = col[1], ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), type = "l", lty = 1, lwd = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$LM_L3, col = col[2], ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), type = "l", lty = 3, lwd = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$LM_L3,
  col = col[2], ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), type = "l", lty = 3, lwd = 1,
  ylab = "TLM", xlab = "LM_L3"
)
rug(dt$LM_L3, col = "blue")
# 2.4 阈值效应分析
# 2.4.1 计算拐点
fml <- "TLM ~ LM_L3 + Gender"
source("/home/wane/Documents/RDocu/平滑曲线1/get_cutoff_lm.R")
cut_off <- get_cutoff_lm("LM_L3", dt, fml)
print(cut_off)
# 2.4.2 生成分段变量
x <- dt[, "LM_L3"]
X1 <- (x <= cut_off) * (x - cut_off)
X2 <- (x > cut_off) * (x - cut_off)
dt1 <- cbind(dt, x, X1, X2)
# 2.4.3 构建分段模型
mdl0 <- glm(TLM ~ x + X2 + Gender, family = "gaussian", weights = dt1$weights, data = dt1)
mdl1 <- glm(TLM ~ X1 + X2 + Gender, family = "gaussian", weights = dt1$weights, data = dt1)
mdl2 <- glm(TLM ~ x + Gender, family = "gaussian", weights = dt1$weights, data = dt1)
# 直线拟合数据
summary(mdl2)
summary(mdl1)
summary(mdl0)
### 似然比检验
round(1 - pchisq(2 * (logLik(mdl0)[1] - logLik(mdl2)[1]), 1), 3)

#### 折点作图到图上
co <- c(0, 30, -15, 15) ## 定义坐标
col <- c("blue", "purple")

plot(dt$mfit ~ dt$LM_L3, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[1], type = "l", lty = 1, lwd = 2, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$LM_L3, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$LM_L3,
  ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1,
  ylab = "TLM", xlab = "LM_L3"
)
rug(dt$LM_L3, col = "blue")
abline(v = cut_off, col = "black", lty = 2)


##### gamlss #####
library(gamlss) 
data(aids)
a <- gamlss(y ~ pb(x) + qrt, family = PO, data = aids)
summary(a)
print(a)
plot(a)
rm(a)
mod <- gamlss(y ~ pb(x), sigma.fo = ~ pb(x), family = BCT, data = abdom, method = mixed(1, 20))
plot(mod)


##### Same plot with custom colors #####
# our own (very beta) plot package: details later
library(WVPlots)
ScatterHist(dt, "Age", "TBV",
  smoothmethod = "gam",
  # annot_size=2,
  title = "L2_4 with Age"
)

WVPlots::ScatterHist(dt, "Age", "TBV",
  title = "Example Fit",
  smoothmethod = "gam",
  # contour = TRUE, annot_size=1,
  point_color = "#006d2c", # dark green
  hist_color = "#6baed6", # medium blue
  smoothing_color = "#54278f", # dark purple
  density_color = "#08519c", # darker blue
  contour_color = "#9e9ac8"
) # lighter purple


# [绘制散点相关图并自动添加相关系数和拟合方程](https://blog.csdn.net/zhouhucheng00/article/details/106413401/?utm_medium=distribute.pc_relevant.none-task-blog-2~default~baidujs_baidulandingword~default-0--blog-112583698.pc_relevant_3mothn_strategy_recovery&spm=1001.2101.3001.4242.1&utm_relevant_index=3)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
theme_set(ggpubr::theme_pubr() +
  theme(legend.position = "top"))

b <- ggplot(dt, aes(x = Age, y = TBV)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 28, 2)) +
  scale_y_continuous(expand = c(0, 0))
# Scatter plot with regression line
b + geom_point() +
  geom_smooth(method = "lm", color = "black", fill = "lightgray")
# Add a loess smoothed fit curve
b + geom_point() +
  geom_smooth(method = "loess", color = "black", fill = "lightgray")

b + geom_point(shape = 17) +
  geom_smooth(method = "gam", color = "black", fill = "lightgray")

# Add regression line and confidence interval
# Add correlation coefficient: stat_cor()
ggscatter(dt,
  x = "Age", y = "TBV",
  add = "reg.line", conf.int = TRUE,
  add.params = list(fill = "lightgray")
) + stat_cor(method = "pearson")

formula <- TBV ~ Age
b + geom_point(shape = 17) +
  geom_smooth(method = "lm", color = "black", fill = "lightgray") +
  stat_cor(method = "pearson") +
  stat_poly_eq(
    aes(label = after_stat(eq.label)),
    formula = formula, parse = TRUE, geom = "text", hjust = 0
  )

b + geom_point(shape = 17) +
  geom_smooth(method = "lm", color = "black", fill = "lightgray") +
  stat_cor(method = "pearson", label.x.npc = 0.5, label.y.npc = 0.9) +
  stat_poly_eq(
    aes(label = after_stat(eq.label)),
    formula = formula, parse = TRUE, label.x.npc = 0.5, label.y.npc = 0.8, hjust = 0
  )

##### 采用多项式回归拟合并添加拟合方程 #####
# Polynomial regression. Sow equation and adjusted R2
formula <- TLM$L2_4 ~ poly(TLM$Age, 3, raw = TRUE)
formula <- y ~ s(x, bs = "cs")
p <- ggplot(TLM, aes(Age, L2_4)) +
  geom_point() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 28, 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_smooth(aes(), method = "gam", formula = formula) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
    formula = formula, parse = TRUE
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
p

# 注意：可以在 label 中添加 ..AIC.label.. 和 ..BIC.label.. ，
# 将会显示拟合方程的AIC值和BIC值。stat_poly_eq()中的 label.x 和 label.y 可用于调整标签显示的位置。
# 想要查看更多的示例，请键入该命令进行查看：
browseVignettes("ggpmisc")
# 将打开如下网页：http://127.0.0.1:18537/session/Rvig.2970595b7d23.html
