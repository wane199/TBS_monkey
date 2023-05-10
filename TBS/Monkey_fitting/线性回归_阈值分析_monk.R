# R语言绘图|2.基于GAMM：分层的平滑曲线怎么画？https://zhuanlan.zhihu.com/p/489149912
# https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0
rm(list = ls()) ## 清空当前环境
options(digits = 3) # 限定输出小数点后数字的位数为3位
library(mgcv) ## GAMM
library(writexl)
library(dplyr)
library(gamm4)
library(ggplot2) # 画图
library(ggthemes) ## ggplot主题
theme_set(theme_classic() + theme(legend.position = "bottom"))

# dt <- read.csv("jixian.csv")
dt <- read.csv("./TBS/Monkey_fitting/T1_TBV_1209.csv") # , sep = '\t'
dt <- read.csv("C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\T1_TBVratio_L&R.csv", sep = ';', fileEncoding = "GBK")
# write.csv(dt,'C:\\Users\\wane1\\Documents\\file\\TBS&Mon\\Monkey\\QIANG\\0417\\T1_TBV.csv')
# TLM <- read_excel("/home/wane/Desktop/TBS/TLMey/BMC.xlsx")
# 数据探索EDA
dt <- dt[c(-1, -2, -3)]
dt$Sex <- as.factor(dt$Sex)
dt$Side <- as.factor(dt$Side)
summary(dt)
str(dt)
# colnames(dt) <- toupper(colnames(dt))
# rcssci(linear models with RCS splines were performed to explore the shape linear or nonlinear(U, inverted U,J,S,L,log,-log,temporary plateau shape)
library(rcssci)
rcssci_linear(
  data = dt, y = "TBV", x = "Age", covs = c("Sex"), ref.zero = F,
  prob = 0.1, filepath = "C:\\Users\\wane199\\Desktop\\TBS&Mon\\Monkey\\QIANG\\1030\\"
) # 默认prob = 0.5   ggplot2::theme_classic()

fml <- "SUVr_whole_refPons ~ s(Age,k=4,fx=FALSE)+factor(Sex)"
gam1 <- mgcv::gam(formula(fml), weights = dt$weights, data = dt, family = gaussian(link = "identity"))
summary(gam1) # 检验自变量的显著性以及评估回归整体的方差解释率
vis.gam(gam1, color = "gray", theta = 30, phi = 30) # "topo", "heat", "cm", "terrain", "gray" or "bw"
gam1$weights
plot(gam1, pages = 1, col = "blue", las = 1, se = T, rug = T)

mgam <- gam(TBV ~ s(Age) + factor(Sex),
  data = dt, family = gaussian(link = "identity"), #  poisson(), gaussian(link = "identity")，binomial(link = "logit"),
  model = T
)
summary(mgam)
vis.gam(mgam, ticktype = "detailed", color = "bw", theta = 40, phi = 40)
vis.gam(mgam, color = "gray", theta = 30, phi = 30)
vis.gam(mgam, theta = 30, phi = 30, plot.type = "contour", color = "cm")
plot(mgam, pages = 1, col = "blue", las = 1, se = T, rug = T)

m <- mgcv::gam(TBV ~ s(Age, k = 4) + factor(Sex), data = dt) # , by = Sex
summary(m)
anova(m)
plot(modelbased::estimate_relation(m, length = 100, preserve_range = FALSE))

plot(gam1, scale = 0, page = 1, shade = TRUE, las = 1, all.terms = TRUE, cex.axis = 1.2, cex.lab = 1.5, main = "Univariable Model")
AIC(gam1)
abline(v = 0, col = "blue")

# 广义可加模型gam**
# https://mp.weixin.qq.com/mp/appmsgalbum?__biz=MzI1NjM3NTE1NQ==&action=getalbum&album_id=2077935014574374912&scene=173&from_msgid=2247485011&from_itemidx=1&count=3&nolastread=1#wechat_redirect
pr.gam <- predict(gam1, dt) # 生成预测值
# 计算RSME和R方
library(caret)
data.frame(
  RMSE = RMSE(pr.gam, dt$TBV),
  R2 = R2(pr.gam, dt$TBV)
)
# 查看模型拟合情况
ggplot(dt, aes(Age, TBV)) +
  scale_x_continuous(breaks = seq(0, 30, 1)) +
  geom_point() +
  geom_vline(aes(xintercept = 5.0), linetype = 4, col = "red") +
  theme_classic() +
  stat_smooth(method = mgcv::gam, formula = y ~ s(x, k = 5))

# 年龄段分组汇总
dt.summary <- dt %>%
  group_by(Age,Sex) %>%  # Sex
  summarise(
    sd = sd(TBV),
    TBV = mean(TBV)
  )
write.csv(dt.summary,'C:\\Users\\wane199\\Desktop\\TBS&Mon\\Monkey\\QIANG\\1030\\T1_TBV_1127_summary.csv')
my.formula <- y ~ s(x, k = 7, bs = "cs")
ggplot(dt.summary, aes(Age, len)) +
  geom_point() +
  stat_cor(aes(), label.x = 6) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 30, 1)) +  # expand = c(0, 0),
  scale_y_continuous(breaks = seq(45, 85, 5)) +  # expand = c(0, 0), 
  stat_smooth(method = mgcv::gam, se = TRUE, formula = my.formula) +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

ggplot(dt,aes(x=Age, y=TBV, fill=Sex, colour = Sex, group = Sex, shape = Sex)) + 
  stat_summary(fun=mean,geom="point",color="black",alpha=1.5,size=3.5,position=position_dodge(0.3)) +
  # geom_jitter(alpha = 0.5, size = 3.0) + 
  geom_line(aes(x=Age, y=len, group = Sex, linetype = Sex), size = 1.0, data = dt.summary, position=position_dodge(0.3)) + 
  theme_classic() + theme(plot.title = element_text(size=11)) + # ylim(0.4,1.6) + 
  xlab('') + ylab(expression(TBV(cm^3))) + theme(plot.title = element_text(hjust = 0.5)) +
  # rotate_x_text(30) + ylab(expression(BMD(g/cm^2)))
  theme(axis.text = element_text(size = 8, face = "bold"))

library(ggpmisc)
library(ggpubr)
library(scales) # 构建一个 squash_axis 函数来实现坐标轴压缩功能，这个函数需要使用scales包(https://zhuanlan.zhihu.com/p/358781655)
squash_axis <- function(from, to, factor) {
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
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

my.formula <- y ~ s(x, k = 7, bs = "cs")
ggplot(dt, aes(Age, TBV)) + # SUVr_whole_refPons
  geom_point(alpha = 1) +
  stat_cor(aes(), label.x = 3) + 
  stat_smooth(method = mgcv::gam, se = TRUE, formula = my.formula) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 1, 3, 5, 13, 20)) + # seq(0, 32, 1)
  scale_y_continuous(expand = c(0, 0)) + # scale_x_log10() +
  coord_trans(x = squash_axis(0, 5, 0.40)) + geom_vline(xintercept=5,colour="lightgrey",linetype="dashed") + geom_vline(xintercept=7.5,colour="#990000",linetype="dashed") + 
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

##########################################
# triple in one,分组与不分组曲线拟合汇总
summary(dt)
summary(dt.summary)
my.formula <- y ~ s(x, k = 12, bs = "cs")
p11 <- ggplot(dt, aes(Age, TBV)) + # dt.summary
  geom_point(aes(colour = Sex,shape = Sex), alpha = 1.0, size = 2.5) +
  theme_classic() +
  ylab(bquote(TBV(cm^3))) + # TBV(cm^3) TBV.BW(cm^3/kg) Weight(kg) SUVr_whole_refPons Whole(KBq/cc)
  scale_x_continuous(breaks = seq(0, 30, 1), expand = c(0, 0)) + # expand = c(0, 0),
  scale_y_continuous(breaks = seq(55.0, 85.0, 2.0), expand = c(0, 0)) + # expand = c(0, 0),
  geom_vline(xintercept = 5.0, colour = "#990000", linetype = "dashed") +
  stat_smooth(method = mgcv::gam, se = TRUE, colour = "black", formula = my.formula) +
  # stat_smooth(method = mgcv::gam, se = TRUE, formula = y ~ s(x, bs = "cs")) +
  geom_smooth(
    data = dt, mapping = aes(x = Age, y = TBV, colour = Sex),
    method = "gam", formula = my.formula
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p11
library(patchwork) # 拼图
p11 + p12 + p13 + p14 + p15 + plot_annotation(tag_levels = "A") + plot_layout(ncol = 3) + 
  plot_layout(guides = "collect") -> p
p
ggsave("./TBS/Monkey_fitting/1209.pdf", p, width = 20, height = 9, dpi = 900) # 保存为精度为600 dpi的tiff文件

# 分类gam曲线拟合
ggplot(dt, aes(Age, TBV, colour = Sex)) +
  geom_point(alpha = 0.1) +
  stat_cor(aes(), label.x = 3) + 
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 1, 3, 5, 13, 20)) + # seq(0, 32, 1)
  scale_y_continuous(expand = c(0, 0)) + # scale_x_log10() +
  stat_smooth(method = mgcv::gam, se = TRUE, formula = my.formula) +
  coord_trans(x = squash_axis(0, 5, 0.40)) + geom_vline(aes(xintercept=5),colour="lightgrey",linetype="dashed") + geom_vline(xintercept=7.5,colour="#990000",linetype="dashed") + 
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )

library(plotly)
df1:p %>% add_markers() %>% add_lines()p %>%
      layout(xaxis=list(ticktext=list("one","two","three","four","five","six"),tickvals=list(1,2,0,4,0,0),  tickmode="array"))  #下图只会显示对应了1，2，4的图，其它的0对应的位置不存在(x中没有)

paste0(colnames(dt[4:16]), collapse = "+")
Frontal_Cortex + Temporal_Cortex + Parietal_Cortex + Occipital_Cortex + Insula_Cortex +
  Striatum + Hippocampus +
  Thalamus + Amygdala + Cingulate + Globus_pallidus + corpus_callosum + Cerebellum

# ?formula.gam
library(ggsci)
library(ggpmisc)
library(ggpubr)
theme_set(theme_classic() + theme(legend.position = "bottom"))
my.formula <- y ~ s(x, k = 7, bs = "cs")
my.formula <- y ~ x + I(x^2)
# 散点图
ggplot(data = dt, mapping = aes(x = Age, y = TBV, color = Sex, shape = Sex)) +
  geom_point(size = 2) +
  theme_classic() +
  scale_colour_nejm() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
# 置信区间虚线
ggplot(data = dt, mapping = aes(x = Age, y = TBV, colour = Sex)) +
  # scale_colour_nejm() +
  # geom_point(size = 2) + stat_cor(aes(), label.x = 6) # 显示p值和R值
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 30, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_smooth(
    method = "gam", formula = my.formula, size = 3,
    se = FALSE
  ) +
  stat_cor(aes(), label.x = 6) +
  geom_ribbon(
    stat = "smooth", formula = my.formula, size = 1,
    method = "gam", se = TRUE, alpha = 0, # or, use fill = NA
    linetype = "dotted"
  ) +
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"), legend.position = "bottom",
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  ) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = my.formula, parse = TRUE
  )
# 置信区间带
p2 <- ggplot(dt, mapping = aes(x = Age, y = TBV)) + # , colour = Side, fill = Side, linetype = Side
  ylab(bquote(TBV(cm^3)))  + xlab("Age (year)") + # scale_fill_nejm() + scale_colour_nejm() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 31, 1)) + scale_y_continuous(expand = c(0, 0), breaks = seq(45, 90, 5)) +
  # stat_cor(aes(), label.x = 3) + geom_vline(aes(xintercept=8.0),linetype=4,col="red") +
  geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 3), se = T, colour = "black") + # ylab(bquote(TBV/BW(cm^3/kg))) +
  geom_point(aes(), size = 2) + # colour = Side, shape = Side, fill = Side
  # coord_trans(x = squash_axis(1, 99, 30)) # 局部压缩坐标轴
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  )
# stat_poly_eq(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula =  y ~ s(x, k = 4), parse = TRUE
# )
p2
# for循环
theme_set(theme_classic() + theme(legend.position = "bottom"))
plot_list <- list()
for (i in 3:ncol(dt)) {
  print(p <- ggplot(dt, aes_string(x = "Age", y = colnames(dt)[i], colour = "Side", fill = "Side", linetype = "Side")) +
    scale_x_continuous(limits = c(0,30), expand = c(0, 0), breaks = seq(0, 30, 2)) +
    geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 4), se = T) +
    geom_point(aes(colour = Side, shape = Side, fill = Side), size = 1.5, shape = 21) +
    ylab(bquote("Vr_ref Whole")) + xlab("Age (year)") + # KBq/cc bquote(Volume(cm^3)) 'Vr_ref Whole' "SUVr_ref_Pons"
    scale_fill_brewer(palette = "Paired") + 
    ggtitle(paste0(colnames(dt)[i])) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(
      axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
      axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
    ))
  plot_list[[i - 2]] <- p
}
# 拼图
# library(cowplot)
# combined_plot <- cowplot::plot_grid(plotlist = plot_list, align = "h", nrow = 4)
# legend <- get_legend(plot_list[[13]] + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
# plot_grid(combined_plot, legend,ncol=1,rel_heights = c(1, .1))
library("patchwork")
wrap_plots(plot_list, byrow = T, ncol = 5) + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

# Fit polynomial equation:
ggplot(data = dt, mapping = aes(x = Age, y = TBV, colour = Side)) +
  geom_point() +
  stat_cor(aes(), label.x = 3) +
  geom_smooth(aes(fill = Side), method = "lm", formula = y ~ poly(x, 3, raw = T), se = T)
# Polynomial regression. Sow equation and adjusted R2
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(data = dt, mapping = aes(x = Age, y = TBV, colour = Side)) +
  # geom_point() +
  geom_smooth(aes(fill = Side), method = "lm", formula = formula) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula, parse = TRUE
  ) -> p
ggpar(p, palette = "jco")

# [combine into single plot](https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/ggplot2.html)
ggplot(dt, mapping = aes(x = Age, y = TBV)) + # , colour = Side, fill = Side, linetype = Side
  ylab("bquote(Volume(cm^3))") + xlab("Age (year)") + 
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 31, 1)) + scale_y_continuous(expand = c(0, 0), breaks = seq(45, 90, 5)) +
  # stat_cor(aes(), label.x = 3) + geom_vline(aes(xintercept=8.0),linetype=4,col="red") +
  geom_smooth(method = mgcv::gam, formula = y ~ s(x, k = 3), se = T, colour = "black") + # ylab(bquote(TBV/BW(cm^3/kg))) +
  geom_point(aes(), size = 2, shape = 1) + # colour = Side, shape = Side, fill = Side
  # coord_trans(x = squash_axis(1, 99, 30)) # 局部压缩坐标轴
  theme(
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.25, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  )

summary(dt)
my.formula <- y ~ s(x, k = 5, bs = "cs")
p6 <- ggplot(dt, aes(Age, CSF.TBV)) + # dt.summary SUVr_whole_refPons
  geom_point(aes(), alpha = 1.0, size = 1.5) + # colour = Sex,shape = Sex
  theme_classic() +
  ylab(bquote("Ratio")) + xlab("Age (year)") + # "Volume " (cm^3) "TBV " (cm^3) TBV.BW(cm^3/kg) Weight(kg) SUVr_ref Pons SUVr_whole_refPons Whole(KBq/cc)
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 2), expand = c(0, 0)) + # expand = c(0, 0),
  scale_y_continuous(limits = c(0.150, 0.165), breaks = seq(0.150, 0.165, 0.002), expand = c(0, 0)) + # expand = c(0, 0),
  # geom_vline(xintercept = 5.0, colour = "#990000", linetype = "dashed") +
  stat_smooth(method = mgcv::gam, se = TRUE, colour = "black", formula = my.formula) +
  # stat_smooth(method = mgcv::gam, se = TRUE, formula = y ~ s(x, bs = "cs")) +
  geom_smooth(colour = "black",
    data = dt, mapping = aes(x = Age, y = CSF.TBV), # , colour = Sex
    method = "gam", formula = my.formula
  ) + ggtitle("CSF_ratio") + theme(plot.title = element_text(hjust = 0.5)) + #设置标题居中
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p6

p4 <- ggplot(dt, aes(Age, GM.TBV)) + # dt.summary SUVr_whole_refPons
  geom_point(aes(colour = Sex,shape = Sex), alpha = 1.0, size = 1.5) +
  theme_classic() +
  ylab(bquote("Ratio")) + xlab("Age (year)") + # Volume(cm^3) TBV.BW(cm^3/kg) Weight(kg) SUVr_whole_refPons Whole(KBq/cc)
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 2), expand = c(0, 0)) + # expand = c(0, 0),
  scale_y_continuous(limits = c(0.400, 0.450), breaks = seq(0.400, 0.450, 0.005), expand = c(0, 0)) + # expand = c(0, 0),
  # geom_vline(xintercept = 5.0, colour = "#990000", linetype = "dashed") +
  # stat_smooth(method = mgcv::gam, se = TRUE, colour = "black", formula = my.formula) +
  # stat_smooth(method = mgcv::gam, se = TRUE, formula = y ~ s(x, bs = "cs")) +
  geom_smooth(
    data = dt, mapping = aes(x = Age, y = GM.TBV , colour = Sex),
    method = "gam", formula = my.formula
  ) + ggtitle("GM_ratio") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
    axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  )
p4

library("patchwork")
p1 + p2 + p3 + p4 + p5 + p6 + # + p7 + p8 + p9 + p10 + p11 + p12 + p13 + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

p1 + p2 + p3 + p4 + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")
ggsave("p.tiff", p, dpi = 600) # 保存为精度为600 dpi的tiff文件
# gam
coef(gam1)[1] # Intercept
par(mfrow = c(1, 2)) # 2*2画布
plot(gam1, shade = T, shade.col = "lightblue")
plot(gam1,
  rug = T, residuals = T, seWithMean = T,
  pch = 2, cex = 1
)
# vis.gam(model.gam, type = "link", plot.type = "contour")
vis.gam(gam1, type = "link", plot.type = "contour")
vis.gam(gam1, type = "response", plot.type = "persp", border = NA, phi = 30, theta = 30)

gam.check(gam1)
concurvity(gam1, full = F)
anova(model.log, model.gam)

# 组间比较
library(ggpubr)
library(ggsci)
# 根据变量分面绘制箱线图
dt$Group <- factor(dt$Side, levels = c("Left", "Right")) # 调整分面顺序
ggviolin(dt,
  x = "Side", y = "Frontal_Cortex", ylab = "Frontal_Cortex",
  color = "Side", palette = "jco", add = "jitter", xlab = "", legend = "right"
)

library(ggstatsplot)
ggbetweenstats(
  data = dt,
  x = Side,
  y = Frontal_Cortex,
  xlab = "",
  ylab = "Frontal_Cortex",
  plot.type = "violin",
  package = "ggsci",
  palette = "uniform_startrek"
) +
  ggtitle("") +
  ggeasy::easy_center_title()
p <- ggboxplot(dt,
  x = "Age", y = "Frontal_Cortex", ylab = "Frontal_Cortex",
  color = "Side", palette = "jco",
  add = "jitter", xlab = "", legend = "right",
  facet.by = "Side", short.panel.labs = FALSE
) + labs(color = "Side", shape = "Side")
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", method = "t.test")
# Or use significance symbol as label
p + stat_compare_means(label = "p.signif", label.x = 1.5)
# https://cloud.tencent.com/developer/article/1972411
library(patchwork)
p4 + p1 + p6 + plot_layout(nrow = 2, byrow = FALSE) #  从上到下
p4 + p5 + p6 +
  plot_layout(ncol = 2) # 从左到右
p4 / p5 | (p6)
# https://zhuanlan.zhihu.com/p/384189537
library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6,
  label_size = 12,
  hjust = -0.2, vjust = 1.4,
  labels = c("LM", "LOG", "SEGMENTED", "RCS", "LOWESS", "GAM")
)
cowplot::plot_grid(p1, p2, p3, p4, p5, p6,
  ncol = 3, labels = "AUTO"
)

##################################################
# 平滑曲线与阈值效应分析(https://zhuanlan.zhihu.com/p/400169105)
#### 阈值效应分析
## model I 一条直线效应
fit1 <- lm(TBV ~ Age, data = dt)
summary(fit1)

## model II 分段模型
fml <- TBV ~ s(Age, k = 8, bs = "cs")
# my.formula <- y ~ x + I(x^2) + I(x^3)
# fml<- "TBV ~ s(Age,fx=FALSE) + Sex"
gam1<-mgcv::gam(formula(fml),weights=dt$weights,data=dt, family=gaussian(link="identity"))

# 2.3.2 计算拟合值
pred<-predict.gam(gam1,type="terms",se.fit=TRUE)
pred$fit
mfit <- pred$fit[, "s(Age)"]
sfit <- pred$se.fit[, "s(Age)"]
mfit <- mfit + (mean(gam1$fitted.values) - mean(mfit)) ### 考虑是随机截距
dt<-cbind(dt,mfit,sfit)

# 2.3.3 计算95%CI
y.low <- dt$mfit-1.96*dt$sfit; y.upp<-dt$mfit+1.96*dt$sfit
dt <- cbind(dt,y.low,y.upp)

# 2.3.4 绘制曲线
dt <- arrange(dt,Age)
summary(dt)
co <- c(0,30,55,75)  ## 定义坐标
col <- c('blue','purple')
plot(dt$mfit~dt$Age,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[1],las = 1, type="l", lty=1, lwd=2, ylab="", xlab="")
par(new=TRUE); 
plot(dt$y.low~dt$Age,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[2],las = 1, type="l", lty=3, lwd=1, ylab="", xlab="")
par(new=TRUE); 
plot(dt$y.upp~dt$Age,ylim=c(co[3],co[4]),xlim=c(co[1],co[2]),col=col[2],las = 1, type="l", lty=3, lwd=1, ylab='TBV', xlab='Age')
## 绘制一条水平线和垂直线，并指定颜色
abline(v = 7.5, col = "lightgray", lty = 3) # col = "gray60"
rug(dt$Age,col='blue')

# 2.4.1 计算拐点
getwd()
source("./TBS/Monkey_fitting/get_cutoff_lm.R")
cut_off <- get_cutoff_lm('Age',dt,fml)
cut_off = 7.5
print(cut_off )

x <- dt[, "Age"]
X1 <- (x <= cut_off) * (x - cut_off)
X2 <- (x > cut_off) * (x - cut_off)
dt1 <- cbind(dt, x, X1, X2)

mdl0 <- glm(TBV ~ x + X2, family = "gaussian", data = dt1)
mdl1 <- glm(TBV ~ X1 + X2, family = "gaussian", data = dt1)
mdl2 <- glm(TBV ~ x, family = "gaussian", data = dt1)
#### model I 一条直线效应(线性回归)
summary(mdl2)
#### 折点
print(cut_off)
#### < K 段效应(B+se)/P
#### > K 段效应(B+se)/P
summary(mdl1)
### > K 段与< K的效应(B+se)/P
summary(mdl0)

### 考虑分段后模型和不分段对数似然比检验
round(1 - pchisq(2 * (logLik(mdl0)[1] - logLik(mdl2)[1]), 1), 3)

#### 折点作图到图上
plot(dt$mfit ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[1], type = "l", lty = 1, lwd = 2, las = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.low ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, las = 1, ylab = "", xlab = "")
par(new = TRUE)
plot(dt$y.upp ~ dt$Age, ylim = c(co[3], co[4]), xlim = c(co[1], co[2]), col = col[2], type = "l", lty = 3, lwd = 1, las = 1, ylab = "TBV", xlab = "Age")
rug(dt$Age, col = "blue")
abline(v = cut_off, col = "black", lty = 2)

#######################################
# 限制性立方样条RCS-R语言代码实战
library(rms) # 限制性立方样条需要的包
library(survminer) # 曲线
library(ggplot2) # 画图
library(ggsci) # 调色板 作者：data小白 https://www.bilibili.com/read/cv16417407?spm_id_from=333.999.0.0 出处：bilibili
str(dt)
dt$Sex <- factor(dt$Sex)
dt$Side <- as.numeric(as.character(dt$Side, levels = c("Left", "Right"), labels = c("1", "2")))


dd <- datadist(dt) # 为后续程序设定数据环境
options(datadist = "dd") # 为后续程序设定数据环境
fit <- ols(TBV ~ rcs(Age, 5) + Sex, data = dt) # 做变量的回归
summary(fit)
an <- anova(fit)
an

Predict(fit, Age)
plot(Predict(fit, Age), anova = an, pval = T)
OLS1 <- Predict(fit, Age)

# RCS绘图，不分组
ggplot() +
  geom_line(data = OLS1, aes(Age, yhat), linetype = 1, size = 1, alpha = 0.9, colour = "grey") +
  geom_ribbon(data = OLS1, aes(Age, ymin = lower, ymax = upper), alpha = 0.3, fill = "grey") +
  theme_classic() +
  labs(title = "RCS", x = "Age", y = "Frontal_Cortex")

# 不同分组(男女组)之间HR与协变量变化关系
Pre1 <- rms::Predict(fit, Age, Sex = c("F", "M"), type = "predictions", ref.zero = F, conf.int = 0.95, digits = 2)
par(mfrow = c(1, 2))
ggplot(Pre1) +
  geom_line(
    data = Pre1,
    aes(Age, yhat), alpha = 0.7
  ) +
  scale_color_nejm() + ## 采用ggsci包中英格兰调色，也可以其他
  geom_ribbon(
    data = Pre1,
    aes(Age, ymin = lower, ymax = upper), alpha = 0.1
  ) +
  scale_fill_nejm() +
  scale_colour_discrete(
    name = "Sex", breaks = c("F", "M"),
    labels = c("F", "M")
  ) +
  scale_shape_discrete(
    name = "Sex", breaks = c("F", "M"),
    labels = c("F", "M")
  ) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.75)
View(Pre1)

# [非线性拟合及寻找数据拐点](https://www.jianshu.com/p/e8f8f7ec10d2)
library(ggrcs)
library(rms)
library(ggplot2)
library(scales)
# dt<-smoke
dt <- read.csv("/home/wane/Desktop/TBS&Mon/Monkey/QIANG/1030/T1_TBV_1127.csv")
dd<-datadist(dt)
options(datadist='dd')
dt$logage <- log(dt$Age)
summary(dt)
# fit<- cph(Surv(time,status==1) ~ rcs(age,4)+gender, x=TRUE, y=TRUE,data=dt)
fit <- ols(TBV ~ rcs(Age,3) + Sex, data = dt)
ggrcs(data = dt, fit = fit, x = "Age", group="Sex", histbinwidth = 1, histcol = "blue")

###single group
ggrcs(data=dt,fit=fit,x="age")
##two groups
ggrcs(data=dt,fit=fit,x="age",group="gender")

