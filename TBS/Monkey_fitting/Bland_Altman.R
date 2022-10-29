# B_A plot
rm(list = ls())
library(readxl)
library(ggplot2)
library(ggExtra)
set.seed(123)

BAdata2 <- read_xlsx('/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/Ziqing/vali.xlsx')
# BAdata2 <- BAdata2[,c(-2,-5)]
summary(BAdata2)
BAdata2 <- transform(BAdata2, diff=Measured_TFM - Predicted_TFM, meand =(Measured_TFM + Predicted_TFM)/2)

BAplot <- ggplot(BAdata2, aes(meand, diff)) + 
  geom_point(size= 2,shape=21,colour = "blue") + 
  geom_hline(yintercept = 0, lty = 3,lwd=1,color="pink") +
  geom_hline(yintercept = mean(BAdata2$diff),lty = 1,lwd=1,color="black") +
  geom_hline(yintercept = mean(BAdata2$diff) + 1.96*sd(BAdata2$diff), linetype = 3,color="grey") +
  geom_hline(yintercept = mean(BAdata2$diff) - 1.96*sd(BAdata2$diff), linetype = 3,color="grey") +
  labs(x="mean of TFM and TAT4M",y="")+ theme_classic() +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))+
  # theme(axis.text= element_text(face="bold",  size=12),
  #       axis.title.x=element_text(size=18,face="bold"),
  #       axis.title.y =element_text(size=18,face="bold"))+
  theme(axis.text = element_text(size = 10, face = "bold"), axis.ticks.length=unit(-0.25, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"))) +
  scale_x_continuous(limits=c(0,5)) + scale_y_continuous(limits=c(-1.0,1.5)) +
  geom_text(x=4.8, y=0.9, label="+1.96SD=0.83",size=5)+ 
  geom_text(x=4.8, y=-0.9, label="-1.96SD=-0.77",size=5)+
  geom_text(x=4.8, y=0.1, label="Mean=0.03",size=5)
  # geom_text(x=155, y=-18, label="Mean=0.03 \n 95%CI(-16.9,-0.9) ",size=5)
BAplot
ggsave(BAplot, file = "/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/Ziqing/B_A_1.tiff", device='tiff', dpi = 300)
# Bland-Altman Plot的packages还有qwraps2、PairedData和blandr
ggMarginal(BAplot, type="histogram", bins = 20,fill="grey")
#type的类型包括density, histogram, boxplot, violin
# BAdata+theme( panel.background = element_rect(fill = "white"))

# 拟合
library(ggpmisc)
library(ggpubr)
colnames(BAdata2)
my.formula <- y ~ x
p0 <- ggplot(BAdata2, aes(Predicted_TFM, Measured_TFM)) +  geom_point() + stat_cor(aes(), label.x = 4) + 
  theme_classic() + scale_x_continuous(expand = c(0,0), breaks=seq(0, 5, 1)) + scale_y_continuous(expand = c(0,0)) +  
  stat_smooth(method = lm, se=TRUE, formula = my.formula) + 
  theme(axis.text = element_text(size = 10, face = "bold"), axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")))
p0
# https://cloud.tencent.com/developer/article/1854758
lm <- ggplot(data = BAdata2, aes(x = Predicted_TFM, y = Measured_TFM)) +
  geom_smooth(method = "lm", 
              se=FALSE, color="black", 
              formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., 
                                 ..rr.label.., 
                                 sep = "~~~")), 
               parse = TRUE) + geom_point(size= 2,shape=21,colour = "black") +
  coord_cartesian(ylim = c(0,4.000),
                  xlim = c(0,4.000))+
  # scale_y_continuous(breaks = c(0,100,200,300))+
  # scale_x_continuous(breaks = c(0,25,50,75,100,105))+#x轴比上个图多个105
  theme_bw() +theme(axis.text = element_text(size = 10, face = "bold"), axis.ticks.length=unit(-0.25, "cm"), 
                     axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                     axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"))) +
  geom_smooth(method = "lm",
              color="black",
              # fill="black",
              alpha=.7,
              size=0.7,se=T,
              formula = y~x)+
  theme(panel.grid = element_blank())
ggsave(lm, file = "/Users/mac/Desktop/Nomo-TBS/TBS&Mon/Monkey/Ziqing/lm_TFM.tiff", device='tiff', dpi = 300)



  
