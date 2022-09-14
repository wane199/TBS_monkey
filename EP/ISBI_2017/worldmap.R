# 世界地图并用md语法添加文字标签
# https://mp.weixin.qq.com/s?__biz=MzI3NzQ3MTcxMg==&mid=2247494159&idx=1&sn=8ef93f44487ece66782e1b37c5f6e764&chksm=eb676280dc10eb969a94ad1a1191c1b9ede9e74d971338b5a2e2121aa67829124ddb63eee2a6&mpshare=1&scene=1&srcid=0913ruJ04vtAUDapsi9sC9eV&sharer_sharetime=1663159518571&sharer_shareid=13c9050caaa8b93ff320bbf2c743f00b#rd
library(ggplot2)
world<-map_data("world")
ggplot() +
  geom_polygon(data=world,aes(x=long,y=lat,group=group),
               fill="#dedede")+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult=c(0,0)))+
  scale_x_continuous(expand = expansion(add=c(0,0))) -> world.map
world.map

# 添加采样点
df<-read.csv("figure1df.csv")
world.map+
  geom_point(data = df,
             aes(x=Longitude, 
                 y=Latitude, 
                 colour = Process_type2), size=2)+
  scale_color_manual(values = c("#3373a5","#97b1c0",
                                "#f8ad63",
                                "#d8191a",
                                "#c4c4c4"))
# 添加文本标签
library(ggtext)                                  
dftext<-data.frame(
  x=c(-180,-150,-10,100,50,-50),
  y=c(40,-50,-50,-50,50,60),
  label=c("**North america**<br>3 countries<br>107 cities<br>145WWTPs",
          "**South american**<br>2 countries<br>29 cities<br>38 WWTPs",
          3,4,5,6)
)
world.map01 +
  theme(legend.position = c(0.9,0.7),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(color="gray",
                                             fill="transparent"),
        legend.key = element_rect(fill="transparent"))+
  geom_richtext(data=dftext,
                aes(x=x,y=y,label=label),
                nudge_x =0,hjust=0,
                fill="transparent")
