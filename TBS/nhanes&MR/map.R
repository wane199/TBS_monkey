# https://m.xueshufan.com/#chat
# 加载所需的R包
library(sf)
library(ggplot2)

# 读取地理数据
foshan <- st_read("C:\\Users\\wane1\\Downloads\\佛山市.json")

# 绘制地图
ggplot() +
  geom_sf(data = foshan, fill = "lightblue", color = "black") +
  labs(title = "佛山市地图") +
  theme_bw()


library(ggplot2)
library(maptools)
# install.packages('ggplot2')
# install.packages('maptools')
# install.packages('mapproj')
y <- readShapePoly("CHN_adm2.shp")
y1 <- subset(y, y$NAME_1 == "Guangdong")
y1$NAME_2 # 按照这个顺序输入你的数据
gddat <- fortify(y1)
gddat <- transform(gddat, id = iconv(id, from = "GBK"), group = iconv(group, from = "GBK"))
names(gddat)[1:2] <- c("x", "y")
sub_gddat <- data.frame(id = unique(sort(gddat$id)))
sub_gddat$income <- c(21, 258, 219, 601.8, 16.55, 78.8, 73.56, 34.03, 44.9, 27.11, 33.5, 63.23, 9.6481, 31.12, 548.66, 20.856362, 16.39, 56.19, 33.540357, 96.92, 72.61)
gdmap <- ggplot(sub_gddat) +
  geom_map(aes(map_id = id, fill = income), color = "white", map = gddat) +
  scale_fill_gradient(high = "darkgreen", low = "lightgreen") +
  expand_limits(gddat) +
  coord_map()
print(gdmap)
temp <- coordinates(y1)
temp <- as.data.frame(temp)
temp$name <- c("潮州", "东莞", "佛山", "广州", "河源", "惠州", "江门", "揭阳", "茂名", "梅州", "清远 ", "汕头", "汕尾", "韶关", "深圳", "阳江", "云浮", "湛江", "肇庆", "中山", "珠海")
gdmap + geom_text(aes(x = V1, y = V2, label = name), family = "GB1", data = temp)
