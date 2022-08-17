# https://blog.csdn.net/u011402596/article/details/43913983
# 判断url是否存在
library(RCurl)
library(readxl)
url.exists(url = "www.baidu.com") # 判断url是否存在
url.exists(url = "https://github.com/wane199/Presentation/blob/master/TBS/app/data/M_1018.csv")

d <- debugGatherer() # 收集调试信息
tmp <- getURL(url = "www.baidu.com", debugfunction = d$update, verbose = TRUE)
x <- getURL("https://raw.github.com/wane199/Presentation/blob/master/TBS/app/data/M_1018.csv")
y <- read.csv(text = x)

dt <- read.table("https://github.com/wane199/Presentation/tree/master/TBS/app/data/F_3061.txt", header = T)

dt <- read.csv(text = getURL("https://github.com/wane199/Presentation/blob/master/TBS/app/data/M_1018.csv"))
names(d$value())


cat(d$value()[1]) # 服务器地址及端口号
cat(d$value()[2]) # 服务器返回的头信息
cat(d$value()[3]) # 提交给服务器的头信息
d$reset() # 清除d$value()
d$value() # 清除之后全部为空
# 查看服务器返回的头信息
## 列表形式
h <- basicHeaderGatherer()
txt <- getURL(url = "http://www.baidu.com", headerfunction = h$update)
names(h$value())

h$value()
# 查看服务器返回的头信息
## 字符串形式
h <- basicTextGatherer()
txt <- getURL("http://www.baidu.com", headerfunction = h$update)
names(h$value())

h$value() # 所有的内容只是一个字符串


cat(h$value()) # 用cat显示的，会比较好看

# 查看url请求的访问信息
curl <- getCurlHandle()

txt <- getURL(url = "http://www.baidu.com", curl = curl)
names(getCurlInfo(curl))

getCurlInfo(curl)$response.code

getCurlInfo(curl = curl)

# 设置自己的header，把系统设置成ihpone的系统Mac OS
myheader <- c(
  "User-Agent" = "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language" = "en-us",
  "Connection" = "keep-alive",
  "Accept-Charset" = "GB2312,utf-8;q=0.7,*;q=0.7"
)

d <- debugGatherer()
tmp <- getURL(url = "http://www.baidu.com", httpheader = myheader, debugfunction = d$update, verbose = T)

cat(d$value()[3]) # 提交给服务器的头信息，发现设置成功




# XML简介
# 缺点：在windows下对中文支持不理想（我在ubuntu下也不理想）
library(XML)
url <- "http://data.earthquake.cn/datashare/datashare_more_quickdata_new.jsp" # 中文界面，抓出来是乱码
url <- "http://219.143.71.11/wdc4seis@bj/earthquakes/csn_quakes_p001.jsp" # 英文界面，抓出来是对的
wp <- getURL(url)
doc <- htmlParse(wp, asText = TRUE) # 这里切记encoding
tables <- readHTMLTable(doc, header = F, which = 2)
# 选取第二个表
head(tables)
