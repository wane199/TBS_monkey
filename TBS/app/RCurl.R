# https://blog.csdn.net/u011402596/article/details/43913983
# 判断url是否存在
library(RCurl)
url.exists(url="www.baidu.com") # 判断url是否存在
url.exists(url="https://github.com/wane199/Presentation/blob/master/TBS/app/data/F_3061.csv") 
# [1] TRUE
d <- debugGatherer() #收集调试信息
# verbose = TRUE 这时候，d$value()值是会叠加的
tmp <- getURL(url="www.baidu.com", debugfunction = d$update, verbose = TRUE)  

dt <- read.csv("https://github.com/wane199/Presentation/blob/master/TBS/app/data/F_3061.csv")
dt <- read.csv(text = getURL("https://github.com/wane199/Presentation/blob/master/TBS/app/data/F_3061.csv"))
names(d$value())
# [1] "text"       "headerIn"   "headerOut"  "dataIn"     "dataOut"    "sslDataIn"  "sslDataOut"

cat(d$value()[1]) #服务器地址及端口号
cat(d$value()[2]) #服务器返回的头信息
cat(d$value()[3]) #提交给服务器的头信息
d$reset() # 清除d$value()
d$value() # 清除之后全部为空
# text   headerIn  headerOut  dataIn    dataOut  sslDataIn sslDataOut 
# ""         ""         ""         ""         ""         ""         "" 
# 查看服务器返回的头信息
## 列表形式
h <- basicHeaderGatherer()
txt <- getURL(url="http://www.baidu.com", headerfunction = h$update)
names(h$value())

h$value()
# 查看服务器返回的头信息
## 字符串形式
h <- basicTextGatherer()
txt <- getURL("http://www.baidu.com", headerfunction = h$update)
names(h$value())
# NULL # 说明是字符串形式，没有列
h$value() # 所有的内容只是一个字符串
# [1] "HTTP/1.1 200 OK\r\nDate: Mon, 23 Feb 2015 15:18:28 GMT\r\nContent-Type: text/html\r\nContent-Length: 14613\r\nLast-Modified: Wed, 03 Sep 2014 02:48:32 GMT\r\nConnection: Keep-Alive\r\nVary: Accept-Encoding\r\nSet-Cookie: BAIDUID=FFF680C9F9631969198A77AAFF56096E:FG=1; expires=Thu, 31-Dec-37 23:55:55 GMT; max-age=2147483647; path=/; domain=.baidu.com\r\nSet-Cookie: BAIDUPSID=FFF680C9F9631969198A77AAFF56096E; expires=Thu, 31-Dec-37 23:55:55 GMT; max-age=2147483647; path=/; domain=.baidu.com\r\nSet-Cookie: BDSVRTM=0; path=/\r\nP3P: CP=\" OTI DSP COR IVA OUR IND COM \"\r\nServer: BWS/1.1\r\nPragma: no-cache\r\nCache-control: no-cache\r\nBDPAGETYPE: 1\r\nBDQID: 0xc1ae773200820725\r\nBDUSERID: 0\r\nAccept-Ranges: bytes\r\n\r\n"

cat(h$value()) # 用cat显示的，会比较好看

# 查看url请求的访问信息
curl <- getCurlHandle()

txt <- getURL(url="http://www.baidu.com", curl = curl)
names(getCurlInfo(curl))

getCurlInfo(curl)$response.code
# [1] 200
getCurlInfo(curl=curl)

# 设置自己的header，把系统设置成ihpone的系统Mac OS
myheader <- c(
  "User-Agent"="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0_1 like Mac OS X; ja-jp) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A306 Safari/6531.22.7",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
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
doc <-htmlParse(wp, asText = TRUE) # 这里切记encoding  
tables <- readHTMLTable(doc, header=F, which = 2)
# 选取第二个表
head(tables)
