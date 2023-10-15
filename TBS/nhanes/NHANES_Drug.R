## Drug 提取示例
library(haven)
#### 1. 读取文件 ####
# 文件1: 编码
drug.data.code <- read_xpt('C:/Users/wane1/Downloads/NHANES数据挖掘从入门到精通/rxq_drug.xpt')
# View(drug.data.code)

# 文件1: 人群服药数据
drug.data <- read_xpt('C:/Users/wane1/Downloads/NHANES数据挖掘从入门到精通/P_rxq_rx.xpt')
# View(drug.data)
head(drug.data)

#### 2. 找到目标药物对应编码（用文本关键词进行多列搜索） #### 
# 使用apply函数搜索多列，通过关键词（一类药物可能有多个关键词）确定满足条件的药物代码 RXDDRGID，并通过 RXDDRGID 与 人群服药的数据相匹配
cols_to_search <- colnames(drug.data.code) # 确定要搜索的列名

matches.1 <- apply(drug.data.code[cols_to_search], 1, function(x) any(grepl("epilepsy", x, ignore.case = TRUE)))
matches.2 <- apply(drug.data.code[cols_to_search], 1, function(x) any(grepl("G40", x, ignore.case = TRUE)))
matches.3 <- apply(drug.data.code[cols_to_search], 1, function(x) any(grepl("seizure", x, ignore.case = TRUE)))
matches.4 <- apply(drug.data.code[cols_to_search], 1, function(x) any(grepl("phosphodiesterase", x, ignore.case = TRUE)))

# 查看匹配的情况（不同关键词对应的匹配编码数量） 
sum(matches.1) # 11
sum(matches.2) # 2
sum(matches.3) # 2
sum(matches.4) # 1
# 
# sum(matches.1|matches.2|matches.3|matches.4) #16
# 
# matches <- matches.1|matches.2|matches.3|matches.4

#### 3. 返回包含匹配项的行，并且标记 drug.index = 1 #### 
match.code.data <- drug.data.code[matches, ]
match.code.data$drug.index <- 1

#### 4. 合并 drug.data 和 match.code.data #### 
match.drug.data <- merge(drug.data[, c('SEQN', 'RXDDRGID')], 
                         match.code.data[,c('RXDDRGID', 'drug.index')],
                         by = 'RXDDRGID', all.x = T)

# 确认下人数
length(which(match.drug.data$drug.index == 1)) # 143
