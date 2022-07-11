# https://rpubs.com/bambangpe/647606
# https://blog.csdn.net/dingming001/article/details/72886841
# dummy variables https://blog.csdn.net/qq_42458954/article/details/86694980
rm(list = ls())
library(neuralnet)
dt <- read.csv("/media/wane/wade/EP/EPTLE_PET/PET-TLE234-radscore-RCS2.csv", header = T)
data <- read.csv(file.choose(), header = T)
# 进行数据的重新编码(recode), 批量分类变量转化为因子
dt <- dt[, -1:-4]
str(dt)
for (i in names(dt)[c(1:3, 5:13)]) {
  dt[, i] <- as.numeric(as.factor(dt[, i]))
}

# Splitting the dataset
# set.seed(123)
# ind <- sample(2, nrow(dt), replace = T, prob = c(0.7, 0.3))
# trainset <- dt[ind == 1, ]
# testset <- dt[ind == 2, ]
library(caTools)
set.seed(123)
split <- sample.split(dt$oneyr, SplitRatio = 0.7)
training_set <- subset(dt, split == 1)
test_set <- subset(dt, split == 0)

# Feature scaling
training_set[-1] <- scale(training_set[-1])
test_set[-1] <- scale(test_set[-1])

# Fitting Deep Feed Forward (DFF) Neural Network (NN)
library(h2o)
h2o.init(nthreads = -1)
classifier <- h2o.deeplearning(
  y = "oneyr",
  training_frame = as.h2o(training_set),
  activation = "Rectifier",
  hidden = c(6, 6),
  epochs = 100,
  train_samples_per_iteration = -2
)

# Predicting using the test set
prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set[-1]))
y_pred <- (prob_pred > 0.5)
y_pred <- as.vector(y_pred)

# Evaluating the model
cm <- table(test_set[, 1], y_pred > 0.5)
cm

# Disconnect from the h2o server
h2o.shutdown()
Y

training_set$heal <- training_set$oneyr == "0"
training_set$frac <- training_set$oneyr == "1"
attach(dt)
n <- names(dt)
paste(colnames(dt)[2:14], collapse = " + ")
f <- as.formula(paste("oneyr ~", paste(n[!n %in% "oneyr"], collapse = " + ")))
f
softplus <- function(x) log(1 + exp(x))
set.seed(123)
## 建立全连接网络分类器
mlpcla <- neuralnet(f, data = training_set,
                    hidden = c(5,5), ## 隐藏层神经元数量
                    act.fct = "logistic", ## 激活函数
                    linear.output = FALSE,
                    algorithm = "rprop+")
network <- neuralnet(f,
training_set,
hidden = 10, threshold = 0.01
)
network
# 输出构建好的神经网络模型的结果矩阵：
network$result.matrix
head(network$generalized.weights[[1]])
network$result.matrix
plot(network)
plot(network,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')
plotnet(network, pos_col = "red", neg_col = "grey") # NeuralNetTools

# gwplot函数可视化泛化权
par(mfrow = c(2, 2)) # 2*2画布
gwplot(network, selected.covariate = "side")
gwplot(network, selected.covariate = "Sex")
gwplot(network, selected.covariate = "Durmon")
gwplot(network, selected.covariate = "SGS")

testset[-5]
net.predict <- neuralnet::compute(network, test_set)$net.result
net.prediction <- c("1", "0")[apply(net.predict, 1, which.max)]
predict.table <- table(test_set$oneyr, net.prediction)
predict.table

library(e1071)
classAgreement(predict.table)
library(caret)
confusionMatrix(predict.table)

# Plot the neural network
plot(network)
# https://zhuanlan.zhihu.com/p/313525099
## 可视化全连接模型的网络结构
library(NeuralNetTools)
library(ggpol)
par(cex = .9)
plotnet(mlpcla,pos_col = "red", neg_col = "grey")
## 可视化模型中变量的重要性
olden(mlpcla)+ggtitle("Variable importance using connection weights")
## 可视化模型的预测效果
mlppre <- predict(mlpcla,training_set)
## 计算出预测的类别
mlpprelab <- apply(mlppre , 1, which.max)
## 可视化预测结果的## 可视化预测的混淆矩阵
ggplot() + geom_confmat(aes(x = training_set$oneyr, y = mlpprelab),
                        normalize = TRUE, text.perc = TRUE)+
  labs(x = "Reference",y = "Prediction")+
  scale_fill_gradient2(low="darkblue", high="lightgreen")

# Test the neural network on some training data
net.results <- neuralnet::compute(network, test_set) # Run them through the neural network
# Lets see what properties net.sqrt has
ls(net.results)
# Lets see the results
print(net.results$net.result)
# Lets display a better version of the results
cleanoutput <- cbind(test_set, sqrt(test_set), as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input", "Expected Output", "Neural Net Output")
print(cleanoutput)


# 4.Neural Networks Classifing  https://rpubs.com/bambangpe/647606
library("mlbench")
library(neuralnet)
boxplot(dt[, ])
hist(as.numeric(dt$SE))
par(mfrow = c(3, 3))
hist(as.numeric(data_cleaned$Cl.thickness))
hist(as.numeric(data_cleaned$Cell.size))
hist(as.numeric(data_cleaned$Cell.shape))
hist(as.numeric(data_cleaned$Marg.adhesion))
hist(as.numeric(data_cleaned$Epith.c.size))
hist(as.numeric(data_cleaned$Bare.nuclei))
hist(as.numeric(data_cleaned$Bl.cromatin))
hist(as.numeric(data_cleaned$Normal.nucleoli))
hist(as.numeric(data_cleaned$Mitoses))

max_data <- apply(input, 2, max)
min_data <- apply(input, 2, min)
input_scaled <- as.data.frame(scale(input, center = min_data, scale = max_data - min_data))
# View(input_scaled)





# 将目标变量转换为因子
dt$Y <- factor(dt$Y, levels = c(1, 0), labels = c("H", "F"))
# 构建训练样本集和测试样本集
set.seed(123)
index <- sample(c(1, 2), nrow(dt), replace = TRUE, prob = c(0.7, 0.3))
trainset <- dt[index == 1, ]
testset <- dt[index == 2, ]

# 使用nnet包中的nnet()函数建模
library(nnet)
# 通过循环，确定最佳的节点数
err1 <- 0
err2 <- 0
for (i in 1:45) {
  set.seed(123)
  model <- nnet(Y ~ ., data = trainset, maxit = 300, size = i, trace = FALSE)
  err1[i] <- sum(predict(model, trainset, type = "class") != trainset$Y) / nrow(trainset)
  err2[i] <- sum(predict(model, testset, type = "class") != testset$Y) / nrow(testset)
}
plot(err1, type = "b", col = "black", lty = 2, lwd = 2, ylab = "误差", xlab = "节点数", ylim = c(0, 0.05), pch = 10)
lines(err2, type = "b", col = "blue", lty = 2, lwd = 2, pch = 23)
legend(locator(1),
  legend = c("训练集误差率", "测试集误差率"), col = c("black", "blue"), lty = c(2, 2), lwd = c(2, 2), bty = "n",
  pch = c(10, 23)
)
# 通过返回的图形结果，选择最佳的节点数为4

# 通过循环，确定最大迭代次数
err1 <- numeric()
err2 <- numeric()
for (i in 1:500) {
  set.seed(1234)
  model <- nnet(Y ~ ., data = trainset, maxit = i, size = 4, trace = FALSE)
  err1[i] <- sum(predict(model, train, type = "class") != trainset$Y) / nrow(trainset)
  err2[i] <- sum(predict(model, test, type = "class") != trainset$Y) / nrow(testset)
}
plot(err1, type = "l", col = "black", lty = 1, ylab = "误差", xlab = "节点数")
lines(err2, type = "l", col = "blue", lty = 4)
legend(locator(1), legend = c("训练集误差率", "测试集误差率"), col = c("black", "blue"), lty = c(1, 4), bty = "n")
# 通过返回的图形结果，选择最大迭代次数为50

# 建立最终的神经网络模型
set.seed(1234)
model_nnet <- nnet(Y ~ ., data = trainset, maxit = 50, size = 4, trace = FALSE)
pred_nnet <- predict(model_nnet, testset, type = "class")
# 预测精度
Freq_nnet <- table(testset$Y, pred_nnet)
Freq_nnet
accuracy_nnet <- sum(diag(Freq_nnet)) / sum(Freq_nnet)
accuracy_nnet
# 模型准确判断率超过99%，模型非常完美的刻画了数据。

# 使用RSNNS包中的mlp()函数建模
library(RSNNS)
# 将数据顺序打乱
data_cancer <- cancer[sample(1:nrow(cancer), length(1:nrow(cancer))), 2:ncol(cancer)]
# 定义网络输入
cancerValues <- dt[, -1]
# 定义网络输出，并将数据进行格式转换
cancerTargets <- decodeClassLabels(dt[, 1])
# 从中划分出训练样本和检验样本
set.seed(1234)
model_cancer <- splitForTrainingAndTest(cancerValues, cancerTargets, ratio = 0.20)
# 数据标准化
model_cancer <- normTrainingAndTestSet(model_cancer, type = "0_1")
# 利用mlp命令执行前馈反向传播神经网络算法
model_mlp <- mlp(model_cancer$inputsTrain, model_cancer$targetsTrain, size = 4, maxit = 100, inputsTest = model_cancer$inputsTest, targetsTest = model_cancer$targetsTest)
# 利用上面建立的模型进行预测
pred_mlp <- predict(model_mlp, model_cancer$inputsTest)
# 生成混淆矩阵，观察预测精度
Freq_mlp <- confusionMatrix(model_cancer$targetsTest, pred_mlp)
Freq_mlp
accuracy_mlp <- sum(diag(Freq_mlp)) / sum(Freq_mlp)
accuracy_mlp


# 全连接神经网络回归的相关可视化
library(RSNNS)
library(caret)
library(neuralnet)
library(NeuralNetTools)
library(readxl)
## 读取要使用的数据
# diadf <- read.csv("data/chap11/diabetes.csv", sep = "\t")
diadf <- read_excel("/home/wane/Desktop/TBS/Monkey/BMC.xlsx")
diadf <- diadf[-1]
## 数据max-min归一化到0-1之间
diadf_s <- normalizeData(diadf, type = "0_1")
## 数据切分
set.seed(123)
datasplist <- splitForTrainingAndTest(diadf_s[, 2:6], diadf_s[, 1],
  ratio = 0.3
)
## MLP回归模型
mlpreg <- mlp(datasplist$inputsTrain, ## 训练数据
  datasplist$targetsTrain,
  size = c(10, 10, 10), ## 隐藏层和神经元数量
  maxit = 200, ## 最大迭代次数
  learnFunc = "Rprop", ## 学习算法
  hiddenActFunc = "Act_TanH", ## 激活函数
  inputsTest = datasplist$inputsTest, ## 测试数据
  targetsTest = datasplist$targetsTest,
  metric = "RSME"
) 
## 评价指标
## 可视化模型训练过程中误差的变化情况
plotIterativeError(mlpreg, main = "MLP Iterative Error")
summary(mlpreg)

## 可视化训练集上的拟合结果
par(mfrow = c(1, 2))
plotRegressionError(datasplist$targetsTrain, mlpreg$fitted.values,
  main = "MLP train fit"
)
## 可视化测试集上的拟合结果
plotRegressionError(datasplist$targetsTest, mlpreg$fittedTestValues,
  main = "MLP test fit"
)

# https://deeplearningmath.org/resources/illustration-shallow-nn
# Illustration Neural Network
set.seed(123)
library(MASS)
data <- read.csv("/home/wane/Desktop/TBS/Monkey/VoxelNumbers_InMachin_atlas_whole.csv")
data <- data[, 10:15]
any(is.na(data))

# Split the data and Run a linear model
index <- sample(1:nrow(data), round(0.70 * nrow(data)))
train <- data[index, ]
test <- data[-index, ]
lm.fit <- glm(Age ~ ., data = train)
summary(lm.fit)

pr.lm <- predict(lm.fit, test)
(MSE.lm <- sum((pr.lm - test$medv)^2) / nrow(test))

# A shallow Neural network
library(neuralnet)
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index, ]
test_ <- scaled[-index, ]

n <- names(train_)
f <- as.formula(paste("Age ~", paste(n[!n %in% "Age"], collapse = " + ")))
lin <- function(x) x
nn <- neuralnet(f, data = train_, hidden = 3, linear.output = TRUE)
# # Plot the neural network
plot(nn, rep = "best")

# Performance
pr.nn <- predict(nn, test_[, 2:6])
print(head(pr.nn))
pr.nn_ <- pr.nn * (max(data$Age) - min(data$Age)) + min(data$Age)
test.r <- (test_$Age) * (max(data$Age) - min(data$Age)) + min(data$Age)

MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test_)
(res <- c(MSE.lm, MSE.nn))

# Plot regression line
plot(test$Age, pr.nn_,
  col = "red",
  main = "Real vs Predicted"
)
abline(0, 1, lwd = 2)

# add some units
nn8 <- neuralnet(f, data = train_, hidden = 8, linear.output = TRUE)
plot(nn8, rep = "best")

pr.nn <- predict(nn8, test_[, 2:6])
pr.nn_ <- pr.nn * (max(data$Age) - min(data$Age)) + min(data$aAge)
test.r <- (test_$Age) * (max(data$Age) - min(data$Age)) + min(data$Age)

MSE.nn8 <- sum((test.r - pr.nn_)^2) / nrow(test_)
res <- c(res, MSE.nn8)
res

# Plot regression line
plot(test$Age, pr.nn_,
  col = "red",
  main = "Real vs Predicted"
)
abline(0, 1, lwd = 2)

# add a layer
set.seed(123)
nn8.4 <- neuralnet(f, data = train_, hidden = c(8, 4), linear.output = TRUE)
plot(nn8.4, rep = "best")

pr.nn <- predict(nn8.4, test_[, 2:6])
pr.nn_ <- pr.nn * (max(data$Age) - min(data$Age)) + min(data$Age)
test.r <- (test_$Age) * (max(data$Age) - min(data$Age)) + min(data$Age)

MSE.nn8 <- sum((test.r - pr.nn_)^2) / nrow(test_)
res <- c(res, MSE.nn8)
res

# Plot regression line
plot(test$Age, pr.nn_,
  col = "red",
  main = "Real vs Predicted"
)
abline(0, 1, lwd = 2)


