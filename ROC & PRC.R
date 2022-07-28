# ROC & PRC
# https://blog.csdn.net/NickyCat/article/details/124952044?utm_medium=distribute.pc_relevant.none-task-blog-2~default~baidujs_baidulandingword~default-0-124952044-blog-107793043.pc_relevant_multi_platform_whitelistv3&spm=1001.2101.3001.4242.1&utm_relevant_index=2
library(pROC)
# 在得到最终模型后，算出预测值
pred <- predict(model, newdata = X_test)
# ROC, 输入真实的label和预测的值（如果是做某个指标用于预测死亡率的ROC曲线，那就用是否死亡代替Y_test, 用这个指标的值代替pred），ci是为了算95%CI
roc_data <- roc(Y_test, as.numeric(pred), ci = TRUE)
# 我习惯把数据整理出来，用ggplot2再作图
roc_data <- data.table(
  sensitivity = roc_data$sensitivities,
  specificity = roc_data$specificities,
  yorden = roc_data$sensitivities + roc_data$specificities - 1, # 约登指数
  auc = roc_data$auc,
  auc_low = as.numeric(roc_data$ci)[1],
  auc_up = as.numeric(roc_data$ci)[3]
)
# 顺便把95%CI也存一下

# ROC曲线
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "red") +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 1),
    linetype = "dotted",
    color = "grey50"
  ) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle("ROC Curve") +
  annotate("text",
    x = 0.7, y = 0.2,
    label = paste0("AUC = ", round(xgb_roc$auc, 3))
  ) +
  annotate("text",
    x = 0.7, y = 0.12,
    label = paste0("Sensitivity = ", round(roc_data$sensitivity[which.max(roc_data$yorden)], 3))
  ) +
  annotate("text",
    x = 0.7, y = 0.05,
    label = paste0("Specificity = ", round(roc_data$specificity[which.max(roc_data$yorden)], 3))
  ) +
  theme_bw() # 这里打出了约登指数最大时的敏感性与特异性


library(modEvA)
# 和ROC曲线的方法差不多
pred <- predict(model, newdata = X_test)
# curve也可以选ROC，可以直接用AUC()画图，但我还是习惯保存一下数据，用ggplot2画图
pr_auc <- AUC(obs = Y_test, pred = as.numeric(pred), curve = "PR")
pr_auc_data <- data.table(
  precision = pr_auc$thresholds$precision,
  recall = pr_auc$thresholds$sensitivity,
  prauc = pr_auc$AUC
)
# 画图
ggplot(pr_auc_data, aes(x = recall, y = precision)) +
  geom_line() +
  geom_segment(
    aes(x = 0, y = 1, xend = 1, yend = 0),
    linetype = "dashed",
    color = "grey50",
    alpha = 0.8
  ) +
  xlab("Recall") +
  ylab("Precision") +
  ggtitle("Precision-Recall Curve") +
  guides(linetype = "none") +
  annotate("text",
    label = paste0("PR-AUC: ", round(unique(pr_auc_data$prauc), 3)),
    x = 0.40, y = 0.18
  ) +
  theme_bw()
