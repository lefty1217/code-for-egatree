# 创建数据框，包含每个条目在7个节点中的维度归属
data <- data.frame(
  Item = paste0("F", 39:73),
  Node_2 = c(1,1,1,1,1,1,1,2,2,2,3,3,3,1,1,1,1,1,1,4,4,4,4,4,5,5,6,6,6,5,5,5,5),
  Node_7 = c(1,1,1,1,1,2,1,3,3,3,4,4,2,2,1,1,1,1,1,5,5,5,5,5,6,6,7,7,7,2,2,2,2),
  Node_8 = c(1,1,1,1,1,2,1,3,3,3,3,3,2,2,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,2,2,2,2),
  Node_9 = c(1,1,1,1,1,2,1,3,3,3,2,2,2,2,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,6,6,6,6),
  Node_10 = c(1,1,1,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,3,3,3,3,3,3,3,4,4,4,5,5,5,5),
  Node_12 = c(1,1,1,1,1,2,1,3,3,3,2,2,2,2,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,6,6,6,6),
  Node_13 = c(1,1,1,1,1,2,1,3,3,3,3,3,2,2,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,2,2,2,2)
)

# 计算香农熵的函数
shannon_entropy <- function(x) {
  freq <- table(x) / length(x)
  entropy <- -sum(freq * log2(freq))
  return(entropy)
}

# 计算每个条目的熵
data$Entropy <- apply(data[, -1], 1, shannon_entropy)

# 显示结果
result <- data[, c("Item", "Entropy")]
print(result, row.names = FALSE)



# 创建数据框，包含每个条目在7个节点中的维度归属
data <- data.frame(
  Item = paste0("F", 39:73),
  Node_2 = c(1,1,1,1,1,1,1,1,2,2,2,3,3,3,1,1,1,1,1,1,1,4,4,4,4,4,5,5,6,6,6,5,5,5,5),
  Node_7 = c(1,1,1,1,1,2,1,1,3,3,3,4,4,2,2,1,1,1,1,1,1,5,5,5,5,5,6,6,7,7,7,2,2,2,2),
  Node_8 = c(1,1,1,1,1,2,1,1,3,3,3,3,3,2,2,1,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,2,2,2,2),
  Node_9 = c(1,1,1,1,1,2,1,1,3,3,3,2,2,2,2,1,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,6,6,6,6),
  Node_10 = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,1,3,3,3,3,3,3,3,4,4,4,5,5,5,5),
  Node_12 = c(1,1,1,1,1,2,1,1,3,3,3,2,2,2,2,1,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,6,6,6,6),
  Node_13 = c(1,1,1,1,1,2,1,1,3,3,3,3,3,2,2,1,1,1,1,1,1,4,4,4,4,4,4,4,5,5,5,2,2,2,2)
)

# 检查数据框结构
str(data)
head(data)

# 计算香农熵的函数
shannon_entropy <- function(x) {
  # 计算频率
  freq <- table(x) / length(x)
  # 计算熵
  entropy <- -sum(freq * log2(freq))
  return(entropy)
}

# 计算每个条目的熵（稳定性）
stability_results <- data.frame(
  Item = character(),
  Entropy = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(data)) {
  item <- data$Item[i]
  dimension_assignments <- as.numeric(data[i, c("Node_2", "Node_7", "Node_8", "Node_9", "Node_10", "Node_12", "Node_13")])
  entropy <- shannon_entropy(dimension_assignments)
  stability_results <- rbind(stability_results, data.frame(Item = item, Entropy = entropy))
}

# 显示结果
print(stability_results)

# 按熵值排序
stability_results_sorted <- stability_results[order(stability_results$Entropy, decreasing = TRUE), ]
print("按稳定性排序结果:")
print(stability_results_sorted)

# 可视化结果
library(ggplot2)
ggplot(stability_results, aes(x = reorder(Item, Entropy), y = Entropy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "条目维度归属稳定性分析", 
       x = "条目", 
       y = "香农熵（熵值越高越不稳定）") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 添加稳定性分类
stability_results$Stability_Level <- cut(stability_results$Entropy,
                                         breaks = c(-Inf, 0.5, 1.0, Inf),
                                         labels = c("高稳定", "中等稳定", "低稳定"))

# 显示分类结果
print("稳定性分类:")
print(stability_results[, c("Item", "Entropy", "Stability_Level")])
