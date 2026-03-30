setwd("C:\\Users\\Administrator\\Desktop\\MI-0909")
# 加载必要的包
library(dplyr)
library(igraph)
library(ggplot2)

# 导入CSV文件
association_rules <- read.csv("Supplementary_Table_S1_Association_Rules_Full.csv", 
                              stringsAsFactors = FALSE)
# 查看数据结构
head(association_rules)
str(association_rules)

# 加载必要的包
library(igraph)
library(dplyr)
library(stringr)

# 修复的数据预处理函数
# 重新编写规则解析函数
# 重新编写规则解析函数（修正版）
parse_rules_correct <- function(association_rules) {
  edges_df <- data.frame(
    from = character(), 
    to = character(), 
    support = numeric(), 
    confidence = numeric(),
    lift = numeric(), 
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(association_rules)) {
    rule_text <- association_rules$rules[i]
    
    # 用" => "分割字符串（核心修正：移除无效的@ref标记）
    parts <- strsplit(rule_text, " => ")[[1]]
    
    if(length(parts) == 2) {
      # 去除花括号（修正：正确闭合函数调用）
      antecedent <- gsub("\\{|\\}", "", parts[1])  # 移除错误的(@ref)
      consequent <- gsub("\\{|\\}", "", parts[2])  # 移除错误的(@ref)
      
      # 处理多项目的情况（如{F68,F69}）
      antecedent_items <- unlist(strsplit(antecedent, ","))
      consequent_items <- unlist(strsplit(consequent, ","))
      
      # 为每个前项-后项对创建边
      for(ant in antecedent_items) {
        for(cons in consequent_items) {
          edges_df <- rbind(edges_df, data.frame(
            from = ant,
            to = cons,
            support = association_rules$support[i],
            confidence = association_rules$confidence[i],
            lift = association_rules$lift[i],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(edges_df)
}

# 解析所有规则（假设你的数据框名为association_rules）
edges_data <- parse_rules_correct(association_rules)

# 查看解析结果
head(edges_data)
cat("解析出的边数:", nrow(edges_data), "\n")


library(igraph)
library(scales)

# 1. 先处理数据：合并重复边（因为双向关联会重复计算）
# 对有向图按"from+to"去重，保留唯一关联
edges_unique <- unique(edges_data[, c("from", "to", "support", "confidence", "lift")])

# 2. 创建网络对象（保留有向性，但优化连接展示）
network_graph <- graph_from_data_frame(edges_unique, directed = TRUE)

# 3. 节点分组与属性（明确标注+区分大小）
V(network_graph)$group <- ifelse(
  V(network_graph)$name %in% c("F50", "F51"), "限制高脂对",
  ifelse(V(network_graph)$name %in% c("F65", "F66", "F67", "F68", "F69"), 
         "喂养规律组", "监督行为组")
)

# 节点大小：固定且足够大，确保标签可见
V(network_graph)$size <- 30  

# 4. 颜色与边样式（突出组内关联）
group_colors <- c(
  "限制高脂对" = "#E41A1C", 
  "喂养规律组" = "#377EB8", 
  "监督行为组" = "#4DAF4A"
)
V(network_graph)$color <- group_colors[V(network_graph)$group]

# 边的样式：组内边加粗，组间边（如果有的话）变细
E(network_graph)$width <- ifelse(
  V(network_graph)$group[tail_of(network_graph, E(network_graph))] == 
    V(network_graph)$group[head_of(network_graph, E(network_graph))],
  2.5,  # 组内边粗
  1     # 组间边细（当前数据可能没有）
)
E(network_graph)$color <- adjustcolor("gray40", alpha.f = 0.5)

# 5. 关键优化：强制分组布局（确保三组分离且内部紧凑）
# 先按组计算初始中心位置（手动分离三组）
group_centers <- data.frame(
  group = c("限制高脂对", "喂养规律组", "监督行为组"),
  x = c(-15, 0, 15),  # 横向分离
  y = c(0, 0, 0)      # 纵向对齐
)

# 为每个节点分配初始位置（基于组中心+随机扰动）
set.seed(123)
V(network_graph)$x <- group_centers$x[match(V(network_graph)$group, group_centers$group)] + 
  rnorm(vcount(network_graph), 0, 1)  # 组内小范围随机
V(network_graph)$y <- group_centers$y[match(V(network_graph)$group, group_centers$group)] + 
  rnorm(vcount(network_graph), 0, 1)

# 基于初始位置优化布局（组内紧凑，组间分离）
layout <- layout_with_fr(
  network_graph,
  weights = E(network_graph)$width,  # 组内边权重更高，拉得更紧
  start = cbind(V(network_graph)$x, V(network_graph)$y),  # 强制从分组位置开始
  niter = 1000,  # 多次迭代确保稳定
  repulserad = vcount(network_graph)^2.1  # 增大组间排斥力
)

# 6. 绘图（确保标签清晰、关联可见）
par(mar = c(0, 0, 1, 0), bg = "white")
plot(
  network_graph,
  layout = layout,
  vertex.size = V(network_graph)$size,
  vertex.color = V(network_graph)$color,
  vertex.frame.color = "white",
  vertex.label = V(network_graph)$name,  # 显式标注节点名
  vertex.label.color = "black",
  vertex.label.cex = 0.9,
  vertex.label.font = 2,
  edge.width = E(network_graph)$width,
  edge.color = E(network_graph)$color,
  edge.arrow.size = 0.3,  # 箭头适中，体现方向
  edge.curved = 0.1,
  main = "关联规则网络图：三组内部全关联特征"
)

# 图例（清晰说明分组）
legend(
  "bottom",
  legend = c("限制高脂对 (F50-F51)", "喂养规律组 (F65-F69)", "监督行为组 (F70-F73)"),
  fill = group_colors,
  bty = "n",
  cex = 0.8,
  ncol = 3  # 横向排列，节省空间
)

##########图1.0########
install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

# 假设edges_data是你的规则数据，包含from（前项）、to（后项）、support、lift
# 1. 为前项和后项添加分组信息
edges_data <- edges_data %>%
  mutate(
    from_group = case_when(
      from %in% c("F50", "F51") ~ "限制高脂对",
      from %in% c("F65", "F66", "F67", "F68", "F69") ~ "喂养规律组",
      TRUE ~ "监督行为组"
    ),
    to_group = case_when(
      to %in% c("F50", "F51") ~ "限制高脂对",
      to %in% c("F65", "F66", "F67", "F68", "F69") ~ "喂养规律组",
      TRUE ~ "监督行为组"
    )
  )

# 2. 绘制分组矩阵图
ggplot(edges_data, aes(x = to, y = from, size = support, color = factor(lift))) +
  geom_point(alpha = 0.8) +  # 点表示存在关联规则
  facet_grid(from_group ~ to_group, scales = "free", space = "free") +  # 按组分隔
  scale_size(range = c(2, 6)) +  # 支持度越大，点越大
  scale_color_manual(values = c("1.4" = "#377EB8")) +  # 你的lift固定为1.4，单颜色
  labs(
    x = "后项 (RHS)",
    y = "前项 (LHS)",
    size = "支持度 (Support)",
    color = "提升度 (Lift)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray90", color = NA)  # 分组背景色
  )


###########图2.0#######
library(tidyverse)
library(ggplot2)

# 1. 为前项和后项添加英文分组信息
edges_data <- edges_data %>%
  mutate(
    from_group = case_when(
      from %in% c("F50", "F51") ~ "Targeted Food Restriction",
      from %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    to_group = case_when(
      to %in% c("F50", "F51") ~ "Targeted Food Restriction",
      to %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    )
  )

# 2. 定义更鲜明的颜色（基于提升度，支持多值扩展）
# 若lift有多个值，可添加更多颜色；当前固定为1.4时使用亮蓝色
lift_colors <- c("1.4" = "#2E9FDF")  # 亮蓝色，比原#377EB8更鲜明

# 3. 绘制优化后的分组矩阵图
ggplot(edges_data, aes(x = to, y = from, size = support, color = factor(lift))) +
  geom_point(alpha = 0.9) +  # 提高透明度，增强视觉效果
  facet_grid(from_group ~ to_group, scales = "free", space = "free") +
  scale_size(range = c(3, 8), name = "Support") +  # 增大点大小范围，更易区分
  scale_color_manual(values = lift_colors, name = "Lift") +
  labs(
    x = "Right-Hand Side (RHS)",  # 全英文坐标轴
    y = "Left-Hand Side (LHS)",
    title = "Association Rules Matrix of Behavioral Patterns"  # 英文标题
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_text(size = 10, face = "bold"),  # 分组标签加粗
    strip.text.y = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "#F0F0F0", color = NA),  # 浅灰色分组背景
    panel.grid.minor = element_blank(),
    legend.key = element_rect(fill = "white"),  # 图例背景为白色
    legend.position = "right"
  )


#####图3.0###############
library(tidyverse)
library(ggplot2)
library(scales)

# 1. 数据处理：添加英文分组+指定分组顺序（确保Monitor最左，Directive中间，Food最右）
edges_data <- edges_data %>%
  mutate(
    # 定义英文分组
    from_group = case_when(
      from %in% c("F50", "F51") ~ "Targeted Food Restriction",
      from %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    to_group = case_when(
      to %in% c("F50", "F51") ~ "Targeted Food Restriction",
      to %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    # 强制分组顺序（左到右：Monitoring → Directive → Food）
    from_group = factor(from_group, levels = c("Monitoring Behavior", "Directive Feeding", "Targeted Food Restriction")),
    to_group = factor(to_group, levels = c("Monitoring Behavior", "Directive Feeding", "Targeted Food Restriction"))
  )

# 2. 定义颜色系统（基于lift的蓝色渐变，支持多值）
# 提取数据中所有lift值，生成对应蓝色系颜色（值越高颜色越深）
unique_lifts <- unique(edges_data$lift)
lift_palette <- colorRampPalette(c("#99CCFF", "#0066CC"))(length(unique_lifts))  # 浅蓝到深蓝渐变
names(lift_palette) <- as.character(unique_lifts)

# 3. 绘制优化后的矩阵图（参考图5风格）
ggplot(edges_data, aes(x = to, y = from, size = support, color = factor(lift))) +
  geom_point(alpha = 0.85) +  # 适度透明，避免重叠遮挡
  # 按分组分面（行=前项组，列=后项组）
  facet_grid(from_group ~ to_group, scales = "free", space = "free") +
  # 支持度大小映射（自动匹配数据中的support值范围，不固定区间）
  scale_size_continuous(
    name = "Support",
    range = c(2, 8),  # 点大小范围（小→大对应低→高支持度）
    breaks = sort(unique(edges_data$support)),  # 仅显示数据中存在的support值
    labels = label_number(accuracy = 0.01)  # 保留2位小数
  ) +
  # Lift颜色映射（蓝色渐变，无黑框）
  scale_color_manual(
    name = "Lift",
    values = lift_palette,
    breaks = as.character(sort(unique_lifts)),  # 按升序显示
    labels = paste0("+", sort(unique_lifts))  # 格式化为"+1.4"
  ) +
  # 标题和坐标轴（全英文）
  labs(
    x = "Right-Hand Side (RHS)",
    y = "Left-Hand Side (LHS)",
    title = "Grouped Matrix for Association Rules"
  ) +
  # 主题优化（去边框、简洁图例、分组清晰）
  theme_minimal() +
  theme(
    # 坐标轴文本
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    # 标题居中
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    # 分组标签（加粗+背景色区分）
    strip.text.x = element_text(size = 10, face = "bold", color = "black"),
    strip.text.y = element_text(size = 10, face = "bold", color = "black"),
    strip.background = element_rect(fill = "#F5F5F5", color = NA),  # 浅灰背景
    # 图例（无黑框、右对齐）
    legend.position = "right",
    legend.key = element_blank(),  # 图例去背景
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    # 网格线（仅保留主网格，增强可读性）
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.3),
    # 去除多余边框
    panel.border = element_blank(),
    axis.line = element_line(color = "gray50", linewidth = 0.3)
  )

library(tidyverse)
library(ggplot2)
library(scales)

# 1. 数据处理：保持分组顺序（Monitoring → Directive → Food）
edges_data <- edges_data %>%
  mutate(
    from_group = case_when(
      from %in% c("F50", "F51") ~ "Targeted Food Restriction",
      from %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    to_group = case_when(
      to %in% c("F50", "F51") ~ "Targeted Food Restriction",
      to %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    from_group = factor(from_group, levels = c("Monitoring Behavior", "Directive Feeding", "Targeted Food Restriction")),
    to_group = factor(to_group, levels = c("Monitoring Behavior", "Directive Feeding", "Targeted Food Restriction"))
  )

# 2. 定义红色系颜色（lift值越高，颜色越深）
unique_lifts <- unique(edges_data$lift)
lift_palette <- colorRampPalette(c("#FFCCCC", "#CC0905"))(length(unique_lifts))  # 浅红→深红渐变
names(lift_palette) <- as.character(unique_lifts)

# 3. 绘制红色系矩阵图（其他参数不变）
ggplot(edges_data, aes(x = to, y = from, size = support, color = factor(lift))) +
  geom_point(alpha = 0.85) +
  facet_grid(from_group ~ to_group, scales = "free", space = "free") +
  scale_size_continuous(
    name = "Support",
    range = c(2, 8),
    breaks = sort(unique(edges_data$support)),
    labels = label_number(accuracy = 0.01)
  ) +
  scale_color_manual(
    name = "Lift",
    values = lift_palette,
    breaks = as.character(sort(unique_lifts)),
    labels = paste0("+", sort(unique_lifts))
  ) +
  labs(
    x = "Right-Hand Side (RHS)",
    y = "Left-Hand Side (LHS)",
    title = "Grouped Matrix for Association Rules"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    strip.text.x = element_text(size = 10, face = "bold", color = "black"),
    strip.text.y = element_text(size = 10, face = "bold", color = "black"),
    strip.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.position = "right",
    legend.key = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.3),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray50", linewidth = 0.3)
  )


library(tidyverse)
library(ggplot2)
library(scales)

# 1. 数据处理（保持不变）
edges_data <- edges_data %>%
  mutate(
    from_group = case_when(
      from %in% c("F50", "F51") ~ "Targeted Food Restriction",
      from %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    to_group = case_when(
      to %in% c("F50", "F51") ~ "Targeted Food Restriction",
      to %in% c("F65", "F66", "F67", "F68", "F69") ~ "Directive Feeding",
      TRUE ~ "Monitoring Behavior"
    ),
    from_group = factor(from_group, levels = c("Monitoring Behavior", "Directive Feeding", "Targeted Food Restriction")),
    to_group = factor(to_group, levels = c("Monitoring Behavior", "Directive Feeding", "Targeted Food Restriction"))
  )

# 2. 红色系颜色（保持不变）
unique_lifts <- unique(edges_data$lift)
lift_palette <- colorRampPalette(c("#de8382", "#CC0905"))(length(unique_lifts))
names(lift_palette) <- as.character(unique_lifts)

# 3. 绘制图表（核心调整：Support图例为空心圆）
ggplot(edges_data, aes(x = to, y = from, size = support, color = factor(lift))) +
  geom_point(alpha = 0.85) +
  facet_grid(from_group ~ to_group, scales = "free", space = "free") +
  # 关键调整：support图例用空心圆（shape = 21，fill = NA）
  scale_size_continuous(
    name = "Support",
    range = c(2, 8),
    breaks = sort(unique(edges_data$support)),
    labels = label_number(accuracy = 0.01),
    guide = guide_legend(
      override.aes = list(
        shape = 21,       # 空心圆形状
        fill = NA,        # 内部无填充
        color = "#cc0905",  # 边框颜色（与点的边框一致）
        stroke = 1        # 边框粗细
      )
    )
  ) +
  scale_color_manual(
    name = "Lift",
    values = lift_palette,
    breaks = as.character(sort(unique_lifts)),
    labels = paste0("+", sort(unique_lifts))
  ) +
  labs(
    x = "Right-Hand Side (RHS)",
    y = "Left-Hand Side (LHS)",
    title = "Grouped Matrix for Association Rules"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    strip.text.x = element_text(size = 10, face = "bold", color = "black"),
    strip.text.y = element_text(size = 10, face = "bold", color = "black"),
    strip.background = element_rect(fill = "#F5F5F5", color = NA),
    legend.position = "right",
    legend.key = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.3),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray50", linewidth = 0.3)
  )
