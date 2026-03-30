

items <- analysis_data_clean[, paste0("F", 70:73)] 

covs <- analysis_data_clean[, c("MOEDU", "INCOME", "AREA", "GENDER", "Age", "zbmi", "N1", "N2")]

tree = EGAtree(data = items,
               covariates = covs)

print(tree)#查看egatree构建结果
summary(tree)

EGAtree_teststats(tree,node.id = 3)

#EGAtree构建结果可视化-总图
EGAtree_plot(tree)

#EGAtree构建结果可视化-每个子图 node.id 是终端节点，代表最终划分的异质性子群。 

EGAtree_ega(tree, node.id = 8)
