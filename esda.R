#附录E ESDA分析代码（R语言）
#注意：因原始数据涉及保密要求，本代码不包含数据文件。
#运行前请修改 setwd() 为您本地的工作目录，并确保 shp 和 xlsx 文件在该目录下。

#漳州市乡镇老龄化空间格局ESDA分析
library(sf)
library(readxl)
library(spdep)
library(dplyr)
#设置工作目录
setwd("C:/Users/12568/Documents/四")
#一、数据读取与匹配
#读取矢量数据
town_sf <- st_read("zhangzhou_township_120.shp", quiet = TRUE)
#读取综合得分数据
score_df <- read_excel("漳州市乡镇老龄化综合得分排名.xlsx")
#去除名称中的空格
town_sf$县 <- trimws(town_sf$县)
town_sf$乡 <- trimws(town_sf$乡)
score_df$区县 <- trimws(score_df$区县)
score_df$乡镇街道名称 <- trimws(score_df$乡镇街道名称)
#按区县和乡镇名称合并数据
town_merge <- merge(town_sf, score_df, 
                    by.x = c("县", "乡"), 
                    by.y = c("区县", "乡镇街道名称"),
                    all.x = TRUE)
#提取老龄化综合得分
scores <- town_merge$综合得分
#二、空间权重矩阵构建（三种矩阵对比）
#提取质心坐标
town_centroid <- st_centroid(town_merge)
coord_mat <- st_coordinates(town_centroid)
# ---------- 2.1 Queen 一阶邻接矩阵（基准） ----------
nb_queen <- poly2nb(town_merge, queen = TRUE)
listw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
cat("Queen邻接：邻居关系总数 =", sum(card(nb_queen)), "\n")
cat("Queen邻接：平均邻居数 =", mean(card(nb_queen)), "\n")
# ---------- 2.2 距离阈值矩阵（18公里） ----------
nb_dist18 <- dnearneigh(coord_mat, 0, 18000)
listw_dist18 <- nb2listw(nb_dist18, style = "W", zero.policy = TRUE)
cat("距离阈值18km：邻居关系总数 =", sum(card(nb_dist18)), "\n")
cat("距离阈值18km：平均邻居数 =", mean(card(nb_dist18)), "\n")
# ---------- 2.3 K近邻矩阵（K=5） ----------
nb_knn5 <- knearneigh(coord_mat, k = 5)
nb_knn5 <- knn2nb(nb_knn5)
listw_knn5 <- nb2listw(nb_knn5, style = "W", zero.policy = TRUE)
cat("K近邻K=5：邻居关系总数 =", sum(card(nb_knn5)), "\n")
cat("K近邻K=5：平均邻居数 =", mean(card(nb_knn5)), "\n")
#三、全局空间自相关检验（三种矩阵对比）
moran_queen <- moran.test(town_merge$综合得分, listw_queen, zero.policy = TRUE)
moran_dist18 <- moran.test(town_merge$综合得分, listw_dist18, zero.policy = TRUE)
moran_knn5  <- moran.test(town_merge$综合得分, listw_knn5, zero.policy = TRUE)
moran_table <- data.frame(
  矩阵类型 = c("Queen一阶邻接", "距离阈值18km", "K近邻K=5"),
  Moran_I = c(moran_queen$estimate[1], moran_dist18$estimate[1], moran_knn5$estimate[1]),
  期望值 = c(moran_queen$estimate[2], moran_dist18$estimate[2], moran_knn5$estimate[2]),
  Z得分 = c(moran_queen$statistic, moran_dist18$statistic, moran_knn5$statistic),
  P值 = c(moran_queen$p.value, moran_dist18$p.value, moran_knn5$p.value)
)
print(moran_table)
write.csv(moran_table, "全局Moran_I稳健性对比.csv", row.names = FALSE, fileEncoding = "UTF-8")
# 四、局部空间关联分析（LISA）
# 用中心化值判断 HH / LL / HL / LH
get_lisa <- function(listw, scores, alpha = 0.05) {
  lisa <- localmoran(scores, listw, zero.policy = TRUE)
  p <- lisa[, "Pr(z != E(Ii))"]
  z <- lisa[, "Z.Ii"]
  scores_c <- scores - mean(scores)
  lag_scores <- lag.listw(listw, scores, zero.policy = TRUE)
  lag_c <- lag_scores - mean(scores)
  type <- rep("不显著", length(scores))
  type[p < alpha & scores_c > 0 & lag_c > 0] <- "HH"
  type[p < alpha & scores_c < 0 & lag_c < 0] <- "LL"
  type[p < alpha & scores_c > 0 & lag_c < 0] <- "HL"
  type[p < alpha & scores_c < 0 & lag_c > 0] <- "LH"
  return(data.frame(LISA_type = type, LISA_Z = z, LISA_P = p))
}
lisa_queen <- get_lisa(listw_queen, town_merge$综合得分)
lisa_dist18 <- get_lisa(listw_dist18, town_merge$综合得分)
lisa_knn5 <- get_lisa(listw_knn5, town_merge$综合得分)
town_merge$LISA_queen <- lisa_queen$LISA_type
town_merge$LISA_dist18 <- lisa_dist18$LISA_type
town_merge$LISA_knn5 <- lisa_knn5$LISA_type
cat("Queen矩阵 LISA 分类：\n"); print(table(town_merge$LISA_queen))
cat("距离阈值18km LISA 分类：\n"); print(table(town_merge$LISA_dist18))
cat("K近邻K=5 LISA 分类：\n"); print(table(town_merge$LISA_knn5))
lisa_compare <- town_merge[, c("县", "乡", "综合得分",
                               "LISA_queen", "LISA_dist18", "LISA_knn5")]
write.csv(lisa_compare, "LISA三种矩阵对比.csv", row.names = FALSE, fileEncoding = "UTF-8")
# 五、Getis-Ord 冷热点分析——三种矩阵对比
get_gi <- function(listw, scores) {
  gi <- localG(scores, listw)
  gi_z <- as.numeric(gi)
  type <- rep("不显著", length(gi_z))
  type[gi_z > 1.96] <- "热点"
  type[gi_z < -1.96] <- "冷点"
  return(data.frame(Gi_Z = gi_z, Gi_type = type))
}
gi_queen <- get_gi(listw_queen, town_merge$综合得分)
gi_dist18 <- get_gi(listw_dist18, town_merge$综合得分)
gi_knn5 <- get_gi(listw_knn5, town_merge$综合得分)
town_merge$Gi_queen <- gi_queen$Gi_type
town_merge$Gi_dist18 <- gi_dist18$Gi_type
town_merge$Gi_knn5 <- gi_knn5$Gi_type
cat("Queen矩阵 冷热点：\n"); print(table(town_merge$Gi_queen))
cat("距离阈值18km 冷热点：\n"); print(table(town_merge$Gi_dist18))
cat("K近邻K=5 冷热点：\n"); print(table(town_merge$Gi_knn5))
gi_compare <- town_merge[, c("县", "乡", "综合得分",
                             "Gi_queen", "Gi_dist18", "Gi_knn5")]
write.csv(gi_compare, "GetisOrd三种矩阵对比.csv", row.names = FALSE, fileEncoding = "UTF-8")
#六、导出结果
#===== 导出三种矩阵 LISA 图 =====
plot(town_merge["LISA_queen"], main = "LISA集聚图（Queen一阶邻接）")
plot(town_merge["LISA_dist18"], main = "LISA集聚图（距离阈值18km）")
plot(town_merge["LISA_knn5"], main = "LISA集聚图（K近邻K=5）")
# 导出LISA分类结果
lisa_export <- town_merge[, c("县","乡","综合得分","LISA_queen","LISA_dist18","LISA_knn5")]
write.csv(lisa_export, "LISA分类结果.csv", row.names = FALSE, fileEncoding = "UTF-8")
# 导出Getis-Ord冷热点结果
gi_export <- town_merge[, c("县","乡","综合得分","Gi_queen","Gi_dist18","Gi_knn5")]
write.csv(gi_export, "GetisOrd冷热点结果.csv", row.names = FALSE, fileEncoding = "UTF-8")
print("ESDA分析完成，结果已导出")