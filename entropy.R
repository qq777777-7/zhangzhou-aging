#附录D 熵值法测算代码（R语言）
#注意：因原始数据涉及保密要求，本代码不包含数据文件。
#运行前请将下方 file_path 修改为您本地的数据文件路径。
#数据格式需包含：区县、乡镇街道名称、老年人口比重、老年抚养比、少儿抚养比、少年儿童比重、劳动年龄人口占比。

#漳州市乡镇老龄化综合得分测算
library(readxl)
library(writexl)
#设置数据文件路径
file_path <- "C:/Users/12568/Documents/漳州乡镇老龄化数据模板.xlsx"
df <- read_excel(file_path)
#指定五项评价指标
indicator_cols <- c("老年人口比重", "老年抚养比", "少儿抚养比", "少年儿童比重", "劳动年龄人口占比")
#确保指标数据为数值型
for (col in indicator_cols) {
  df[[col]] <- as.numeric(df[[col]])
}
#区分正向指标和负向指标
positive_cols <- c("老年人口比重", "老年抚养比") #正向指标：数值越大老龄化越严重
negative_cols <- c("少儿抚养比", "少年儿童比重", "劳动年龄人口占比") #负向指标：数值越大老龄化越轻
all_indicator_cols <- c(positive_cols, negative_cols)
#步骤1：极差法标准化处理
df_norm <- df
#正向指标标准化：X' = (X - min) / (max - min)
for (col in positive_cols) {
  min_val <- min(df[[col]], na.rm = TRUE)
  max_val <- max(df[[col]], na.rm = TRUE)
  df_norm[[paste0(col, "_norm")]] <- (df[[col]] - min_val) / (max_val - min_val)
}
#负向指标标准化：X' = (max - X) / (max - min)
for (col in negative_cols) {
  min_val <- min(df[[col]], na.rm = TRUE)
  max_val <- max(df[[col]], na.rm = TRUE)
  df_norm[[paste0(col, "_norm")]] <- (max_val - df[[col]]) / (max_val - min_val)
}
#步骤2：计算指标比重
norm_cols <- paste0(all_indicator_cols, "_norm")
X <- as.matrix(df_norm[, norm_cols])
n <- nrow(X) #乡镇数量
m <- ncol(X) #指标数量
#计算第j项指标下第i个乡镇所占比重p_ij
p <- X / matrix(colSums(X), nrow = n, ncol = m, byrow = TRUE)
#步骤3：计算信息熵
k <- 1 / log(n)
e <- -k * colSums(p * log(p + 1e-12)) #加微小项避免对数无意义
#步骤4：计算差异系数
d <- 1 - e
#步骤5：计算权重
w <- d / sum(d)
#步骤6：计算综合得分
df_norm$综合得分 <- as.vector(X %*% w)
#导出综合得分排名
df_sorted <- df_norm[order(-df_norm$综合得分), ]
df_sorted$全市排名 <- 1:nrow(df_sorted)
result <- df_sorted[, c("区县", "乡镇街道名称", "综合得分", "全市排名")]
result$综合得分 <- round(result$综合得分, 6)
write_xlsx(result, "漳州市乡镇老龄化综合得分排名.xlsx")
#描述统计
min_val <- min(df_norm$综合得分, na.rm = TRUE)
max_val <- max(df_norm$综合得分, na.rm = TRUE)
mean_val <- mean(df_norm$综合得分, na.rm = TRUE)
sd_val <- sd(df_norm$综合得分, na.rm = TRUE)
cv_val <- sd_val / mean_val
n_val <- nrow(df_norm)
desc_stats <- data.frame(
  指标 = "综合得分",
  样本量 = n_val,
  最小值 = round(min_val, 4),
  最大值 = round(max_val, 4),
  均值 = round(mean_val, 4),
  标准差 = round(sd_val, 4),
  变异系数 = round(cv_val, 4)
)
print("表2 漳州市乡镇老龄化综合得分描述统计")
print(desc_stats)
write_xlsx(desc_stats, "漳州市乡镇老龄化综合得分描述统计.xlsx")
print("全部计算完成，结果已导出")