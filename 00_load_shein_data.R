library(tidyverse)

# 加载数据
shein <- read_csv(
  "./data/raw/shein.csv",
  locale = locale(encoding = "UTF-8"),
  na = c("", "NA", "null"),
  trim_ws = TRUE,
  quote = "\"",
  show_col_types = FALSE
)

# 数据探索
cat(sprintf("数据维度: %d行, %d 列\n", nrow(shein), ncol(shein)))
cat("前 6 行预览:\n")
print(head(shein, 6))
cat("\n数据结构 (str):\n")
cat(paste(capture.output(str(shein)), collapse = "\n"), "\n")
cat("\n变量摘要 (summary):\n")
print(summary(shein))  # 查看行列数
cat("\n列名:\n")
print(colnames(shein))  # 查看所有列名