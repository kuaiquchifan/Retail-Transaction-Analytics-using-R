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

# 检查数据加载是否正确
cat("数据维度:", dim(shein), "\n")
cat("列数:", ncol(shein), "\n")

# 数据清洗
shein_clean <- shein %>%
  # 1. 计算折扣率
  mutate(
    discount_rate = round(
      (initial_price - final_price) / initial_price * 100,
      2
    ),
    discount_rate = ifelse(discount_rate < 0, 0, discount_rate)
  ) %>%
  # 2. 移除无用列
  select(
    -category_url, -url, -category_tree, -image_urls,
    -offers, -related_products, -top_reviews,
    -other_attributes, -all_available_sizes,
    -main_image
  )

# 保存清洗后的数据
dir.create("./data/processed", showWarnings = FALSE, recursive = TRUE)

# 保存为 RDS 格式（推荐用于 R 分析）
saveRDS(shein_clean, "./data/processed/shein_clean.rds")

# 保存为 XLSX 格式
if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}
write.xlsx(shein_clean, "./data/processed/shein_clean.xlsx")

# 同时保存 CSV 格式
write_csv(
  shein_clean,
  "./data/processed/shein_clean.csv",
  na = "",
  quote = "all"
)

# 验证保存的数据
shein_verify <- readRDS("./data/processed/shein_clean.rds")
shein_csv_verify <- read_csv(
  "./data/processed/shein_clean.csv",
  show_col_types = FALSE
)

cat("\n✓ 数据清洗完成！\n")
cat("原始数据行数:", nrow(shein), "\n")
cat("清洗后数据行数:", nrow(shein_clean), "\n")
cat("清洗后数据列数:", ncol(shein_clean), "\n")
cat("\n验证 RDS 文件:", nrow(shein_verify), "行,", ncol(shein_verify), "列\n")
cat("验证 CSV 文件:", nrow(shein_csv_verify), "行,", ncol(shein_csv_verify), "列\n")
cat("\n保存位置:\n")
cat("  - RDS:  ./data/processed/shein_clean.rds\n")
cat("  - XLSX: ./data/processed/shein_clean.xlsx\n")
cat("  - CSV:  ./data/processed/shein_clean.csv\n")