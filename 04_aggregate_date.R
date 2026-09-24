library(tidyverse)
if (!requireNamespace("arrow", quietly = TRUE)) {
  install.packages("arrow", repos = "https://cloud.r-project.org")
}
library(arrow)

# -----------------------------
# 1) 读取数据
# -----------------------------
project_root <- getwd()
input_dir <- file.path(project_root, "data", "processed-v2")

product_path <- file.path(input_dir, "01-shein_clean.parquet")
customer_path <- file.path(input_dir, "02-customer_simulation_20000.parquet")
order_items_path <- file.path(input_dir, "03-order_items_simulation_240000.parquet")
orders_path <- file.path(input_dir, "03-orders_simulation_160000.parquet")

if (!file.exists(product_path)) stop("找不到商品表: ", product_path)
if (!file.exists(customer_path)) stop("找不到客户表: ", customer_path)
if (!file.exists(order_items_path)) stop("找不到订单明细表: ", order_items_path)
if (!file.exists(orders_path)) stop("找不到订单表: ", orders_path)

product_data <- arrow::read_parquet(product_path)
customer_data <- arrow::read_parquet(customer_path)
order_items <- arrow::read_parquet(order_items_path)
orders <- arrow::read_parquet(orders_path)

# -----------------------------
# 2) 先看字段名，确保主键正确
# -----------------------------
cat("product_data 列名：\n")
print(names(product_data))
cat("\ncustomer_data 列名：\n")
print(names(customer_data))
cat("\norder_items 列名：\n")
print(names(order_items))
cat("\norders 列名：\n")
print(names(orders))

# -----------------------------
# 3) 做合并
# -----------------------------
# 订单明细 -> 订单主表
if (!"customer_id" %in% names(order_items)) {
  stop("order_items 中没有 customer_id，先重跑 03_build_order_data.R，确保 customer_id 没被删掉")
}

final_df <- order_items %>%
  left_join(
    orders,
    by = c("order_id", "customer_id")
  ) %>%
  left_join(
    customer_data,
    by = "customer_id"
  ) %>%
  left_join(
    product_data,
    by = "product_id"
  ) %>%
  mutate(
    free_shipping = coalesce(free_shipping.x, free_shipping.y),
    discount_rate = coalesce(discount_rate.x, discount_rate.y)
  ) %>%
  select(
    -free_shipping.x,
    -free_shipping.y,
    -discount_rate.x,
    -discount_rate.y
  )

# -----------------------------
# 4) 输出目录
# -----------------------------
output_dir <- file.path(project_root, "data", "processed-v2")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

output_path <- file.path(output_dir, "04-combined_order_product_customer.parquet")

write_parquet(final_df, sink = output_path)

cat("\n最终合并表已保存：", output_path, "\n")
cat("行数：", nrow(final_df), "\n")
cat("列数：", ncol(final_df), "\n")
cat("最终列名：\n")
print(names(final_df))
print(head(final_df))