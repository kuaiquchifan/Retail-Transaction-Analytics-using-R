library(tidyverse)
if (!requireNamespace("arrow", quietly = TRUE)) {
  install.packages("arrow", repos = "https://cloud.r-project.org")
}
library(arrow)

set.seed(2024)

# 输出目录
project_root <- getwd()
output_dir <- file.path(project_root, "data", "processed-v2")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# 1) 读取真实数据来源
# -----------------------------
customer_data_path <- file.path("data", "processed-v2", "02-customer_simulation_20000.parquet")
product_data_path <- file.path("data", "processed-v2", "01-shein_clean.parquet")

if (!file.exists(customer_data_path)) {
  stop("找不到客户数据文件: ", customer_data_path)
}
if (!file.exists(product_data_path)) {
  stop("找不到商品数据文件: ", product_data_path)
}

customer_data <- arrow::read_parquet(customer_data_path)
product_data <- arrow::read_parquet(product_data_path)

customer_ids <- customer_data$customer_id
if (length(customer_ids) == 0) {
  stop("客户数据为空，无法生成订单")
}

# 安全版价格处理函数
safe_discount_rate <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x <- pmin(pmax(x, 0), 0.9)
  x
}

safe_unit_price <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  pmax(x, 0.01)
}

# 真实商品信息：从商品表中抽取 product_id, unit_price, discount_rate
product_lookup <- product_data %>%
  transmute(
    product_id = as.integer(product_id),
    unit_price = safe_unit_price(final_price),
    discount_rate = safe_discount_rate(discount_rate),
    free_shipping = as.logical(free_shipping)
  ) %>%
  filter(!is.na(product_id), product_id > 0) %>%
  distinct(product_id, .keep_all = TRUE)

if (nrow(product_lookup) == 0) {
  stop("商品数据中没有可用的 product_id / unit_price")
}

# -----------------------------
# 2) 参数
# -----------------------------
n_orders <- 160000
n_order_items <- 240000

# -----------------------------
# 3) 生成订单表
# -----------------------------
order_status_levels <- c("Completed", "Pending", "Shipped", "Cancelled", "Refunded")
order_status_probs <- c(0.62, 0.15, 0.15, 0.05, 0.03)

order_dates <- seq.Date(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")

orders <- tibble(
  order_id = 1:n_orders,
  customer_id = sample(customer_ids, size = n_orders, replace = TRUE),
  order_date = sample(order_dates, size = n_orders, replace = TRUE),
  order_status = sample(
    order_status_levels,
    size = n_orders,
    replace = TRUE,
    prob = order_status_probs
  ),
  shipping_fee = round(runif(n_orders, min = 0, max = 25), 2),
  total_amount = NA_real_
)

# -----------------------------
# 4) 生成订单明细表
# -----------------------------
base_counts <- rep(1L, n_orders)
remaining_items <- n_order_items - n_orders

if (remaining_items > 0L) {
  extra_positions <- sample.int(n_orders, size = remaining_items, replace = TRUE)
  extra_counts <- table(factor(extra_positions, levels = 1:n_orders))
  base_counts <- base_counts + as.integer(extra_counts)
}

if (sum(base_counts) != n_order_items) {
  stop("订单明细总数不等于 n_order_items")
}

order_id_for_items <- rep(seq_len(n_orders), times = base_counts)

item_product_rows <- product_lookup[
  sample(seq_len(nrow(product_lookup)), size = n_order_items, replace = TRUE),
]

order_items <- tibble(
  order_item_id = 1:n_order_items,
  order_id = order_id_for_items,
  customer_id = NA_integer_,
  product_id = item_product_rows$product_id,
  quantity = sample(1:5, size = n_order_items, replace = TRUE),
  unit_price = item_product_rows$unit_price,
  discount_rate = item_product_rows$discount_rate,
  free_shipping = item_product_rows$free_shipping,
  line_total = NA_real_
)

order_customer_map <- orders %>%
  select(order_id, customer_id) %>%
  rename(customer_id_order = customer_id)

order_items <- order_items %>%
  left_join(order_customer_map, by = "order_id") %>%
  mutate(
    customer_id = as.integer(customer_id_order),
    line_total = round(
      pmax(quantity * unit_price * (1 - discount_rate), 0),
      2
    )
  ) %>%
  select(
    order_item_id,
    order_id,
    customer_id,
    product_id,
    quantity,
    unit_price,
    discount_rate,
    free_shipping,
    line_total
  )

# -----------------------------
# 5) 最终统一规范化：输出前处理 discount_rate
# -----------------------------
order_items <- order_items %>%
  mutate(
    discount_rate_adjusted = ifelse(
      discount_rate >= 0 & discount_rate <= 1,
      1 - discount_rate,
      discount_rate
    ),
    line_total = round(
      pmax(quantity * unit_price * (1 - discount_rate_adjusted), 0),
      2
    )
  ) %>%
  select(
    order_item_id,
    order_id,
    customer_id,
    product_id,
    quantity,
    unit_price,
    discount_rate,
    discount_rate_adjusted,
    free_shipping,
    line_total
  )

# 重新汇总订单总额
order_summary <- order_items %>%
  group_by(order_id) %>%
  summarise(
    item_total = sum(line_total),
    has_free_shipping = any(free_shipping),
    .groups = "drop"
  )

orders <- orders %>%
  left_join(order_summary, by = "order_id") %>%
  mutate(
    shipping_fee = ifelse(
      has_free_shipping,
      0,
      pmin(round(runif(n(), min = 1, max = 25), 2), item_total)
    ),
    total_amount = round(item_total + shipping_fee, 2)
  ) %>%
  select(order_id, customer_id, order_date, order_status, total_amount, shipping_fee)

# -----------------------------
# 6) 保存为 parquet
# -----------------------------
orders_output <- file.path(output_dir, "03-orders_simulation_160000.parquet")
items_output <- file.path(output_dir, "03-order_items_simulation_240000.parquet")

write_parquet(orders, sink = orders_output)
write_parquet(order_items, sink = items_output)

cat("已生成订单数据：", nrow(orders), "条\n")
cat("已生成订单明细数据：", nrow(order_items), "条\n")
cat("订单文件：", orders_output, "\n")
cat("明细文件：", items_output, "\n")

cat("\n订单样例：\n")
print(head(orders))

cat("\n订单明细样例：\n")
print(head(order_items))