library(tidyverse)
if (!requireNamespace("arrow", quietly = TRUE)) {
  install.packages("arrow", repos = "https://cloud.r-project.org")
}
library(arrow)

if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra", repos = "https://cloud.r-project.org")
}
library(factoextra)
library(ggplot2)
library(cluster)

set.seed(42)

# -----------------------------
# 1) 读取合并后的结果
# -----------------------------
project_root <- getwd()
input_dir <- file.path(project_root, "data", "processed-v2")

merged_path <- file.path(input_dir, "04-combined_order_product_customer.parquet")
if (!file.exists(merged_path)) {
  stop("找不到合并后的 parquet: ", merged_path)
}

merged_df <- arrow::read_parquet(merged_path)

# -----------------------------
# 2) 检查字段
# -----------------------------
required_cols <- c(
  "customer_id", "order_id", "order_date", "total_amount",
  "shipping_fee", "quantity", "unit_price", "line_total",
  "discount_rate_adjusted", "discount_rate", "free_shipping",
  "product_id", "region", "gender", "age", "channel_source",
  "customer_type", "customer_hierachy", "preferred_category",
  "category"
)

missing_cols <- setdiff(required_cols, names(merged_df))
if (length(missing_cols) > 0) {
  stop("缺少字段：", paste(missing_cols, collapse = ", "))
}

# -----------------------------
# 3) 订单级特征：按订单去重后计算消费金额
# -----------------------------
order_level_customer <- merged_df %>%
  mutate(
    order_date = as.Date(order_date),
    total_amount = as.numeric(total_amount),
    shipping_fee = as.numeric(shipping_fee),
    quantity = as.numeric(quantity),
    unit_price = as.numeric(unit_price),
    line_total = as.numeric(line_total),
    discount_rate_adjusted = as.numeric(discount_rate_adjusted),
    free_shipping = as.logical(free_shipping),
    age = as.numeric(age)
  ) %>%
  distinct(customer_id, order_id, order_date, total_amount) %>%
  group_by(customer_id) %>%
  summarise(
    total_spend = sum(total_amount, na.rm = TRUE),
    avg_order_value = mean(total_amount, na.rm = TRUE),
    order_count = n_distinct(order_id),
    first_order_date = min(order_date, na.rm = TRUE),
    last_order_date = max(order_date, na.rm = TRUE),
    recency_days = as.numeric(as.Date("2026-08-29") - last_order_date),
    .groups = "drop"
  )

# -----------------------------
# 4) 订单项级特征：从订单明细粒度计算行为特征
# -----------------------------
item_level_customer <- merged_df %>%
  mutate(
    order_date = as.Date(order_date),
    total_amount = as.numeric(total_amount),
    shipping_fee = as.numeric(shipping_fee),
    quantity = as.numeric(quantity),
    unit_price = as.numeric(unit_price),
    line_total = as.numeric(line_total),
    discount_rate_adjusted = as.numeric(discount_rate_adjusted),
    free_shipping = as.logical(free_shipping),
    category = as.character(category)
  ) %>%
  group_by(customer_id) %>%
  summarise(
    total_quantity = sum(quantity, na.rm = TRUE),
    avg_quantity_per_order = mean(quantity, na.rm = TRUE),
    total_line_value = sum(line_total, na.rm = TRUE),
    avg_discount_rate = mean(discount_rate_adjusted, na.rm = TRUE),
    avg_unit_price = mean(unit_price, na.rm = TRUE),
    unique_product_count = n_distinct(product_id),
    category_diversity = n_distinct(category, na.rm = TRUE),
    discounted_item_ratio = mean(discount_rate_adjusted > 0, na.rm = TRUE),
    avg_line_total = mean(line_total, na.rm = TRUE),
    frequency = n_distinct(order_date),
    .groups = "drop"
  )

# -----------------------------
# 5) 客户画像字段
# -----------------------------
customer_profile <- merged_df %>%
  group_by(customer_id) %>%
  summarise(
    gender = first(na.omit(gender)),
    age = first(na.omit(age)),
    region = first(na.omit(region)),
    channel_source = first(na.omit(channel_source)),
    customer_type = first(na.omit(customer_type)),
    customer_hierachy = first(na.omit(customer_hierachy)),
    preferred_category = first(na.omit(preferred_category)),
    .groups = "drop"
  )

customer_features <- order_level_customer %>%
  left_join(item_level_customer, by = "customer_id") %>%
  left_join(customer_profile, by = "customer_id")

# -----------------------------
# 6) 选择用于聚类的数值特征
# -----------------------------
cluster_features <- c(
  "total_spend",
  "avg_order_value",
  "order_count",
  "recency_days",
  "frequency",
  "avg_discount_rate",
  "avg_unit_price",
  "unique_product_count",
  "category_diversity",
  "discounted_item_ratio"
)

customer_features <- customer_features %>%
  mutate(across(all_of(cluster_features), ~ replace_na(.x, 0)))

# -----------------------------
# 6.5) 检查并处理聚类特征
# -----------------------------
X <- customer_features %>%
  select(all_of(cluster_features)) %>%
  as.data.frame()

# 1. 再保险：把剩余 NA 填 0，Inf/-Inf 也处理掉
X <- X %>%
  mutate(across(everything(), ~ {
    x <- as.numeric(.x)
    x[!is.finite(x)] <- 0   # 同时处理 NA、NaN、Inf、-Inf
    x
  }))

# 2. 检查方差为 0 的列（导致 scale 产生 NaN）
zero_var_cols <- names(X)[sapply(X, function(col) {
  s <- sd(col, na.rm = TRUE)
  is.na(s) || s == 0
})]

if (length(zero_var_cols) > 0) {
  cat("发现方差为 0 的特征（将从聚类中移除）：\n")
  print(zero_var_cols)
  
  # 打印这些列的取值，方便排查
  for (col in zero_var_cols) {
    cat("\n列", col, "的唯一值：\n")
    print(table(X[[col]], useNA = "ifany"))
  }
  
  # 从聚类特征中移除这些列
  X <- X %>% select(-all_of(zero_var_cols))
  cluster_features <- setdiff(cluster_features, zero_var_cols)
}

if (ncol(X) < 2) {
  stop("有效聚类特征少于 2 个，无法进行 k-means")
}

# 3. 标准化
X_scaled <- scale(X)

# 4. 最终检查
if (any(!is.finite(X_scaled))) {
  bad_cols <- colnames(X_scaled)[apply(X_scaled, 2, function(x) any(!is.finite(x)))]
  stop("标准化后仍有非有限值，问题列：", paste(bad_cols, collapse = ", "))
}

cat("最终用于聚类的特征：\n")
print(colnames(X_scaled))
cat("样本量：", nrow(X_scaled), "\n")

# -----------------------------
# 7) K-means 聚类
# -----------------------------
k <- 3
set.seed(42)

km_model <- kmeans(X_scaled, centers = k, nstart = 25)
customer_features$cluster <- km_model$cluster

# -----------------------------
# 8) WSS：肘部法则
# -----------------------------
wss_plot <- fviz_nbclust(
  X_scaled,
  kmeans,
  method = "wss",
  k.max = 10
)

# print(wss_plot)

# -----------------------------
# 9) Silhouette：轮廓系数
# -----------------------------
sil_plot <- fviz_nbclust(
  X_scaled,
  kmeans,
  method = "silhouette",
  k.max = 10
)


sil_values <- silhouette(customer_features$cluster, dist(X_scaled))
sil_summary <- summary(sil_values)
# print(summary(sil_values))

# -----------------------------
# 10) PCA 可视化
# -----------------------------
pca_df <- as.data.frame(prcomp(X_scaled, center = FALSE, scale. = FALSE)$x[, 1:2])
pca_df$customer_id <- customer_features$customer_id
pca_df$cluster <- as.factor(customer_features$cluster)

pca_plot <- ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = customer_id), check_overlap = TRUE, size = 2, vjust = -0.6) +
  theme_minimal() +
  labs(title = "Customer Clusters by PCA", color = "Cluster")


# -----------------------------
# 11) 聚类结果汇总
# -----------------------------
cluster_summary <- customer_features %>%
  group_by(cluster) %>%
  summarise(
    customer_count = n(),
    avg_total_spend = mean(total_spend, na.rm = TRUE),
    avg_order_value = mean(avg_order_value, na.rm = TRUE),
    avg_order_count = mean(order_count, na.rm = TRUE),
    avg_recency_days = mean(recency_days, na.rm = TRUE),
    avg_discount_rate = mean(avg_discount_rate, na.rm = TRUE),
    avg_category_diversity = mean(category_diversity, na.rm = TRUE),
    avg_discounted_item_ratio = mean(discounted_item_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 12) 输出结果
# -----------------------------
output_dir <- file.path(project_root, "data", "processed-v2")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# 让 K 放在文件名最后
# output_suffix <- sprintf("-k%s", k)
output_suffix <- sprintf("-k=%s", k)

ggsave(
  filename = file.path(output_dir, paste0("05-wss_elbow_plot", output_suffix, ".png")),
  plot = wss_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("05-silhouette_k_selection_plot", output_suffix, ".png")),
  plot = sil_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# 保存 PCA 可视化图
ggsave(
  filename = file.path(output_dir, paste0("05-customer_cluster_pca", output_suffix, ".png")),
  plot = pca_plot,
  width = 10,
  height = 7,
  dpi = 300
)

customer_feature_path <- file.path(output_dir, paste0("05-customer_features", output_suffix, ".parquet"))
cluster_path <- file.path(output_dir, paste0("05-customer_cluster_result", output_suffix, ".parquet"))
cluster_csv_path <- file.path(output_dir, paste0("05-customer_cluster_result", output_suffix, ".csv"))
# 保存摘要到文本
capture.output(
  sil_summary,
  file = file.path(output_dir, paste0("05-silhouette_summary", output_suffix, ".txt"))
)


write_parquet(customer_features, sink = customer_feature_path)
write_parquet(cluster_summary, sink = cluster_path)
write.csv(cluster_summary, file = cluster_csv_path, row.names = FALSE)
# 可选：保存每个样本的 silhouette 明细到 CSV
sil_detail <- as.data.frame(unclass(sil_values))
write.csv(
  sil_detail,
  file = file.path(output_dir, paste0("05-silhouette_detail", output_suffix, ".csv")),
  row.names = FALSE
)
cat("客户特征表已保存：", customer_feature_path, "\n")
cat("聚类结果已保存：", cluster_path, "\n")
cat("聚类结果 CSV 已保存：", cluster_csv_path, "\n")

cat("\n客户特征表样例：\n")
print(head(customer_features))

cat("\n聚类汇总：\n")
print(cluster_summary)

cat("\n聚类中心：\n")
print(km_model$centers)

# 可选：看肘部法则图 / silhouette 图
# print(wss_plot)
# print(sil_plot)