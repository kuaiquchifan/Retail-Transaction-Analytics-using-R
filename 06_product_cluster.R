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
# 2) 检查字段（产品聚类需要的字段）
# -----------------------------
required_cols <- c(
  "product_id", "order_id", "customer_id", "order_date",
  "quantity", "unit_price", "line_total",
  "discount_rate_adjusted", "discount_rate",
  "category", "root_category", "brand",
  "initial_price", "final_price", "rating",
  "in_stock", "free_shipping", "color", "size"
)

missing_cols <- setdiff(required_cols, names(merged_df))
if (length(missing_cols) > 0) {
  warning("缺少部分可选字段：", paste(missing_cols, collapse = ", "),
          "。将尽量使用可用字段。")
}

# -----------------------------
# 3) 产品级销售 / 行为特征
# -----------------------------
product_sales <- merged_df %>%
  mutate(
    order_date = as.Date(order_date),
    quantity = as.numeric(quantity),
    unit_price = as.numeric(unit_price),
    line_total = as.numeric(line_total),
    discount_rate_adjusted = as.numeric(discount_rate_adjusted),
    initial_price = suppressWarnings(as.numeric(initial_price)),
    final_price = suppressWarnings(as.numeric(final_price)),
    rating = suppressWarnings(as.numeric(rating))
  ) %>%
  group_by(product_id) %>%
  summarise(
    # 销量 & 金额
    total_quantity = sum(quantity, na.rm = TRUE),
    total_revenue = sum(line_total, na.rm = TRUE),
    avg_line_total = mean(line_total, na.rm = TRUE),
    
    # 订单与客户覆盖
    order_count = n_distinct(order_id),
    customer_count = n_distinct(customer_id),
    
    # 价格与折扣
    avg_unit_price = mean(unit_price, na.rm = TRUE),
    avg_discount_rate = mean(discount_rate_adjusted, na.rm = TRUE),
    discounted_item_ratio = mean(discount_rate_adjusted > 0, na.rm = TRUE),
    
    # 时间
    first_order_date = min(order_date, na.rm = TRUE),
    last_order_date = max(order_date, na.rm = TRUE),
    recency_days = as.numeric(as.Date("2026-08-29") - last_order_date),
    active_days = as.numeric(last_order_date - first_order_date) + 1,
    
    # 频率相关
    frequency = n_distinct(order_date),
    
    .groups = "drop"
  )

# -----------------------------
# 4) 产品静态属性（取第一个非缺失值）
# -----------------------------
product_profile <- merged_df %>%
  group_by(product_id) %>%
  summarise(
    product_name = first(na.omit(product_name)),
    category = first(na.omit(category)),
    root_category = first(na.omit(root_category)),
    brand = first(na.omit(brand)),
    initial_price = first(na.omit(as.numeric(initial_price))),
    final_price = first(na.omit(as.numeric(final_price))),
    rating = first(na.omit(as.numeric(rating))),
    color = first(na.omit(color)),
    size = first(na.omit(size)),
    in_stock = first(na.omit(in_stock)),
    free_shipping = first(na.omit(free_shipping)),
    .groups = "drop"
  )

product_features <- product_sales %>%
  left_join(product_profile, by = "product_id")

# -----------------------------
# 5) 选择用于聚类的数值特征
# -----------------------------
cluster_features <- c(
  "total_quantity",
  "total_revenue",
  "avg_line_total",
  "order_count",
  "customer_count",
  "avg_unit_price",
  "avg_discount_rate",
  "discounted_item_ratio",
  "recency_days",
  "frequency",
  "rating"          # 如果 rating 全是 NA，后面会自动剔除
)

# 只保留实际存在的列
cluster_features <- intersect(cluster_features, names(product_features))

product_features <- product_features %>%
  mutate(across(all_of(cluster_features), ~ replace_na(as.numeric(.x), 0)))

# -----------------------------
# 6) 检查并处理聚类特征
# -----------------------------
X <- product_features %>%
  select(all_of(cluster_features)) %>%
  as.data.frame()

# 处理 NA / Inf
X <- X %>%
  mutate(across(everything(), ~ {
    x <- as.numeric(.x)
    x[!is.finite(x)] <- 0
    x
  }))

# 剔除方差为 0 的列
zero_var_cols <- names(X)[sapply(X, function(col) {
  s <- sd(col, na.rm = TRUE)
  is.na(s) || s == 0
})]

if (length(zero_var_cols) > 0) {
  cat("发现方差为 0 的特征（将从聚类中移除）：\n")
  print(zero_var_cols)
  
  for (col in zero_var_cols) {
    cat("\n列", col, "的唯一值：\n")
    print(table(X[[col]], useNA = "ifany"))
  }
  
  X <- X %>% select(-all_of(zero_var_cols))
  cluster_features <- setdiff(cluster_features, zero_var_cols)
}

if (ncol(X) < 2) {
  stop("有效聚类特征少于 2 个，无法进行 k-means")
}

# 标准化
X_scaled <- scale(X)

if (any(!is.finite(X_scaled))) {
  bad_cols <- colnames(X_scaled)[apply(X_scaled, 2, function(x) any(!is.finite(x)))]
  stop("标准化后仍有非有限值，问题列：", paste(bad_cols, collapse = ", "))
}

cat("最终用于聚类的特征：\n")
print(colnames(X_scaled))
cat("产品样本量：", nrow(X_scaled), "\n")

# -----------------------------
# 7) K-means 聚类
# -----------------------------
k <- 2
set.seed(42)

km_model <- kmeans(X_scaled, centers = k, nstart = 25)
product_features$cluster <- km_model$cluster

# -----------------------------
# 8) WSS：肘部法则
# -----------------------------
wss_plot <- fviz_nbclust(
  X_scaled,
  kmeans,
  method = "wss",
  k.max = 10
)

# -----------------------------
# 9) Silhouette：轮廓系数
# -----------------------------
sil_plot <- fviz_nbclust(
  X_scaled,
  kmeans,
  method = "silhouette",
  k.max = 10
)

sil_values <- silhouette(product_features$cluster, dist(X_scaled))
sil_summary <- summary(sil_values)

# -----------------------------
# 10) PCA 可视化
# -----------------------------
pca_df <- as.data.frame(prcomp(X_scaled, center = FALSE, scale. = FALSE)$x[, 1:2])
pca_df$product_id <- product_features$product_id
pca_df$cluster <- as.factor(product_features$cluster)

pca_plot <- ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = product_id), check_overlap = TRUE, size = 2, vjust = -0.6) +
  theme_minimal() +
  labs(title = "Product Clusters by PCA", color = "Cluster")

# -----------------------------
# 11) 聚类结果汇总
# -----------------------------
cluster_summary <- product_features %>%
  group_by(cluster) %>%
  summarise(
    product_count = n(),
    avg_total_quantity = mean(total_quantity, na.rm = TRUE),
    avg_total_revenue = mean(total_revenue, na.rm = TRUE),
    avg_order_count = mean(order_count, na.rm = TRUE),
    avg_customer_count = mean(customer_count, na.rm = TRUE),
    avg_unit_price = mean(avg_unit_price, na.rm = TRUE),
    avg_discount_rate = mean(avg_discount_rate, na.rm = TRUE),
    avg_recency_days = mean(recency_days, na.rm = TRUE),
    avg_discounted_item_ratio = mean(discounted_item_ratio, na.rm = TRUE),
    avg_rating = mean(rating, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 12) 输出结果
# -----------------------------
output_dir <- file.path(project_root, "data", "processed-v2")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

output_suffix <- sprintf("-k=%s", k)

ggsave(
  filename = file.path(output_dir, paste0("06-product_wss_elbow_plot", output_suffix, ".png")),
  plot = wss_plot,
  width = 8, height = 6, dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("06-product_silhouette_k_selection_plot", output_suffix, ".png")),
  plot = sil_plot,
  width = 8, height = 6, dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("06-product_cluster_pca", output_suffix, ".png")),
  plot = pca_plot,
  width = 10, height = 7, dpi = 300
)

product_feature_path <- file.path(output_dir, paste0("06-product_features", output_suffix, ".parquet"))
cluster_path <- file.path(output_dir, paste0("06-product_cluster_result", output_suffix, ".parquet"))
cluster_csv_path <- file.path(output_dir, paste0("06-product_cluster_result", output_suffix, ".csv"))

capture.output(
  sil_summary,
  file = file.path(output_dir, paste0("06-product_silhouette_summary", output_suffix, ".txt"))
)

write_parquet(product_features, sink = product_feature_path)
write_parquet(cluster_summary, sink = cluster_path)
write.csv(cluster_summary, file = cluster_csv_path, row.names = FALSE)

sil_detail <- as.data.frame(unclass(sil_values))
write.csv(
  sil_detail,
  file = file.path(output_dir, paste0("06-product_silhouette_detail", output_suffix, ".csv")),
  row.names = FALSE
)

cat("产品特征表已保存：", product_feature_path, "\n")
cat("聚类结果已保存：", cluster_path, "\n")
cat("聚类结果 CSV 已保存：", cluster_csv_path, "\n")

cat("\n产品特征表样例：\n")
print(head(product_features))

cat("\n聚类汇总：\n")
print(cluster_summary)

cat("\n聚类中心：\n")
print(km_model$centers)