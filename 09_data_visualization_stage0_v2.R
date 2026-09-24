library(tidyverse)
library(scales)
library(ggplot2)
library(lubridate)
library(arrow)

# 数据源（合并并带聚类标签）
data_path <- "data/processed-v2/04-combined_order_product_customer_with_clusters.parquet"
if (!file.exists(data_path)) stop("找不到文件：", data_path)
df <- arrow::read_parquet(data_path)

# 输出目录
out_dir <- "/output-final-data-visual/eda_stage0"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 统一列类型
df <- df %>%
  mutate(
    customer_id = as.character(customer_id),
    order_id    = as.character(order_id),
    order_date  = lubridate::as_date(order_date),
    line_total  = as.numeric(line_total),
    final_price = as.numeric(final_price),
    initial_price = as.numeric(initial_price),
    discount_rate = coalesce(as.numeric(discount_rate), as.numeric(discount_rate_adjusted)),
    root_category = as.character(root_category)
  )

# 假设 df 已存在，out_dir 已定义
metrics_out_csv <- file.path(out_dir, "01_summary_metrics.csv")

# 过滤有效行（根据字段说明）
df_clean <- df %>%
  mutate(
    final_price = as.numeric(final_price),
    initial_price = as.numeric(initial_price),
    discount_rate = coalesce(as.numeric(discount_rate), as.numeric(discount_rate_adjusted)),
    brand = coalesce(as.character(brand), "UNKNOWN"),
    root_category = coalesce(as.character(root_category), "UNKNOWN"),
    color = coalesce(as.character(color), "UNKNOWN"),
    size = coalesce(as.character(size), "UNKNOWN"),
    order_date = lubridate::as_date(order_date)
  )

# 基础统计指标
price_stats <- df_clean %>%
  filter(!is.na(final_price) & final_price >= 0) %>%
  summarise(
    avg_price = mean(final_price, na.rm = TRUE),
    median_price = median(final_price, na.rm = TRUE)
  )

discount_stats <- df_clean %>%
mutate(
  has_discount = !is.na(discount_rate) & discount_rate > 0,
  product_id = as.character(product_id)
  ) %>%
  summarise(
    discounted_product_count = n_distinct(product_id[has_discount]),
    total_product_count = n_distinct(product_id),
    discounted_ratio = ifelse(total_product_count > 0,
    discounted_product_count / total_product_count, NA_real_),
    avg_discount_rate = mean(discount_rate[has_discount], na.rm = TRUE),
    max_discount_rate = max(discount_rate, na.rm = TRUE)
)


category_brand_counts <- df_clean %>%
  summarise(
    root_category_count = n_distinct(root_category),
    brand_count = n_distinct(brand)
  )

# 价格区间分布（0-10,10-20,...以100为止，100+）
price_bins <- c(seq(0,100,by=10), Inf)
price_labels <- c(paste0(seq(0,90,by=10), "-", seq(10,100,by=10)), "100+")
price_dist <- df_clean %>%
  filter(!is.na(final_price) & final_price >= 0) %>%
  mutate(price_bin = cut(final_price, breaks = price_bins, labels = price_labels, right = FALSE)) %>%
  count(price_bin) %>%
  mutate(pct = n / sum(n))

# Top5 按销量（quantity 总和或行数） 的根分类表现
top5_category_sales <- df_clean %>%
  mutate(qty = coalesce(as.numeric(quantity), 0)) %>%
  group_by(root_category) %>%
  summarise(
    total_qty = sum(qty, na.rm = TRUE),
    total_sales = sum(coalesce(line_total, 0), na.rm = TRUE),
    avg_price = mean(final_price, na.rm = TRUE),
    avg_discount = mean(discount_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_qty)) %>%
  slice_head(n = 5)

# 颜色偏好与尺码分布
color_pref <- df_clean %>%
  group_by(color) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

size_dist <- df_clean %>%
  group_by(size) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# 品牌 TOP10（按商品数量或销量）
brand_top10 <- df_clean %>%
  mutate(qty = coalesce(as.numeric(quantity), 0)) %>%
  group_by(brand) %>%
  summarise(
    items = n(),
    total_qty = sum(qty, na.rm = TRUE),
    avg_price = mean(final_price, na.rm = TRUE),
    avg_discount = mean(discount_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(items)) %>%
  slice_head(n = 10)

# 汇总 metrics 写入 CSV
summary_metrics <- tibble(
  metric = c("商品平均价格", "商品中位数价格",
             "有折扣商品数量", "折扣商品占比", "平均折扣率", "最大折扣率",
             "根分类数量", "品牌数量"),
  value = c(
    round(price_stats$avg_price, 4),
    round(price_stats$median_price, 4),
    # as.integer(discount_stats$discounted_count),
    as.integer(discount_stats$discounted_product_count),
    round(discount_stats$discounted_ratio, 4),
    round(discount_stats$avg_discount_rate, 4),
    round(discount_stats$max_discount_rate, 4),
    as.integer(category_brand_counts$root_category_count),
    as.integer(category_brand_counts$brand_count)
  )
)

write_csv(summary_metrics, metrics_out_csv)

# 保存各分表 CSV
write_csv(price_dist, file.path(out_dir, "02_price_distribution.csv"))
write_csv(top5_category_sales, file.path(out_dir, "03_top5_category_sales.csv"))
write_csv(color_pref, file.path(out_dir, "04_color_preference.csv"))
write_csv(size_dist, file.path(out_dir, "05_size_distribution.csv"))
write_csv(brand_top10, file.path(out_dir, "06_brand_top10.csv"))

# 绘图：价格区间分布条形图
p_price_dist <- price_dist %>%
  ggplot(aes(x = factor(price_bin, levels = price_labels), y = n)) +
  geom_col(fill = "#2E86AB") +
  labs(title = "价格区间分布", x = "价格区间（美元）", y = "数量") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "02_price_distribution_bar.png"), p_price_dist, width = 8, height = 4, dpi = 300)

# 绘图：Top5 分类销量（条形图）
p_top5_cat <- top5_category_sales %>%
  mutate(root_category = fct_reorder(root_category, total_qty)) %>%
  ggplot(aes(x = root_category, y = total_qty)) +
  geom_col(fill = "#6A994E") +
  coord_flip() +
  labs(title = "Top5 根分类按销量（件数）", x = "", y = "销量（件）") +
  theme_minimal()

ggsave(file.path(out_dir, "03_top5_category_sales.png"), p_top5_cat, width = 8, height = 5, dpi = 300)

# 绘图：颜色偏好（Top20）
p_color <- color_pref %>% slice_head(n = 20) %>%
  mutate(color = fct_reorder(color, count)) %>%
  ggplot(aes(x = color, y = count)) +
  geom_col(fill = "#A23B72") +
  coord_flip() +
  labs(title = "颜色偏好（Top20）", x = "", y = "数量") +
  theme_minimal()

ggsave(file.path(out_dir, "04_color_preference_top20.png"), p_color, width = 8, height = 6, dpi = 300)

# 绘图：尺码分布（Top20）
p_size <- size_dist %>% slice_head(n = 20) %>%
  mutate(size = fct_reorder(size, count)) %>%
  ggplot(aes(x = size, y = count)) +
  geom_col(fill = "#FF006E") +
  coord_flip() +
  labs(title = "尺码分布（Top20）", x = "", y = "数量") +
  theme_minimal()

ggsave(file.path(out_dir, "05_size_distribution_top20.png"), p_size, width = 8, height = 6, dpi = 300)

# 绘图：品牌 TOP10（按 items）
p_brand <- brand_top10 %>%
  mutate(brand = fct_reorder(brand, items)) %>%
  ggplot(aes(x = brand, y = items)) +
  geom_col(fill = "#4C78A8") +
  coord_flip() +
  labs(title = "品牌 Top10（按商品数）", x = "", y = "商品数量") +
  theme_minimal()

ggsave(file.path(out_dir, "06_brand_top10.png"), p_brand, width = 8, height = 6, dpi = 300)

# 控制台打印提示
cat("已生成并保存：", metrics_out_csv, "\n")
cat("其他 CSV/PNG 文件保存至：", out_dir, "\n")