library(tidyverse)
library(arrow)
library(scales)
library(lubridate)
library(patchwork)

# 读取带聚类标签的合并表
data_path <- "data/processed-v2/04-combined_order_product_customer_with_clusters.parquet"
if (!file.exists(data_path)) stop("找不到文件：", data_path)
df <- arrow::read_parquet(data_path)

# 输出目录
out_dir <- "/output-final-data-visual/eda_stage2"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 统一类型与必要列
df <- df %>%
  mutate(
    customer_id = as.character(customer_id),
    order_id    = as.character(order_id),
    order_date  = as_datetime(order_date, tz = "UTC") %>% as_date(),
    cluster     = as.integer(cluster),
    customer_type = as.character(customer_type),
    customer_hierachy = as.character(customer_hierachy),
    line_total  = as.numeric(line_total),
    total_amount = as.numeric(total_amount),
    quantity    = as.numeric(quantity),
    initial_price = as.numeric(initial_price),
    final_price = as.numeric(final_price),
    discount_rate_adjusted = as.numeric(discount_rate_adjusted),
    rating = as.numeric(rating),
    image_count = as.numeric(image_count),
    in_stock = as.logical(in_stock)
  )

# 预计算：按 customer 聚合订单数、总消费、平均客单价、最近一次下单（Recency 计算基础）
cust_agg <- df %>%
  group_by(customer_id, customer_type, customer_hierachy) %>%
  summarise(
    order_count = n_distinct(order_id),
    monetary = sum(line_total, na.rm = TRUE),
    avg_order_value = if_else(order_count > 0, monetary / order_count, NA_real_),
    last_order_date = max(order_date, na.rm = TRUE),
    first_order_date = min(order_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    recency_days = as.integer(as_date(Sys.Date()) - as_date(last_order_date)),
    frequency = order_count,
    monetary = monetary
  )

# 1) 客户分群对比：总消费与订单数箱型图 + 分群客户数条形图
p1_monetary_box <- ggplot(cust_agg %>% filter(!is.na(customer_type)),
                          aes(x = customer_type, y = monetary, fill = customer_type)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_y_log10(labels = comma_format(accuracy = 1)) +
  labs(title = "按客户类型的总消费（箱型，Log10）", x = "customer_type", y = "总消费 (log10)") +
  theme_minimal() +
  theme(legend.position = "none")

p1_ordercount_box <- ggplot(cust_agg %>% filter(!is.na(customer_type)),
                            aes(x = customer_type, y = order_count, fill = customer_type)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(title = "按客户类型的订单数（箱型）", x = "customer_type", y = "订单数") +
  theme_minimal() +
  theme(legend.position = "none")

p1_count_bar <- ggplot(cust_agg %>% count(customer_type, name = "n"),
                       aes(x = reorder(customer_type, -n), y = n, fill = customer_type)) +
  geom_col() +
  labs(title = "各客户类型人数分布", x = "customer_type", y = "客户数") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "01_cluster_monetary_ordercount_box.png"),
       p1_monetary_box + p1_ordercount_box + plot_layout(ncol = 2),
       width = 14, height = 6, dpi = 300)
ggsave(file.path(out_dir, "02_cluster_count_bar.png"), p1_count_bar, width = 8, height = 5, dpi = 300)

# 2) RFM 分布图：recency/frequency/monetary 的箱型图（按 customer_type）
p_r <- ggplot(cust_agg %>% filter(!is.na(customer_type)),
              aes(x = customer_type, y = recency_days, fill = customer_type)) +
  geom_boxplot() +
  labs(title = "Recency（天）按客户类型", x = "customer_type", y = "Recency (days)") +
  theme_minimal() + theme(legend.position = "none")

p_f <- ggplot(cust_agg %>% filter(!is.na(customer_type)),
              aes(x = customer_type, y = frequency, fill = customer_type)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "Frequency（订单数）按客户类型", x = "customer_type", y = "订单数") +
  theme_minimal() + theme(legend.position = "none")

p_m <- ggplot(cust_agg %>% filter(!is.na(customer_type)),
              aes(x = customer_type, y = monetary, fill = customer_type)) +
  geom_boxplot() +
  scale_y_log10(labels = comma_format(accuracy = 1)) +
  labs(title = "Monetary（总消费，Log10）按客户类型", x = "customer_type", y = "总消费 (log10)") +
  theme_minimal() + theme(legend.position = "none")

ggsave(file.path(out_dir, "03_rfm_box_by_type.png"), p_r + p_f + p_m + plot_layout(ncol = 3), width = 18, height = 6, dpi = 300)

# 3) 客单价与订单数散点图（每个 customer_id）
p_scatter_aov <- ggplot(cust_agg %>% filter(!is.na(customer_type)),
                        aes(x = order_count, y = avg_order_value, color = customer_type)) +
  geom_point(alpha = 0.6) +
  scale_y_log10(labels = dollar_format(prefix = "")) +
  scale_x_continuous(labels = comma) +
  labs(title = "平均客单价 vs 订单数（按客户着色）", x = "订单数", y = "平均客单价（美元）") +
  theme_minimal()

ggsave(file.path(out_dir, "04_avg_order_value_vs_order_count.png"), p_scatter_aov, width = 10, height = 6, dpi = 300)

# 4) 时间序列 — 销售/订单趋势（按 customer_type）
ts_daily <- df %>%
  filter(!is.na(order_date)) %>%
  group_by(order_date, customer_type) %>%
  summarise(
    daily_sales = sum(line_total, na.rm = TRUE),
    daily_orders = n_distinct(order_id),
    .groups = "drop"
  )

p_ts_sales <- ggplot(ts_daily, aes(x = order_date, y = daily_sales, color = customer_type)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(title = "每日销售额（按客户类型）", x = "日期", y = "销售额（美元）") +
  theme_minimal()

p_ts_orders <- ggplot(ts_daily, aes(x = order_date, y = daily_orders, color = customer_type)) +
  geom_line(size = 0.7, alpha = 0.9) +
  labs(title = "每日订单数（按客户类型）", x = "日期", y = "订单数") +
  theme_minimal()

ggsave(file.path(out_dir, "05_time_series_sales_by_type.png"), p_ts_sales, width = 12, height = 5, dpi = 300)
ggsave(file.path(out_dir, "06_time_series_orders_by_type.png"), p_ts_orders, width = 12, height = 5, dpi = 300)

# 5) 复购率/留存：计算首次下单月内 N 天复购（示例：30/60/90 天留存率）
cust_first <- df %>%
  group_by(customer_id) %>%
  summarise(first_order = min(order_date, na.rm = TRUE), .groups = "drop")

purchases <- df %>%
  inner_join(cust_first, by = "customer_id") %>%
  mutate(days_since_first = as.integer(order_date - first_order)) %>%
  filter(days_since_first >= 0) %>%
  group_by(first_order, days_since_first) %>%
  summarise(customers = n_distinct(customer_id), .groups = "drop")

# 按 cohort 首次下单年月计算并增加 cohort_year
cohort <- df %>%
  inner_join(cust_first, by = "customer_id") %>%
  mutate(
    cohort_month = floor_date(first_order, "month"),
    cohort_year = lubridate::year(first_order),
    days_since_first = as.integer(order_date - first_order)
  ) %>%
  filter(days_since_first >= 0) %>%
  group_by(cohort_year, cohort_month, days_since_first) %>%
  summarise(customers = n_distinct(customer_id), .groups = "drop") %>%
  group_by(cohort_year, cohort_month) %>%
  mutate(retention = customers / max(customers, na.rm = TRUE)) %>%
  ungroup()

# 可选：只显示指定三年（例如 2023, 2024, 2025），按需修改 years 向量
years <- c(2023, 2024, 2025)
cohort_sub <- cohort %>% filter(cohort_year %in% years)

p_retention <- cohort_sub %>%
  filter(days_since_first <= 90) %>%
  mutate(cohort_month = as.character(cohort_month)) %>%
  ggplot(aes(
    x = days_since_first,
    y = retention,
    group = cohort_month,
    color = cohort_month
  )) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ cohort_year, ncol = 1) +
  labs(title = "Cohort 留存率（按首次下单年分面，最多展示90天）",
       x = "距首次下单天数",
       y = "留存率",
       color = "cohort_month") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "07_cohort_retention_90days_by_year.png"),
       p_retention, width = 10, height = 12, dpi = 300)











# 6) 品类/品牌业绩热图（root_category x brand）
cat_brand <- df %>%
  group_by(root_category, brand) %>%
  summarise(sales = sum(line_total, na.rm = TRUE), .groups = "drop") %>%
  group_by(root_category) %>%
  arrange(desc(sales)) %>%
  slice_head(n = 20) %>%   # 限制每类 top 20 brand
  ungroup()

p_cat_brand_heat <- ggplot(cat_brand, aes(x = brand, y = root_category, fill = sales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#005f73", labels = comma) +
  labs(title = "root_category x brand 销售额热图（每类 top 20 品牌）", x = "brand", y = "root_category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "08_category_brand_sales_heatmap.png"), p_cat_brand_heat, width = 14, height = 10, dpi = 300)

# 7) 渠道/地区转换效率：按 channel_source 和 region 的订单数与平均客单价
chan_region <- df %>%
  group_by(channel_source, region) %>%
  summarise(
    orders = n_distinct(order_id),
    sales = sum(line_total, na.rm = TRUE),
    avg_aov = if_else(orders > 0, sales / orders, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(desc(sales)) %>%
  slice_head(n = 200)   # 限制展示行数

p_chan_region_sales <- ggplot(chan_region, aes(x = reorder(channel_source, sales), y = sales, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "渠道 x 地区 销售额（按渠道排序）", x = "channel_source", y = "销售额（美元）") +
  theme_minimal()

ggsave(file.path(out_dir, "09_channel_region_sales.png"), p_chan_region_sales, width = 12, height = 10, dpi = 300)

# 8) 价格敏感度分析：initial_price vs final_price 与 discount_rate_adjusted 对 line_total 的影响
p_price_scatter <- df %>%
  filter(!is.na(initial_price) & !is.na(final_price)) %>%
  ggplot(aes(x = initial_price, y = final_price, color = discount_rate_adjusted)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(labels = dollar_format(prefix = "")) +
  scale_y_continuous(labels = dollar_format(prefix = "")) +
  scale_color_viridis_c() +
  labs(title = "初始价 vs 最终价（按折扣着色）", x = "initial_price", y = "final_price", color = "discount_rate_adj") +
  theme_minimal()

p_discount_effect <- df %>%
  filter(!is.na(discount_rate_adjusted)) %>%
  ggplot(aes(x = discount_rate_adjusted, y = line_total)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "折扣率 vs 单行销售额（箱型）", x = "discount_rate_adjusted", y = "line_total") +
  theme_minimal()

ggsave(file.path(out_dir, "10_price_vs_final_price.png"), p_price_scatter, width = 10, height = 7, dpi = 300)
ggsave(file.path(out_dir, "11_discount_vs_line_total_box.png"), p_discount_effect, width = 10, height = 6, dpi = 300)

# 9) 库存与上架影响：in_stock 对销售量影响（按 root_category 汇总）
stock_effect <- df %>%
  group_by(root_category, in_stock) %>%
  summarise(sales = sum(line_total, na.rm = TRUE), orders = n_distinct(order_id), .groups = "drop")

# 以 in_stock == TRUE 的销量排序 root_category（若某类没有 in_stock==TRUE，则视为 0）
order_df <- stock_effect %>%
  filter(in_stock == TRUE) %>%
  arrange(desc(sales)) %>%
  mutate(root_category = factor(root_category, levels = unique(root_category)))

# 若有 root_category 在 order_df 中缺失（全部为 FALSE），将它们追加到因子末尾
all_levels <- unique(c(as.character(order_df$root_category),
                       setdiff(unique(stock_effect$root_category),
                               as.character(order_df$root_category))))

# 将因子级别倒序（使绘图从上到下按销量从大到小）
stock_effect <- stock_effect %>%
  mutate(root_category = factor(root_category, levels = rev(all_levels)))

p_stock_effect <- ggplot(stock_effect, aes(x = root_category, y = sales, fill = in_stock)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "在售状态对 root_category 销售的影响（按 in_stock=TRUE 销量排序）",
       x = "root_category", y = "销售额（美元）") +
  theme_minimal()

ggsave(file.path(out_dir, "12_in_stock_by_category.png"), p_stock_effect, width = 12, height = 10, dpi = 300)



# 10) 评价与销量关联：rating vs sales（散点 + 平滑）
rating_sales <- df %>%
  group_by(product_id, rating) %>%
  summarise(sales = sum(line_total, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(rating))

p_rating_sales <- rating_sales %>%
  ggplot(aes(x = rating, y = sales, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "#FF6B6B") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Product rating 与 销售额的关系",
    x = "rating",
    y = "销售额（美元）"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "13_rating_vs_sales.png"), p_rating_sales, width = 10, height = 6, dpi = 300)

# 11) 图片/描述丰富度与销售：image_count 与 description 长度（需计算）
df2 <- df %>% mutate(
  desc_len = if ("description" %in% names(df)) nchar(replace_na(description, "")) else NA_integer_
)

p_image_desc <- df2 %>%
  group_by(product_id) %>%
  summarise(image_count = max(image_count, na.rm = TRUE),
            desc_len = max(desc_len, na.rm = TRUE),
            sales = sum(line_total, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = image_count, y = sales, group = 1)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#2E86AB") +
  labs(title = "image_count vs 产品销售额", x = "image_count", y = "销售额（美元）") +
  theme_minimal()

ggsave(file.path(out_dir, "14_image_count_vs_sales.png"), p_image_desc, width = 10, height = 6, dpi = 300)

# 12) 退货/售后影响：按 order_status（若存在）计算退货率
if ("order_status" %in% names(df)) {
  status_summary <- df %>%
    group_by(order_status) %>%
    summarise(orders = n_distinct(order_id),
              sales = sum(line_total, na.rm = TRUE),
              .groups = "drop")

  p_order_status <- ggplot(status_summary, aes(x = reorder(order_status, orders), y = orders, fill = order_status)) +
    geom_col() +
    coord_flip() +
    labs(title = "订单状态分布（包括退货/退款）", x = "order_status", y = "订单数") +
    theme_minimal() + theme(legend.position = "none")

  ggsave(file.path(out_dir, "15_order_status_distribution.png"), p_order_status, width = 10, height = 6, dpi = 300)
}

# 13) 地理热力（按 region 汇总销售额）
if ("region" %in% names(df)) {
  region_sales <- df %>%
    group_by(region) %>%
    summarise(sales = sum(line_total, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(sales))

  p_region_sales <- ggplot(region_sales, aes(x = reorder(region, sales), y = sales)) +
    geom_col(fill = "#FB5607") +
    coord_flip() +
    labs(title = "按 region 的销售额", x = "region", y = "销售额（美元）") +
    theme_minimal()

  ggsave(file.path(out_dir, "16_region_sales_bar.png"), p_region_sales, width = 10, height = 8, dpi = 300)
}

# 14) 多变量聚类可视化（PCA 降维示例）
# 先从 df 生成每客户的数值特征矩阵（示例：frequency, monetary, avg_order_value, recency_days）
pca_data <- cust_agg %>%
  dplyr::select(customer_id, frequency, monetary, avg_order_value, recency_days) %>%
  tidyr::drop_na()

pca_matrix <- pca_data %>%
  dplyr::select(-customer_id) %>%
  dplyr::mutate_all(~ ifelse(is.infinite(.), NA, .)) %>%
  tidyr::replace_na(list(frequency = 0, monetary = 0, avg_order_value = 0, recency_days = 9999))

pca_res <- prcomp(scale(pca_matrix), center = TRUE, scale. = TRUE)
pca_df <- tibble(
  customer_id = pca_data$customer_id,
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2]
) %>%
  left_join(cust_agg %>% dplyr::select(customer_id, customer_type), by = "customer_id")

p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = customer_type)) +
  geom_point(alpha = 0.6) +
  labs(title = "客户特征 PCA 可视化（PC1 vs PC2）", x = "PC1", y = "PC2") +
  theme_minimal()

ggsave(file.path(out_dir, "17_pca_customers_pc1_pc2.png"), p_pca, width = 10, height = 7, dpi = 300)

# 15) 异常订单 / 高价值订单榜单：Top-N line_total
top_orders <- df %>%
  group_by(order_id) %>%
  summarise(order_total = sum(line_total, na.rm = TRUE),
            customer_id = first(customer_id),
            order_date = first(order_date),
            customer_type = first(customer_type),
            .groups = "drop") %>%
  arrange(desc(order_total)) %>%
  slice_head(n = 20)

write_csv(top_orders, file.path(out_dir, "18_top20_orders_by_line_total.csv"))
p_top_orders <- ggplot(top_orders, aes(x = reorder(order_id, order_total), y = order_total, fill = customer_type)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top20 高价值订单", x = "order_id", y = "订单总额（美元）") +
  theme_minimal()

ggsave(file.path(out_dir, "18_top20_orders_bar.png"), p_top_orders, width = 12, height = 8, dpi = 300)

cat("所有图表已生成并保存在：", out_dir, "\n")