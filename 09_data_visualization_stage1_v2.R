library(tidyverse)
library(arrow)
library(scales)

# 数据源（合并并带聚类标签）
data_path <- "data/processed-v2/04-combined_order_product_customer_with_clusters.parquet"
if (!file.exists(data_path)) stop("找不到文件：", data_path)
df <- arrow::read_parquet(data_path)

# 输出目录
out_dir <- "/output-final-data-visual/eda_stage1"
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

# 指标1：客户数量（去重）
customer_count <- df %>% summarise(customers = n_distinct(customer_id)) %>% pull(customers)
# 1) 客户总数：画成带大字号的文本卡片
p_customer_count <- tibble(metric = "客户总数", value = customer_count) %>%
  ggplot(aes(x = 1, y = 1)) +
  geom_text(aes(label = paste0("客户总数\n", formatC(value, big.mark = ","))),
            size = 12, fontface = "bold", lineheight = 0.9) +
  theme_void()
ggsave(file.path(out_dir, "01_customer_count_card.png"), p_customer_count, width = 6, height = 4, dpi = 300)
# write_csv(tibble(metric = "客户总数", value = customer_count), file.path(out_dir, "summary_customer_count.csv"))

# 指标2：按月销售数量（件数）与销售金额
monthly_sales <- df %>%
  filter(!is.na(order_date)) %>%
  mutate(order_month = floor_date(order_date, "month")) %>%
  group_by(order_month) %>%
  summarise(
    monthly_qty = sum(coalesce(quantity, 0), na.rm = TRUE),
    monthly_amount = sum(coalesce(line_total, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(order_month)

# 2) 月度销售数量折线图（件数）
p_monthly_qty <- monthly_sales %>%
  ggplot(aes(x = order_month, y = monthly_qty)) +
  geom_line(color = "#2E86AB", size = 0.9) +
  geom_point(color = "#2E86AB", size = 1.5) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "月度销售数量（件数）", x = "月份", y = "销售数量（件）") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "02_viz_monthly_qty.png"), p_monthly_qty, width = 10, height = 5, dpi = 300)

# 2) 月度销售金额折线图
p_monthly_amount <- monthly_sales %>%
  ggplot(aes(x = order_month, y = monthly_amount)) +
  geom_line(color = "#FF006E", size = 0.9) +
  geom_point(color = "#FF006E", size = 1.5) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "月度销售金额", x = "月份", y = "销售金额（美元）") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "03_viz_monthly_amount.png"), p_monthly_amount, width = 10, height = 5, dpi = 300)


# 指标3：价格分布直方图 + 密度（final_price）
p_price <- df %>%
  filter(!is.na(final_price) & final_price >= 0) %>%
  ggplot(aes(x = final_price)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#2E86AB", alpha = 0.6, color = "white", linewidth = 0.2) +
  geom_density(color = "#FF006E", size = 1) +
  scale_x_continuous(labels = dollar_format(prefix = "")) +
  labs(title = "价格分布：直方图 + 密度曲线", x = "最终价格（美元）", y = "密度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "04_price_hist_density.png"), p_price, width = 10, height = 6, dpi = 300)

# 指标4：折扣率分布直方图
p_discount <- df %>%
  filter(!is.na(discount_rate)) %>%
  mutate(discount_pct = discount_rate * 100) %>%
  filter(discount_pct >= 0, discount_pct <= 100) %>%   # <-- 过滤异常
  ggplot(aes(x = discount_pct)) +
  geom_histogram(bins = 40, fill = "#A23B72", alpha = 0.7, color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), labels = function(x) paste0(x, "%")) +
  labs(title = "折扣率分布", x = "折扣率 (%)", y = "商品/行数") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "05_discount_hist.png"), p_discount, width = 10, height = 6, dpi = 300)

# 指标5：根分类分布（数量与销售额）
category_summary <- df %>%
  filter(!is.na(root_category) & root_category != "") %>%
  group_by(root_category) %>%
  summarise(
    items = n(),
    sales = sum(coalesce(line_total, 0), na.rm = TRUE),
    customers = n_distinct(customer_id),
    .groups = "drop"
  ) %>%
  arrange(desc(items))

p_category <- category_summary %>%
  slice_max(order_by = items, n = 50) %>%   # 展示 top50，太多会拥挤
  ggplot(aes(x = reorder(root_category, items), y = items)) +
  geom_col(fill = "#6A994E") +
  coord_flip() +
  labs(title = "根分类商品/订单数量分布（Top50）", x = "root_category", y = "行数") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(out_dir, "06_category_top50_bar.png"), p_category, width = 10, height = 8, dpi = 300)

# 汇总信息输出到文件
summary_info <- tibble(
  metric = c("customer_count", "monthly_start", "monthly_end", "months_covered"),
  value = c(
    as.character(customer_count),
    as.character(min(monthly_sales$order_month, na.rm = TRUE)),
    as.character(max(monthly_sales$order_month, na.rm = TRUE)),
    as.character(n_distinct(monthly_sales$order_month))
  )
)

p_summary_info <- summary_info %>%
  ggplot(aes(x = 1, y = metric, label = paste0(metric, ": ", value))) +
  geom_text(hjust = 0, size = 4) +
  theme_void() +
  labs(title = "总体概览")

ggsave(file.path(out_dir, "07_overall_summary.png"), p_summary_info, width = 8, height = 3, dpi = 300)
write_csv(summary_info, file.path(out_dir, "overall_summary.csv"))


# 简短打印到控制台
cat("客户总数：", customer_count, "\n")
cat("概览图表已保存到：", out_dir, "\n")
