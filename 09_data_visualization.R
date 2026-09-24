library(tidyverse)
library(scales)
library(gridExtra)

# 加载清洗后的数据和分析结果
shein_clean <- readRDS("/data/processed/shein_clean.rds")
analysis_results <- readRDS("/output/02_analysis_results.rds")

# 创建输出目录
if (!dir.exists("/output/plotsfrom03")) {
  dir.create("/output/plotsfrom03", recursive = TRUE)
}

# ============================================
# 1. 价格分析可视化
# ============================================

# 1.1 价格分布直方图
p1 <- ggplot(shein_clean, aes(x = final_price)) +
  geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.7) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "商品价格分布", x = "最终价格", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/01_price_distribution.png", p1, width = 10, height = 6, dpi = 300)

# 1.2 价格区间饼图
price_ranges <- shein_clean %>%
  mutate(price_range = cut(final_price, 
                           breaks = c(0, 10, 20, 50, 100, 200, Inf),
                           labels = c("$0-10", "$10-20", "$20-50", "$50-100", "$100-200", "$200+"))) %>%
  count(price_range) %>%
  mutate(percentage = n / sum(n) * 100)

p2 <- ggplot(price_ranges, aes(x = "", y = n, fill = price_range)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "价格区间分布", fill = "价格区间") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/02_price_range_pie.png", p2, width = 8, height = 6, dpi = 300)

# 1.3 折扣率分布
p3 <- ggplot(shein_clean %>% filter(discount_rate > 0), aes(x = discount_rate)) +
  geom_histogram(bins = 30, fill = "#A23B72", alpha = 0.7) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  labs(title = "折扣率分布", x = "折扣率 (%)", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/03_discount_distribution.png", p3, width = 10, height = 6, dpi = 300)

# ============================================
# 2. 分类分析可视化
# ============================================
# 2.1 根分类商品数量
p4 <- ggplot(analysis_results$root_category_analysis, 
             aes(x = reorder(root_category, product_count), y = product_count)) +
  geom_col(fill = "#F18F01") +
  coord_flip() +
  labs(title = "各根分类商品数量", x = "根分类", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/04_root_category_count.png", p4, width = 10, height = 6, dpi = 300)

# 2.2 根分类平均价格
p5 <- ggplot(analysis_results$root_category_analysis, 
             aes(x = reorder(root_category, avg_price), y = avg_price)) +
  geom_col(fill = "#C73E1D") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "各根分类平均价格", x = "根分类", y = "平均价格") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/05_root_category_price.png", p5, width = 10, height = 6, dpi = 300)

# 2.3 TOP 20 子分类
p6 <- ggplot(analysis_results$category_analysis, 
             aes(x = reorder(category, product_count), y = product_count)) +
  geom_col(fill = "#6A994E") +
  coord_flip() +
  labs(title = "TOP 20 子分类商品数量", x = "子分类", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8))

ggsave("/output/plotsfrom03/06_top20_categories.png", p6, width = 10, height = 8, dpi = 300)

# 2.4 分类价格箱线图（修复：移除 limits，避免数据丢失警告）
p7 <- ggplot(shein_clean %>% filter(final_price <= 200), 
             aes(x = reorder(root_category, final_price, median), y = final_price)) +
  geom_boxplot(fill = "#BC4B51", alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "各根分类价格分布（箱线图，价格≤$200）", 
       x = "根分类", y = "最终价格") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/07_category_price_boxplot.png", p7, width = 10, height = 6, dpi = 300)

# ============================================
# 3. 颜色/尺码分析可视化
# ============================================

# 3.1 TOP 15 热门颜色
p8 <- ggplot(analysis_results$color_analysis, 
             aes(x = reorder(color, n), y = n)) +
  geom_col(fill = "#8338EC") +
  coord_flip() +
  labs(title = "TOP 15 热门颜色", x = "颜色", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/08_top15_colors.png", p8, width = 10, height = 6, dpi = 300)

# 3.2 颜色价格关系（气泡图）
color_price <- shein_clean %>%
  filter(!is.na(color) & color != "") %>%
  group_by(color) %>%
  summarise(
    count = n(),
    avg_price = mean(final_price, na.rm = TRUE)
  ) %>%
  filter(count >= 10) %>%
  arrange(desc(avg_price)) %>%
  head(15)

p9 <- ggplot(color_price, aes(x = reorder(color, avg_price), y = avg_price, size = count)) +
  geom_point(color = "#FF006E", alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_size_continuous(range = c(3, 15)) +
  labs(title = "TOP 15 颜色平均价格（气泡大小=商品数量）", 
       x = "颜色", y = "平均价格", size = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/09_color_price_bubble.png", p9, width = 10, height = 6, dpi = 300)

# 3.3 TOP 20 尺码分布
p10 <- ggplot(analysis_results$size_analysis, 
              aes(x = reorder(size, n), y = n)) +
  geom_col(fill = "#3A86FF") +
  coord_flip() +
  labs(title = "TOP 20 尺码分布", x = "尺码", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/10_top20_sizes.png", p10, width = 10, height = 8, dpi = 300)

# ============================================
# 4. 品牌分析可视化
# ============================================

# 4.1 TOP 20 品牌商品数量
p11 <- ggplot(analysis_results$brand_analysis, 
              aes(x = reorder(brand, product_count), y = product_count)) +
  geom_col(fill = "#FB5607") +
  coord_flip() +
  labs(title = "TOP 20 品牌商品数量", x = "品牌", y = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8))

ggsave("/output/plotsfrom03/11_top20_brands.png", p11, width = 10, height = 8, dpi = 300)

# 4.2 品牌价格与折扣散点图
p12 <- ggplot(analysis_results$brand_analysis, 
              aes(x = avg_price, y = avg_discount, size = product_count)) +
  geom_point(color = "#FFBE0B", alpha = 0.6) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_size_continuous(range = c(3, 15)) +
  labs(title = "TOP 20 品牌：价格 vs 折扣率", 
       x = "平均价格", y = "平均折扣率", size = "商品数量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/12_brand_price_discount.png", p12, width = 10, height = 6, dpi = 300)

# ============================================
# 5. 综合对比图
# ============================================

# 5.1 价格与折扣关系散点图（修复：抑制 geom_smooth 消息）
p13 <- ggplot(shein_clean %>% filter(discount_rate > 0), 
              aes(x = initial_price, y = discount_rate)) +
  geom_point(alpha = 0.3, color = "#06FFA5") +
  geom_smooth(method = "lm", color = "#FF006E", se = TRUE, formula = y ~ x) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "原价与折扣率关系", x = "原价", y = "折扣率") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/13_price_discount_scatter.png", p13, width = 10, height = 6, dpi = 300)

# 5.2 根分类折扣率对比
category_discount <- shein_clean %>%
  filter(discount_rate > 0) %>%
  group_by(root_category) %>%
  summarise(avg_discount = mean(discount_rate, na.rm = TRUE)) %>%
  arrange(desc(avg_discount))

p14 <- ggplot(category_discount, 
              aes(x = reorder(root_category, avg_discount), y = avg_discount)) +
  geom_col(fill = "#7209B7") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "各根分类平均折扣率", x = "根分类", y = "平均折扣率") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output/plotsfrom03/14_category_discount.png", p14, width = 10, height = 6, dpi = 300)

cat("\n✓ 可视化完成！所有图表已保存到 output/plotsfrom03/ 目录\n")
cat("共生成 14 张图表\n")