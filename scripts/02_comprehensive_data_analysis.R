library(tidyverse)
library(scales)

# 加载清洗后的数据
shein_clean <- readRDS("./data/processed/shein_clean.rds")

# ============================================
# 1. 价格分析
# ============================================

# 1.1 价格分布统计
price_summary <- shein_clean %>%
  summarise(
    mean_initial = mean(initial_price, na.rm = TRUE),
    median_initial = median(initial_price, na.rm = TRUE),
    mean_final = mean(final_price, na.rm = TRUE),
    median_final = median(final_price, na.rm = TRUE),
    avg_discount = mean(discount_rate, na.rm = TRUE)
  )

print("价格统计摘要：")
print(price_summary)

# 1.2 价格区间分布
price_ranges <- shein_clean %>%
  mutate(price_range = cut(final_price, 
                           breaks = c(0, 10, 20, 50, 100, 200, Inf),
                           labels = c("0-10", "10-20", "20-50", "50-100", "100-200", "200+"))) %>%
  count(price_range) %>%
  mutate(percentage = n / sum(n) * 100)

print("价格区间分布：")
print(price_ranges)

# 1.3 折扣率分析
discount_analysis <- shein_clean %>%
  filter(discount_rate > 0) %>%
  summarise(
    avg_discount = mean(discount_rate),
    median_discount = median(discount_rate),
    max_discount = max(discount_rate),
    discount_products = n()
  )

print("折扣分析：")
print(discount_analysis)

# ============================================
# 2. 分类分析
# ============================================

# 2.1 根分类销售表现
root_category_analysis <- shein_clean %>%
  group_by(root_category) %>%
  summarise(
    product_count = n(),
    avg_price = mean(final_price, na.rm = TRUE),
    avg_discount = mean(discount_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(product_count))

print("根分类分析：")
print(root_category_analysis)

# 2.2 子分类 TOP 20
category_analysis <- shein_clean %>%
  group_by(category) %>%
  summarise(
    product_count = n(),
    avg_price = mean(final_price, na.rm = TRUE)
  ) %>%
  arrange(desc(product_count)) %>%
  head(20)

print("TOP 20 子分类：")
print(category_analysis)

# 2.3 分类价格对比
category_price <- shein_clean %>%
  group_by(root_category) %>%
  summarise(
    min_price = min(final_price, na.rm = TRUE),
    q25 = quantile(final_price, 0.25, na.rm = TRUE),
    median_price = median(final_price, na.rm = TRUE),
    q75 = quantile(final_price, 0.75, na.rm = TRUE),
    max_price = max(final_price, na.rm = TRUE)
  )

print("分类价格分布：")
print(category_price)

# ============================================
# 3. 颜色/尺码分析
# ============================================

# 3.1 热门颜色 TOP 15
color_analysis <- shein_clean %>%
  filter(!is.na(color) & color != "") %>%
  count(color, sort = TRUE) %>%
  head(15) %>%
  mutate(percentage = n / sum(n) * 100)

print("TOP 15 热门颜色：")
print(color_analysis)

# 3.2 颜色与价格关系
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

print("颜色价格分析（样本>=10）：")
print(color_price)

# 3.3 尺码分布
size_analysis <- shein_clean %>%
  filter(!is.na(size) & size != "") %>%
  count(size, sort = TRUE) %>%
  head(20) %>%
  mutate(percentage = n / sum(n) * 100)

print("TOP 20 尺码分布：")
print(size_analysis)

# 3.4 尺码与价格关系
size_price <- shein_clean %>%
  filter(!is.na(size) & size != "") %>%
  group_by(size) %>%
  summarise(
    count = n(),
    avg_price = mean(final_price, na.rm = TRUE)
  ) %>%
  filter(count >= 5) %>%  # 从 10 改为 5
  arrange(desc(avg_price)) %>%
  head(15)

print("尺码价格分析（样本>=10）：")
print(size_price)

# ============================================
# 4. 品牌分析
# ============================================

# 4.1 TOP 20 品牌
brand_analysis <- shein_clean %>%
  filter(!is.na(brand) & brand != "") %>%
  group_by(brand) %>%
  summarise(
    product_count = n(),
    avg_price = mean(final_price, na.rm = TRUE),
    avg_discount = mean(discount_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(product_count)) %>%
  head(20)

print("TOP 20 品牌分析：")
print(brand_analysis)

# 4.2 品牌价格定位
brand_positioning <- shein_clean %>%
  filter(!is.na(brand) & brand != "") %>%
  group_by(brand) %>%
  summarise(
    count = n(),
    avg_price = mean(final_price, na.rm = TRUE)
  ) %>%
  filter(count >= 10) %>%
  mutate(
    price_tier = case_when(
      avg_price < 20 ~ "低价位",
      avg_price < 50 ~ "中价位",
      avg_price < 100 ~ "中高价位",
      TRUE ~ "高价位"
    )
  ) %>%
  count(price_tier)

print("品牌价格定位分布：")
print(brand_positioning)


# ============================================
# 5. 保存分析结果
# ============================================

# 创建结果列表
analysis_results <- list(
  price_summary = price_summary,
  price_ranges = price_ranges,
  discount_analysis = discount_analysis,
  root_category_analysis = root_category_analysis,
  category_analysis = category_analysis,
  color_analysis = color_analysis,
  size_analysis = size_analysis,
  brand_analysis = brand_analysis
)

# 保存为RDS
saveRDS(analysis_results, "./output/02_analysis_results.rds")

# 保存为CSV（方便Excel查看）
write.csv(root_category_analysis, "./output/02_root_category_analysis.csv", row.names = FALSE)
write.csv(category_analysis, "./output/02_category_analysis.csv", row.names = FALSE)
write.csv(color_analysis, "./output/02_color_analysis.csv", row.names = FALSE)
write.csv(size_analysis, "./output/02_size_analysis.csv", row.names = FALSE)
write.csv(brand_analysis, "./output/02_brand_analysis.csv", row.names = FALSE)

cat("\n✓ 分析完成！结果已保存到 output/ 目录\n")
cat("注意：本数据集不包含评分和评论数据\n")
