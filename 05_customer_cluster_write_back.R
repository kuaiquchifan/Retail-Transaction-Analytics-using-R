file.copy("data/processed-v2/04-combined_order_product_customer.parquet",
          "data/processed-v2/04-combined_order_product_customer.parquet.bak",
          overwrite = FALSE)


library(arrow)
library(dplyr)
library(readr)

# 1. 读取合并数据与聚类结果（根据你的保存路径调整）
data_path <- "data/processed-v2/04-combined_order_product_customer.parquet"
cluster_features_path <- "data/processed-v2/05-customer_features-k=3.parquet"
cluster_results_path <- "data/processed-v2/05-customer_cluster_result-k=3.parquet"



# 2. 读取原始合并表（尽量使用 arrow::read_parquet 以保留 schema）
merged <- arrow::read_parquet(data_path)


# 3. 读取包含每个 customer_id 聚类标签的表（优先使用包含 cluster 的 customer_features）
if (file.exists(cluster_features_path)) 
  cust_feat <- arrow::read_parquet(cluster_features_path)

print("debug: 读取的 cust_feat 示例：")
print(head(cust_feat))


cust_cluster_map <- arrow::read_parquet(cluster_features_path) %>%
  dplyr::select(customer_id, cluster) %>%
  dplyr::distinct()

if (!is.character(merged$customer_id)) {
  merged <- merged %>% dplyr::mutate(customer_id = as.character(customer_id))
}
if (!is.character(cust_cluster_map$customer_id)) {
  cust_cluster_map <- cust_cluster_map %>% dplyr::mutate(customer_id = as.character(customer_id))
}

merged_updated <- merged %>%
  dplyr::left_join(cust_cluster_map, by = "customer_id") %>%
  dplyr::mutate(
    customer_hierachy = as.integer(cluster),
    customer_type = dplyr::case_when(
      cluster == 1 ~ "低价值低频客户",
      cluster == 2 ~ "忠诚活跃用户",
      cluster == 3 ~ "高贡献客户",
      TRUE ~ NA_character_
    )
  )



# 基于 merged_updated 计算每个 product 的销量与均价（使用 quantity 与 final_price）
product_stats <- merged_updated %>%
  dplyr::group_by(product_id) %>%
  dplyr::summarise(
    total_qty = sum(coalesce(as.numeric(quantity), 0), na.rm = TRUE),
    avg_price = mean(coalesce(as.numeric(final_price), as.numeric(initial_price)), na.rm = TRUE),
    .groups = "drop"
  )

# 标准化与评分逻辑（同前）
prod_norm <- product_stats %>%
  dplyr::mutate(
    qty_scaled = (total_qty - min(total_qty, na.rm = TRUE)) /
                 (max(total_qty, na.rm = TRUE) - min(total_qty, na.rm = TRUE) + 1e-9),
    price_scaled = (avg_price - min(avg_price, na.rm = TRUE)) /
                   (max(avg_price, na.rm = TRUE) - min(avg_price, na.rm = TRUE) + 1e-9)
  )

prod_scored <- prod_norm %>%
  dplyr::mutate(
    price_mid_pref = 1 - abs(price_scaled - 0.5) * 2,
    price_mid_pref = pmax(pmin(price_mid_pref, 1), -1),
    score_raw = 0.7 * qty_scaled + 0.3 * ((price_mid_pref + 1) / 2),
    score_raw = pmax(pmin(score_raw, 1), 0),
    rating = ceiling(score_raw * 5)
  ) %>%
  dplyr::mutate(rating = ifelse(rating < 1, 1L, as.integer(rating))) %>%
  dplyr::select(product_id, rating)

# 将 product rating 回写到每行（按 product_id），并保持已有的 merged_updated 列
merged_updated <- merged_updated %>%
  dplyr::left_join(prod_scored, by = "product_id", suffix = c(".old", ".new")) %>%
  dplyr::mutate(
    rating = as.integer(dplyr::coalesce(rating.new, rating.old))
  ) %>%
  dplyr::select(-rating.old, -rating.new)


# （可选）写回文件
out_path <- "data/processed-v2/04-combined_order_product_customer_with_clusters.parquet"
arrow::write_parquet(merged_updated, sink = out_path)
message("已写入：", out_path)

























# # 确认 cluster 列名与 customer_id 存在
# if (!("customer_id" %in% names(cust_feat)) || !("cluster" %in% names(cust_feat))) {
#   stop("聚类结果文件需包含 customer_id 和 cluster 列")
# }

# print("debug: 读取的 cust_feat 示例：")
# print(head(cust_feat))





# # 4. 定义 cluster -> 标签 的映射（按你给定的描述）
# cluster_map <- tibble::tibble(
#   cluster = c(1L, 2L, 3L),
#   customer_type = c(
#     0L,   # Cluster 1 -> 0: 低活跃客户 / 潜力沉睡客户 / 低价值低频客户
#     1L,   # Cluster 2 -> 1: 高频活跃客户 / 多品类活跃客户 / 忠诚活跃用户
#     2L    # Cluster 3 -> 2: 高价值客户 / 高客单客户 / 高贡献客户
#   ),
#   customer_hierachy = c(
#     "低活跃 / 普通偏沉睡客户（人数最多）; 总消费最低≈433; 客单价≈72; 订单数≈6; recency≈415; 品类≈8.5",
#     "高频活跃 / 多品类客户（人数约四成）; 总消费≈966; 客单价≈91; 订单数≈10.6; recency≈331; 品类≈15.7",
#     "高价值 / 高客单客户（人数最少）; 总消费≈2106; 客单价≈285; 订单数≈7.7; recency≈380; 品类≈11.3"
#   )
# )

# # 5. 构造 customer_id -> cluster 映射（去重）
# cust_cluster_map <- cust_feat %>%
#   dplyr::select(customer_id, cluster) %>%
#   dplyr::distinct()

# # 6. 将映射合并回 merged 表（按 customer_id），并用 mapping 填充 customer_type / customer_hierachy
# # merged_updated <- merged %>%
# #   dplyr::left_join(cust_cluster_map, by = "customer_id") %>%
# #   dplyr::left_join(cluster_map, by = "cluster") %>%
# #   # 若原表已有 customer_type 或 customer_hierachy，不想覆盖空值可使用 coalesce
# #   dplyr::mutate(
# #     customer_type = customer_type,            # 已由 join 填充
# #     customer_hierachy = customer_hierachy     # 已由 join 填充
# #   )

# # 安全合并并检查 join 结果
# merged_updated <- merged %>%
#   dplyr::left_join(cust_cluster_map, by = "customer_id") %>%
#   dplyr::left_join(cluster_map, by = "cluster")

# # 确认 join 是否带来了 customer_type 列
# if (!"customer_type" %in% names(merged_updated)) {
#   stop("Join 未生成 customer_type。请检查 'cust_cluster_map$cluster' 和 'cluster_map$cluster' 的数据类型与取值是否匹配。")
# }

# # 将类型规范化（customer_type 用整数编码；保留 customer_hierachy 文本）
# merged_updated <- merged_updated %>%
#   mutate(
#     customer_type = as.integer(customer_type),
#     customer_hierachy = as.character(customer_hierachy)
#   )

# print("debug: cust_cluster_map 示例：")
# print(head(cust_cluster_map))
# print("debug: cluster_map 示例：")
# print(cluster_map)
# print("debug: cust_cluster_map 中 cluster 类型：")
# print(str(cust_cluster_map$cluster))
# print("debug: cluster_map 中 cluster 类型：")
# print(str(cluster_map$cluster))



# # 7. 可选：检查有多少行未被赋值（即没有匹配到 cluster）并打印
# missing_count <- sum(is.na(merged_updated$cluster))
# message("未匹配到 cluster 的行数：", missing_count)

# # 8. 写回到新的 parquet 文件（强烈建议写新文件以保留备份）
# out_path <- "data/processed-v2/04-combined_order_product_customer.with_clusters.parquet"
# arrow::write_parquet(merged_updated, sink = out_path)
# message("已将带 cluster 标签的表写入：", out_path)