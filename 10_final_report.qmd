# SHEIN 商品销售数据分析报告

---

**项目介绍：**
基于 R 的 SHEIN 电商商品、客户及订单数据分析项目，覆盖商品 EDA、销售趋势分析、客户 RFM/K-means 聚类以及时间序列销售预测。

本项目使用模拟的 SHEIN 电商交易数据，构建从数据清洗、探索性分析、客户分群到销售预测的完整数据分析流程。

---

### 技术栈

- **Language:** R
- **Data Processing:** dplyr, data.table
- **Data Storage:** Parquet
- **EDA:** ggplot2
- **Visualization:** ggplot2, patchwork
- **Customer Analytics:** RFM, K-means, PCA
- **Forecasting:** forecast, fable/tsibble, Prophet, XGBoost, LightGBM, CatBoost
- **Environment:** renv
- **Reporting:** Quarto

## 1. 商品数据表现

本报告对SHEIN平台的 1,000 件商品、20000个客户、160000条订单记录、240,000 条明细行进行了全面分析。

## 项目概览

### 1.1 订单和客户数据集的概览

| metric   | value                                                        |
| -------- | ------------------------------------------------------------ |
| 商品数量 | 1,000                                                        |
| 客户数量 | 20,000                                                       |
| 订单数量 | 160,000                                                      |
| 订单明细 | 240,000                                                      |
| 时间范围 | 2023-01 ~ 2025-12                                            |
| 分析工具 | R                                                            |
| 数据格式 | Parquet                                                      |
| 预测模型 | ETS / ARIMA / TSLM / Prophet / XGBoost / LightGBM / CatBoost |

### 1.2 商品数据概览

| metric         | value   | unit |
| -------------- | ------- | ---- |
| 商品平均价格   | 27.5569 | 美元 |
| 商品中位数价格 | 8.42    | 美元 |
| 有折扣商品数量 | 937     | 个数 |
| 折扣商品占比   | 1       |      |
| 平均折扣率     | 0.8396  |      |
| 最大折扣率     | 0.9     |      |
| 根分类数量     | 21      | 个数 |
| 品牌数量       | 30      | 个数 |

### 1.3 全部商品的价格分布

![图片说明](output-final-data-visual\eda_stage0\02_price_distribution_bar.png)

### 1.4 全部商品的折扣率分布

![图片说明](output-final-data-visual\eda_stage1\05_discount_hist.png)

## 2. 销售数据表现

### 2.1 每月月销售数量表现

![图片说明](output-final-data-visual\eda_stage1\02_viz_monthly_qty.png)

### 2.2 每月月销售金额表现

![图片说明](output-final-data-visual\eda_stage1\03_viz_monthly_amount.png)

### 2.3 全部时间段的订单的状态分布

![图片说明](output-final-data-visual\eda_stage2\15_order_status_distribution.png)

### 2.4 Top5商品类别销量分布

| root_category            | total_qty | total_sales | avg_price          | avg_discount       |
| ------------------------ | --------- | ----------- | ------------------ | ------------------ |
| Home & Living            | 222384    | 3742630     | 20.028890581810828 | 0.8504812445171372 |
| Tools & Home Improvement | 59485     | 3571135.73  | 79.7515977103836   | 0.85980996006216   |
| Apparel Accessories      | 55690     | 596797.85   | 13.104374394835933 | 0.8267192631034199 |
| Bags & Luggage           | 55349     | 1134001.26  | 26.587120026092627 | 0.8398703323866682 |
| Jewelry & Watches        | 50720     | 322850.99   | 7.160246212121212  | 0.8580315948638338 |

![图片说明](output-final-data-visual\eda_stage0\03_top5_category_sales.png)

### 2.5 Top20商品颜色的销量分布

![图片说明](output-final-data-visual\eda_stage0\04_color_preference_top20.png)

### 2.6 Top20商品尺码的销量分布

![图片说明](output-final-data-visual\eda_stage0\05_size_distribution_top20.png)

### 2.7 Top10商品品牌的销量分布

![图片说明](output-final-data-visual\eda_stage0\06_brand_top10.png)

### 2.8 不同销售渠道和地区的商品销售额对比

![图片说明](output-final-data-visual\eda_stage2\09_channel_region_sales.png)

### 2.9 不同在售状态和商品类别的关系

![图片说明](output-final-data-visual\eda_stage2\12_in_stock_by_category.png)

## 3. 客户聚类表现

### 3.1 客户聚类结果与 PCA 可视化

| cluster | customer_count | avg_total_spend  | avg_order_value  | avg_order_count  | avg_recency_days | avg_discount_rate | avg_category_diversity | avg_discounted_item_ratio |
| ------- | -------------- | ---------------- | ---------------- | ---------------- | ---------------- | ----------------- | ---------------------- | ------------------------- |
| 1       | 10245          | 433.422988775012 | 72.1030916339368 | 6.07193753050268 | 414.881015129331 | 0.159382974134957 | 8.54719375305027       | 1                         |
| 2       | 7748           | 966.379935467217 | 90.9029368645232 | 10.6472638100155 | 331.073567372225 | 0.160078930113693 | 15.7169592152814       | 1                         |
| 3       | 1999           | 2106.22093046523 | 284.931148415696 | 7.6528264132066  | 380.324162081041 | 0.166676663613317 | 11.3326663331666       | 1                         |

Cluster 1：低活跃 / 普通偏沉睡客户（人数最多）

数据表现：总消费最低（约 433）
客单价最低（约 72）
订单数最少（约 6 次）
距上次购买最久（约 415 天）
品类最少（约 8.5）

Cluster 2：高频活跃 / 多品类客户

数据表现：总消费中等偏上（约 966）
客单价中等（约 91）
订单数最多（约 10.6）
距上次购买最近（约 331 天）
品类多样性最高（约 15.7）

Cluster 3：高价值 / 高客单客户（人数最少）

数据表现：总消费最高（约 2,106，约是 Cluster 1 的 5 倍）
客单价最高（约 285，约是另外两簇的 3–4 倍）
订单数中等（约 7.7，介于 1 和 2 之间）
recency 中等（约 380 天）
品类中等（约 11.3）

![图片说明](output-final-data-visual\eda_stage2\17_pca_customers_pc1_pc2.png)

![图片说明](output-final-data-visual\eda_stage2\02_cluster_count_bar.png)

### 3.2 不同客户cluster的总消费金额（美元）和订单数的表现

![图片说明](output-final-data-visual\eda_stage2\01_cluster_monetary_ordercount_box.png)

### 3.3 不同客户cluster的RFM表现

![图片说明](output-final-data-visual\eda_stage2\03_rfm_box_by_type.png)

### 3.4 不同客户cluster的AOV和订单数的对比

![图片说明](output-final-data-visual\eda_stage2\04_avg_order_value_vs_order_count.png)

### 3.5 不同客户cluster的每日销售额的对比

![图片说明](output-final-data-visual\eda_stage2\05_time_series_sales_by_type.png)

### 3.6 不同客户cluster的90日存留率对比

![图片说明](output-final-data-visual\eda_stage2\07_cohort_retention_90days_by_year.png)

### 3.7 Kmeans聚类: K选择K=3的silhouette图

![图片说明](data\processed-v2\06-product_silhouette_k_selection_plot-k=3.png)

### 3.8 Kmeans聚类: K选择K=3的elbow图

![图片说明](data\processed-v2\06-product_wss_elbow_plot-k=3.png)

## 4. 销售预测的表现

### 4.1 不同模型预测的销售金额和实际值对比

不同模型预测的销售金额的表现

| Model          | MAE              | RMSE             | MAPE             | MASE             |
| -------------- | ---------------- | ---------------- | ---------------- | ---------------- |
| TSLM           | 189824.61637425  | 290514.182613091 | 11.6228524267116 | 1.61842806549712 |
| Seasonal Naive | 257589.30149425  | 362031.357565177 | 17.0767957535605 | 2.19618383997243 |
| Drift          | 278957.794414299 | 367169.026853096 | 19.3435361342239 | 2.37836974040908 |
| XGBoost        | 284938.491033417 | 413604.609858485 | 18.5084421145313 | 2.42936063634494 |
| Naive          | 308726.41286675  | 438991.677159415 | 19.9563061763514 | 2.63217437594452 |
| ETS            | 308734.629316431 | 439005.835135172 | 19.9566300740724 | 2.63224442867539 |
| Prophet        | 434016.338293879 | 495227.698416285 | 39.0457652074239 | 3.7003853145908  |
| MA(3)          | 411848.74441675  | 546589.119608356 | 27.3452507336705 | 3.5113863493325  |
| ARIMA          | 419774.766333032 | 560612.218459896 | 27.8893277815819 | 3.57896291849445 |
| LightGBM       | 441758.813108417 | 569465.649846891 | 29.8770936567472 | 3.76639697722758 |
| Mean           | 441758.813383833 | 569465.650060543 | 29.8770936800608 | 3.76639697957575 |
| MA(6)          | 475198.820012583 | 595780.325235683 | 32.7077406104782 | 4.05150354937742 |

预测结果的最佳模型是TSLM

<!-- ![图片说明](output-final-data-visual\eda_stage3\01_fig_monthly_sales_ts.png) -->

![图片说明](data\processed-v2\07_monthly_sales_amount_forecast.png)

![图片说明](data\processed-v2\07_monthly_sales_amount_all_models_forecast.png)

### 4.2 预测的销售数量和实际值对比

不同模型预测的销售数量的表现

| Model          | MAE              | RMSE             | MAPE             | MASE             |
| -------------- | ---------------- | ---------------- | ---------------- | ---------------- |
| TSLM           | 9698.45106388889 | 13169.4196890443 | 14.3839955306438 | 1.95410638511381 |
| Prophet        | 11072.8623826392 | 13464.6875926295 | 17.623780463132  | 2.23103162977919 |
| Seasonal Naive | 11749.8165083333 | 16292.0980915735 | 17.8016379282526 | 2.36742870707884 |
| Drift          | 12313.1766416667 | 17150.7943410681 | 18.3950917793493 | 2.48093813517341 |
| XGBoost        | 12765.2225010417 | 18188.4461447101 | 18.6168684405855 | 2.57201924640961 |
| Naive          | 13850.3741416667 | 20249.8696026742 | 19.8002266401932 | 2.79066258809306 |
| ETS            | 13850.8960384428 | 20250.3499139377 | 19.8010607209752 | 2.79076774321689 |
| MA(3)          | 17798.0770583333 | 23926.4203551979 | 26.4451752803446 | 3.58607119769195 |
| ARIMA          | 18594.5845140006 | 24839.7120090156 | 27.828930879482  | 3.74655664992106 |
| LightGBM       | 19391.7646234375 | 25134.4898346885 | 29.4809938483046 | 3.90717763276405 |
| Mean           | 19391.764725     | 25134.489913046  | 29.4809940417712 | 3.90717765322752 |
| MA(6)          | 20731.1822666667 | 26181.748646968  | 32.0324531434662 | 4.17705212630186 |

预测结果的最佳模型是TSLM

<!-- ![图片说明](output-final-data-visual\eda_stage3\01_fig_monthly_ts_qty.png) -->

![图片说明](data\processed-v2\08_monthly_best_sales_qty_forecast.png)

![图片说明](data\processed-v2\08_monthly_sales_qty_all_models_forecast.png)

### 4.3 预测的销售金额的STL分解

![图片说明](output-final-data-visual\eda_stage3\02_fig_stl_decomposition.png)

### 4.4 预测的销售数量的STL分解

![图片说明](output-final-data-visual\eda_stage3\02_fig_stl_decomposition_qty.png)

### 4.5 预测的销售金额的时序图、ACF和PACF情况

![图片说明](output-final-data-visual\eda_stage3\03_fig_acf_pacf.png)

### 4.6 预测的销售数量的时序图、ACF和PACF情况

![图片说明](output-final-data-visual\eda_stage3\03_fig_acf_pacf_qty.png)


## 5. 综合洞察与建议

### 5.1 核心发现

1. **价格策略**

   - 平台以中低价位为主，符合快时尚定位
   - 折扣力度大，超过 61% 的商品有折扣
2. **品类结构**

   - Home & Living 占据主导地位
   - 品类丰富度高，满足多样化需求
3. **商品属性**

   - 颜色和尺码选择丰富
   - Multicolor 等基础色最受欢迎
4. **品牌格局**

   - 品牌集中度适中
   - SHEIN 等头部品牌占据重要位置

### 5.2 商业建议

**1. 定价优化**

- 维持当前亲民价格策略，强化性价比优势
- 针对高价值品类，可适当提升定价空间
- 优化折扣结构，避免过度依赖促销

**2. 品类管理**

- 加强优势品类（ Home & Living ）的深度开发
- 培育高潜力品类，平衡品类结构
- 关注长尾品类，满足细分需求
