[![English](https://img.shields.io/badge/README-English-2ea44f?style=for-the-badge)](README.md)
[![中文](https://img.shields.io/badge/README-中文-ffb703?style=for-the-badge)](README_zh.md)

# 零售交易分析与探索（Retail Transaction Analytics using R）

## 项目简介

本项目从原始 SHEIN 电商样本数据的加载、清洗、特征构建、客户与商品聚类、销量/数量预测，以及分阶段数据可视化与最终报告的完整分析流水线。目标是产出清洁数据集、聚类标签、预测模型与可复现的报告与可视化结果，便于后续分析或产品化。

## 项目重点关注以下业务问题：

- 客户分群与画像分析

  - 识别不同客户群体的消费特征、购买行为和价值差异，支持 RFM、留存和复购分析。
- 商品与类目分析

  - 研究品牌、颜色、尺码、分类和价格带对销量的影响，发现高潜力和低表现商品类型。
- 销售趋势与季节性分析

  - 挖掘月度销售额和销量的变化趋势，分析促销、节假日和周期性因素对销售的影响。
- 价格、折扣与库存影响

  - 评估价格敏感度、折扣策略和库存状态对订单表现与销售额的影响。
- 订单质量与业务异常检测

  - 检查订单金额、订单频次、退款和退货等行为的分布特征，识别异常模式。
- 预测与经营决策支持

  - 使用时间序列和机器学习模型预测未来销售金额和销量，辅助库存规划和营销决策。
- 数据可视化与分析报告

  - 通过分阶段可视化展示数据探索结果，并产出最终的报告与业务结论。

## 项目结构和功能特性

```bash
`00_load_shein_data.R`： # 使用tidyverse读取 SHEIN 的原始csv数据，进行数据分析的EDA步骤
`01_data_and_clean_modify_product_data.R`： # 把原始 SHEIN 产品数据进行数据清洗，整理成可用于后续客户分析、商品聚类和预测建模的干净数据表。
`02_build_customer_data.R`： # 模拟并生成客户数据表。
`03_build_order_data.R`： # 基于客户和商品数据，模拟生成订单主表和订单明细表。
`04_aggregate_date.R`： # 把前面生成的客户、订单、商品、订单明细数据合并成一张统一的大表，并把结果保存下来，便于后续时间维度分析、聚类和预测。
`05_customer_cluster_v3.R`： # 基于客户的消费行为和画像特征等聚类特征，对客户进行Kmeans聚类分群，并评估聚类效果。
`05_customer_cluster_write_back.R`： # 把前面生成的客户聚类结果写回原始订单/商品/客户联合数据中，并生成带聚类标签的最终表。
`06_product_cluster.R`： # 基于商品的销售表现、价格、折扣和覆盖情况，对产品进行聚类分组。
`07_sale_amount_predict_v3.R`： # 根据历史销售数据，构建月销售金额预测模型，并比较多个模型的预测效果，最终输出最优预测结果。。
`08_sale_qty_predict.R`： # 基于历史月销量数据，预测未来每月销量，并比较多种时间序列及机器学习模型的效果。
`09_data_visualization*.R`（多文件）： # 分阶段可视化脚本，逐步生成描述性图表（分布、时序、类别对比、客户/商品行为图等）。
`data`: # 存放原始数据集和基本聚合数据的文件夹
`output-final-data-visual/stage0/1/2/3` # 表示不同复杂度或不同分析阶段的可视化输出。
`10_final_report.qmd`： # 最终分析报告汇总(Quarto),包含关键发现、聚类结果摘要与建议。
`11_final_report.md`： # 最终分析报告汇总(Markdown),包含关键发现、聚类结果摘要与建议。
```

## 数据分析对象

项目中的核心数据来源包括：

- 原始数据源：SHEIN 产品数据集
- 处理后的数据表：产品表 / 客户表 / 订单表 / 订单明细表
- 数据类型：CSV / Parquet / RDS
- 生成方式：数据改编 + 模拟 + 清洗

---

## 数据规模：

用户：20000名
产品：1000种
订单： 16 万行
订单明细： 24 万行

**注意**：整个数据集基于 R 脚本生成的模拟电商数据和kaggle的公开数据

## 目录详细说明

本项目主要文件说明如下：

- 00_load_shein_data.R

  - 使用tidyverse读取 SHEIN 的原始csv数据，进行数据分析的EDA步骤
- 01_data_and_clean_modify_product_data.R

  - 读取原始 SHEIN 数据，做产品数据清洗，对表格字段做标准化和补全，生成一个更适合后续分析的干净产品表，把结果保存成多个格式，为后续商品分析和聚类做准备。
- 02_build_customer_data.R

  - 使用tidyverse拟生成一份 20000 条客户数据集，包括。customer_id、gender、age、region、registration_date、channel_source、preferred_category、customer_type、customer_hierachy、is_active。
  - 并输出为 Parquet 文件，供后续客户聚类、RFM 分析、分群模型及业务分析使用。
- 03_build_order_data.R

  - 基于客户和商品数据(01_data_and_clean_modify_product_data.R和02_build_customer_data.R的输出结果)，使用tidyverse模拟生成 160000 条订单主表记录和240000 条订单明细表，并输出为 Parquet 文件，为后续销售趋势、分群、预测等分析提供订单事实数据。
- 04_aggregate_date.R

  - 使用tidyverse和arrow把前面生成的客户、商品、订单、订单明细数据合并成一张统一的大表，并把结果保存下来，便于后续时间维度分析、聚类和预测。
- 05_customer_cluster_v3.R

  - 基于客户的消费行为、订单行为和画像特征等聚类特征，对客户进行Kmeans聚类分群，
  - 然后，评估聚类效果，包括：
    - 用肘部法则（WSS）评估最佳 K
    - 用轮廓系数（Silhouette）评估聚类质量
    - 可视化 PCA 二维投影图
- 05_customer_cluster_write_back.R

  - 读取聚类结果,将聚类结果加入到订单明细数据表中，供后续运营分析和报表使用
  - 并且计算产品评分。
- 06_product_cluster.R

  - 构建产品级特征、合并产品静态属性、做特征清洗和标准化，然后，基于商品的销售表现、价格、折扣和覆盖情况，对产品进行Kmeans聚类分组。
  - 然后，评估聚类效果，包括：
    - 用肘部法则（WSS）评估最佳 K
    - 用轮廓系数（Silhouette）评估聚类质量
    - 可视化 PCA 二维投影图
- 07_sale_amount_predict_v3.R

  - 根据历史销售数据，构建月销售金额预测模型，并比较多个模型的预测效果，最终输出最优预测结果。
  - 具体步骤：
    - 读取合并后的订单数据、构造月销售金额序列、加入趋势与季节性特征、划分训练集(18个月)与测试集(6个月)
    - 训练多种预测模型:
      - Baseline：Mean、Naive、Drift、Seasonal Naive、MA(3)、MA(6)、
      - 复杂模型：ETS、ARIMA、TSLM、Prophet、XGBoost、LightGBM
    - 评估模型表现
      - 比较 MAE / RMSE / MAPE / MASE
      - 找出最优模型
- 08_sale_qty_predict.R

  - 根据历史销售数据，构建月销售数量预测模型，并比较多个模型的预测效果，最终输出最优预测结果。
  - 具体步骤：
    - 读取合并后的订单数据、构造月销售数量序列、加入趋势与季节性特征、划分训练集(18个月)与测试集(6个月)
    - 训练多种预测模型:
      - Baseline：Mean、Naive、Drift、Seasonal Naive、MA(3)、MA(6)、
      - 复杂模型：ETS、ARIMA、TSLM、Prophet、XGBoost、LightGBM
    - 评估模型表现
      - 比较 MAE / RMSE / MAPE / MASE
      - 找出最优模型
- 09_data_visualization_stage0_v2.R

  - 在 Data vis 阶段 0，读取已合并的订单/商品/客户数据，并生成最基础的数据可视化与摘要统计。
  - 具体步骤：
    - 清洗字段类型(数据字段类型转换、处理缺失值和空字符串)
    - 计算基础统计指标(平均价格、中位数价格、折扣率、颜色/品牌/分类数量、价格分布)
    - 生成数据摘要表
    - 生成几类最基础的业务图表(价格区间分布、Top 5 根分类销量、品牌 Top 10、颜色偏好、尺码分布)
- 09_data_visualization_stage1_v2.R

  - 在 Data vis 阶段 1 中，进一步分析销售趋势、价格分布、折扣分布和类目分布，并输出更偏业务洞察的可视化图表。
  - 具体步骤：
    - 做数据格式统一处理(日期、价格)
    - 计算关键业务指标(客户总数、月度销售数量、月度销售金额、价格分布、折扣率分布、根分类分布)
    - 生成一组更业务化的图表(客户数卡片图、月度销售数量折线图、月度销售金额折线图、价格分布直方图 + 密度曲线、折扣率分布图、根分类 Top50 分布图、总体概览汇总图)
- 09_data_visualization_stage2_v2.R

  - 在 Data vis 阶段 2 中，进一步分析客户分群、RFM 特征、订单趋势和价值分布，并把客户和运营洞察做成更深层的可视化。
  - 具体步骤：
    - 按客户汇总特征(订单数、总消费、平均客单价、Recency、Frequency、customer_type)
    - 分析客户分群的差异(不同客户类型的消费金额分布、不同客户类型的订单数分布、不同客户类型人数分布)
    - 生成 RFM 分析图(Recency 分布、Frequency 分布、Monetary 分布)
    - 做客户行为与业务洞察图(平均客单价 vs 订单数、每日销售额/订单数趋势、cohort 留存率、类目 x 品牌热力图、渠道 x 地区销售额、价格敏感度图、库存状态对销售额影响、rating 与销售额关系、top20 高价值订单)
- 09_data_visualization_stage3_v2_sales.R

  - 在 Data vis 阶段 3 中，重点检查订单金额分布、订单频次分布和客单价特征是否符合常见电商分布特征。
  - 具体步骤：
    - 检查订单金额分布(绘制订单金额直方图、生成 QQ 图)
    - 检查订单频次分布(按客户统计每人订单次数、绘制客户订单频次直方图、试图拟合泊松分布和负二项分布、对比经验分布和理论分布)
    - 检查客单价密度分布(画订单金额密度曲线，观察高峰和集中趋势)
- 09_data_visualization_stage3_v2_qty.R

  - 在 Data vis 阶段 3 中，对月度销售量数据做趋势分析、季节分解、相关性检查和预测结果展示
  - 具体步骤：
    - 读取月度销售量时间序列数据
    - 依次生成 4 类图表：
      - 第一张是月度销售量时间序列图，展示实际值和平滑趋势；
      - 第二张是 STL 分解图，拆分出趋势、季节性和残差；
      - 第三张是 ACF/PACF 图，用来观察时间序列的自相关和偏自相关结构；
      - 第四张是预测结果与实际值对比图，展示最优模型的预测值、置信区间和真实销售量
- 09_data_visualization_stage3.R

  - 用统计图表验证模拟订单数据的分布特征
  - 具体步骤：
    - 读取模拟生成的订单数据
    - 重点检查订单金额分布是否偏态、是否存在长尾
    - 分析客户订单频次分布，看看它更接近泊松分布还是负二项分布
    - 观察客单价/订单金额的集中趋势
- 09_data_visualization.R

  - 把清洗后的电商商品数据和前面分析结果转成价格、分类、颜色、尺码和品牌分布图表
  - 具体步骤：
    - 读取清洗好的数据 shein_clean.rds 和分析结果 analysis_results.rds，
    - 然后创建输出目录，
    - 之后按照价格分析、分类分析、颜色/尺码分析、品牌分析和综合对比五大部分，生成了 14 张图表
- 10_final_report.qmd

  - 上述内容的最终输出报告(quarto版本)
- 11_final_report.md

  - 上述内容的最终输出报告(markdown版本)

## 项目运行环境

### 基础依赖

R 4.5.2
Windows 10/11
microsoft VScode
R Extension for VS Code
Quarto 1.10.8

### 主要使用的renv语言包：

```R
tidyverse
arrow
factoextra
ggplot2
cluster
kmeans
silhouette
prcomp
dplyr
readr
fviz_nbclust
lubridate
forecast
zoo
xgboost
prophet
xts
lightgbm
qqnorm
fitdistr
dpois
broom
car
emmeans
lme4
MASS
ggpubr
```

### 物理机的环境配置步骤

#### 下载项目并且打开文件夹

```bash
git clone <repository-url>
cd Retail-Transaction-Analytics-using-R-v3\
```

然后，打开powershell

```bash
R.exe
```

#### 初始化虚拟环境renv

```R
renv::init()
```

#### 安装依赖

```R
renv::restore()
```

### 运行顺序

按下面顺序逐步执行脚本以保证数据与中间产物可被后续步骤使用：

```R
source("00_load_shein_data.R")
source("01_data_and_clean_modify_product_data.R")
source("02_build_customer_data.R")
source("03_build_order_data.R")
source("04_aggregate_date.R")
source("05_customer_cluster_v3.R")
source("05_customer_cluster_write_back.R")
source("06_product_cluster.R")
source("07_sale_amount_predict_v3.R")
source("08_sale_qty_predict.R")
source("09_data_visualization*.R")
```

### 查看数据可视化报告：

- 打开
  - `10_final_report.qmd`  — 最终报告汇总（Quarto）。
  - `11_final_report.md` — 最终报告汇总（Markdown）。

## 许可协议

本项目为开源项目，遵循 **Apache 2.0** 许可协议。

## 作者与致谢

Author: Junliang Li   
Email: 940747544@qq.com
