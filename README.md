[![English](https://img.shields.io/badge/README-English-2ea44f?style=for-the-badge)](README.md)
[![中文](https://img.shields.io/badge/README-中文-ffb703?style=for-the-badge)](README_zh.md)

# Retail Transaction Analytics and Exploration (Retail Transaction Analytics using R)

## Project Overview

This project covers the full analysis pipeline for the original SHEIN e-commerce sample data, including data loading, cleaning, feature engineering, customer and product clustering, sales and quantity forecasting, staged data visualization, and final reporting. The goal is to produce cleaned datasets, clustering labels, forecasting models, and reproducible reports and visualizations for subsequent analysis or productization.

## Key Business Questions

- Customer segmentation and profiling
  - Identify consumption patterns, purchasing behavior, and value differences across customer groups to support RFM, retention, and repeat purchase analysis.
- Product and category analysis
  - Study the impact of brand, color, size, category, and price range on sales performance, and identify high-potential and underperforming product types.
- Sales trends and seasonality
  - Explore monthly sales and quantity trends, and analyze the effects of promotions, holidays, and cyclical factors on sales.
- Price, discount, and inventory effects
  - Assess price sensitivity, discount strategy, and stock status on order performance and sales revenue.
- Order quality and anomaly detection
  - Examine the distribution of order amount, order frequency, refunds, and returns to identify abnormal patterns.
- Forecasting and business decision support
  - Use time series and machine learning models to predict future sales and order quantities to support inventory planning and marketing decisions.
- Data visualization and reporting
  - Present analysis results through staged visualizations and produce final reports and business insights.

## Project Structure and Features

```bash
`00_load_shein_data.R`: # Reads the original SHEIN CSV data using tidyverse and performs EDA for data analysis.
`01_data_and_clean_modify_product_data.R`: # Cleans the original SHEIN product data and prepares a clean product table suitable for downstream customer analysis, product clustering, and predictive modeling.
`02_build_customer_data.R`: # Simulates and generates a customer dataset.
`03_build_order_data.R`: # Uses customer and product data to simulate an order master table and order detail table.
`04_aggregate_date.R`: # Merges the generated customer, order, product, and order detail data into a unified table for time-based analysis, clustering, and forecasting.
`05_customer_cluster_v3.R`: # Performs customer clustering using behavioral and profile-based features, and evaluates clustering quality.
`05_customer_cluster_write_back.R`: # Writes the customer clustering results back to the combined order/product/customer dataset and generates a final file with cluster labels.
`06_product_cluster.R`: # Performs product clustering based on sales behavior, pricing, discounts, and coverage.
`07_sale_amount_predict_v3.R`: # Builds and compares monthly sales amount forecasting models to identify the best-performing model.
`08_sale_qty_predict.R`: # Builds and compares monthly sales quantity forecasting models to identify the best-performing model.
`09_data_visualization*.R` (multiple files): # Generates staged visualizations for distributions, time series, category comparisons, and customer or product behavior analysis.
`output-final-data-visual/stage0/1/2/3`: # Represents different levels of complexity or analysis stages.
`10_final_report.qmd`: # Final analysis report summary in Quarto format with key findings, clustering summaries, and recommendations.
`11_final_report.md`: # Final analysis report summary in Markdown format with key findings, clustering summaries, and recommendations.
```

## Data Analysis Objects

The project’s core data sources include:

- Original data source: SHEIN product dataset
- Processed data tables: product table / customer table / order table / order detail table
- Data types: CSV / Parquet / RDS
- Generation method: simulated + cleaned

---

## Data Scale

- Users: 20,000
- Products: 1,000
- Orders: 160,000 rows
- Order details: 240,000 rows

 **Note** : The entire dataset is based on simulated e-commerce data generated through R scripts and public data from Kaggle.

## Detailed Directory Description

The main files in this project are described below:

- `00_load_shein_data.R`

  - Uses tidyverse to read the original SHEIN CSV data and performs an EDA step.
- `01_data_and_clean_modify_product_data.R`

  - Reads the original SHEIN data, performs product data cleaning, standardizes and completes fields, and generates a cleaned product table suitable for downstream analysis and modeling.
- 02_build_customer_data.R

  - Uses tidyverse to generate a synthetic customer dataset of 20,000 records, including customer_id, gender, age, region, registration_date, channel_source, preferred_category, customer_type, customer_hierarchy, and is_active.
  - Outputs a Parquet file for subsequent customer clustering, RFM analysis, segmentation modeling, and business analysis.
- 03_build_order_data.R

  - Based on the customer and product data, uses tidyverse to simulate 160,000 order records and 240,000 order detail records, then outputs Parquet files for downstream sales trend analysis, segmentation, and forecasting.
- 04_aggregate_date.R

  - Uses tidyverse and arrow to merge the generated customer, product, order, and order detail data into a unified table for downstream temporal analysis, clustering, and forecasting.
- 05_customer_cluster_v3.R

  - Uses customer behavior, order behavior, and profile-based features to perform K-means customer segmentation.
  - Evaluates clustering effectiveness using:
    - WSS elbow method to estimate the best K
    - Silhouette coefficient to assess clustering quality
    - PCA projection plots for visualization
- 05_customer_cluster_write_back.R

  - Reads the clustering results and writes cluster assignments back to the order-level dataset for downstream operational analysis and reporting.
  - Also calculates product ratings.
- 06_product_cluster.R

  - Builds product-level features, merges static product attributes, performs feature cleaning and standardization, and groups products using K-means clustering based on sales performance, price, discount, and coverage.
  - Evaluates clustering effectiveness using:
    - WSS elbow method
    - Silhouette coefficient
    - PCA visualization
- 07_sale_amount_predict_v3.R

  - Builds a monthly sales revenue forecasting model based on historical sales data and compares multiple models.
  - Specific steps:
    - Read merged order data
    - Construct a monthly sales series
    - Add trend and seasonality features
    - Split data into training (18 months) and testing (6 months)
    - Train multiple forecasting models:
      - Baseline: Mean, Naive, Drift, Seasonal Naive, MA(3), MA(6)
      - Advanced models: ETS, ARIMA, TSLM, Prophet, XGBoost, LightGBM
    - Evaluate model performance using MAE, RMSE, MAPE, and MASE
- 08_sale_qty_predict.R

  - Builds a monthly sales quantity forecasting model and compares multiple models.
  - Specific steps:
    - Read merged order data
    - Construct a monthly quantity series
    - Add trend and seasonality features
    - Split data into training (18 months) and testing (6 months)
    - Train multiple models:
      - Baseline: Mean, Naive, Drift, Seasonal Naive, MA(3), MA(6)
      - Advanced models: ETS, ARIMA, TSLM, Prophet, XGBoost, LightGBM
    - Evaluate model performance using MAE, RMSE, MAPE, and MASE
- 09_data_visualization_stage0_v2.R

  - Data vis Stage 0: reads the merged order/product/customer data and generates basic summary statistics and visualizations.
  - Specific steps:
    - Clean field types
    - Handle missing values and empty strings
    - Compute summary metrics such as average price, median price, discount rate, color/brand/category counts, and price distribution
    - Generate basic business charts such as price band distribution, Top 5 category sales, Top 10 brands, color preference, and size distribution
- 09_data_visualization_stage1_v2.R

  - Data vis Stage 1: further analyzes sales trends, price distribution, discount distribution, and category distribution, and produces more business-oriented visualization outputs.
  - Specific steps:
    - Standardize data formats (date, price)
    - Compute key metrics such as customer count, monthly sales quantity, monthly sales amount, price distribution, discount distribution, and root category distribution
    - Generate business-oriented charts such as customer count cards, monthly sales quantity line chart, monthly sales amount line chart, price histogram and density plot, discount distribution plot, and Top 50 category distribution
- 09_data_visualization_stage2_v2.R

  - Data vis Stage 2: further analyzes customer segmentation, RFM features, order trends, and value distribution, producing deeper operational visualizations.
  - Specific steps:
    - Aggregate customer-level features: order count, total spend, average order value, recency, frequency, and customer_type
    - Analyze differences across customer types
    - Generate RFM charts, customer behavior visualizations, cohort retention charts, category-brand heatmaps, channel-region sales charts, price sensitivity plots, and high-value order analysis
- 09_data_visualization_stage3_v2_sales.R

  - Data vis Stage 3: focuses on checking whether the order amount distribution, order frequency distribution, and customer ticket size follow common e-commerce distribution patterns.
  - Specific steps:
    - Inspect order amount distribution using histograms and QQ plots
    - Inspect order frequency distribution by customer, comparing empirical data with Poisson and negative binomial fits
    - Inspect customer ticket size density to identify concentration and peak trends
- 09_data_visualization_stage3_v2_qty.R

  - In Data vis Stage 3, perform trend analysis, seasonal decomposition, correlation checks, and display forecasting results for monthly sales volume data
  - Specific steps:
    - Read the monthly sales volume time series data
    - Generate the following four types of charts in sequence:
      - The first chart is a monthly sales volume time series plot, showing actual values and the smoothed trend;
      - The second chart is an STL decomposition plot, breaking down the data into trend, seasonal, and residual components;
      - The third chart is an ACF/PACF plot, used to observe the autocorrelation and partial autocorrelation structures of the time series;
      - The fourth chart compares forecast results with actual values, displaying the forecast values, confidence intervals, and actual sales volume from the optimal model
- 09_data_visualization_stage3.R

  - Use statistical charts to verify the distribution characteristics of the simulated order data
  - Specific steps:
    - Read the simulated order data
    - Focus on checking whether the distribution of order amounts is skewed and whether there is a long tail
    - Analyze the distribution of customer order frequency to determine whether it is closer to a Poisson distribution or a negative binomial distribution
    - Observe the central tendency of the average order value and order amount
- 09_data_visualization.R

  - Convert the cleaned e-commerce product data and the previous analysis results into distribution charts for price, category, color, size, and brand
  - Specific steps:
    - Load the cleaned data file `shein_clean.rds` and the analysis results file `analysis_results.rds`,
    - Then create an output directory,
    - Next, generate 14 charts organized into five main sections: price analysis, category analysis, color/size analysis, brand analysis, and comprehensive comparison
- 10_final_report.qmd

  - Final report output in Quarto format
- 11_final_report.md

  - Final report output in Markdown format

## Project Runtime Environment

### Core Dependencies

- R 4.5.2
- Windows 10/11
- Microsoft VS Code
- R Extension for VS Code
- Quarto 1.10.8

### Main renv Packages Used

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
```

### Local Machine Setup Steps

#### Download the project and open the folder

```bash
git clone <repository-url>
cd Retail-Transaction-Analytics-using-R-v3\
```

and open powershell

```bash
R.exe
```

#### Initialize the renv environment

```R
renv::init()
```

#### Install dependencies

```R
renv::restore()
```

### Execution Order

Run the scripts in the following order to ensure that intermediate artifacts are available to downstream steps:

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

### View Data Visualization Reports

- Open:
  - `10_final_report.qmd` — Final report summary in Quarto format
  - `11_final_report.md` — Final report summary in Markdown format

## License

This project is an open-source project distributed under the **Apache 2.0** license.

## Author and Acknowledgements

Author: Junliang Li
Email: [940747544@qq.com](mailto:940747544@qq.com)

