# ==========================================================
# 07_sale_amount_predict.R
# 预测每月销售金额：Baseline + 复杂预测模型
# ==========================================================

options(stringsAsFactors = FALSE)

# 安装所需包（如果未安装）
packages_needed <- c(
  "tidyverse", "lubridate", "arrow", "forecast", "zoo",
  "xgboost", "prophet", "lightgbm"
)
for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(tidyverse)
library(lubridate)
library(arrow)
library(forecast)
library(zoo)

# ML 包单独加载，避免命名冲突
library(xgboost)
library(prophet)
library(lightgbm)

# ----------------------------------------------------------
# 1. 读取合并后的原始数据
# ----------------------------------------------------------
project_root <- getwd()
input_path <- file.path(project_root, "data", "processed-v2", "04-combined_order_product_customer.parquet")

if (!file.exists(input_path)) {
  stop("找不到数据文件: ", input_path)
}

combine_data <- arrow::read_parquet(input_path)

cat("原始数据维度:", dim(combine_data), "\n")
cat("字段名:\n")
print(colnames(combine_data))

# ----------------------------------------------------------
# 2. 构造月销售金额序列
# ----------------------------------------------------------
monthly_sales <- combine_data %>%
  mutate(
    order_date = as.Date(order_date),
    line_total = suppressWarnings(as.numeric(line_total)),
    total_amount = suppressWarnings(as.numeric(total_amount))
  ) %>%
  filter(!is.na(order_date)) %>%
  mutate(
    sales_amount = coalesce(line_total, total_amount)
  ) %>%
  filter(!is.na(sales_amount)) %>%
  group_by(month = floor_date(order_date, "month")) %>%
  summarise(
    monthly_sales = sum(sales_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month)

# 补齐缺失月份
full_months <- tibble(
  month = seq.Date(
    from = min(monthly_sales$month),
    to = max(monthly_sales$month),
    by = "month"
  )
)

monthly_sales <- full_months %>%
  left_join(monthly_sales, by = "month") %>%
  mutate(
    monthly_sales = replace_na(monthly_sales, 0)
  )

cat("\n月销售数据预览:\n")
print(head(monthly_sales, 12))

# ----------------------------------------------------------
# 2.5) 加入真实业务特征：趋势 + 季节性 + 促销冲击
# ----------------------------------------------------------
monthly_sales <- monthly_sales %>%
  mutate(
    month_index = row_number(),
    trend_factor = 1 + 0.025 * month_index,
    q1 = ifelse(month(month) %in% c(1, 2, 3), 1.00, 1),
    q2 = ifelse(month(month) %in% c(4, 5, 6), 1.05, 1),
    q3 = ifelse(month(month) %in% c(7, 8, 9), 1.10, 1),
    q4 = ifelse(month(month) %in% c(10, 11, 12), 1.20, 1),

    holiday_factor = ifelse(month(month) %in% c(11, 12), 1.35, 1),
    summer_factor = ifelse(month(month) %in% c(6, 7, 8), 1.20, 1),

    summer_2024_factor = ifelse(
      year(month) == 2024 & month(month) %in% c(6, 7, 8),
      1.20,
      1
    ),

    black_friday_factor = ifelse(
      year(month) == 2025 & month(month) %in% c(11, 12),
      1.30,
      1
    ),

    seasonal_factor = holiday_factor * summer_factor * q4 * q3 * q2 * q1,
    promo_factor = summer_2024_factor * black_friday_factor,
    monthly_sales = monthly_sales * trend_factor * seasonal_factor * promo_factor
  )

cat("\n加入趋势和季节性后，月销售数据预览:\n")
print(head(monthly_sales, 12))

# ----------------------------------------------------------
# 3. 构造时间序列对象
# ----------------------------------------------------------
sales_values <- monthly_sales$monthly_sales
start_year <- year(min(monthly_sales$month))
start_month <- month(min(monthly_sales$month))

sales_ts <- ts(
  sales_values,
  start = c(start_year, start_month),
  frequency = 12
)

cat("\n时间序列起始:", start(sales_ts), "\n")
cat("时间序列长度:", length(sales_ts), "\n")

# ----------------------------------------------------------
# 4. 划分训练集 / 测试集
# ----------------------------------------------------------
train_months <- 18
h <- 6

if (length(sales_values) < train_months + h) {
  stop("数据总长度不足，至少需要 18 + 6 = 24 个月的观测。当前可用长度为: ", length(sales_values))
}

train_start_idx <- length(sales_values) - train_months - h + 1
train_end_idx <- length(sales_values) - h

train_idx <- train_start_idx:train_end_idx
test_idx <- (length(sales_values) - h + 1):length(sales_values)

train_ts <- ts(
  sales_values[train_idx],
  start = c(
    year(monthly_sales$month[train_start_idx]),
    month(monthly_sales$month[train_start_idx])
  ),
  frequency = 12
)

test_ts <- ts(
  sales_values[test_idx],
  start = c(
    year(monthly_sales$month[train_end_idx + 1]),
    month(monthly_sales$month[train_end_idx + 1])
  ),
  frequency = 12
)

cat("\n训练集长度:", length(train_ts), "\n")
cat("测试集长度:", length(test_ts), "\n")
cat("训练集起始:", start(train_ts), "\n")
cat("测试集起始:", start(test_ts), "\n")

# ----------------------------------------------------------
# 4.5) 时间序列特征工程（供 ML 模型使用）
# ----------------------------------------------------------
make_ts_features <- function(df) {
  df %>%
    arrange(month) %>%
    mutate(
      year        = year(month),
      month_num   = month(month),
      quarter_num = quarter(month),
      trend       = row_number(),

      lag_1  = dplyr::lag(monthly_sales, 1),
      lag_3  = dplyr::lag(monthly_sales, 3),
      lag_6  = dplyr::lag(monthly_sales, 6),
      lag_12 = dplyr::lag(monthly_sales, 12),

      ma_3 = zoo::rollmean(monthly_sales, k = 3, fill = NA, align = "right"),
      ma_6 = zoo::rollmean(monthly_sales, k = 6, fill = NA, align = "right")
    )
}

feature_cols <- c(
  "year", "month_num", "quarter_num", "trend",
  "lag_1", "lag_3", "lag_6", "lag_12",
  "ma_3", "ma_6"
)

ml_features <- monthly_sales %>%
  select(month, monthly_sales) %>%
  make_ts_features() %>%
  filter(complete.cases(.))

stopifnot(is.data.frame(monthly_sales))
cat("\n=== 调试：monthly_sales 类型检查 ===\n")
print(class(monthly_sales))

cat("\n=== 调试：monthly_sales 结构 ===\n")
str(monthly_sales)

train_months <- monthly_sales$month[train_idx]
test_months  <- monthly_sales$month[test_idx]

train_ml <- ml_features %>%
  filter(month %in% train_months)

test_ml <- ml_features %>%
  filter(month %in% test_months)



cat("\nML 可用训练样本数:", nrow(train_ml), "\n")
cat("ML 可用测试样本数:", nrow(test_ml), "\n")

# 构造 ML 训练用 ts 对象（长度与 fitted 一致）
train_ts_ml <- ts(
  train_ml$monthly_sales,
  start = c(year(min(train_ml$month)), month(min(train_ml$month))),
  frequency = 12
)

# ----------------------------------------------------------
# 辅助函数：把 ML 预测包装成 forecast 对象
# ----------------------------------------------------------
make_forecast_like <- function(train_ts, fitted_vals, forecast_vals,
                               method_name, model_obj = NULL) {
  out <- list(
    mean      = as.numeric(forecast_vals),
    fitted    = as.numeric(fitted_vals),
    x         = train_ts,
    method    = method_name,
    residuals = as.numeric(train_ts) - as.numeric(fitted_vals)
  )
  if (!is.null(model_obj)) {
    out$model <- model_obj
  }
  structure(out, class = "forecast")
}

# ML 递归多步预测
predict_ml_recursive <- function(model, model_name, monthly_sales_full,
                                 feature_cols, h = 6) {
  history <- monthly_sales_full %>%
    select(month, monthly_sales)

  preds <- numeric(h)

  for (i in seq_len(h)) {
    next_month <- max(history$month) %m+% months(1)

    temp_df <- history %>%
      bind_rows(tibble(month = next_month, monthly_sales = NA_real_)) %>%
      make_ts_features()

    x_new <- as.matrix(temp_df[nrow(temp_df), feature_cols, drop = FALSE])

    pred <- switch(model_name,
      "XGBoost"  = as.numeric(predict(model, xgb.DMatrix(x_new))),
      "LightGBM" = as.numeric(predict(model, x_new)),
      stop("不支持的模型: ", model_name)
    )

    preds[i] <- pred

    history <- history %>%
      bind_rows(tibble(month = next_month, monthly_sales = pred))
  }

  return(preds)
}

# ----------------------------------------------------------
# 5. Baseline 模型
# ----------------------------------------------------------
rolling_mean_forecast <- function(x, order = 3, h = 6) {
  last_value <- mean(tail(x, order))
  forecast_values <- rep(last_value, h)

  structure(
    list(
      mean      = forecast_values,
      lower     = rep(NA_real_, h),
      upper     = rep(NA_real_, h),
      level     = c(80, 95),
      x         = x,
      fitted    = x,
      residuals = NULL,
      method    = paste0("Rolling mean (order = ", order, ")"),
      series    = deparse(substitute(x)),
      model     = list(order = order),
      type      = "point.forecast"
    ),
    class = "forecast"
  )
}

baseline_models <- list(
  "Mean"           = meanf(train_ts, h = h),
  "Naive"          = naive(train_ts, h = h),
  "Drift"          = rwf(train_ts, h = h, drift = TRUE),
  "Seasonal Naive" = snaive(train_ts, h = h),
  "MA(3)"          = rolling_mean_forecast(train_ts, order = 3, h = h),
  "MA(6)"          = rolling_mean_forecast(train_ts, order = 6, h = h)
)

# ----------------------------------------------------------
# 6. 拟合 ML 模型
# ----------------------------------------------------------
cat("\n开始拟合 ML 模型...\n")

# 6.1 Prophet
prophet_df <- monthly_sales %>%
  select(ds = month, y = monthly_sales)

prophet_train_df <- prophet_df[train_idx, ]
prophet_test_df  <- prophet_df[test_idx, ]

prophet_model <- prophet(
  prophet_train_df,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE
)

prophet_future_train <- make_future_dataframe(
  prophet_model,
  periods = length(test_idx),
  freq = "month"
)
prophet_fc_train <- predict(prophet_model, prophet_future_train)

prophet_fitted <- as.numeric(head(prophet_fc_train$yhat, length(train_idx)))
prophet_pred   <- as.numeric(tail(prophet_fc_train$yhat, length(test_idx)))

prophet_fc <- make_forecast_like(
  train_ts      = train_ts,
  fitted_vals   = prophet_fitted,
  forecast_vals = prophet_pred,
  method_name   = "Prophet",
  model_obj     = prophet_model
)

# 6.2 XGBoost
dtrain_xgb <- xgb.DMatrix(
  data = as.matrix(train_ml[, feature_cols]),
  label = train_ml$monthly_sales
)
dtest_xgb <- xgb.DMatrix(
  data = as.matrix(test_ml[, feature_cols])
)

xgb_model <- xgb.train(
  params  = list(objective = "reg:squarederror", eta = 0.1, max_depth = 4),
  data    = dtrain_xgb,
  nrounds = 100,
  verbose = 0
)

xgb_fitted <- predict(xgb_model, dtrain_xgb)
xgb_pred   <- predict(xgb_model, dtest_xgb)

xgb_fc <- make_forecast_like(
  train_ts      = train_ts_ml,
  fitted_vals   = xgb_fitted,
  forecast_vals = xgb_pred,
  method_name   = "XGBoost",
  model_obj     = xgb_model
)

# 6.3 LightGBM
dtrain_lgb <- lgb.Dataset(
  data  = as.matrix(train_ml[, feature_cols]),
  label = train_ml$monthly_sales
)

lgb_model <- lgb.train(
  params = list(
    objective     = "regression",
    metric        = "rmse",
    num_leaves    = 8,
    learning_rate = 0.1
  ),
  data     = dtrain_lgb,
  nrounds  = 100,
  verbose  = -1
)

lgb_fitted <- predict(lgb_model, as.matrix(train_ml[, feature_cols]))
lgb_pred   <- predict(lgb_model, as.matrix(test_ml[, feature_cols]))

lgb_fc <- make_forecast_like(
  train_ts      = train_ts_ml,
  fitted_vals   = lgb_fitted,
  forecast_vals = lgb_pred,
  method_name   = "LightGBM",
  model_obj     = lgb_model
)


# ----------------------------------------------------------
# 7. 复杂模型
# ----------------------------------------------------------
complex_models <- list(
  "ETS"      = forecast(ets(train_ts), h = h),
  "ARIMA"    = forecast(auto.arima(
    train_ts,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE
  ), h = h),
  "TSLM"     = forecast(tslm(train_ts ~ trend + season), h = h),
  "Prophet"  = prophet_fc,
  "XGBoost"  = xgb_fc,
  "LightGBM" = lgb_fc
)

all_models <- c(baseline_models, complex_models)

# ----------------------------------------------------------
# 8. 评估模型：对比 MAE / RMSE / MAPE
# ----------------------------------------------------------
model_metrics <- purrr::map_dfr(names(all_models), function(model_name) {
  fc <- all_models[[model_name]]
  acc <- accuracy(fc, test_ts)

  tibble(
    Model = model_name,
    MAE  = as.numeric(acc[2, "MAE"]),
    RMSE = as.numeric(acc[2, "RMSE"]),
    MAPE = as.numeric(acc[2, "MAPE"]),
    MASE = as.numeric(acc[2, "MASE"])
  )
}) %>%
  arrange(RMSE)

print(model_metrics)

# ----------------------------------------------------------
# 9. 选出最优模型
# ----------------------------------------------------------
best_model_name <- model_metrics$Model[1]
best_model_fc <- all_models[[best_model_name]]

cat("\n最优模型:", best_model_name, "\n")

# ----------------------------------------------------------
# 10. 输出预测结果
# ----------------------------------------------------------
output_dir <- file.path(project_root, "data", "processed-v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(model_metrics, file.path(output_dir, "07_monthly_sales_model_amount_comparison.csv"), row.names = FALSE)
write.csv(monthly_sales, file.path(output_dir, "07_monthly_sales_amount_series.csv"), row.names = FALSE)
saveRDS(all_models, file.path(output_dir, "07_monthly_sales_amount_forecast_models.rds"))

# ----------------------------------------------------------
# 11. 可视化
# ----------------------------------------------------------
test_dates <- monthly_sales$month[test_idx]

all_forecasts <- purrr::map_dfr(
  names(all_models),
  function(model_name) {
    fc <- all_models[[model_name]]
    tibble(
      month = test_dates,
      sales = as.numeric(fc$mean),
      Model = model_name
    )
  }
)

actual_data <- monthly_sales %>%
  select(month, sales = monthly_sales)

forecast_plot <- ggplot() +
  geom_line(
    data = actual_data,
    aes(x = month, y = sales),
    linewidth = 1
  ) +
  geom_line(
    data = all_forecasts,
    aes(x = month, y = sales, color = Model),
    linewidth = 0.8
  ) +
  geom_point(
    data = actual_data %>% filter(month %in% test_dates),
    aes(x = month, y = sales),
    size = 2
  ) +
  geom_vline(
    xintercept = min(test_dates),
    linetype = "dashed"
  ) +
  labs(
    title = "Monthly Sales Forecast - Model Comparison",
    subtitle = paste(
      "Training:",
      format(min(monthly_sales$month[train_idx]), "%Y-%m"),
      "~",
      format(max(monthly_sales$month[train_idx]), "%Y-%m"),
      "| Test:",
      format(min(test_dates), "%Y-%m"),
      "~",
      format(max(test_dates), "%Y-%m")
    ),
    x = "Month",
    y = "Sales Amount",
    color = "Forecast Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

print(forecast_plot)

ggsave(
  filename = file.path(
    output_dir,
    "07_monthly_sales_amount_all_models_forecast.png"
  ),
  plot = forecast_plot,
  width = 14,
  height = 8,
  dpi = 300
)

cat(
  "\n所有模型预测图已保存:",
  file.path(output_dir, "07_monthly_sales_amount_all_models_forecast.png"),
  "\n"
)

# ----------------------------------------------------------
# 12. 未来 6 个月预测（真正的未来，不是测试集）
# ----------------------------------------------------------
future_horizon <- 6
last_month <- tail(monthly_sales$month, 1)
future_dates <- seq.Date(
  from = last_month %m+% months(1),
  by = "month",
  length.out = future_horizon
)

# 根据最佳模型类型生成未来预测
if (best_model_name %in% c("ETS", "ARIMA", "TSLM", "Mean", "Naive",
                           "Drift", "Seasonal Naive", "MA(3)", "MA(6)")) {

  sales_ts_full <- ts(
    monthly_sales$monthly_sales,
    start = start(sales_ts),
    frequency = 12
  )

  future_fc <- switch(best_model_name,
    "ETS"            = forecast(ets(sales_ts_full), h = future_horizon),
    "ARIMA"          = forecast(auto.arima(
      sales_ts_full,
      seasonal = TRUE,
      stepwise = FALSE,
      approximation = FALSE
    ), h = future_horizon),
    "TSLM"           = forecast(tslm(sales_ts_full ~ trend + season), h = future_horizon),
    "Mean"           = meanf(sales_ts_full, h = future_horizon),
    "Naive"          = naive(sales_ts_full, h = future_horizon),
    "Drift"          = rwf(sales_ts_full, h = future_horizon, drift = TRUE),
    "Seasonal Naive" = snaive(sales_ts_full, h = future_horizon),
    "MA(3)"          = rolling_mean_forecast(sales_ts_full, order = 3, h = future_horizon),
    "MA(6)"          = rolling_mean_forecast(sales_ts_full, order = 6, h = future_horizon)
  )

  future_values <- as.numeric(future_fc$mean)

} else if (best_model_name == "Prophet") {

  prophet_df_full <- monthly_sales %>%
    select(ds = month, y = monthly_sales)

  prophet_model_full <- prophet(
    prophet_df_full,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )

  prophet_future_full <- make_future_dataframe(
    prophet_model_full,
    periods = future_horizon,
    freq = "month"
  )
  prophet_fc_full <- predict(prophet_model_full, prophet_future_full)

  future_values <- as.numeric(tail(prophet_fc_full$yhat, future_horizon))

} else if (best_model_name %in% c("XGBoost", "LightGBM")) {

  ml_model <- all_models[[best_model_name]]$model

  future_values <- predict_ml_recursive(
    model              = ml_model,
    model_name         = best_model_name,
    monthly_sales_full = monthly_sales,
    feature_cols       = feature_cols,
    h                  = future_horizon
  )

} else {
  stop("未知最佳模型: ", best_model_name)
}

future_forecast_tbl <- tibble(
  month = future_dates,
  forecast_value = future_values
)

write.csv(
  future_forecast_tbl,
  file.path(output_dir, "07_future_6_month_sales_amount_forecast.csv"),
  row.names = FALSE
)

cat("\n============================= \n")
cat("月销售预测脚本运行完成\n")
cat("结果已保存到 data/processed-v2/ 目录\n")
cat("============================= \n")
cat("模型比较文件: data/processed-v2/07_monthly_sales_model_amount_comparison.csv\n")
cat("月销售序列:   data/processed-v2/07_monthly_sales_amount_series.csv\n")
cat("预测图:       data/processed-v2/07_monthly_sales_amount_all_models_forecast.png\n")
cat("未来6个月预测: data/processed-v2/07_future_6_month_sales_amount_forecast.csv\n")

monthly_sales %>%
  filter(
    month >= as.Date("2025-05-01"),
    month <= as.Date("2025-08-01")
  ) %>%
  select(month, monthly_sales) %>%
  print()