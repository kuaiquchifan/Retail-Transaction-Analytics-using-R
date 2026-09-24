# === 阶段 3：模拟订单数据验证（对应 03） ===
cat("### 阶段 3：模拟订单数据验证（对应 03）\n")

library(ggplot2)
library(tidyverse)
library(lubridate)
library(forecast)
library(MASS)
library(ggpubr)

monthly_sales <- read_csv("data/processed-v2/07_monthly_sales_amount_series.csv", show_col_types = FALSE) %>%
  mutate(month = as.Date(month))

viz_outdir <- "output-final-data-visual/eda_stage3"
if (!dir.exists(viz_outdir)) dir.create(viz_outdir, recursive = TRUE, showWarnings = FALSE)


# 1. 月度销售额时间序列（geom_line + geom_smooth）
p1 <- monthly_sales %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = monthly_sales, color = "实际"), linewidth = 1) +
  geom_smooth(aes(y = monthly_sales, color = "平滑趋势"), method = "loess", span = 0.3, se = TRUE, fill = "pink", linewidth = 0.8) +
  scale_color_manual(
    name = "系列",
    values = c("实际" = "steelblue", "平滑趋势" = "firebrick")
  ) +
  labs(title = "月度销售额时间序列", x = "月份", y = "销售额") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

ggsave(file.path(viz_outdir, "01_fig_monthly_sales_ts.png"), p1, width = 12, height = 5, dpi = 300)



# 2. STL 分解图（trend / seasonal / remainder）使用 forecast::stl + autoplot
ms <- monthly_sales %>% arrange(month)
start_year <- year(min(ms$month)); start_month <- month(min(ms$month))
ts_sales <- ts(ms$monthly_sales, start = c(start_year, start_month), frequency = 12)

stl_fit <- stl(ts_sales, s.window = "periodic", robust = TRUE)
autoplot(stl_fit) + ggtitle("STL 分解：趋势 / 季节 / 残差")
ggsave(file.path(viz_outdir, "02_fig_stl_decomposition.png"), width = 10, height = 6)

# 3.ACF / PACF 图（查看自相关）使用 forecast::ggtsdisplay
# 对原始序列或残差序列都可以查看；这里用差分后的序列示例（若有趋势）
ggtsdisplay(ts_sales, main = "时间序列：时序图 / ACF / PACF")
# 若需要保存为文件，可以用 png()/dev.off()

png(file.path(viz_outdir, "03_fig_acf_pacf.png"), width = 1000, height = 900)
ggtsdisplay(ts_sales, main = "时间序列：时序图 / ACF / PACF")
dev.off()

# 4. 预测 vs 实际 对比图（geom_line + geom_ribbon）
library(tidyverse)
all_models <- readRDS("data/processed-v2/07_monthly_sales_amount_forecast_models.rds")
metrics <- read_csv("data/processed-v2/07_monthly_sales_model_amount_comparison.csv", show_col_types = FALSE)

# 选择最佳模型（按 CSV 第一行或按 RMSE 最小）
best_name <- metrics$Model[1]
fc_obj <- all_models[[best_name]]

# 测试期时间序列范围：从脚本生成的 test dates 位置推断
# 为安全起见：用 monthly_sales 的最后 length(fc_obj$mean) 个月作为“预测期对应日期”
h <- length(fc_obj$mean)
test_dates <- tail(monthly_sales$month, h)

pred_df <- tibble(
  month = test_dates,
  forecast = as.numeric(fc_obj$mean),
  lower80 = ifelse(!is.null(fc_obj$lower), as.numeric(fc_obj$lower[,1]), NA_real_),
  upper80 = ifelse(!is.null(fc_obj$upper), as.numeric(fc_obj$upper[,1]), NA_real_)
)

actual_df <- monthly_sales %>% filter(month %in% test_dates) %>% dplyr::select(month, actual = monthly_sales)

p2 <- ggplot() +
  geom_line(data = monthly_sales, aes(x = month, y = monthly_sales), color = "grey60", linewidth = 0.8) +
  geom_line(data = actual_df, aes(x = month, y = actual), color = "black", linewidth = 1) +
  geom_ribbon(data = pred_df, aes(x = month, ymin = lower80, ymax = upper80), fill = "orange", alpha = 0.25) +
  geom_line(data = pred_df, aes(x = month, y = forecast), color = "orange", linewidth = 1, linetype = "dashed") +
  labs(title = paste0("预测 vs 实际：", best_name), x = "月份", y = "销售额") +
  theme_minimal()
ggsave(file.path(viz_outdir, "04_fig_pred_vs_actual.png"), p2, width = 12, height = 5)


cat("阶段 3 图表已保存到 ", viz_outdir, "\n", sep = "")