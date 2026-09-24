# === 阶段 3：模拟订单数据验证（对应 03） ===
cat("### 阶段 3：模拟订单数据验证（对应 03）\n")

library(MASS)
library(ggpubr) 

# 读取模拟订单数据（请根据实际文件名调整）
orders_path <- "/data/processed-v2/03-orders_simulation_160000.parquet"
if (!file.exists(orders_path)) {
  stop("找不到模拟订单数据，请检查路径：", orders_path)
}
orders <- arrow::read_parquet(orders_path)

# 创建输出目录（如不存在）
if (!dir.exists("/output-final-data-visual/eda_stage3")) {
  dir.create("/output-final-data-visual/eda_stage3", recursive = TRUE)
}

# 确认关键列：假设订单金额列名为 order_amount，顾客 id 为 customer_id
if (!("total_amount" %in% names(orders))) {
  stop("orders 中未找到 total_amount 列，请确认列名。")
}

# 1) 订单金额分布（直方图） + QQ 图（检验长尾 / 右偏）
order_amt <- orders %>% filter(!is.na(total_amount) & total_amount > 0) %>% pull(total_amount)

p_order_hist <- ggplot(data.frame(total_amount = order_amt), aes(x = total_amount)) +
  geom_histogram(bins = 60, fill = "#2E86AB", alpha = 0.7) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(title = "订单金额分布（直方图）", x = "订单金额", y = "订单数") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output-final-data-visual/eda_stage3/order_amount_hist.png", p_order_hist, width = 10, height = 6, dpi = 300)

# QQ 图（原始金额）——使用基础 qqnorm / qqline 并保存为图片
png(filename = "/output-final-data-visual/eda_stage3/order_amount_qq.png", width = 1200, height = 800, res = 150)
  qqnorm(order_amt, main = "QQ 图：订单金额（原始）", xlab = "理论分位数", ylab = "样本分位数")
  qqline(order_amt, col = "red", lwd = 2)
dev.off()

# 2) 订单金额对数变换后 QQ 图（验证对数正态）
log_order_amt <- log(order_amt)

png(filename = "/output-final-data-visual/eda_stage3/log_order_amount_qq.png", width = 1200, height = 800, res = 150)
  qqnorm(log_order_amt, main = "QQ 图：订单金额（对数变换）", xlab = "理论分位数", ylab = "样本分位数（log）")
  qqline(log_order_amt, col = "red", lwd = 2)
dev.off()

# 3) 订单频次分布（按客户统计订单数）——检验是否符合泊松或负二项
if (!("customer_id" %in% names(orders))) {
  warning("orders 中未找到 customer_id 列，跳过订单频次分布分析。")
} else {
  order_freq <- orders %>%
    filter(!is.na(customer_id)) %>%
    count(customer_id, name = "freq") %>%
    pull(freq)

  # 直方图
  freq_df <- data.frame(freq = order_freq)
  p_freq_hist <- ggplot(freq_df, aes(x = freq)) +
    geom_histogram(bins = max(30, min(100, length(unique(order_freq)))), fill = "#A23B72", alpha = 0.7) +
    labs(title = "客户订单频次分布（直方图）", x = "订单次数（客户）", y = "客户数") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave("/output-final-data-visual/eda_stage3/order_freq_hist.png", p_freq_hist, width = 10, height = 6, dpi = 300)

  # 拟合泊松 & 负二项
  pois_lambda <- mean(order_freq)
  nb_fit <- tryCatch({
    fitdistr(order_freq, "Negative Binomial")
  }, error = function(e) NULL)

  # 绘制拟合曲线（以概率质量函数形式覆盖直方图）
  max_x <- max(order_freq)
  x_vals <- 0:max_x
  pois_pmf <- dpois(x_vals, lambda = pois_lambda)

  if (!is.null(nb_fit)) {
    nb_size <- nb_fit$estimate["size"]
    nb_mu   <- nb_fit$estimate["mu"]
    nb_pmf <- dnbinom(x_vals, size = nb_size, mu = nb_mu)
  } else {
    nb_pmf <- rep(NA_real_, length(x_vals))
  }

  # 为显示，转换 pmf 到与直方图相同的频数尺度： scale = N * binwidth (这里 binwidth=1)
  total_counts <- length(order_freq)
  pois_counts <- pois_pmf * total_counts
  nb_counts <- nb_pmf * total_counts

  fit_df <- tibble(x = x_vals, pois = pois_counts, nb = nb_counts)

  p_freq_fit <- ggplot(freq_df, aes(x = freq)) +
    geom_histogram(aes(y = ..count..), bins = max_x + 1, fill = "#A23B72", alpha = 0.6, boundary = -0.5) +
    geom_line(data = fit_df, aes(x = x, y = pois), color = "#06FFA5", size = 1, linetype = "dashed") +
    { if (!is.null(nb_fit)) geom_line(data = fit_df, aes(x = x, y = nb), color = "#FF006E", size = 1) } +
    labs(title = "订单频次分布与泊松/负二项拟合（曲线）", x = "订单次数（客户）", y = "客户数",
         caption = paste0("泊松 lambda=", round(pois_lambda,3),
                          if (!is.null(nb_fit)) paste0("; 负二项 size=", round(nb_size,3), ", mu=", round(nb_mu,3)) else "")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave("/output-final-data-visual/eda_stage3/order_freq_fit.png", p_freq_fit, width = 10, height = 6, dpi = 300)
}

# 4) 客单价分布（每笔订单的平均或直接使用 order_amount）——使用密度图观察集中趋势
# 如果有列 order_total 或 使用 order_amount 的 density
p_ticket_density <- ggplot(data.frame(total_amount = order_amt), aes(x = total_amount)) +
  geom_density(fill = "#2E86AB", alpha = 0.6) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(title = "客单价（订单金额）密度分布（观察集中趋势）", x = "订单金额", y = "密度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("/output-final-data-visual/eda_stage3/order_amount_density.png", p_ticket_density, width = 10, height = 6, dpi = 300)

cat("阶段 3 图表已保存到 /output-final-data-visual/eda_stage3/\n")