library(tidyverse)
if (!requireNamespace("arrow", quietly = TRUE)) {
  install.packages("arrow", repos = "https://cloud.r-project.org")
}
library(arrow)

set.seed(2024)

# 设定输出目录
project_root <- getwd()
output_dir <- file.path(project_root, "data", "processed-v2")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

n_customers <- 20000

customer_data <- tibble(
  customer_id = 1:n_customers,
  gender = sample(
    c("Male", "Female"),
    size = n_customers,
    replace = TRUE,
    prob = c(0.48, 0.52)
  ),
  age = pmax(
    18,
    pmin(75, round(rnorm(n_customers, mean = 32, sd = 12)))
  ),
  region = sample(
    c("California", "Texas", "Florida", "New York", "Illinois",
      "Pennsylvania", "Ohio", "Georgia", "North Carolina", "Washington",
      "Arizona", "Massachusetts", "Colorado", "New Jersey", "Virginia"),
    size = n_customers,
    replace = TRUE,
    prob = c(0.14, 0.11, 0.08, 0.09, 0.06,
             0.05, 0.05, 0.04, 0.04, 0.04,
             0.04, 0.03, 0.03, 0.03, 0.03)
  ),
  registration_date = as.Date(sample(
    seq(as.Date("2018-01-01"), as.Date("2024-12-31"), by = "day"),
    size = n_customers,
    replace = TRUE
  )),
  channel_source = sample(
    c("Website", "Mobile App", "Social Media", "Referral", "Email", "Store"),
    size = n_customers,
    replace = TRUE,
    prob = c(0.30, 0.25, 0.18, 0.12, 0.08, 0.07)
  ),
  preferred_category = {
    root_category_values <- read.csv(
      "data/processed-v2/01-shein_clean_utf8_bom.csv",
      stringsAsFactors = FALSE,
      na.strings = c("", "NA", "null")
    )$root_category

    root_category_values <- unique(
      root_category_values[!is.na(root_category_values) & root_category_values != ""]
    )

    sample(
      root_category_values,
      size = n_customers,
      replace = TRUE
    )
  },
  customer_type = rep(NA_character_, n_customers),
  customer_hierachy = rep(NA_character_, n_customers),
  is_active = sample(
    c(TRUE, FALSE),
    size = n_customers,
    replace = TRUE,
    prob = c(0.75, 0.25)
  )
)

output_file <- file.path(output_dir, "02-customer_simulation_20000.parquet")
write_parquet(customer_data, sink = output_file)

cat("已生成", nrow(customer_data), "条客户数据\n")
cat("输出文件：", output_file, "\n")
print(head(customer_data))