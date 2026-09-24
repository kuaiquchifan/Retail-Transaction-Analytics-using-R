library(tidyverse)

# 加载数据
shein <- read_csv(
  "/data/raw/shein.csv",
  locale = locale(encoding = "UTF-8"),
  na = c("", "NA", "null"),
  trim_ws = TRUE,
  quote = "\"",
  show_col_types = FALSE
)

# 检查数据加载是否正确
cat("数据维度:", dim(shein), "\n")
cat("列数:", ncol(shein), "\n")

# 数据清洗
shein_clean <- shein %>%
  # 先过滤：删除 color 列为 "N/A"（忽略大小写/前后空格）或缺失的行
  filter(!is.na(color) & toupper(trimws(color)) != "N/A") %>%
  # 1. 计算折扣率
  mutate(
    # 从 CSV 第一行到最后一行，按顺序重新编号
    model_number = row_number(),
    product_id = row_number(),
    # 先保留当前全为 TRUE 的状态，再随机把一小部分改成 FALSE
    in_stock = ifelse(runif(n()) < 0.05, FALSE, TRUE),
    # 大多数商品都是 true，小部分为 false
    free_returns = ifelse(runif(n()) < 0.08, FALSE, TRUE),
    free_shipping = ifelse(runif(n()) < 0.10, FALSE, TRUE),

    # 随机模拟 rating：1~5
    rating = rep(NA_real_, n()),
    # 计算折扣率
    discount_rate = case_when(
      is.na(initial_price) | is.na(final_price) | initial_price <= 0 ~ 0,
      (initial_price - final_price) < 0 ~ 0,
      ((initial_price - final_price) / initial_price) < 1 ~
        1 - ((initial_price - final_price) / initial_price),
      TRUE ~ (initial_price - final_price)
    )
  ) %>%
  # 2. 移除无用列
  select(
    -category_url, -url, -category_tree, -image_urls,
    -offers, -related_products, -top_reviews,
    -other_attributes, -all_available_sizes,
    -main_image, -reviews_count
  ) %>%
  {
    color_map <- list(
      default = c("ykl545-btz", "wifi 2+32gb", "style 1",
                  "Retro Telephone", "Pencil Case + Eraser",
                  "Multifunctional Vegetable Cutter", "Lick Itself",
                  "Envelope Box", "DIY Children's Sewing Fun",
                  "As the Picture0", "As the Picture Shows",
                  "As Pic", "3cm Random Color", "30 Pieces",
                  "1810264", "15 Items", "02C 270pcs"),
      Yellow = c("yellow grid", "yellow", "Rice Flower 3 Pieces"),
      Gold = c("yellow gold", "rose gold", "champagne", "marble/gold"),
      Green = c("xiuyu jade pagoda", "sage green",
                "spruce green", "mint green", "dark green", "blackish green"),
      Brown = c("wood color", "dark brown", "light brown",
                "brown ombre", "ombre", "coffee brown", "mocha brown",
                "espresso", "khaki", "walnut",
                "havana/pink gradient brown", "Black Brown",
                "Happy Turkey Luxe"),
      Pink = c("watermelon pink", "hot pink", "pink rabbit box",
               "coral pink", "Coral", "dusty pink", "transparent pink",
               "strawberry box", "sakura refrigerator mat",
               "pink+blue", "22. Enchantment Coral"),
      Grey = c("terracotta warriors set", "grey", "gray",
               "light grey", "dark grey",
               "Dark Grey(3 Seater)",
               "neutral gray", "charcoal gray", "dimgrey",
               "gray wood grain", "white grey",
               "Grey Three-petal Flower Earrings"),
      Teal = c("teal blue", "enchantment coral", "blue-green",
               "Light Cyan", "Ink-blue Colour"),
      Blue = c("blue", "sky blue", "baby blue", "denim blue",
               "steelblue", "navy blue", "royal blue",
               "medium blue", "blue denim", "ink-blue colour",
               "blue highlight", "c2 blue frame/blue lens",
               "blue and white", "blue and white 6pcs/set",
               "Dark Blue Check", "C4 Black Frame/Gold Lens",
               "06 Black Blue Frame / 4 Blue Lens"),
      Black = c("piano color", "solemn black", "natural black",
                "black2", "black & red", "black and white",
                "rusticbrown/black", "nine-grid sudoku",
                "Black-58*58cm", "Black/ Gray/ White"),
      White = c("white", "clear", "warm white",
                "cream color", "beige", "beige+light coffee",
                "transparent", "transparent pink",
                "3 Pieces Of Purple and White Powder",
                "【Basic Set】Warm White"),
      Red = c("red", "rose red", "burgundy", "Red and White",
              "Red + Champagne"),
      Purple = c("purple", "violet purple", "light purple"),
      Orange = c("orange", "orange color", "burnt orange", "apricot"),
      Multicolor = c("multicolor", "multicolor random",
                     "Multicolor Five", "Multi-color",
                     "Multicolor", "Jewellery Pop Beads",
                     "Black, White, Red and Yellow", "Color Mixing",
                     "Bright")
    )
    classify_color <- function(x) {
      if (is.na(x) || trimws(x) == "") {
        NA_character_
      } else {
        s <- tolower(trimws(x))
        s <- gsub("\\s+", " ", s)
        res <- NA_character_
        # 精确匹配优先
        for (cat in names(color_map)) {
          if (any(s == tolower(color_map[[cat]]))) {
            res <- cat
            break
          }
        }
        # 包含或子串匹配（仅在未匹配时进行）
        if (is.na(res)) {
          for (cat in names(color_map)) {
            pats <- tolower(color_map[[cat]])
            for (p in pats) {
              if (p != "" && grepl(p, s, fixed = TRUE)) {
                res <- cat
                break
              }
            }
            if (!is.na(res)) break
          }
        }
        # 模糊匹配（允许轻微拼写/编号差异）
        if (is.na(res)) {
          # 优先尝试 default 列表
          if (length(agrep(s, tolower(color_map$default), max.distance = 0.1)) > 0) {
            res <- "default"
          } else {
            for (cat in names(color_map)) {
              if (length(agrep(s, tolower(color_map[[cat]]), max.distance = 0.1)) > 0) {
                res <- cat
                break
              }
            }
          }
        }
        if (is.na(res)) stringr::str_to_title(s) else res
      }
    }
    # 向下传递：对 color 列进行向量化分类，生成新列 color_group
    mutate(., color_group =
             vapply(.$color, classify_color, FUN.VALUE = character(1)))
  }

# 保存清洗后的数据
dir.create("/data/processed-v2", showWarnings = FALSE, recursive = TRUE)

# 保存为 RDS 格式（推荐用于 R 分析）
saveRDS(shein_clean, "/data/processed-v2/01-shein_clean.rds")

# 保存为 XLSX 格式
if (!require(openxlsx, quietly = TRUE)) {
  install.packages("openxlsx")
  library(openxlsx)
}
write.xlsx(shein_clean, "/data/processed-v2/01-shein_clean.xlsx")

# 写带 UTF-8 BOM 的 CSV（Excel 在 Windows 上会正确识别 UTF-8）
bom_file <- "/data/processed-v2/01-shein_clean_utf8_bom.csv"
# # 先写入 BOM，再追加 CSV 内容（append=TRUE 时需显式写列名）
# 以二进制写入 BOM（不带换行），避免出现空白第一行
writeBin(charToRaw("\ufeff"), bom_file)
# 追加 CSV 内容并写入列名
readr::write_csv(shein_clean, bom_file,
                 append = TRUE, na = "", col_names = TRUE)



# # 同时保存 parquet 格式
arrow::write_parquet(shein_clean, "/data/processed-v2/01-shein_clean.parquet")


# 验证保存的数据
shein_verify <- readRDS("/data/processed-v2/01-shein_clean.rds")
# 验证使用带 BOM 的 CSV 文件（Excel 友好）
shein_csv_verify <- read_csv(
  bom_file,
  show_col_types = FALSE
)

cat("\n✓ 数据清洗完成！\n")
cat("原始数据行数:", nrow(shein), "\n")
cat("清洗后数据行数:", nrow(shein_clean), "\n")
cat("清洗后数据列数:", ncol(shein_clean), "\n")
cat("\n验证 RDS 文件:", nrow(shein_verify), "行,", ncol(shein_verify), "列\n")
cat("验证 CSV 文件 (UTF-8 BOM):", nrow(shein_csv_verify), "行,"
  , ncol(shein_csv_verify), "列\n"
)
cat("\n保存位置:\n")
cat("  - RDS:  /data/processed-v2/01-shein_clean.rds (推荐)\n")
cat("  - XLSX: /data/processed-v2/01-shein_clean.xlsx\n")
cat("  - CSV (UTF-8 BOM):  /data/processed-v2/01-shein_clean_utf8_bom.csv\n")
cat("  - CSV (no BOM):     /data/processed-v2/01-shein_clean.csv\n")

# 统计 unique 数量
model_num_unique <- length(unique(shein$model_number[!is.na(shein$model_number)]))
product_id_unique <- length(unique(shein$product_id[!is.na(shein$product_id)]))

cat("model_number 唯一值数量:", model_num_unique, "\n")
cat("product_id 唯一值数量:", product_id_unique, "\n")

model_num_unique <- length(unique(shein_clean$model_number[!is.na(shein_clean$model_number)]))
product_id_unique <- length(unique(shein_clean$product_id[!is.na(shein_clean$product_id)]))

cat("cleaned model_number 唯一值数量:", model_num_unique, "\n")
cat("cleaned product_id 唯一值数量:", product_id_unique, "\n")