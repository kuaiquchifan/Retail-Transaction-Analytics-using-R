# 初始化renv环境
renv::init()

install.packages("jsonlite")
# 安装所有依赖
packages <- c(
  "tidyverse",
  "lubridate",
  "ggplot2",
  "gridExtra",
  "scales"
)

renv::install(packages)

# 保存快照
renv::snapshot()

cat("✓ 项目环境设置完成！\n")
cat("使用 renv::restore() 可恢复此环境\n")