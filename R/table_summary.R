#!/usr/bin/env Rscript
install.packages("dplyr")
install.packages("arrow")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("gridExtra")

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)   # для сохранения таблицы как картинки
})

in_file <- "./out/reviews_master_clean.feather"
out_png <- "./out/summary_table_R.png"

# Загружаем
df <- read_feather(in_file)

# Таблица
tab <- df %>%
  count(case, source_platform) %>%
  tidyr::pivot_wider(names_from = source_platform, values_from = n, values_fill = 0) %>%
  mutate(Всего = GoogleMaps + YandexMaps)

tab_final <- bind_rows(
  tab,
  tibble(case = "Total",
         GoogleMaps = sum(tab$GoogleMaps, na.rm = TRUE),
         YandexMaps = sum(tab$YandexMaps, na.rm = TRUE),
         Total = sum(tab$Всего, na.rm = TRUE))
)

# Графический вывод
table_plot <- gridExtra::tableGrob(tab_final, rows = NULL)

# Сохраняем PNG
png(out_png, width = 800, height = 400)
grid::grid.draw(table_plot)
dev.off()

message("Таблица сохранена как ", out_png)


