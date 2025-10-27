#!/usr/bin/env Rscript
# Minimal, self-contained script that builds a small summary table from
# the cleaned reviews file and saves it as a PNG image.
# Notes:
# - It reads ./out/reviews_master_clean.feather (produced by your preprocessing).
# - It counts rows by case × source_platform.
# - It pivots to wide form (columns for GoogleMaps and YandexMaps), adds totals,
#   then renders the table to ./out/summary_table_R.png.
# - This script purposefully avoids fancy theming so it runs anywhere.

install.packages("dplyr")
install.packages("arrow")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("gridExtra")

suppressPackageStartupMessages({
  library(dplyr)  # data wrangling (count, mutate, bind_rows, etc.)
  library(arrow) # read_feather()
  library(tidyr)  # pivot_wider()
  library(ggplot2)  # not used directly for plotting here, but often handy
  library(gridExtra)  # tableGrob() to render a data.frame as a table graphic
})

in_file <- "./out/reviews_master_clean.feather" # input Feather file (clean layer)
out_png <- "./out/summary_table_R.png" # output table image path

# Load the cleaned dataset (must exist beforehand)
df <- read_feather(in_file)

# Build a counts table: number of rows for each combination of case × source_platform
tab <- df %>%
  count(case, source_platform) %>%
  tidyr::pivot_wider(
    names_from = source_platform,  # columns become platforms (GoogleMaps / YandexMaps)
    values_from = n,  # cell values are counts
    values_fill = 0  # fill missing combos with 0
  ) %>%
  mutate(Всего = GoogleMaps + YandexMaps) # add a Russian-labeled total column (“Всего”)

# Add a grand total row at the bottom.
# NOTE: This introduces a second total column named "Total" in English, while the row above
# computed a Russian-labeled total (“Всего”). That means the resulting table will contain
# both columns (“Всего” and “Total”). This is fine if you want to show both; otherwise,
# you can harmonize the naming later.
tab_final <- bind_rows(
  tab,
  tibble(
    case = "Total",
    GoogleMaps = sum(tab$GoogleMaps, na.rm = TRUE),
    YandexMaps = sum(tab$YandexMaps, na.rm = TRUE),
    Total = sum(tab$Всего, na.rm = TRUE)
  )
)

# Turn the data frame into a graphical table object (no row labels)
table_plot <- gridExtra::tableGrob(tab_final, rows = NULL)

# Save as a PNG image so it can be pasted into the research
png(out_png, width = 800, height = 400)
grid::grid.draw(table_plot)
dev.off()

message("Table saved to: ", out_png)
