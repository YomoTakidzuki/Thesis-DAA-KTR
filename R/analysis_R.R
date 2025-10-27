# ------------------------------------------------------------
# analysis_R.R — Basic summaries and plots for reviews_master_clean.feather
# Required packages: tidyverse, arrow, janitor, lubridate
# ------------------------------------------------------------

# Set the working directory (adjust to your own path if needed)
setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

# Install required packages (uncomment if not installed yet)
install.packages("tidyverse")
install.packages("arrow")
install.packages("janitor")
install.packages("lubridate")

# Load core libraries
library(tidyverse)
library(arrow)
library(janitor)
library(lubridate)

# ==== Paths ====
# Create output directory if it doesn’t exist
out_dir <- "./out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==== Load data ====
# Load cleaned review data (produced by process_reviews.py)
# clean_names() ensures consistent column naming (snake_case)
df <- read_feather(file.path(out_dir, "reviews_master_clean.feather")) %>%
  clean_names()

# Expected columns:
# case, source_platform, rating_1_5, text, stance, theme_codes,
# pulled_by, post_date, has_text, post_date_norm

# Convert key variables to proper types
df <- df %>%
  mutate(
    post_date_norm = as_date(post_date_norm),   # convert to Date
    rating_1_5_num = suppressWarnings(as.integer(rating_1_5)), # safely coerce to integer
    has_text = as.factor(has_text),    # categorical yes/no
    stance = as.factor(stance),   # stance categories
    case = as.factor(case),  # DAA / KTR
    source_platform = as.factor(source_platform) # GoogleMaps / YandexMaps
  )

# ==== Summaries =====

# 1) Count of reviews by case × source_platform
# Shows how reviews are distributed across both objects (DAA, KTR)
# and data sources (GoogleMaps, YandexMaps)
tab_case_platform <- df %>%
  count(case, source_platform, name = "n") %>%
  arrange(case, source_platform)

write_csv(tab_case_platform, file.path(out_dir, "R_counts_by_case_platform.csv"))

# 2) Overall stance distribution
# Total number of reviews per stance category (positive / neutral / negative / mixed)
tab_stance_overall <- df %>%
  count(stance, name = "n") %>%
  arrange(desc(n))

write_csv(tab_stance_overall, file.path(out_dir, "R_stance_overall.csv"))

# 3) Stance distribution by platform
# Calculates both absolute and relative (share) values of stance for each platform.
tab_stance_by_platform <- df %>%
  count(source_platform, stance, name = "n") %>%
  group_by(source_platform) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

write_csv(tab_stance_by_platform, file.path(out_dir, "R_stance_by_platform.csv"))

# 4) Stance distribution by case
# Same logic as above but grouped by DAA and KTR instead of platform.
tab_stance_by_case <- df %>%
  count(case, stance, name = "n") %>%
  group_by(case) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

write_csv(tab_stance_by_case, file.path(out_dir, "R_stance_by_case.csv"))

# 5) Temporal summaries (by year and by year-month)
# Extracts temporal trends in review activity.
df_time <- df %>%
  filter(!is.na(post_date_norm)) %>%
  mutate(
    year = year(post_date_norm),    # extract year
    ym = floor_date(post_date_norm, unit = "month") # extract year-month
  )

# Count of reviews by year
tab_by_year <- df_time %>%
  count(year, name = "n") %>%
  arrange(year)

# Count of reviews by year-month
tab_by_ym <- df_time %>%
  count(ym, name = "n") %>%
  arrange(ym)

# Export CSV summaries for transparency and reuse
write_csv(tab_by_year, file.path(out_dir, "R_counts_by_year.csv"))
write_csv(tab_by_ym,   file.path(out_dir, "R_counts_by_year_month.csv"))

# ==== Plots (ggplot2, ready for publication-quality output) =====

# Histogram of ratings (if the column is available)
# Shows how many reviews fall into each 1–5 rating category.
p_rating <- df %>%
  filter(!is.na(rating_1_5_num)) %>%
  ggplot(aes(x = rating_1_5_num)) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Rating 1–5 (Histogram)",
    x = "Rating",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

# Save the plot to the 'out' directory
ggsave(
  file.path(out_dir, "R_hist_rating_1_5.png"),
  p_rating,
  width = 7,
  height = 4.5,
  dpi = 200
)

# Stance by platform (stacked bar chart)
# Visualizes how different stance categories are distributed across platforms.
p_stance_platform <- tab_stance_by_platform %>%
  ggplot(aes(x = source_platform, y = n, fill = stance)) +
  geom_col(position = "stack") +
  labs(
    title = "Stance by Source Platform",
    x = "Platform",
    y = "Count",
    fill = "Stance"
  ) +
  theme_minimal(base_size = 12)

# Export the plot as PNG
ggsave(
  file.path(out_dir, "R_bar_stance_by_platform.png"),
  p_stance_platform,
  width = 8,
  height = 5,
  dpi = 200
)

# Stance by case (stacked bar chart)
# Compares stance categories between the two case study sites: DAA and KTR.
p_stance_case <- tab_stance_by_case %>%
  ggplot(aes(x = case, y = n, fill = stance)) +
  geom_col(position = "stack") +
  labs(
    title = "Stance by Case",
    x = "Case",
    y = "Count",
    fill = "Stance"
  ) +
  theme_minimal(base_size = 12)

# Save the bar chart to output folder
ggsave(
  file.path(out_dir, "R_bar_stance_by_case.png"),
  p_stance_case,
  width = 7,
  height = 4.5,
  dpi = 200
)

# Time series by month (all platforms combined)
# Displays temporal dynamics of review activity over time.
p_ts_all <- tab_by_ym %>%
  ggplot(aes(x = ym, y = n)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(
    title = "Reviews per Month",
    x = "Year-Month",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

# Save time series plot to output directory
ggsave(
  file.path(out_dir, "R_ts_reviews_per_month.png"),
  p_ts_all,
  width = 8,
  height = 4.5,
  dpi = 200
)

# Time series by month and platform
# Breaks down review activity over time for each source platform.
tab_by_ym_platform <- df_time %>%
  count(source_platform, ym, name = "n") %>%
  arrange(source_platform, ym)

# Time series by month, split by platform
# Shows how review frequency changes over time for Google Maps and Yandex Maps separately.
p_ts_platform <- tab_by_ym_platform %>%
  ggplot(aes(x = ym, y = n, color = source_platform)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(
    title = "Reviews per Month by Platform",
    x = "Year-Month",
    y = "Count",
    color = "Platform"
  ) +
  theme_minimal(base_size = 12)

# Save line chart by platform
ggsave(
  file.path(out_dir, "R_ts_reviews_per_month_by_platform.png"),
  p_ts_platform,
  width = 9,
  height = 5,
  dpi = 200
)

# Faceted histogram: rating distribution by platform
# Visualizes rating patterns separately for each platform (GoogleMaps, YandexMaps).
p_rating_facet <- df %>%
  filter(!is.na(rating_1_5_num)) %>%
  ggplot(aes(x = rating_1_5_num)) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Rating 1–5 by Platform",
    x = "Rating",
    y = "Count"
  ) +
  facet_wrap(~ source_platform, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 12)

# Save the faceted histogram
ggsave(
  file.path(out_dir, "R_hist_rating_by_platform.png"),
  p_rating_facet,
  width = 9,
  height = 6,
  dpi = 200
)

# ==== Draft for difference tests (placeholder) ====

# Example: compare the distribution of 'stance' across platforms (Chi-square test).
# What this does:
# 1) Tallies counts for each combination of source_platform × stance.
# 2) Pivots to a wide contingency table with one row per platform and one column per stance.
# 3) Converts to a matrix suitable for chisq.test().
# Notes:
# - Ensure sample sizes are sufficient; expected counts per cell should generally be ≥ 5.
# - Decide in advance whether to collapse rare stance categories (e.g., merge 'mixed' with 'neutral')
#   if expected counts are too small.
# - After creating 'stance_xtab', we can run:
#       chisq.test(stance_xtab)
#   and inspect residuals with:
#       chisq.test(stance_xtab)$residuals
# - If assumptions are violated, consider Fisher’s exact test (for 2×k with small counts)
#   or simulate p-values in chisq.test(simulate.p.value = TRUE, B = 10000).

stance_xtab <- df %>%
  count(source_platform, stance) %>%  # 1) contingency tallies
  pivot_wider(names_from = stance,   # 2) wide table: columns = stance
              values_from = n,
              values_fill = 0) %>%
  column_to_rownames("source_platform") %>%  # rows = platforms
  as.matrix()     # 3) matrix for chisq.test()

# chi_sq_result <- chisq.test(stance_xtab) # <- Uncomment after deciding on comparison groups

# Example: compare mean rating between platforms (optionally filtered by has_text, etc.)
# This block demonstrates exploratory statistical testing, but is not run automatically.
# Uncomment for manual analysis.

# df %>%
#   filter(!is.na(rating_1_5_num)) %>%
#   group_by(source_platform) %>%
#   summarise(
#     mean_rating = mean(rating_1_5_num),
#     n = n()
#   )

# t.test(
#   rating_1_5_num ~ source_platform,
#   data = df %>% filter(!is.na(rating_1_5_num))
# )

# ==== End ====
message("-analysis: summaries and plots saved in ./out")
