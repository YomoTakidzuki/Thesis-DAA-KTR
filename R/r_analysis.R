# r_analysis.R
# Descriptive tables, ggplot figures, theme_codes frequencies, and basic statistical tests.
# Required packages: tidyverse, arrow, janitor, lubridate, tidytext, ggplot2, scales, ggthemes (optional)
install.packages(c("tidyverse","arrow","janitor","lubridate","tidytext","ggthemes"))

# Load libraries quietly to keep console output clean
suppressPackageStartupMessages({
  library(tidyverse)  # dplyr/tidyr/readr/ggplot2, etc.
  library(arrow)      # read/write Feather/Parquet
  library(janitor)    # clean_names(), quick tabulations
  library(lubridate)  # date handling
  library(tidytext)   # text tokenization (if needed)
  library(ggthemes)   # optional ggplot themes
  library(scales)     # number/date scales for plots
})

# Output folder (created if missing)
out_dir <- "./out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==== Load data ====
# Expect the cleaned, merged dataset saved by the preprocessing step
feather_path <- file.path(out_dir, "reviews_master_clean.feather")
stopifnot(file.exists(feather_path))           # fail early if the file is missing
df <- read_feather(feather_path) |> clean_names()  # normalize column names for safe use

# Convert and standardize column types
# Ensures consistent data types and controlled factor ordering for plots and tests.
df <- df |>
  mutate(
    post_date_norm   = as_date(post_date_norm),                                # normalize date
    rating_1_5_num   = suppressWarnings(as.integer(rating_1_5)),               # safely cast to integer
    has_text         = factor(has_text),                                       # yes/no indicator
    stance           = factor(stance, levels = c("negative","mixed","neutral","positive")), # ordered stance
    case             = factor(case, levels = c("DAA","KTR")),                  # two study cases
    source_platform  = factor(source_platform, levels = c("GoogleMaps","YandexMaps")) # unified platform naming
  )

# 1) Frequency tables
# Compute basic distributions across cases, platforms, and stance categories.
# Each table includes both absolute counts (n) and relative shares within group.

# stance × case (proportion of each stance within DAA/KTR)
tab_stance_case <- df |>
  count(case, stance, name = "n") |>
  group_by(case) |>
  mutate(share = n / sum(n)) |>
  ungroup()

# stance × platform (proportion of each stance within GoogleMaps/YandexMaps)
tab_stance_platform <- df |>
  count(source_platform, stance, name = "n") |>
  group_by(source_platform) |>
  mutate(share = n / sum(n)) |>
  ungroup()

# has_text × case (share of reviews with/without text for each case)
tab_has_text_case <- df |>
  count(case, has_text, name = "n") |>
  group_by(case) |>
  mutate(share = n / sum(n)) |>
  ungroup()


# has_text × platform (share of reviews with/without text by platform)
tab_has_text_platform <- df |>
  count(source_platform, has_text, name = "n") |>
  group_by(source_platform) |>
  mutate(share = n / sum(n)) |>
  ungroup()

# Average rating by case (DAA vs KTR)
# Includes mean, standard deviation, and number of valid ratings.
tab_rating_case <- df |>
  group_by(case) |>
  summarise(
    mean_rating = mean(rating_1_5_num, na.rm = TRUE),
    sd_rating   = sd(rating_1_5_num, na.rm = TRUE),
    n           = sum(!is.na(rating_1_5_num)),
    .groups = "drop"
  )

# Average rating by platform (GoogleMaps vs YandexMaps)
tab_rating_platform <- df |>
  group_by(source_platform) |>
  summarise(
    mean_rating = mean(rating_1_5_num, na.rm = TRUE),
    sd_rating   = sd(rating_1_5_num, na.rm = TRUE),
    n           = sum(!is.na(rating_1_5_num)),
    .groups = "drop"
  )

# Save all summary tables to the output directory
write_csv(tab_stance_case,       file.path(out_dir, "summary_stance_by_case.csv"))
write_csv(tab_stance_platform,   file.path(out_dir, "summary_stance_by_platform.csv"))
write_csv(tab_has_text_case,     file.path(out_dir, "summary_has_text_by_case.csv"))
write_csv(tab_has_text_platform, file.path(out_dir, "summary_has_text_by_platform.csv"))
write_csv(tab_rating_case,       file.path(out_dir, "summary_rating_by_case.csv"))
write_csv(tab_rating_platform,   file.path(out_dir, "summary_rating_by_platform.csv"))

# 2) Visualization setup (publication-quality minimalist style)
# Define a clean, readable ggplot theme for consistency across all charts.
theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),          # remove minor gridlines for clarity
    plot.title = element_text(face = "bold")     # bold titles for emphasis
  )

# Stance by case (stacked bar chart)
# Compares the stance distribution between DAA and KTR.
p_stance_case <- tab_stance_case |>
  ggplot(aes(x = case, y = n, fill = stance)) +
  geom_col() +
  labs(
    title = "Stance by Case",
    x = "Case",
    y = "Count",
    fill = "Stance"
  ) +
  theme_pub

ggsave(
  file.path(out_dir, "stance_by_case.png"),
  p_stance_case,
  width = 7,
  height = 4.5,
  dpi = 200
)

# Stance by platform (stacked bar chart)
# Shows stance distribution for GoogleMaps and YandexMaps.
p_stance_platform <- tab_stance_platform |>
  ggplot(aes(x = source_platform, y = n, fill = stance)) +
  geom_col() +
  labs(
    title = "Stance by Platform",
    x = "Platform",
    y = "Count",
    fill = "Stance"
  ) +
  theme_pub

ggsave(
  file.path(out_dir, "stance_by_platform.png"),
  p_stance_platform,
  width = 8,
  height = 5,
  dpi = 200
)

# Boxplot of ratings by stance
# Visualizes how review scores (1–5) differ across stance categories.
p_box_ratings <- df |>
  filter(!is.na(rating_1_5_num)) |>
  ggplot(aes(x = stance, y = rating_1_5_num)) +
  geom_boxplot(outlier.alpha = 0.25) +  # faint outliers for cleaner appearance
  scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title = "Rating 1–5 by Stance",
    x = "Stance",
    y = "Rating"
  ) +
  theme_pub

ggsave(
  file.path(out_dir, "ratings_boxplot.png"),
  p_box_ratings,
  width = 8,
  height = 5,
  dpi = 200
)

# Line chart of yearly review dynamics
# Tracks the total number of reviews per year to reveal temporal trends.

df_time <- df |>
  filter(!is.na(post_date_norm)) |>
  mutate(year = year(post_date_norm))

# Aggregate yearly counts
tab_year <- df_time |>
  count(year, name = "n") |>
  arrange(year)

# Save yearly frequency table
write_csv(tab_year, file.path(out_dir, "counts_by_year_R.csv"))

# Plot yearly review counts
p_year <- tab_year |>
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point(size = 1.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Number of Reviews by Year",
    x = "Year",
    y = "Count"
  ) +
  theme_pub

ggsave(
  file.path(out_dir, "reviews_by_year.png"),
  p_year,
  width = 7.5,
  height = 4.5,
  dpi = 200
)

# 3) Wordcloud (R branch, only if you specifically need it in R)
# (We primarily build word clouds in Python; leaving a minimal R stub here.)
# library(ggwordcloud)
# tokens <- df |> filter(has_text == "yes") |> select(case, source_platform, text) |>
#   unnest_tokens(word, text) |> anti_join(stop_words, by = "word") |> count(case, word, sort = TRUE)

# 4) theme_codes to long format, frequencies, and heatmap-ready table
theme_long <- df |>
  select(case, source_platform, theme_codes) |>                          # keep only fields we need
  mutate(theme_codes = if_else(is.na(theme_codes) | theme_codes == "",   # turn empty strings into NA
                               NA_character_, theme_codes)) |>
  separate_rows(theme_codes, sep = ";", convert = FALSE) |>              # split "a;b;c" into rows
  filter(!is.na(theme_codes), theme_codes != "") |>                      # drop empties after split
  mutate(theme_codes = factor(theme_codes))                              # factor for plotting/tables

write_csv(theme_long, file.path(out_dir, "theme_codes_long.csv"))        # save long-format tags

# Top-15 most frequent theme codes
# Summarizes which thematic tags appear most often across all reviews.

top_themes <- theme_long |>
  count(theme_codes, name = "n") |>
  arrange(desc(n)) |>
  slice_head(n = 15)

# Save ranked list as CSV
write_csv(top_themes, file.path(out_dir, "top15_themes.csv"))

# Horizontal bar plot for visual clarity
p_top_themes <- top_themes |>
  mutate(theme_codes = fct_reorder(theme_codes, n)) |>
  ggplot(aes(x = theme_codes, y = n)) +
  geom_col() +
  coord_flip() +  # flip axes so labels are readable
  labs(
    title = "Top-15 Theme Codes",
    x = "Theme",
    y = "Count"
  ) +
  theme_pub

# Export figure
ggsave(
  file.path(out_dir, "top_themes_barplot.png"),
  p_top_themes,
  width = 7,
  height = 5,
  dpi = 200
)

# Heatmap input: counts and within-case shares of theme codes
# - Count how many times each theme code appears per case (DAA vs KTR)
# - For each case, compute the share of each code within that case
#   (so rows sum to 1 per case), which is better for comparing profiles
#   when total review counts differ across cases.

heat_dat <- theme_long |>
  count(case, theme_codes, name = "n") |>
  group_by(case) |>
  mutate(share = n / sum(n)) |>
  ungroup()

# Heatmap of theme-code prevalence by case
# - y: theme codes ordered by overall count (fct_reorder on n)
# - x: case (DAA vs KTR)
# - fill: within-case share (so colors are comparable across cases)

p_heat <- heat_dat |>
  ggplot(aes(x = case, y = fct_reorder(theme_codes, n), fill = share)) +
  geom_tile() +
  scale_fill_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Theme × Case (share within case)", x = "Case", y = "Theme", fill = "Share") +
  theme_pub
ggsave(file.path(out_dir, "themes_heatmap.png"), p_heat, width = 7.5, height = 8.5, dpi = 200)

# ==== 5) Statistics ===========================================================
# 5.1 Chi-square test: does stance distribution differ by platform?
# Build a platform × stance contingency table, then run chisq.test().
# suppressWarnings() hides warnings about low expected counts; results are saved to a .txt file.

xtab_stance_platform <- df |>
  count(source_platform, stance) |>
  pivot_wider(names_from = stance, values_from = n, values_fill = 0) |>
  column_to_rownames("source_platform") |>
  as.matrix()
chisq_result <- suppressWarnings(chisq.test(xtab_stance_platform))
capture.output(chisq_result, file = file.path(out_dir, "chisq_stance_by_platform.txt"))

# 5.2 Differences in rating between cases (t-test and Wilcoxon)
rate_case <- df |>
  filter(!is.na(rating_1_5_num)) |>
  select(case, rating_1_5_num)      # keep only the grouping factor (case) and the numeric rating

tt <- t.test(rating_1_5_num ~ case, data = rate_case)
# Two-sample (independent) t-test comparing mean rating_1_5_num between DAA and KTR.
# Assumes approximate normality of group means; Welch correction is used by default for unequal variances.

wt <- wilcox.test(rating_1_5_num ~ case, data = rate_case, exact = FALSE)
# Mann–Whitney (Wilcoxon rank-sum) test comparing distributions/medians without normality assumption.
# exact = FALSE avoids exact calculation on large samples and suppresses warnings.

capture.output(tt, file = file.path(out_dir, "ttest_rating_by_case.txt"))
capture.output(wt, file = file.path(out_dir, "wilcoxon_rating_by_case.txt"))
# Save both test summaries to plain-text files in ./out for reproducible reporting.

# 5.3 Effect of 'has_text' on stance distribution (Chi-square test)
# Tests whether reviews that include text differ in stance distribution from those without text.

xtab_hastext_stance <- df |>
  count(has_text, stance) |>
  pivot_wider(names_from = stance, values_from = n, values_fill = 0) |>
  column_to_rownames("has_text") |>
  as.matrix()

# Run the chi-square test on the contingency table
chisq_ht <- suppressWarnings(chisq.test(xtab_hastext_stance))

# Save test results to a text file for documentation
capture.output(chisq_ht, file = file.path(out_dir, "chisq_stance_by_has_text.txt"))

# Final confirmation message in the R console
message("R analysis complete. Output files saved in ./out")

# ==== yearly dynamics by platform ====
# Tracks how the number of reviews evolves over time for each platform (GoogleMaps vs YandexMaps).

tab_by_year_platform <- df %>%
  filter(!is.na(post_date_norm)) %>%
  mutate(year = year(post_date_norm)) %>%
  count(source_platform, year, name = "n") %>%
  arrange(source_platform, year)

# Line chart comparing review counts per year across platforms
p_ts_year_platform <- tab_by_year_platform %>%
  ggplot(aes(x = year, y = n, color = source_platform)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(
    title = paste0("Reviews per Year by Platform — ", case_target),
    x = "Year",
    y = "Count",
    color = "Platform"
  ) +
  theme_minimal(base_size = 12)

# Save the platform-by-year time series plot to ./out, naming the file
# with the current case (DAA/KTR). PNG size is 9×5 inches at 200 dpi.
ggsave(
  file.path(out_dir, paste0("R_", case_target, "_ts_reviews_per_year_by_platform.png")),
  p_ts_year_platform, width = 9, height = 5, dpi = 200
)


# ==== Heatmap theme_codes × stance ====
# Load packages needed for reading Feather/Parquet (arrow) and data wrangling/plotting (tidyverse).
library(arrow)
library(tidyverse)  # dplyr, tidyr, readr, ggplot2

# Define the case and file paths
case_target <- "DAA"   # change to "KTR" for the other site
base_out <- "./out"
in_file <- file.path(base_out, case_target, paste0("reviews_", case_target, ".feather"))

# Print the expected file path for verification
print(in_file)            # should show ./out/DAA/reviews_DAA.feather
file.exists(in_file)      # returns TRUE if the file actually exists

# Set the working directory to your thesis root folder
setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

# Load the case-specific Feather dataset
dat <- arrow::read_feather(in_file)

# Ensure proper data types for downstream operations
# (especially important for tidyr::separate_rows() and ggplot)
dat$theme_codes     <- as.character(dat$theme_codes)
dat$stance          <- as.character(dat$stance)
dat$text            <- as.character(dat$text)
dat$source_platform <- as.character(dat$source_platform)

# ==== HEATMAP: theme_codes × stance (Top-30 tags) ====
# Expand multiple theme_codes into separate rows, count how often each appears
# per stance category, and prepare for visualization.
themes_long <- dat %>%
  filter(!is.na(theme_codes), theme_codes != "") %>%
  tidyr::separate_rows(theme_codes, sep = ";") %>%
  count(theme_codes, stance, name = "n")

if (nrow(themes_long) > 0) {
  # Select top 30 most frequent theme codes overall (by total count)
  top_themes <- themes_long %>%
    group_by(theme_codes) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    slice_max(total, n = 30, with_ties = FALSE) %>%
    pull(theme_codes)
  
  # Prepare a wide-format table for plotting (stance columns)
  heatmap_data <- themes_long %>%
    filter(theme_codes %in% top_themes) %>%
    tidyr::pivot_wider(
      names_from = stance,
      values_from = n,
      values_fill = 0
    ) %>%
    arrange(theme_codes)
}
  
  # Convert all stance columns (everything except 'theme_codes') to a numeric matrix
  mat <- as.matrix(heatmap_data[, setdiff(names(heatmap_data), "theme_codes")])
  # Set row names of the matrix to the theme code labels for readable axis ticks
  rownames(mat) <- heatmap_data$theme_codes
  
  # Open a PNG graphics device for saving the heatmap
  png(file.path(out_dir, paste0("R_", case_target, "_heatmap_themes_stance.png")),
      width = 1400, height = 1000)
  heatmap(
          mat,
          Rowv = NA,            # do not cluster rows; keep the given order
          Colv = NA,            # do not cluster columns; keep the given order
          scale = "row",        # z-score each row so colors reflect within-theme variation
          col = heat.colors(256), # base R palette; 256 color levels
          margins = c(10, 12),  # extra space for axis labels (bottom, left)
          main = paste0("Theme Codes × Stance — ", case_target) # plot title
  )
  # Close the graphics device and write the PNG to disk
  dev.off()
} else {
  # Informative message if there is nothing to plot
  message("No data for heatmap (themes_long пуст).")
}


# ==== Sample review excerpts: up to 3 per stance category ====
set.seed(123)

examples <- dat %>%
  dplyr::filter(!is.na(text), text != "") %>%          # keep rows that actually have text
  dplyr::group_by(stance) %>%                           # group by stance category
  dplyr::group_modify(~ dplyr::slice_sample(            # sample up to 3 rows within each stance
    .x, n = min(3, nrow(.x))
  )) %>%
  dplyr::ungroup() %>%
  dplyr::select(case, source_platform, stance,          # keep a compact, readable set of columns
                rating_1_5, text)

if (nrow(examples) > 0) {
  out_csv <- file.path(out_dir, paste0("R_", case_target, "_examples_reviews.csv"))
  readr::write_csv(examples, out_csv)                  # write sampled quotes to CSV
  message("Quotes saved: ", out_csv)
} else {
  message("No texts found; CSV with quotes was not created.")
}


# ==== Analysis for KTR ====
case_target <- "KTR"   # switch to the KTR case
base_out <- "./out"
in_file <- file.path(base_out, case_target, paste0("reviews_", case_target, ".feather"))
out_dir <- file.path(base_out, case_target)

print(in_file)                 # should print "./out/KTR/reviews_KTR.feather"
file.exists(in_file)           # quick check: TRUE if the file actually exists

# Load the Feather dataset for KTR
dat <- arrow::read_feather(in_file)

# Ensure correct data types for analysis
# Convert potential factor or logical columns to character
# (prevents issues when splitting strings or filtering text)
dat$theme_codes     <- as.character(dat$theme_codes)
dat$stance          <- as.character(dat$stance)
dat$text            <- as.character(dat$text)
dat$source_platform <- as.character(dat$source_platform)

# ==== Heatmap: theme_codes × stance (Top 30 tags) ====
# Expand multiple theme codes per review, count their occurrences by stance,
# and prepare the data for visualization.

themes_long <- dat %>%
  filter(!is.na(theme_codes), theme_codes != "") %>%   # drop empty theme entries
  tidyr::separate_rows(theme_codes, sep = ";") %>%     # split multiple codes into separate rows
  count(theme_codes, stance, name = "n")               # count how often each tag appears per stance

if (nrow(themes_long) > 0) {
  # Identify top 30 most frequent tags across all stance categories
  top_themes <- themes_long %>%
    group_by(theme_codes) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    slice_max(total, n = 30, with_ties = FALSE) %>%
    pull(theme_codes)
  
  # Create a wide-format table (stance columns) for plotting the heatmap
  heatmap_data <- themes_long %>%
    filter(theme_codes %in% top_themes) %>%
    tidyr::pivot_wider(names_from = stance, values_from = n, values_fill = 0) %>%
    arrange(theme_codes)
  
  # Convert the wide table to a numeric matrix (rows = themes, cols = stance categories)
  mat <- as.matrix(heatmap_data[, setdiff(names(heatmap_data), "theme_codes")])
  rownames(mat) <- heatmap_data$theme_codes
  
  # Save the base R heatmap to a PNG file
  png(file.path(out_dir, paste0("R_", case_target, "_heatmap_themes_stance.png")),
      width = 1400, height = 1000)
  heatmap(
    mat,
    Rowv = NA,            # do not reorder rows (keep declared order)
    Colv = NA,            # do not reorder columns
    scale = "row",        # z-score per row to compare relative intensities
    col = heat.colors(256),
    margins = c(10, 12),
    main = paste0("Theme Codes × Stance — ", case_target)
  )
  dev.off()
} else {
  # Informative message if no themes available after preprocessing
  message("No data for heatmap (themes_long is empty).")
}


# ==== Sample review excerpts: up to 3 per stance category ====
set.seed(123)  # ensures reproducibility of random sampling

examples <- dat %>%
  dplyr::filter(!is.na(text), text != "") %>%              # keep reviews with non-empty text
  dplyr::group_by(stance) %>%                              # group by stance (positive, negative, etc.)
  dplyr::group_modify(~ dplyr::slice_sample(.x,            # randomly sample up to 3 per stance
                                            n = min(3, nrow(.x)))) %>%
  dplyr::ungroup() %>%
  dplyr::select(case, source_platform, stance, rating_1_5, text)  # keep only relevant fields

if (nrow(examples) > 0) {
  out_csv <- file.path(out_dir, paste0("R_", case_target, "_examples_reviews.csv"))
  readr::write_csv(examples, out_csv)                     # export selected examples
  message("Quotes saved: ", out_csv)
} else {
  message("No texts found; CSV with quotes was not created.")
}
