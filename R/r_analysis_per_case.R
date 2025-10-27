# ——— per-case R analysis & real wordclouds ———
suppressPackageStartupMessages({
  library(tidyverse);  # dplyr/ggplot2/readr/tidyr for data wrangling and plotting
  library(arrow); # fast Feather/Parquet I/O
  library(janitor); # clean_names() and quick tabulations
  library(lubridate); # convenient date handling
  library(tidytext); # text tokenization and tidy NLP helpers
  library(ggwordcloud);# word cloud plotting with ggplot2 grammar
  library(widyr)  # pairwise correlations/similarities on tidy data
})

case_target <- Sys.getenv("CASE", "DAA")  # read target case from env; default to "DAA"
in_dir  <- file.path("out", case_target) # input/output directory for the selected case
out_dir <- in_dir    # write results next to inputs (same folder)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)  # create if missing

in_file <- file.path(in_dir, paste0("reviews_", case_target, ".feather"))  # e.g., out/DAA/reviews_DAA.feather
df <- read_feather(in_file) %>% clean_names()  # load per-case dataset; standardize column names (snake_case)

# Type harmonization for consistency across all analyses
# Ensures that numeric, categorical, and date fields are correctly formatted.
df <- df %>%
  mutate(
    post_date_norm  = as_date(post_date_norm),   # convert to Date
    rating_1_5_num  = suppressWarnings(as.integer(rating_1_5)),  # safely cast ratings to integer
    has_text        = factor(has_text),    # yes/no as factor
    stance          = factor(stance),      # categorical stance (pos/neg/etc.)
    source_platform = factor(source_platform)       # categorical platform
  )

# ==== Summary tables: frequencies and aggregates ====

# 1. stance × platform — absolute and relative frequencies
df %>%
  count(source_platform, stance, name = "n") %>%
  group_by(source_platform) %>%
  mutate(p = n / sum(n)) %>%    # relative share within platform
  ungroup() %>%
  arrange(source_platform, desc(n)) %>%
  write_csv(file.path(out_dir, paste0("R_", case_target, "_stance_by_platform.csv")))

# 2. has_text × platform — proportion of text vs no-text reviews by platform
df %>%
  count(has_text, source_platform, name = "n") %>%
  group_by(source_platform) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  write_csv(file.path(out_dir, paste0("R_", case_target, "_hastext_by_platform.csv")))

# 3. mean rating × platform — average rating and sample size
df %>%
  group_by(source_platform) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating_1_5_num, na.rm = TRUE)
  ) %>%
  arrange(desc(n)) %>%
  write_csv(file.path(out_dir, paste0("R_", case_target, "_mean_rating_by_platform.csv")))

# ==== Plots (ggplot) ====
# Stance by platform (stacked bar chart)
# Visualizes how stance categories are distributed across review platforms for the given case.
p1 <- df %>%
  count(source_platform, stance, name = "n") %>%
  ggplot(aes(x = source_platform, y = n, fill = stance)) +
  geom_col() +
  labs(
    title = paste("Stance by Platform —", case_target),
    x = "Platform",
    y = "Count",
    fill = "Stance"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, paste0("R_", case_target, "_stance_by_platform.png")),
  p1,
  width = 8,
  height = 5,
  dpi = 200
)

# Boxplot: rating distribution by stance
# Shows how average ratings vary across stance categories.
p2 <- df %>%
  filter(!is.na(rating_1_5_num)) %>%
  ggplot(aes(x = stance, y = rating_1_5_num)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(
    title = paste("Rating by Stance —", case_target),
    x = "Stance",
    y = "Rating (1–5)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, paste0("R_", case_target, "_box_rating_by_stance.png")),
  p2,
  width = 7,
  height = 5,
  dpi = 200
)


# Line chart: yearly review counts
# Depicts how the number of reviews changes per year for the selected case.
p_year <- df %>%
  filter(!is.na(post_date_norm)) %>%
  mutate(year = year(post_date_norm)) %>%
  count(year, name = "n") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point(size = 1.1) +
  labs(
    title = paste("Reviews per Year —", case_target),
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, paste0("R_", case_target, "_reviews_per_year.png")),
  p_year,
  width = 7.5,
  height = 4.5,
  dpi = 200
)

# ==== theme_codes: long + top lists + heatmap base (by platform within case) ====
themes_long <- df %>%
  mutate(theme_codes = if_else(is.na(theme_codes), "", theme_codes)) %>%  # replace NA with empty strings so splitting works
  separate_longer_delim(theme_codes, delim = ";") %>%  # split semicolon-separated tags into long format (one tag per row)
  mutate(theme_codes = str_trim(theme_codes)) %>%   # trim accidental whitespace around tags
  filter(theme_codes != "")   # drop empty rows after splitting
write_csv(
  themes_long,
  file.path(out_dir, paste0("R_", case_target, "_theme_codes_long.csv"))
)

# Compute top-15 most frequent theme codes
top_themes <- themes_long %>% 
  count(theme_codes, sort = TRUE) %>% 
  slice_head(n = 15)

# Bar chart of top-15 theme codes
p_top <- top_themes %>%
  ggplot(aes(x = reorder(theme_codes, n), y = n)) +
  geom_col() +
  coord_flip() +  # flip for readable long labels
  labs(
    title = paste("Top-15 themes —", case_target),
    x = "Theme",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, paste0("R_", case_target, "_top_themes.png")),
  p_top,
  width = 7,
  height = 5,
  dpi = 200
)

# ==== wordcloud ====
# RU+EN stopwords (simple combined list).
# We merge a small custom Russian stoplist with tidytext's built-in English stopwords.
stop_ru <- c(
  "и","в","во","не","что","он","на","я","с","со","как","а","то","все",
  "она","так","его","но","да","ты","к","у","же","вы","за","бы","по",
  "ее","мне","если","или","ни","мы","такой","это","этот","эта","этом",
  "из","от","до","для","без","над","о","об","при","под","через","после",
  "где","когда","здесь","там","тут","ну","ли","же","быть","есть","нет"
)
stop_en <- tidytext::stop_words$word  # English stopwords from tidytext
stops  <- unique(c(stop_ru, stop_en)) # combined unique RU+EN stoplist

# Keep only rows that actually contain review text (has_text == "yes" and non-NA).
# Also retain stance and source_platform for possible faceting or grouped clouds.
txt <- df %>%
  dplyr::filter(has_text == "yes", !is.na(text)) %>% # keep only reviews that contain text
  dplyr::select(text, stance, source_platform)  # keep relevant columns for context

# Tokenize text into individual words and clean
tokens <- txt %>%
  unnest_tokens(word, text, token = "words") %>%   # break each review into words
  mutate(word = str_to_lower(word)) %>%  # lowercase for consistency
  filter(str_detect(word, "^[\\p{L}\\p{N}]{3,}$")) %>%  # keep only words ≥3 chars (letters/numbers)
  filter(!word %in% stops)   # exclude RU+EN stopwords

# Take top 250 most frequent tokens for visualization
wc <- tokens %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 250)

# Build and save the word cloud
# Word sizes reflect frequency of occurrence in the reviews.
p_wc <- ggplot(wc, aes(label = word, size = n)) +
  ggwordcloud::geom_text_wordcloud_area() +  # generate layout automatically (no overlap)
  scale_size_area(max_size = 16) +   # control overall word scaling
  labs(title = paste("Wordcloud —", case_target)) +
  theme_minimal(base_size = 12)

# Export to PNG in the same per-case output folder
ggsave(
  file.path(out_dir, paste0("R_", case_target, "_wordcloud.png")),
  p_wc,
  width = 8,
  height = 6,
  dpi = 200
)

# Platform-specific word clouds within this case
# Generates separate word clouds for GoogleMaps and YandexMaps if data is available.

for (plat in levels(df$source_platform)) {
  wc_p <- tokens %>%
    filter(source_platform == plat) %>%
    count(word, sort = TRUE) %>%
    slice_max(n, n = 200)

  if (nrow(wc_p) > 0) {
    p <- ggplot(wc_p, aes(label = word, size = n)) +
      ggwordcloud::geom_text_wordcloud_area() +
      scale_size_area(max_size = 14) +
      labs(
        title = paste("Wordcloud —", case_target, "—", plat),
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 12)

    ggsave(
      file.path(out_dir, paste0("R_", case_target, "_wordcloud_", plat, ".png")),
      p,
      width = 8,
      height = 6,
      dpi = 200
    )
  }
}

# — Simple statistical tests —
# 1) Chi-square test: checks whether the distribution of stance differs by source_platform.
# We first build a contingency table source_platform × stance, then run chisq.test()
# only if we have at least 2 rows (i.e., at least two platforms present).
stance_xtab <- df %>%
  count(source_platform, stance) %>%
  pivot_wider(names_from = stance, values_from = n, values_fill = 0) %>%
  column_to_rownames("source_platform") %>%
  as.matrix()

if (nrow(stance_xtab) >= 2) {
  chisq_res <- suppressWarnings(chisq.test(stance_xtab))
  capture.output(
    chisq_res,
    file = file.path(out_dir, paste0("R_", case_target, "_chisq_stance_vs_platform.txt"))
  )
}

# 2) Two-sample t-test: compares average rating_1_5_num across platforms.
# We keep only rows with non-missing ratings and run t.test() if we
# actually have ≥2 distinct platforms in the filtered data.
tt <- df %>%
  filter(!is.na(rating_1_5_num)) %>%
  select(source_platform, rating_1_5_num)

if (dplyr::n_distinct(tt$source_platform) >= 2) {
  t_res <- try(t.test(rating_1_5_num ~ source_platform, data = tt), silent = TRUE)
  if (!inherits(t_res, "try-error")) {
    capture.output(
      t_res,
      file = file.path(out_dir, paste0("R_", case_target, "_ttest_rating_vs_platform.txt"))
    )
  }
}

# Final status message with the output folder for convenience.
message("Done. Results are here:", out_dir)
