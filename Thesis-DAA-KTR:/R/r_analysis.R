# r_analysis.R
# Описательные таблички, ggplot-графики, частоты theme_codes и статистические тесты.
# Требуемые пакеты: tidyverse, arrow, janitor, lubridate, tidytext, ggplot2, scales, ggthemes (по желанию)
install.packages(c("tidyverse","arrow","janitor","lubridate","tidytext","ggthemes"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(janitor)
  library(lubridate)
  library(tidytext)
  library(ggthemes)
  library(scales)
})

out_dir <- "./out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==== Загрузка ====
feather_path <- file.path(out_dir, "reviews_master_clean.feather")
stopifnot(file.exists(feather_path))
df <- read_feather(feather_path) |> clean_names()

# Приведение типов
df <- df |>
  mutate(
    post_date_norm = as_date(post_date_norm),
    rating_1_5_num = suppressWarnings(as.integer(rating_1_5)),
    has_text = factor(has_text),
    stance   = factor(stance, levels = c("negative","mixed","neutral","positive")),
    case     = factor(case, levels = c("DAA","KTR")),
    source_platform = factor(source_platform, levels = c("GoogleMaps","YandexMaps"))
  )

# ==== 1) Частотные таблицы ====================================================
# stance × case × platform с долями
tab_stance_case <- df |>
  count(case, stance, name = "n") |>
  group_by(case) |>
  mutate(share = n/sum(n)) |>
  ungroup()

tab_stance_platform <- df |>
  count(source_platform, stance, name = "n") |>
  group_by(source_platform) |>
  mutate(share = n/sum(n)) |>
  ungroup()

tab_has_text_case <- df |>
  count(case, has_text, name = "n") |>
  group_by(case) |>
  mutate(share = n/sum(n)) |>
  ungroup()

tab_has_text_platform <- df |>
  count(source_platform, has_text, name = "n") |>
  group_by(source_platform) |>
  mutate(share = n/sum(n)) |>
  ungroup()

tab_rating_case <- df |>
  group_by(case) |>
  summarise(mean_rating = mean(rating_1_5_num, na.rm = TRUE),
            sd_rating   = sd(rating_1_5_num, na.rm = TRUE),
            n = sum(!is.na(rating_1_5_num)), .groups = "drop")

tab_rating_platform <- df |>
  group_by(source_platform) |>
  summarise(mean_rating = mean(rating_1_5_num, na.rm = TRUE),
            sd_rating   = sd(rating_1_5_num, na.rm = TRUE),
            n = sum(!is.na(rating_1_5_num)), .groups = "drop")

# сохраняем
write_csv(tab_stance_case,      file.path(out_dir, "summary_stance_by_case.csv"))
write_csv(tab_stance_platform,  file.path(out_dir, "summary_stance_by_platform.csv"))
write_csv(tab_has_text_case,    file.path(out_dir, "summary_has_text_by_case.csv"))
write_csv(tab_has_text_platform,file.path(out_dir, "summary_has_text_by_platform.csv"))
write_csv(tab_rating_case,      file.path(out_dir, "summary_rating_by_case.csv"))
write_csv(tab_rating_platform,  file.path(out_dir, "summary_rating_by_platform.csv"))

# ==== 2) Графики (публикационного качества, минималистично) ===================
theme_pub <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# Stance по кейсам (stacked)
p_stance_case <- tab_stance_case |>
  ggplot(aes(x = case, y = n, fill = stance)) +
  geom_col() +
  labs(title = "Stance by Case", x = "Case", y = "Count", fill = "Stance") +
  theme_pub
ggsave(file.path(out_dir, "stance_by_case.png"), p_stance_case, width = 7, height = 4.5, dpi = 200)

# Stance по платформам (stacked)
p_stance_platform <- tab_stance_platform |>
  ggplot(aes(x = source_platform, y = n, fill = stance)) +
  geom_col() +
  labs(title = "Stance by Platform", x = "Platform", y = "Count", fill = "Stance") +
  theme_pub
ggsave(file.path(out_dir, "stance_by_platform.png"), p_stance_platform, width = 8, height = 5, dpi = 200)

# Boxplot рейтингов по stance
p_box_ratings <- df |>
  filter(!is.na(rating_1_5_num)) |>
  ggplot(aes(x = stance, y = rating_1_5_num)) +
  geom_boxplot(outlier.alpha = 0.25) +
  scale_y_continuous(breaks = 1:5, limits = c(1,5)) +
  labs(title = "Rating 1–5 by Stance", x = "Stance", y = "Rating") +
  theme_pub
ggsave(file.path(out_dir, "ratings_boxplot.png"), p_box_ratings, width = 8, height = 5, dpi = 200)

# Линейный график динамики по годам
df_time <- df |>
  filter(!is.na(post_date_norm)) |>
  mutate(year = year(post_date_norm))
tab_year <- df_time |>
  count(year, name = "n") |>
  arrange(year)
write_csv(tab_year, file.path(out_dir, "counts_by_year_R.csv"))

p_year <- tab_year |>
  ggplot(aes(x = year, y = n)) +
  geom_line() + geom_point(size = 1.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Number of Reviews by Year", x = "Year", y = "Count") +
  theme_pub
ggsave(file.path(out_dir, "reviews_by_year.png"), p_year, width = 7.5, height = 4.5, dpi = 200)

# ==== 3) Wordcloud (R-ветка, если понадобится именно в R) =====================
# (вордклауды основно делаем в Python; здесь оставляю заготовку)
# library(ggwordcloud)
# tokens <- df |> filter(has_text == "yes") |> select(case, source_platform, text) |>
#   unnest_tokens(word, text) |> anti_join(stop_words, by = "word") |> count(case, word, sort = TRUE)

# ==== 4) theme_codes в long-format, частоты и heatmap =========================
theme_long <- df |>
  select(case, source_platform, theme_codes) |>
  mutate(theme_codes = if_else(is.na(theme_codes) | theme_codes == "", NA_character_, theme_codes)) |>
  separate_rows(theme_codes, sep = ";", convert = FALSE) |>
  filter(!is.na(theme_codes), theme_codes != "") |>
  mutate(theme_codes = factor(theme_codes))

write_csv(theme_long, file.path(out_dir, "theme_codes_long.csv"))

# Топ-15 тегов
top_themes <- theme_long |>
  count(theme_codes, name = "n") |>
  arrange(desc(n)) |>
  slice_head(n = 15)
write_csv(top_themes, file.path(out_dir, "top15_themes.csv"))

p_top_themes <- top_themes |>
  mutate(theme_codes = fct_reorder(theme_codes, n)) |>
  ggplot(aes(x = theme_codes, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top-15 Theme Codes", x = "Theme", y = "Count") +
  theme_pub
ggsave(file.path(out_dir, "top_themes_barplot.png"), p_top_themes, width = 7, height = 5, dpi = 200)

# Heatmap: теги × case
heat_dat <- theme_long |>
  count(case, theme_codes, name = "n") |>
  group_by(case) |>
  mutate(share = n / sum(n)) |>
  ungroup()

p_heat <- heat_dat |>
  ggplot(aes(x = case, y = fct_reorder(theme_codes, n), fill = share)) +
  geom_tile() +
  scale_fill_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Theme × Case (share within case)", x = "Case", y = "Theme", fill = "Share") +
  theme_pub
ggsave(file.path(out_dir, "themes_heatmap.png"), p_heat, width = 7.5, height = 8.5, dpi = 200)

# ==== 5) Статистика ===========================================================
# 5.1 Хи-квадрат: распределение stance между платформами
xtab_stance_platform <- df |>
  count(source_platform, stance) |>
  pivot_wider(names_from = stance, values_from = n, values_fill = 0) |>
  column_to_rownames("source_platform") |>
  as.matrix()
chisq_result <- suppressWarnings(chisq.test(xtab_stance_platform))
capture.output(chisq_result, file = file.path(out_dir, "chisq_stance_by_platform.txt"))

# 5.2 Различия rating между кейсами (t-test и Wilcoxon)
rate_case <- df |>
  filter(!is.na(rating_1_5_num)) |>
  select(case, rating_1_5_num)

tt <- t.test(rating_1_5_num ~ case, data = rate_case)
wt <- wilcox.test(rating_1_5_num ~ case, data = rate_case, exact = FALSE)

capture.output(tt, file = file.path(out_dir, "ttest_rating_by_case.txt"))
capture.output(wt, file = file.path(out_dir, "wilcoxon_rating_by_case.txt"))

# 5.3 Влияние has_text на распределение stance (хи-квадрат)
xtab_hastext_stance <- df |>
  count(has_text, stance) |>
  pivot_wider(names_from = stance, values_from = n, values_fill = 0) |>
  column_to_rownames("has_text") |>
  as.matrix()
chisq_ht <- suppressWarnings(chisq.test(xtab_hastext_stance))
capture.output(chisq_ht, file = file.path(out_dir, "chisq_stance_by_has_text.txt"))

message("R-анализ готов. Файлы в ./out")

# ==== NEW: динамика по платформам ====
tab_by_year_platform <- df %>%
  filter(!is.na(post_date_norm)) %>%
  mutate(year = year(post_date_norm)) %>%
  count(source_platform, year, name = "n") %>%
  arrange(source_platform, year)

p_ts_year_platform <- tab_by_year_platform %>%
  ggplot(aes(x = year, y = n, color = source_platform)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(title = paste0("Reviews per Year by Platform — ", case_target),
       x = "Year", y = "Count", color = "Platform") +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, paste0("R_", case_target, "_ts_reviews_per_year_by_platform.png")),
       p_ts_year_platform, width = 9, height = 5, dpi = 200)


# ==== NEW: heatmap theme_codes × stance ====
library(arrow)
library(tidyverse)  # dplyr, tidyr, readr, ggplot2

# Укажи кейс и пути
case_target <- "DAA"   # или "KTR"
base_out <- "./out"
in_file <- file.path(base_out, case_target, paste0("reviews_", case_target, ".feather"))

print(in_file)   # должен показать ./out/DAA/reviews_DAA.feather
file.exists(in_file)   # проверка: TRUE, если файл реально доступен

setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")


# Загрузка
dat <- arrow::read_feather(in_file)

# Страховка типов под наши операции
dat$theme_codes     <- as.character(dat$theme_codes)
dat$stance          <- as.character(dat$stance)
dat$text            <- as.character(dat$text)
dat$source_platform <- as.character(dat$source_platform)

# ==== HEATMAP: theme_codes × stance (ТОП-30 тегов) ====
themes_long <- dat %>%
  filter(!is.na(theme_codes), theme_codes != "") %>%
  tidyr::separate_rows(theme_codes, sep = ";") %>%
  count(theme_codes, stance, name = "n")

if (nrow(themes_long) > 0) {
  top_themes <- themes_long %>%
    group_by(theme_codes) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    slice_max(total, n = 30, with_ties = FALSE) %>%
    pull(theme_codes)
  
  heatmap_data <- themes_long %>%
    filter(theme_codes %in% top_themes) %>%
    tidyr::pivot_wider(names_from = stance, values_from = n, values_fill = 0) %>%
    arrange(theme_codes)
  
  mat <- as.matrix(heatmap_data[, setdiff(names(heatmap_data), "theme_codes")])
  rownames(mat) <- heatmap_data$theme_codes
  
  png(file.path(out_dir, paste0("R_", case_target, "_heatmap_themes_stance.png")),
      width = 1400, height = 1000)
  heatmap(mat,
          Rowv = NA, Colv = NA,
          scale = "row",
          col = heat.colors(256),
          margins = c(10, 12),
          main = paste0("Theme Codes × Stance — ", case_target))
  dev.off()
} else {
  message("Нет данных для heatmap (themes_long пуст).")
}

# ==== Примеры отзывов (цитаты) по 3 на каждую категорию stance ====
set.seed(123)

examples <- dat %>%
  dplyr::filter(!is.na(text), text != "") %>%
  dplyr::group_by(stance) %>%
  dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(3, nrow(.x)))) %>%
  dplyr::ungroup() %>%
  dplyr::select(case, source_platform, stance, rating_1_5, text)

if (nrow(examples) > 0) {
  out_csv <- file.path(out_dir, paste0("R_", case_target, "_examples_reviews.csv"))
  readr::write_csv(examples, out_csv)
  message("Цитаты сохранены: ", out_csv)
} else {
  message("Нет текстов для примеров, CSV с цитатами не создан.")
}

# ==== Анализ для KTR ====
case_target <- "KTR"   # <-- переключаем кейс
base_out <- "./out"
in_file <- file.path(base_out, case_target, paste0("reviews_", case_target, ".feather"))
out_dir <- file.path(base_out, case_target)

print(in_file)   
file.exists(in_file)   # TRUE если файл реально есть

# Загрузка
dat <- arrow::read_feather(in_file)

# Страховка типов
dat$theme_codes     <- as.character(dat$theme_codes)
dat$stance          <- as.character(dat$stance)
dat$text            <- as.character(dat$text)
dat$source_platform <- as.character(dat$source_platform)

# ==== HEATMAP: theme_codes × stance (ТОП-30 тегов) ====
themes_long <- dat %>%
  filter(!is.na(theme_codes), theme_codes != "") %>%
  tidyr::separate_rows(theme_codes, sep = ";") %>%
  count(theme_codes, stance, name = "n")

if (nrow(themes_long) > 0) {
  top_themes <- themes_long %>%
    group_by(theme_codes) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    slice_max(total, n = 30, with_ties = FALSE) %>%
    pull(theme_codes)
  
  heatmap_data <- themes_long %>%
    filter(theme_codes %in% top_themes) %>%
    tidyr::pivot_wider(names_from = stance, values_from = n, values_fill = 0) %>%
    arrange(theme_codes)
  
  mat <- as.matrix(heatmap_data[, setdiff(names(heatmap_data), "theme_codes")])
  rownames(mat) <- heatmap_data$theme_codes
  
  png(file.path(out_dir, paste0("R_", case_target, "_heatmap_themes_stance.png")),
      width = 1400, height = 1000)
  heatmap(mat,
          Rowv = NA, Colv = NA,
          scale = "row",
          col = heat.colors(256),
          margins = c(10, 12),
          main = paste0("Theme Codes × Stance — ", case_target))
  dev.off()
} else {
  message("Нет данных для heatmap (themes_long пуст).")
}

# ==== Примеры отзывов (цитаты) по 3 на каждую категорию stance ====
set.seed(123)

examples <- dat %>%
  dplyr::filter(!is.na(text), text != "") %>%
  dplyr::group_by(stance) %>%
  dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(3, nrow(.x)))) %>%
  dplyr::ungroup() %>%
  dplyr::select(case, source_platform, stance, rating_1_5, text)

if (nrow(examples) > 0) {
  out_csv <- file.path(out_dir, paste0("R_", case_target, "_examples_reviews.csv"))
  readr::write_csv(examples, out_csv)
  message("Цитаты сохранены: ", out_csv)
} else {
  message("Нет текстов для примеров, CSV с цитатами не создан.")
}
