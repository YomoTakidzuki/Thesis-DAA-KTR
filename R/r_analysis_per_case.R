# ——— per-case R analysis & real wordclouds ———
suppressPackageStartupMessages({
  library(tidyverse); library(arrow); library(janitor); library(lubridate)
  library(tidytext);  library(ggwordcloud); library(widyr)
})

case_target <- Sys.getenv("CASE", "DAA")
in_dir  <- file.path("out", case_target)
out_dir <- in_dir
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

in_file <- file.path(in_dir, paste0("reviews_", case_target, ".feather"))
df <- read_feather(in_file) %>% clean_names()

# приведения типов
df <- df %>%
  mutate(
    post_date_norm = as_date(post_date_norm),
    rating_1_5_num = suppressWarnings(as.integer(rating_1_5)),
    has_text = factor(has_text),
    stance   = factor(stance),
    source_platform = factor(source_platform)
  )

# ==== Таблички: частоты/сводки ====
df %>% count(source_platform, stance, name="n") %>%
  group_by(source_platform) %>% mutate(p = n/sum(n)) %>% ungroup() %>%
  arrange(source_platform, desc(n)) %>%
  write_csv(file.path(out_dir, paste0("R_", case_target, "_stance_by_platform.csv")))

df %>% count(has_text, source_platform, name="n") %>%
  group_by(source_platform) %>% mutate(p = n/sum(n)) %>% ungroup() %>%
  write_csv(file.path(out_dir, paste0("R_", case_target, "_hastext_by_platform.csv")))

df %>% group_by(source_platform) %>%
  summarise(n=n(), mean_rating = mean(rating_1_5_num, na.rm=TRUE)) %>%
  arrange(desc(n)) %>%
  write_csv(file.path(out_dir, paste0("R_", case_target, "_mean_rating_by_platform.csv")))

# ==== Графики (ggplot) ====
# stance by platform (stacked)
p1 <- df %>% count(source_platform, stance, name="n") %>%
  ggplot(aes(x=source_platform, y=n, fill=stance)) +
  geom_col() + labs(title=paste("Stance by platform —", case_target),
                    x="Platform", y="Count", fill="Stance") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, paste0("R_", case_target, "_stance_by_platform.png")),
       p1, width=8, height=5, dpi=200)

# boxplot rating by stance
p2 <- df %>% filter(!is.na(rating_1_5_num)) %>%
  ggplot(aes(x=stance, y=rating_1_5_num)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title=paste("Rating by stance —", case_target),
       x="Stance", y="Rating 1–5") + theme_minimal(base_size = 12)
ggsave(file.path(out_dir, paste0("R_", case_target, "_box_rating_by_stance.png")),
       p2, width=7, height=5, dpi=200)

# линия по годам
p_year <- df %>% filter(!is.na(post_date_norm)) %>%
  mutate(year = year(post_date_norm)) %>%
  count(year, name="n") %>%
  ggplot(aes(x=year, y=n)) + geom_line() + geom_point(size=1.1) +
  labs(title=paste("Reviews per year —", case_target), x="Year", y="Count") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, paste0("R_", case_target, "_reviews_per_year.png")),
       p_year, width=7.5, height=4.5, dpi=200)

# ==== theme_codes: long + топы + heatmap-подложка (по платформе внутри кейса) ====
themes_long <- df %>%
  mutate(theme_codes = if_else(is.na(theme_codes), "", theme_codes)) %>%
  separate_longer_delim(theme_codes, delim = ";") %>%
  mutate(theme_codes = str_trim(theme_codes)) %>%
  filter(theme_codes != "")
write_csv(themes_long, file.path(out_dir, paste0("R_", case_target, "_theme_codes_long.csv")))

top_themes <- themes_long %>% count(theme_codes, sort = TRUE) %>% slice_head(n=15)
p_top <- top_themes %>%
  ggplot(aes(x=reorder(theme_codes, n), y=n)) +
  geom_col() + coord_flip() +
  labs(title=paste("Top-15 themes —", case_target), x="Theme", y="Count") +
  theme_minimal(base_size=12)
ggsave(file.path(out_dir, paste0("R_", case_target, "_top_themes.png")),
       p_top, width=7, height=5, dpi=200)

# ==== НАСТОЯЩИЙ wordcloud ====
# стоп-слова RU+EN (простая смесь)
stop_ru <- c("и","в","во","не","что","он","на","я","с","со","как","а","то","все",
             "она","так","его","но","да","ты","к","у","же","вы","за","бы","по",
             "ее","мне","если","или","ни","мы","такой","это","этот","эта","этом",
             "из","от","до","для","без","над","о","об","при","под","через","после",
             "где","когда","здесь","там","тут","ну","ли","же","быть","есть","нет")
stop_en <- tidytext::stop_words$word
stops  <- unique(c(stop_ru, stop_en))

txt <- df %>% filter(has_text == "yes", !is.na(text)) %>%
  select(text, stance, source_platform)

tokens <- txt %>%
  unnest_tokens(word, text, token = "words") %>%
  mutate(word = str_to_lower(word)) %>%
  filter(str_detect(word, "^[\\p{L}\\p{N}]{3,}$")) %>%     # ≥3 символов, буквы/цифры
  filter(!word %in% stops)

wc <- tokens %>% count(word, sort = TRUE) %>% slice_max(n, n = 250)

p_wc <- ggplot(wc, aes(label = word, size = n)) +
  ggwordcloud::geom_text_wordcloud_area() +
  scale_size_area(max_size = 16) +
  labs(title=paste("Wordcloud —", case_target)) +
  theme_minimal(base_size=12)
ggsave(file.path(out_dir, paste0("R_", case_target, "_wordcloud.png")),
       p_wc, width=8, height=6, dpi=200)

# (опционально) вордклауды по платформам в этом кейсе
for (plat in levels(df$source_platform)) {
  wc_p <- tokens %>% filter(source_platform == plat) %>%
    count(word, sort = TRUE) %>% slice_max(n, n = 200)
  if (nrow(wc_p) > 0) {
    p <- ggplot(wc_p, aes(label = word, size = n)) +
      ggwordcloud::geom_text_wordcloud_area() +
      scale_size_area(max_size = 14) +
      labs(title=paste("Wordcloud —", case_target, "—", plat)) +
      theme_minimal(base_size=12)
    ggsave(file.path(out_dir, paste0("R_", case_target, "_wordcloud_", plat, ".png")),
           p, width=8, height=6, dpi=200)
  }
}

# — Простые тесты —
stance_xtab <- df %>% count(source_platform, stance) %>%
  pivot_wider(names_from=stance, values_from=n, values_fill=0) %>%
  column_to_rownames("source_platform") %>% as.matrix()
if (nrow(stance_xtab)>=2) {
  chisq_res <- suppressWarnings(chisq.test(stance_xtab))
  capture.output(chisq_res, file=file.path(out_dir, paste0("R_", case_target, "_chisq_stance_vs_platform.txt")))
}
tt <- df %>% filter(!is.na(rating_1_5_num)) %>% select(source_platform, rating_1_5_num)
if (dplyr::n_distinct(tt$source_platform)>=2) {
  t_res <- try(t.test(rating_1_5_num ~ source_platform, data=tt), silent=TRUE)
  if (!inherits(t_res, "try-error")) {
    capture.output(t_res, file=file.path(out_dir, paste0("R_", case_target, "_ttest_rating_vs_platform.txt")))
  }
}
message("Готово: результаты сохранены в ", out_dir)
