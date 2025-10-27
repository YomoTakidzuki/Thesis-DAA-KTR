# ------------------------------------------------------------
# analysis_R.R — базовые сводки и графики по reviews_master_clean.feather
# Требуемые пакеты: tidyverse, arrow, janitor, lubridate
# ------------------------------------------------------------

setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

# install.packages(c("tidyverse","arrow","janitor","lubridate")) # при необходимости
install.packages("tidyverse")
install.packages("arrow")
install.packages("janitor")
install.packages("lubridate")

library(tidyverse)
library(arrow)
library(janitor)
library(lubridate)

# ==== Пути ====
out_dir <- "./out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==== Загрузка ====
df <- read_feather(file.path(out_dir, "reviews_master_clean.feather")) %>%
  clean_names()  # безопасно нормализуем имена столбцов для удобства

# Ожидаемые поля:
# case, source_platform, rating_1_5, text, stance, theme_codes,
# pulled_by, post_date, has_text, post_date_norm

# Приводим некоторые типы
df <- df %>%
  mutate(
    post_date_norm = as_date(post_date_norm),
    rating_1_5_num = suppressWarnings(as.integer(rating_1_5)),
    has_text = as.factor(has_text),
    stance = as.factor(stance),
    case = as.factor(case),
    source_platform = as.factor(source_platform)
  )

# ==== Сводки =====

# 1) Разбивка по case × source_platform
tab_case_platform <- df %>%
  count(case, source_platform, name = "n") %>%
  arrange(case, source_platform)

write_csv(tab_case_platform, file.path(out_dir, "R_counts_by_case_platform.csv"))

# 2) Распределение stance (в целом)
tab_stance_overall <- df %>%
  count(stance, name = "n") %>%
  arrange(desc(n))
write_csv(tab_stance_overall, file.path(out_dir, "R_stance_overall.csv"))

# 3) Распределение stance по платформам
tab_stance_by_platform <- df %>%
  count(source_platform, stance, name = "n") %>%
  group_by(source_platform) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
write_csv(tab_stance_by_platform, file.path(out_dir, "R_stance_by_platform.csv"))

# 4) Распределение stance по кейсам
tab_stance_by_case <- df %>%
  count(case, stance, name = "n") %>%
  group_by(case) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()
write_csv(tab_stance_by_case, file.path(out_dir, "R_stance_by_case.csv"))

# 5) Сводки по времени (год и год-месяц)
df_time <- df %>%
  filter(!is.na(post_date_norm)) %>%
  mutate(
    year = year(post_date_norm),
    ym = floor_date(post_date_norm, unit = "month")
  )

tab_by_year <- df_time %>% count(year, name = "n") %>% arrange(year)
tab_by_ym   <- df_time %>%
  count(ym, name = "n") %>%
  arrange(ym)

write_csv(tab_by_year, file.path(out_dir, "R_counts_by_year.csv"))
write_csv(tab_by_ym,   file.path(out_dir, "R_counts_by_year_month.csv"))

# ==== Графики (ggplot2, «публикационное» качество по-умолчанию) =====

# Гистограмма рейтингов (если есть)
p_rating <- df %>%
  filter(!is.na(rating_1_5_num)) %>%
  ggplot(aes(x = rating_1_5_num)) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Rating 1–5 (histogram)", x = "Rating", y = "Count") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "R_hist_rating_1_5.png"), p_rating, width = 7, height = 4.5, dpi = 200)

# Stance по платформам (stacked)
p_stance_platform <- tab_stance_by_platform %>%
  ggplot(aes(x = source_platform, y = n, fill = stance)) +
  geom_col(position = "stack") +
  labs(title = "Stance by Source Platform", x = "Platform", y = "Count", fill = "Stance") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "R_bar_stance_by_platform.png"), p_stance_platform, width = 8, height = 5, dpi = 200)

# Stance по кейсам (stacked)
p_stance_case <- tab_stance_by_case %>%
  ggplot(aes(x = case, y = n, fill = stance)) +
  geom_col(position = "stack") +
  labs(title = "Stance by Case", x = "Case", y = "Count", fill = "Stance") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "R_bar_stance_by_case.png"), p_stance_case, width = 7, height = 4.5, dpi = 200)

# Временной ряд по месяцам (все платформы вместе)
p_ts_all <- tab_by_ym %>%
  ggplot(aes(x = ym, y = n)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(title = "Reviews per Month", x = "Year-Month", y = "Count") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "R_ts_reviews_per_month.png"), p_ts_all, width = 8, height = 4.5, dpi = 200)

# Временной ряд по месяцам в разрезе платформ
tab_by_ym_platform <- df_time %>%
  count(source_platform, ym, name = "n") %>%
  arrange(source_platform, ym)

p_ts_platform <- tab_by_ym_platform %>%
  ggplot(aes(x = ym, y = n, color = source_platform)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(title = "Reviews per Month by Platform", x = "Year-Month", y = "Count", color = "Platform") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "R_ts_reviews_per_month_by_platform.png"), p_ts_platform, width = 9, height = 5, dpi = 200)

# Фасет: распределение рейтингов по платформам
p_rating_facet <- df %>%
  filter(!is.na(rating_1_5_num)) %>%
  ggplot(aes(x = rating_1_5_num)) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Rating 1–5 by Platform", x = "Rating", y = "Count") +
  facet_wrap(~ source_platform, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "R_hist_rating_by_platform.png"), p_rating_facet, width = 9, height = 6, dpi = 200)

# ==== Заготовки для тестов различий (пока только каркас) ====

# Пример: сравнить распределение stance между платформами (хи-квадрат)
# (Нужно будет обсудить, какие группы сравниваем и как агрегировать)
stance_xtab <- df %>%
  count(source_platform, stance) %>%
  pivot_wider(names_from = stance, values_from = n, values_fill = 0) %>%
  column_to_rownames("source_platform") %>%
  as.matrix()

# chi_sq_result <- chisq.test(stance_xtab)   # <- раскомментировать после согласования

# Пример: сравнить средний рейтинг между платформами (при наличии текста/без и т.п.)
# df %>%
#   filter(!is.na(rating_1_5_num)) %>%
#   group_by(source_platform) %>%
#   summarise(mean_rating = mean(rating_1_5_num), n = n())

# t.test(rating_1_5_num ~ source_platform, data = df %>% filter(!is.na(rating_1_5_num)))

# ==== Конец ====
message("-анализ: сводки и графики сохранены в ./out")


