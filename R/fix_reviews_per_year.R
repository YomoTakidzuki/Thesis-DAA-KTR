# fix_reviews_per_year.R
suppressPackageStartupMessages({library(tidyverse); library(arrow); library(lubridate)})
setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

case_target <- Sys.getenv("CASE", "DAA")          # DAA или KTR
in_dir  <- file.path("out", case_target)
out_png <- file.path(in_dir, paste0("R_", case_target, "_reviews_per_year.png"))
in_feather <- file.path(in_dir, paste0("reviews_", case_target, ".feather"))

df <- arrow::read_feather(in_feather) |> 
  mutate(post_date_norm = as_date(post_date_norm)) |>
  filter(!is.na(post_date_norm)) |>
  mutate(year = year(post_date_norm)) |>
  filter(year >= 2015, year <= year(Sys.Date()))   # срезаем фигню

tab_year <- df |> count(year, name="n") |> arrange(year)

p_year <- ggplot(tab_year, aes(x = year, y = n)) +
  geom_line() + geom_point(size = 1.1) +
  scale_x_continuous(breaks = seq(2015, year(Sys.Date()), 1),
                     limits = c(2015, year(Sys.Date()))) +
  labs(title = paste("Reviews per year —", case_target), x = "Year", y = "Count") +
  theme_minimal(base_size = 12)

ggsave(out_png, p_year, width = 7.5, height = 4.5, dpi = 200)



# для КТ

suppressPackageStartupMessages({library(tidyverse); library(arrow); library(lubridate)})

setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

case_target <- "KTR"   # <-- фиксируем явно
in_dir   <- file.path("out", case_target)
in_feath <- file.path(in_dir, paste0("reviews_", case_target, ".feather"))
out_png  <- file.path(in_dir, paste0("R_", case_target, "_reviews_per_year.png"))

cat("in_dir   =", normalizePath(in_dir), "\n")
cat("in_feath =", normalizePath(in_feath), "\n")
cat("out_png  =", normalizePath(out_png), "\n")

stopifnot(file.exists(in_feath))

df <- read_feather(in_feath) |>
  mutate(post_date_norm = as_date(post_date_norm)) |>
  filter(!is.na(post_date_norm)) |>
  mutate(year = as.integer(format(post_date_norm, "%Y"))) |>
  filter(between(year, 2015L, lubridate::year(Sys.Date())))

tab_year <- df |> count(year, name = "n") |> arrange(year)
print(tab_year, n = Inf)

# принудительно сносим старый файл
if (file.exists(out_png)) unlink(out_png, force = TRUE)

p_year <- ggplot(tab_year, aes(x = year, y = n)) +
  geom_line() + geom_point(size = 1.1) +
  scale_x_continuous(
    breaks = seq(min(tab_year$year), max(tab_year$year), 1),
    limits = c(min(tab_year$year), max(tab_year$year))
  ) +
  labs(title = paste("Reviews per year —", case_target), x = "Year", y = "Count") +
  theme_minimal(base_size = 12)

ggsave(out_png, p_year, width = 7.5, height = 4.5, dpi = 200, device = "png")
file.info(out_png)[, c("size","mtime")]
