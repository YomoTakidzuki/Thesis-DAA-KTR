# fix_reviews_per_year.R
# Creates a corrected year-level plot for each case (DAA or KTR)
# Filters out unrealistic early years and ensures consistent time range.

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(lubridate)
})

# Set working directory
setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

# Choose the target case (DAA or KTR)
case_target <- Sys.getenv("CASE", "DAA")

# Define input/output paths
in_dir  <- file.path("out", case_target)
out_png <- file.path(in_dir, paste0("R_", case_target, "_reviews_per_year.png"))
in_feather <- file.path(in_dir, paste0("reviews_", case_target, ".feather"))

# Load cleaned review data and normalize the date column
df <- arrow::read_feather(in_feather) |> 
  mutate(post_date_norm = as_date(post_date_norm)) |>
  filter(!is.na(post_date_norm)) |>
  mutate(year = year(post_date_norm)) |>
  filter(year >= 2015, year <= year(Sys.Date()))  # remove out-of-range data

# Count reviews per year
tab_year <- df |> count(year, name = "n") |> arrange(year)

# Build the line plot
p_year <- ggplot(tab_year, aes(x = year, y = n)) +
  geom_line() +
  geom_point(size = 1.1) +
  scale_x_continuous(
    breaks = seq(2015, year(Sys.Date()), 1),
    limits = c(2015, year(Sys.Date()))
  ) +
  labs(
    title = paste("Reviews per Year —", case_target),
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

# Save the plot as a PNG
ggsave(out_png, p_year, width = 7.5, height = 4.5, dpi = 200)

# для КТ
# For the KTR case: this block prepares paths, checks input availability,
# and prints absolute paths to help you verify you're reading/writing
# in the intended folders.

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr/ggplot2/readr helpers
  library(arrow)   # fast Feather/Parquet I/O
  library(lubridate)  # date handling (year(), etc.)
})

# Use your thesis working directory (adjust if needed)
setwd("/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR")

case_target <- "KTR"  # <-- fix the target case explicitly

# Build input/output paths inside ./out/KTR
in_dir   <- file.path("out", case_target)
in_feath <- file.path(in_dir, paste0("reviews_", case_target, ".feather"))
out_png  <- file.path(in_dir, paste0("R_", case_target, "_reviews_per_year.png"))

# Print absolute paths for quick sanity check in console
cat("in_dir   =", normalizePath(in_dir), "\n")
cat("in_feath =", normalizePath(in_feath), "\n")
cat("out_png  =", normalizePath(out_png), "\n")

# Hard-stop if the input Feather file is missing to avoid silent failures
stopifnot(file.exists(in_feath))

# Load the Feather dataset and clean the date column
# The script extracts only valid review dates, converts them to year format,
# and keeps entries between 2015 and the current year.
df <- read_feather(in_feath) |>
  mutate(post_date_norm = as_date(post_date_norm)) |>
  filter(!is.na(post_date_norm)) |>
  mutate(year = as.integer(format(post_date_norm, "%Y"))) |>
  filter(between(year, 2015L, lubridate::year(Sys.Date())))

# Count number of reviews per year and sort chronologically
tab_year <- df |> count(year, name = "n") |> arrange(year)

# Print full table in console (no truncation)
print(tab_year, n = Inf)

# Force-delete any old output file to avoid overwrite warnings
if (file.exists(out_png)) unlink(out_png, force = TRUE)

# Create a line chart of yearly review counts for KTR
p_year <- ggplot(tab_year, aes(x = year, y = n)) +
  geom_line() +
  geom_point(size = 1.1) +
  scale_x_continuous(
    breaks = seq(min(tab_year$year), max(tab_year$year), 1),
    limits = c(min(tab_year$year), max(tab_year$year))
  ) +
  labs(
    title = paste("Reviews per Year —", case_target),
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

# Save the updated plot and confirm size + modification time
ggsave(out_png, p_year, width = 7.5, height = 4.5, dpi = 200, device = "png")
file.info(out_png)[, c("size", "mtime")]
