#!/usr/bin/env bash
set -euo pipefail

# активируем окружение
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate daa_ktr

# запускаем Python-скрипт
python "/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR/process_reviews.py" \
  --input "/Users/maria/Desktop/Padova/Thesis/archival_log_template.xlsx" \
  --sheet "Discourse" \
  --outdir "/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR/out"

