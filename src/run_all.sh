#!/usr/bin/env bash
set -euo pipefail

# Activate the conda environment (ensures all dependencies are available)
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate daa_ktr

# Run the Python preprocessing script for review data
# Arguments:
#   --input   : path to the Excel/CSV file with raw reviews
#   --sheet   : name of the Excel sheet (used only for .xlsx)
#   --outdir  : directory where all cleaned data and plots will be saved
python "/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR/process_reviews.py" \
  --input "/Users/maria/Desktop/Padova/Thesis/archival_log_template.xlsx" \
  --sheet "Discourse" \
  --outdir "/Users/maria/Desktop/Padova/Thesis/Thesis-DAA-KTR/out"
