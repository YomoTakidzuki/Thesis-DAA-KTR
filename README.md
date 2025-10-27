## From Silence to Reuse in St Petersburg Industrial Heritage  
### _Comparative analysis of two post-industrial sites: Design District DAA (former KUB) and Krasny Treugolnik_

---

### Overview

This repository contains all scripts, data structure, and reproducible outputs used for the master’s thesis:

> _“From Silence to Reuse in St Petersburg Industrial Heritage through Two Case Studies”_  
> Mariia Poputneva, MA Local Development, University of Padua (2025)

The research explores how two former industrial buildings in St Petersburg followed different post-industrial trajectories —  
**Design District DAA** (a successfully reused creative cluster) and **Krasny Treugolnik** (a largely derelict “silent” site).  

The repository contains **fully reproducible R and Python code** for:
- parsing and cleaning online review data,  
- separating datasets by case (DAA / KTR),  
- running statistical and visual analyses in R,  
- exporting publication-quality figures and summary tables.  

No personal information (e.g. usernames or author IDs) is ever stored. All data were anonymized before processing.

---

### Repository structure

```
Thesis-DAA-KTR/
│
├── data/                        # Clean input and master data files
│   ├── reviews_master.xlsx      # Combined, cleaned review dataset
│   └── raw/                     # (empty placeholder — raw data excluded for privacy)
│
├── src/                         # Python scripts (data preparation & filtering)
│   ├── process_reviews.py       # Initial text cleaning and field normalization
│   ├── fix_theme_codes.py       # Harmonization of thematic codes
│   ├── split_by_case.py         # Separates DAA vs KTR datasets automatically
│   ├── analysis.py              # High-level orchestration of analysis tasks
│   └── run_all.sh               # Batch-execution shell script
│
├── R/                           # R scripts (statistical and visual analysis)
│   ├── analysis_R.R             # Main descriptive plots and summaries
│   ├── r_analysis.R             # Refined analysis (stance, rating, time trends)
│   ├── r_analysis_per_case.R    # Case-specific heatmaps and wordclouds
│   ├── fix_reviews_per_year.R   # Adjusts year-range and removes empty years
│   └── table_summary.R          # Generates summary tables
│
├── out/                         # Final non-sensitive outputs used in thesis
│   ├── DAA/
│   │   ├── R_DAA_reviews_per_year.png
│   │   ├── R_DAA_heatmap_themes_stance.png
│   │   ├── R_DAA_wordcloud.png
│   │   ├── R_DAA_top_themes.png
│   │   ├── R_DAA_box_rating_by_stance.png
│   │   ├── R_DAA_stance_by_platform.png
│   │   ├── R_DAA_mean_rating_by_platform.csv
│   │   ├── R_DAA_haste_by_platform.csv
│   │   ├── R_DAA_stance_by_platform.csv
│   │   ├── R_DAA_ttest_rating_vs_platform.txt
│   │   └── R_DAA_chisq_stance_vs_platform.txt
│   │
│   └── KTR/
│       ├── R_KTR_reviews_per_year.png
│       ├── R_KTR_heatmap_themes_stance.png
│       ├── R_KTR_wordcloud.png
│       ├── R_KTR_top_themes.png
│       ├── R_KTR_box_rating_by_stance.png
│       ├── R_KTR_stance_by_platform.png
│       ├── R_KTR_mean_rating_by_platform.csv
│       ├── R_KTR_haste_by_platform.csv
│       ├── R_KTR_stance_by_platform.csv
│       ├── R_KTR_ttest_rating_vs_platform.txt
│       └── R_KTR_chisq_stance_vs_platform.txt
│
├── Appendix. Reproducible Code - Colab.pdf
└── README.md
```

## How to кun the фnalysis

This section describes how to reproduce the data-processing and visual analysis pipeline step by step.  
Both **Python (≥ 3.10)** and **R (≥ 4.3)** are required.

---

### Preparation

#### Install dependencies

**Python**
```bash
pip install pandas numpy pyarrow matplotlib seaborn
```
**R**
```
install.packages(c(
  "tidyverse", "arrow", "janitor", "lubridate",
  "ggthemes", "ggplot2", "scales", "tidytext"
))
```
The analysis was run in macOS / Linux terminal and RStudio 2024.
All scripts were tested under reproducible environments; no external APIs are called.

### Data folder structure

```
data/
│
├── reviews_master.xlsx          # Combined cleaned dataset
└── raw/                         # empty placeholder (original raw data excluded)
```

All potentially identifying information (such as usernames or profile IDs) was removed prior to import.  
The dataset contains only **anonymized text**, **review dates**, **numeric ratings (1–5)**, and **thematic codes** used for content analysis.  
The raw (non-anonymized) files are intentionally excluded from the repository to ensure compliance with research ethics and data protection standards.

---

## Description of Scripts

This section briefly explains the purpose of each Python and R file used in the project.  
Scripts are grouped by language for clarity.

---

### Python scripts (`/src`)

```
| File | Description |
|------|--------------|
| `process_reviews.py` | Cleans and standardizes review data: removes HTML tags, fixes date formats, converts ratings to numeric, and saves as Feather/Parquet. |
| `fix_theme_codes.py` | Harmonizes and validates the column of thematic codes (e.g. `design; architecture; reuse`). |
| `split_by_case.py` | Automatically separates the dataset into two subsets: DAA and KTR, storing results in `out/DAA/` and `out/KTR/`. |
| `analysis.py` | High-level coordination script for Python-side pre-processing. |
| `run_all.sh` | Shell helper to run the entire sequence (Python + R) in the correct order. |
```
---

### R scripts (`/R`)

```
| File | Description |
|------|--------------|
| `analysis_R.R` | Initial descriptive statistics and visualizations (stance, ratings, timelines). |
| `r_analysis.R` | Full statistical workflow: stance proportions, rating distributions, χ² and t-tests. |
| `r_analysis_per_case.R` | Generates case-specific heatmaps and wordclouds for DAA and KTR separately. |
| `fix_reviews_per_year.R` | Filters out empty or irrelevant early years (before 2015) to avoid misleading charts. |
| `table_summary.R` | Exports consolidated summary tables (mean rating, stance by platform, etc.) to CSV format. |
```
---

> All scripts can be run individually or sequentially through `run_all.sh`.  
> Each script automatically checks whether input files exist and creates missing output directories.

---

## License and citation

This repository is distributed under the **MIT License**, which allows reuse for academic, educational, and non-commercial purposes provided that proper credit is given.  
For details, see the accompanying `LICENSE` file or visit [MIT License on opensource.org](https://opensource.org/licenses/MIT).

If you use this code, figures, or methodology in your own research, please cite as follows:

> **Poputneva, M. (2025).**  
> *From Silence to Reuse in St Petersburg Industrial Heritage through Two Case Studies.*  
> Master’s thesis, University of Padua, Local Development Programme.  
> Repository: [[https://github.com/YomoTakidzuki/Thesis-DAA-KTR](https://github.com/YomoTakidzuki/Thesis-DAA-KTR)

---

## Acknowledgements

This work was developed as part of the Master’s thesis under the supervision of  
**Prof. David Celetti**, Department of Economics and Management, University of Padua.

The author would like to thank:
- the University of Padua and the Local Development programme for academic support, 
- open data contributors whose platforms (Google Maps & Yandex Maps) made the textual corpus accessible,  
- and all reviewers and colleagues who provided feedback during research design and analysis.

---

## Repository summary
```
| Component | Role |
|------------|------|
| `/data/` | Contains anonymized, cleaned input dataset (`reviews_master.xlsx`). |
| `/src/` | Python preprocessing scripts (data cleaning, case splitting). |
| `/R/` | R scripts for statistical and visual analysis. |
| `/out/` | Final non-sensitive outputs used in the thesis. |
| `Appendix.Reproducible_Code-Colab.pdf` | Step-by-step code appendix for reproducibility. |
| `README.md` | This documentation file. |
```
---

### Contact

For academic inquiries or collaboration:

**Mariia Poputneva**
MA Local Development (University of Padua)
poputnevam@gmail.com
Padova — St Petersburg — Leipzig
