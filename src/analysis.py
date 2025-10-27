#!/usr/bin/env python3
"""
analysis.py
Анализ очищенных отзывов (DAA/KTR).
Строит графики и сохраняет их в ./out
"""

import os
import pandas as pd
import matplotlib.pyplot as plt

OUTDIR = "./out"

def main():
    os.makedirs(OUTDIR, exist_ok=True)

    # Загружаем чистый слой
    df = pd.read_parquet(os.path.join(OUTDIR, "reviews_master_clean.parquet"))

    # --- Распределение по годам ---
    year_counts = df["post_date_norm"].dt.year.value_counts().sort_index()
    year_counts.plot(kind="bar")
    plt.title("Отзывы по годам")
    plt.xlabel("Год")
    plt.ylabel("Количество отзывов")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "bar_counts_by_year.png"), dpi=200)
    plt.close()

    # --- Распределение по месяцам (год-месяц) ---
    month_counts = df["post_date_norm"].dt.to_period("M").astype(str).value_counts().sort_index()
    month_counts.plot(kind="line", marker="o")
    plt.title("Отзывы по месяцам")
    plt.xlabel("Год-месяц")
    plt.ylabel("Количество отзывов")
    plt.xticks(rotation=45, ha="right")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "line_counts_by_month.png"), dpi=200)
    plt.close()

    # --- Stance по времени (год-месяц) ---
    stance_over_time = (
        df.groupby([df["post_date_norm"].dt.to_period("M"), "stance"])
        .size()
        .unstack(fill_value=0)
    )
    stance_over_time.index = stance_over_time.index.astype(str)
    stance_over_time.plot(kind="line", marker="o")
    plt.title("Stance по месяцам")
    plt.xlabel("Год-месяц")
    plt.ylabel("Количество отзывов")
    plt.xticks(rotation=45, ha="right")
    plt.legend(title="stance")
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "line_stance_over_time.png"), dpi=200)
    plt.close()

    print("✅ Анализ завершён. Графики сохранены в ./out")

if __name__ == "__main__":
    main()
