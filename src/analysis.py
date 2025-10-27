#!/usr/bin/env python3
"""
analysis.py
Analysis of the cleaned reviews (DAA/KTR).
Builds figures and saves them into ./out
"""

import os
import pandas as pd
import matplotlib.pyplot as plt

# All outputs (tables/plots) will be written here.
OUTDIR = "./out"

def main():
    # Ensure the output folder exists so subsequent saves don't fail.
    os.makedirs(OUTDIR, exist_ok=True)

    # Load the cleaned, consolidated dataset produced by process_reviews.py.
    # We read the Parquet file for speed and consistent dtypes.
    df = pd.read_parquet(os.path.join(OUTDIR, "reviews_master_clean.parquet"))

    # --- Distribution by year ---
    # Count how many reviews were posted in each year and sort chronologically
    year_counts = df["post_date_norm"].dt.year.value_counts().sort_index()

    # Plot the yearly distribution as a bar chart
    year_counts.plot(kind="bar")

    # Add labels and a title for readability
    plt.title("Reviews by Year")
    plt.xlabel("Year")
    plt.ylabel("Number of Reviews")

    # Save the plot and close the figure to free memory
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "bar_counts_by_year.png"), dpi=200)
    plt.close()

    # --- Distribution by months (year-month) ---
    # Convert dates to "year-month" periods, count reviews per month, and sort chronologically
    month_counts = df["post_date_norm"].dt.to_period("M").astype(str).value_counts().sort_index()

    # Plot monthly review activity as a line chart
    month_counts.plot(kind="line", marker="o")

    # Add title, axis labels, and rotated x-axis labels for readability
    plt.title("Reviews by Month")
    plt.xlabel("Year-Month")
    plt.ylabel("Number of Reviews")
    plt.xticks(rotation=45, ha="right")

    # Save the line chart and close the figure
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "line_counts_by_month.png"), dpi=200)
    plt.close()

    # --- Stance over time (by year-month) ---
    # Group reviews by month and stance, count occurrences, and reshape into a wide table
    stance_over_time = (
        df.groupby([df["post_date_norm"].dt.to_period("M"), "stance"])
        .size()
        .unstack(fill_value=0)
    )

    # Convert period index to string for cleaner axis labels
    stance_over_time.index = stance_over_time.index.astype(str)

    # Plot stance dynamics as a multi-line chart
    stance_over_time.plot(kind="line", marker="o")

    # Add title, axis labels, and legend
    plt.title("Stance by Month")
    plt.xlabel("Year-Month")
    plt.ylabel("Number of Reviews")
    plt.xticks(rotation=45, ha="right")
    plt.legend(title="Stance")

    # Save the chart and close the figure
    plt.tight_layout()
    plt.savefig(os.path.join(OUTDIR, "line_stance_over_time.png"), dpi=200)
    plt.close()

    print("All done. Figures saved to ./out")

# Entry point guard: run main() only when this file is executed directly
# (and not when it's imported as a module in another script).
if __name__ == "__main__":
    main()
