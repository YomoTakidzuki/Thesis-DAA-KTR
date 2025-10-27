#!/usr/bin/env python3
import os
import pandas as pd

IN_FEATHER = "./out/reviews_master_clean.feather"
BASE_OUT   = "./out"

def save_case(df, case):
    sub = df[df["case"] == case].copy()
    outdir = os.path.join(BASE_OUT, case)
    os.makedirs(outdir, exist_ok=True)
    sub.reset_index(drop=True, inplace=True)
    sub.to_feather(os.path.join(outdir, f"reviews_{case}.feather"))
    sub.to_parquet(os.path.join(outdir, f"reviews_{case}.parquet"), index=False)
    sub.to_csv(os.path.join(outdir, f"reviews_{case}.csv"), index=False)
    print(f"{case}: {len(sub)} rows → {outdir}")

def main():
    df = pd.read_feather(IN_FEATHER)
    for case in ["DAA", "KTR"]:
        save_case(df, case)

if __name__ == "__main__":
    main()
