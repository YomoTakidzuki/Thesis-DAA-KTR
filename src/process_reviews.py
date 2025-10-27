#!/usr/bin/env python3
"""
process_reviews.py
Предобработка отзывов для кейсов DAA/KTR:
- валидация схемы,
- отчёты по пропускам/частотам/дубликатам,
- нормализация дат в post_date_norm (якорь 2025-09-16),
- сохранение чистых данных в Parquet/Feather,
- стартовые выгрузки CSV + список уникальных theme_codes,
- два простых графика (матплотлиб, без seaborn).
"""

import argparse
import os
import re
from collections import Counter
from datetime import datetime, timedelta

import pandas as pd
from dateutil.parser import parse as du_parse
import dateparser
import matplotlib.pyplot as plt


ANCHOR_DATE = datetime(2025, 9, 16)   # якорная дата из ТЗ
REQUIRED_COLS = ["case", "source_platform", "rating_1_5", "text", "stance", "theme_codes", "pulled_by", "post_date", "has_text"]
ALLOWED_CASE = {"DAA", "KTR"}
ALLOWED_SOURCE = {"GoogleMaps", "YandexMaps"}
ALLOWED_STANCE = {"positive", "negative", "neutral", "mixed"}
ALLOWED_HAS_TEXT = {"yes", "no"}

REL_PATTERNS = [
    # порядок важен: сначала годы/месяцы/недели/дни (мн./ед.), русские варианты
    (r"(\d+)\s*(год|года|лет)\s*назад", "years"),
    (r"(\d+)\s*(месяц|месяца|месяцев)\s*назад", "months"),
    (r"(\d+)\s*(неделю|недели|недель)\s*назад", "weeks"),
    (r"(\d+)\s*(день|дня|дней)\s*назад", "days"),
    # на всякий — английские формы, если вдруг встречаются
    (r"(\d+)\s*(year|years)\s*ago", "years"),
    (r"(\d+)\s*(month|months)\s*ago", "months"),
    (r"(\d+)\s*(week|weeks)\s*ago", "weeks"),
    (r"(\d+)\s*(day|days)\s*ago", "days"),
]


def parse_args():
    ap = argparse.ArgumentParser(description="Preprocess DAA/KTR reviews")
    ap.add_argument("--input", required=True,
                    help="Путь к файлу с отзывами (.xlsx или .csv)")
    ap.add_argument("--sheet", default=None,
                    help="Имя листа Excel (если .xlsx и нужен конкретный лист)")
    ap.add_argument("--outdir", default=os.path.expanduser("~/Thesis-DAA-KTR/out"),
                    help="Папка для сохранения результатов")
    return ap.parse_args()


def load_data(path: str, sheet: str | None) -> pd.DataFrame:
    ext = os.path.splitext(path)[1].lower()
    if ext in [".xlsx", ".xls"]:
        return pd.read_excel(path, sheet_name=sheet, engine="openpyxl")
    elif ext == ".csv":
        # попробуем сначала utf-8, затем угадать
        try:
            return pd.read_csv(path)
        except UnicodeDecodeError:
            import chardet
            with open(path, "rb") as f:
                enc = chardet.detect(f.read())["encoding"] or "utf-8"
            return pd.read_csv(path, encoding=enc)
    else:
        raise ValueError(f"Неподдерживаемое расширение файла: {ext}")


def validate_schema(df: pd.DataFrame) -> dict:
    report = {"missing_required": [], "value_issues": {}}
    for col in REQUIRED_COLS:
        if col not in df.columns:
            report["missing_required"].append(col)

    # Если критических колонок нет — дальше смысла валидировать значения нет
    if report["missing_required"]:
        return report

    # Приведём некоторые типы/нижний регистр по правилам
    df["case"] = df["case"].astype(str)
    df["source_platform"] = df["source_platform"].astype(str)
    df["stance"] = df["stance"].astype(str).str.lower()
    df["has_text"] = df["has_text"].astype(str).str.lower()

    # Проверка допустимых значений
    issues = {}

    bad_case = df.loc[~df["case"].isin(ALLOWED_CASE), "case"]
    if not bad_case.empty:
        issues["case"] = Counter(bad_case)

    bad_src = df.loc[~df["source_platform"].isin(ALLOWED_SOURCE), "source_platform"]
    if not bad_src.empty:
        issues["source_platform"] = Counter(bad_src)

    bad_stance = df.loc[~df["stance"].isin(ALLOWED_STANCE), "stance"]
    if not bad_stance.empty:
        issues["stance"] = Counter(bad_stance)

    bad_has_text = df.loc[~df["has_text"].isin(ALLOWED_HAS_TEXT), "has_text"]
    if not bad_has_text.empty:
        issues["has_text"] = Counter(bad_has_text)

    # rating_1_5 как целое 1–5 (если столбец есть)
    if "rating_1_5" in df.columns:
        # принудительно к числу
        df["rating_1_5"] = pd.to_numeric(df["rating_1_5"], errors="coerce").astype("Int64")
        bad_rating = df.loc[~df["rating_1_5"].isin([1, 2, 3, 4, 5]), "rating_1_5"]
        if not bad_rating.empty:
            issues["rating_1_5"] = Counter(bad_rating.dropna())

    report["value_issues"] = {k: dict(v) for k, v in issues.items()}
    return report


def normalize_theme_codes_format(df: pd.DataFrame) -> pd.Series:
    """
    Просто проверяем формат: латиница/цифры/_ и ; без пробелов.
    Возвращаем булеву серию — True если формат ок или пусто.
    """
    if "theme_codes" not in df.columns:
        return pd.Series([True] * len(df), index=df.index)
    pat = re.compile(r"^[a-z0-9_]+(?:;[a-z0-9_]+)*$")  # допустимы цифры тоже
    ok = df["theme_codes"].fillna("").astype(str).str.strip().apply(lambda s: (s == "") or bool(pat.fullmatch(s)))
    return ok


def rel_to_date(anchor: datetime, n: int, unit: str) -> datetime:
    if unit == "years":
        return anchor.replace(year=anchor.year - n)
    if unit == "months":
        # грубо: минус n месяцев через year/month арифметику
        y = anchor.year
        m = anchor.month - n
        while m <= 0:
            y -= 1
            m += 12
        # день — минимум из текущего дня и макс. дней в новой месяце
        day = min(anchor.day, [31, 29 if y % 4 == 0 and (y % 100 != 0 or y % 400 == 0) else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m-1])
        return anchor.replace(year=y, month=m, day=day)
    if unit == "weeks":
        return anchor - timedelta(weeks=n)
    if unit == "days":
        return anchor - timedelta(days=n)
    return None

from datetime import date

def parse_post_date(raw) -> pd.Timestamp:
    # 0) пусто
    if pd.isna(raw):
        return pd.NaT

    # 1) уже дата (Timestamp/datetime/date)
    if isinstance(raw, (pd.Timestamp, datetime, date)):
        return pd.to_datetime(raw).normalize()

    # 2) excel-serial (число дней от 1899-12-30)
    if isinstance(raw, (int, float)):
        try:
            return pd.to_datetime(raw, unit="D", origin="1899-12-30").normalize()
        except Exception:
            pass  # попробуем как строку дальше

    # 3) строка
    s = str(raw).strip()
    if s == "":
        return pd.NaT

    # 3a) только год → берём 1 июля этого года (как договорились)
    if re.fullmatch(r"\d{4}", s):
        return pd.Timestamp(f"{s}-07-01")

    # 3b) относительные форматы «N лет/месяцев/… назад»
    s_lower = s.lower()
    for pat, unit in REL_PATTERNS:
        m = re.search(pat, s_lower)
        if m:
            try:
                n = int(m.group(1))
                dt = rel_to_date(ANCHOR_DATE, n, unit)
                return pd.Timestamp(dt.date()) if dt else pd.NaT
            except Exception:
                return pd.NaT

    # 3c) абсолютные форматы (рус/англ)
    dt = dateparser.parse(s, settings={"DATE_ORDER": "DMY"})
    if dt:
        return pd.Timestamp(dt.date())

    try:
        dt2 = du_parse(s, dayfirst=True, fuzzy=True)
        return pd.Timestamp(dt2.date())
    except Exception:
        return pd.NaT


def main():
    args = parse_args()
    os.makedirs(args.outdir, exist_ok=True)

    # ===== Load =====
    df = load_data(args.input, args.sheet)

    # --- Нормализация названий платформ ---
    platform_map = {
        "Google Maps": "GoogleMaps",
        "Yandex Maps": "YandexMaps",
    }
    df["source_platform"] = df["source_platform"].astype(str).str.strip().replace(platform_map)

    # --- Убираем возможную строку-заголовок, попавшую в данные ---
    for col in ["case", "source_platform", "stance", "has_text"]:
        df = df[df[col].astype(str).str.lower() != col]

    # --- Нормализация theme_codes ---
    if "theme_codes" in df.columns:
        df["theme_codes"] = (
            df["theme_codes"]
            .astype(str)
            .fillna("")
            .str.strip()
            .str.replace(r"[,\s/]+", ";", regex=True)  # запятая/пробел/слеш → ;
            .str.replace(r";{2,}", ";", regex=True)    # убираем повторные ;;
            .str.strip(";")
            .str.lower()                               # всё в нижний регистр
        )

    # ===== Schema validation =====
    schema_report = validate_schema(df.copy())

    # Стоп, если не хватает критичных столбцов
    critical_missing = [c for c in ["case", "source_platform", "stance"] if c in schema_report.get("missing_required", [])]
    if critical_missing:
        print("ОШИБКА: отсутствуют критически важные столбцы:", ", ".join(critical_missing))
        print("Пожалуйста, поправь файл и запусти скрипт снова.")
        return

    # Отчёт по пропускам и пустым text
    missing_cols = schema_report.get("missing_required", [])
    value_issues = schema_report.get("value_issues", {})

    # гарантируем наличие некоторых столбцов (если нет — создадим пустые, чтобы не падать на сводках)
    for col in REQUIRED_COLS:
        if col not in df.columns:
            df[col] = pd.NA

    # ===== Приведение типов в ОСНОВНОМ df =====
    df["case"] = df["case"].astype(str)
    df["source_platform"] = df["source_platform"].astype(str)
    df["stance"] = df["stance"].astype(str).str.lower()
    df["has_text"] = df["has_text"].astype(str).str.lower()
    df["rating_1_5"] = pd.to_numeric(df["rating_1_5"], errors="coerce").astype("Int64")

    # Пустые тексты (включая NaN/пустые строки)
    text_empty = df["text"].isna() | (df["text"].astype(str).str.strip() == "")


    # ===== Возможные дубликаты =====
    dup_cols = ["case", "source_platform", "text", "rating_1_5"]
    dup_key = df[dup_cols].astype(str).agg("||".join, axis=1)
    dup_counts = dup_key.value_counts()
    possible_dups = dup_counts[dup_counts > 1]
    top10_dups = possible_dups.head(10)

    # ===== Нормализация дат =====
    df["post_date_norm"] = df["post_date"].apply(parse_post_date)
    not_norm_share = float(df["post_date_norm"].isna().mean())
    bad_dates_csv = os.path.join(args.outdir, "bad_post_dates.csv")
    df.loc[df["post_date_norm"].isna(), ["case", "source_platform", "post_date"]].to_csv(bad_dates_csv, index=False)

    # === Дополнительно: распределения по годам и месяцам ===
    df["year"] = df["post_date_norm"].dt.year
    df["ym"]   = df["post_date_norm"].dt.to_period("M").astype(str)
    df["year"].value_counts().sort_index().to_csv(os.path.join(args.outdir, "counts_by_year.csv"))
    df["ym"].value_counts().sort_index().to_csv(os.path.join(args.outdir, "counts_by_year_month.csv"))

    # Явно задаём типы для текстовых колонок (чтобы pyarrow не путался)
    for col in ["text", "theme_codes", "pulled_by", "post_date"]:
        df[col] = df[col].astype("string")  # pandas StringDtype

    # На всякий — ключевые категориальные тоже в строковый тип (уже были, но точнее)
    for col in ["case", "source_platform", "stance", "has_text"]:
        df[col] = df[col].astype("string")


    # ===== Сохранение «чистого» слоя =====
    clean_path_parquet = os.path.join(args.outdir, "reviews_master_clean.parquet")
    clean_path_feather = os.path.join(args.outdir, "reviews_master_clean.feather")
    df.to_parquet(clean_path_parquet, index=False)
    df.to_feather(clean_path_feather)

    # ===== Отчёты/сводки =====
    total = len(df)
    crosstab_case_platform = pd.crosstab(df["case"], df["source_platform"], dropna=False)

    has_text_counts = df["has_text"].value_counts(dropna=False)
    stance_overall = df["stance"].value_counts(dropna=False)
    stance_by_platform = pd.crosstab(df["source_platform"], df["stance"], dropna=False)
    stance_by_case = pd.crosstab(df["case"], df["stance"], dropna=False)

    # Гистограмма рейтинга (если есть)
    rating_hist_path = os.path.join(args.outdir, "hist_rating_1_5.png")
    if "rating_1_5" in df.columns:
        plt.figure()
        df["rating_1_5"].dropna().astype(int).plot(kind="hist", bins=[1,2,3,4,5,6], rwidth=0.9)
        plt.title("Histogram: rating_1_5")
        plt.xlabel("Rating (1-5)")
        plt.ylabel("Count")
        plt.savefig(rating_hist_path, dpi=200, bbox_inches="tight")
        plt.close()

    # ===== Стартовые выгрузки =====
    counts_by_case_platform = crosstab_case_platform.copy()
    counts_by_case_platform.to_csv(os.path.join(args.outdir, "counts_by_case_platform.csv"))

    stance_by_case_platform = pd.crosstab([df["case"], df["source_platform"]], df["stance"], dropna=False)
    stance_by_case_platform.to_csv(os.path.join(args.outdir, "stance_by_case_platform.csv"))

    # theme_codes — уникальные теги одной строкой
    theme_codes_path = os.path.join(args.outdir, "theme_codes_raw.txt")
    if "theme_codes" in df.columns:
        # собираем все теги, фильтруем пустые
        all_codes = []
        for s in df["theme_codes"].fillna(""):
            s = str(s).strip()
            if s:
                all_codes.extend([t.strip() for t in s.split(";") if t.strip()])
        uniq = sorted(set(all_codes))
        with open(theme_codes_path, "w", encoding="utf-8") as f:
            f.write(";".join(uniq))
    else:
        with open(theme_codes_path, "w", encoding="utf-8") as f:
            f.write("")

    # ===== Простые графики по stance =====
    bar1_path = os.path.join(args.outdir, "bar_stance_by_case.png")
    plt.figure()
    stance_by_case.plot(kind="bar")
    plt.title("Stance by CASE")
    plt.xlabel("case")
    plt.ylabel("count")
    plt.legend(title="stance")
    plt.tight_layout()
    plt.savefig(bar1_path, dpi=200)
    plt.close()

    bar2_path = os.path.join(args.outdir, "bar_stance_by_platform.png")
    plt.figure()
    stance_by_platform.plot(kind="bar")
    plt.title("Stance by SOURCE PLATFORM")
    plt.xlabel("source_platform")
    plt.ylabel("count")
    plt.legend(title="stance")
    plt.tight_layout()
    plt.savefig(bar2_path, dpi=200)
    plt.close()

    # ===== Отчёт в консоль =====
    print("\n=== САММАРИ ===")
    print(f"Всего отзывов: {total}")
    print("\nРазбивка по case × source_platform:")
    print(crosstab_case_platform)

    print("\nДоли has_text (yes/no):")
    print((has_text_counts / total).round(3))

    print("\nРаспределение stance (в целом):")
    print(stance_overall)

    print("\nРаспределение stance по платформам:")
    print(stance_by_platform)

    print("\nРаспределение stance по кейсам:")
    print(stance_by_case)

    print(f"\nДоля записей с НЕ нормализованной датой (post_date_norm is NaT): {not_norm_share:.3%}")

    if missing_cols:
        print("\nОтсутствуют обязательные столбцы (не критично, но учти):", ", ".join(missing_cols))

    if value_issues:
        print("\nПодозрительные значения в полях:")
        for k, v in value_issues.items():
            print(f"- {k}: {v}")

    print("\nПустые 'text' (включая NaN/пустые строки):", int(text_empty.sum()), "из", total)

    if not possible_dups.empty:
        print("\nВозможные дубликаты (по ключу case+source_platform+text+rating_1_5): всего групп =", len(possible_dups))
        print("Топ-10 повторов:")
        print(top10_dups)
    else:
        print("\nВозможных дубликатов по ключу не найдено.")

    # Проверка формата theme_codes
    ok_theme = normalize_theme_codes_format(df)
    bad_theme_n = int((~ok_theme).sum())
    if bad_theme_n > 0:
        print(f"\nВНИМАНИЕ: {bad_theme_n} строк с некорректным форматом theme_codes (разрешены a-z, 0-9, '_', ';' без пробелов).")

    # Вывод путей
    print("\n=== ФАЙЛЫ СОХРАНЕНЫ В ===")
    print(args.outdir)
    print(" - reviews_master_clean.parquet")
    print(" - reviews_master_clean.feather")
    print(" - counts_by_case_platform.csv")
    print(" - stance_by_case_platform.csv")
    print(" - theme_codes_raw.txt")
    print(" - hist_rating_1_5.png (если rating присутствует)")
    print(" - bar_stance_by_case.png")
    print(" - bar_stance_by_platform.png")


if __name__ == "__main__":
    main()
