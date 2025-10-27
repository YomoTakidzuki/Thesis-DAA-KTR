import pandas as pd

# Загружаем Excel
df = pd.read_excel("../archival_log_template.xlsx", sheet_name="Discourse")

def clean_code(code):
    if pd.isna(code):
        return code
    return (
        str(code)
        .lower()                       # всё в нижний регистр
        .strip()                       # убираем пробелы в начале/конце
        .replace(" ", "")              # обычные пробелы
        .replace("\u00A0", "")         # неразрывные пробелы
        .replace("\u200b", "")         # zero-width пробелы
        .replace("；", ";")            # китайский/юникодный «;»
        .replace(",", ";")             # если вдруг где-то была запятая
    )

# Чистим колонку
df["theme_codes"] = df["theme_codes"].apply(clean_code)

# Сохраняем новый Excel
df.to_excel("archival_log_template_FIXED.xlsx", sheet_name="Discourse", index=False)

print("✅ Готово: файл archival_log_template_FIXED.xlsx создан")
