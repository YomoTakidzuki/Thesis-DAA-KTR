import pandas as pd

# Load the Excel file (worksheet: "Discourse")
df = pd.read_excel("../archival_log_template.xlsx", sheet_name="Discourse")

def clean_code(code):
    # Clean and normalize theme code strings.
    # Removes extra spaces, Unicode separators, and normalizes delimiters.
    if pd.isna(code):
        return code
    return (
        str(code)
        .lower()                       # convert to lowercase for consistency
        .strip()                       # remove leading/trailing spaces
        .replace(" ", "")              # remove regular spaces
        .replace("\u00A0", "")         # remove non-breaking spaces
        .replace("\u200b", "")         # remove zero-width spaces
        .replace("；", ";")            # replace full-width (CJK) semicolon with normal one
        .replace(",", ";")             # replace commas with semicolons
    )

# Apply cleaning function to the 'theme_codes' column
df["theme_codes"] = df["theme_codes"].apply(clean_code)

# Save the cleaned DataFrame to a new Excel file
df.to_excel("archival_log_template_FIXED.xlsx", sheet_name="Discourse", index=False)

# Confirmation message for the user
print("Done: file 'archival_log_template_FIXED.xlsx' has been created")
