# forage

Minimal Shiny app to code open-ended survey responses with a theme list.

## Run locally

1. Install packages (if needed):

```
install.packages(c("shiny", "dotenv", "readr", "readxl", "writexl", "tibble", "dplyr", "purrr", "stringr", "httr2", "rlang"))
```

2. Set your OpenAI API key:

```
Sys.setenv(OPENAI_API_KEY = "your_api_key")
```

Or create a local `.env` file in the project root:

```
OPENAI_API_KEY="your_api_key"
```

3. Start the app:

```
shiny::runApp()
```

## Files

- `app.R` Shiny UI to upload data, optionally generate themes, and download coded output as Excel.
- `Functions/code_gpt.R` and `Functions/theme_gpt.R` accept data frames or `.csv`/`.xlsx`/`.xls` file paths.